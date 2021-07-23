const fs = require('fs').promises;
const {constants: fs_constants} = require('fs');
const path = require('path');
const http = require('http');
const Docker = require('dockerode');
//const compose = require('docker-compose');
const internetAvailable = require('internet-available');
const {dialog} = require('electron');
const {tl} = require('@am5/translate');
const {app, session, BrowserWindow, Menu, shell} = require('electron');
const {ipcMain} = require('electron');
const Store = require('electron-store');
const exitHook = require('exit-hook');
const {getSchema} = require('@am5/controller/state_schema.js');
const isDev = !app.isPackaged;
const isMac = process.platform === 'darwin';
const prompt = require('electron-dynamic-prompt');
const streamToPromise = require('stream-to-promise');

class Controller {
  constructor(opt) {
    const ctr = this;
    if (opt) {
      ctr.initState(opt);
    }
  }

  /**
   * State config
   */
  setState(key, value) {
    const ctr = this;
    ctr._state.set(key, value);
  }

  getState(key) {
    const ctr = this;
    return ctr._state.get(key);
  }

  async initState(state) {
    const ctr = this;
    try {
      if (ctr._state) {
        return;
      }
      const schema = await getSchema();
      ctr._state = new Store({schema});
      ctr._state.store = Object.assign({}, ctr._state.store, state);
    } catch (e) {
      ctr.dialogShowError(e);
    }
  }

  async dialogDataLoc() {
    const ctr = this;
    const language = ctr.getState('language');
    let dataLoc = null;
    const choice = await dialog.showMessageBox(ctr._mainWindow, {
      type: 'question',
      buttons: [
        tl('data_loc_opt_docker_volume', language),
        tl('data_loc_opt_directory', language)
      ],
      title: tl('data_loc_options_title', language),
      message: tl('data_loc_options', language),
      defaultId: 0
    });

    switch (choice.response) {
      case 1:
        ctr.log('choice is select folder');
        let resp = await dialog.showOpenDialog({
          properties: ['openDirectory']
        });
        dataLoc = resp.filePaths[0];
        break;
      case 0:
        ctr.log('choice is docker volume');
        dataLoc = ctr.getState('docker_volume');
        break;
      default:
        ctr.log('choice is default');
        dataLoc = ctr.getState('docker_volume');
    }

    const writable = await ctr.testLoc(dataLoc);

    ctr.log('Selected path', dataLoc, 'writable', writable);

    if (!writable) {
      /**
       * Relaunch script
       */
      await ctr.dialogDataLoc();
    } else {
      return dataLoc;
    }
  }

  async initContainer() {
    const ctr = this;
    const tag = ctr.getRepoTag();
    const port_guest = ctr.getState('port_guest');
    const port_host = ctr.getState('port_host');
    const name = ctr.getState('container_name');
    const volume = ctr.getState('data_location');
    const volumeTmp =  ctr.getState('docker_volume_tmp');
    const dbgrass = ctr.getState('grass_db_location');
    const optBindPort = {};
    const optExposedPort = {};
    optBindPort[`${port_guest}/tcp`] = [{HostPort: `${port_host}`}];
    optExposedPort[`${port_guest}/tcp`] = {};
    await ctr.cleanAllContainers();
    const container = await ctr._docker.createContainer({
      name: name,
      Image: tag,
      ExposedPorts: optExposedPort,
      HostConfig: {
        PortBindings: optBindPort,
        AutoRemove: true,
        Binds: [
          '/var/run/docker.sock:/var/run/docker.sock',
          `${volume}:${dbgrass}`,
          `${volumeTmp}:/tmp`
        ]
      }
    });
    return container;
  }

  async workerRun(opt) {
    opt = Object.assign({}, {binds: [], cmd: ['ls']}, opt);
    const ctr = this;
    try {
      const tag = ctr.getRepoTag();
      const data = await ctr._docker.run(
        tag,
        opt.cmd,
        [process.stdout, process.stderr],
        {
          Tty: false,
          HostConfig: {
            AutoRemove: true,
            Cmd: opt.cmd,
            Binds: opt.binds
          }
        }
      );
      return data[0];
    } catch (e) {
      ctr.dialogShowError(e);
    }
  }

  async importProject(path, name) {
    const ctr = this;
    try {
      const volume = ctr.getState('data_location');
      const dbgrass = ctr.getState('grass_db_location');
      const archivePath = ctr.randomString(`/tmp/import_${name}_`, '.zip');
      const res = await ctr.workerRun({
        binds: [`${volume}:${dbgrass}`, `${path}:${archivePath}`],
        cmd: [
          'sh',
          '/app/sh/import_project_archive.sh',
          name,
          archivePath,
          dbgrass
        ]
      });
      return res;
    } catch (e) {
      ctr.dialogShowError(e);
    }
  }

  async cleanAllContainers() {
    const ctr = this;
    try {
      const name = ctr.getState('container_name');
      const refName = {};
      refName[name] = true;
      const containerOld = await ctr._docker.listContainers({
        filters: {name: refName}
      });
      for (let c of containerOld) {
        const cOld = await ctr._docker.getContainer(c.Id);
        console.log('Remove old container', c.Id);
        if (cOld) {
          await cOld.stop();
          await cOld.remove();
        }
      }
    } catch (e) {
      ctr.log('Error removing containers', e);
    }
  }

  async initDockerVolumes() {
    const ctr = this;
    const app_name = ctr.getState('app_name');
    const volumes = [
      ctr.getState('docker_volume'),
      ctr.getState('docker_volume_tmp')
    ];
    for (let v of volumes) {
      const ref = {};
      ref[v] = true;
      const res = await ctr._docker.listVolumes({filters: {name: ref}});
      if (!res.Volumes.length) {
        /**
         * Missing volume, create it
         */
        await ctr._docker.createVolume({
          name: v,
          labels: {app: app_name}
        });
      }
    }
  }

  async initDataLocation(config) {
    const ctr = this;
    config = Object.assign({}, {reset: false}, config);
    let dataLoc = ctr.getState('data_location');

    if (!dataLoc) {
      dataLoc = ctr.getState('docker_volume');
      ctr.setState('data_location', dataLoc);
    }

    const writable = await ctr.testLoc(dataLoc);

    if (!writable || config.reset === true) {
      dataLoc = await ctr.dialogDataLoc();
    }
    await ctr.updateDataLocation(dataLoc);
  }

  /**
   * Test data loc path (grass db files) and save in state
   * @param {String} dataLoc Path to data location folder or volume;
   */
  async updateDataLocation(dataLoc) {
    const ctr = this;
    const writable = await ctr.testLoc(dataLoc);
    if (writable) {
      const oldLoc = ctr.getState('data_location');
      if (dataLoc != oldLoc) {
        ctr.setState('data_location', dataLoc);
        await ctr.restart();
      }
    } else {
      throw new Error('Unexpected non-writable location');
    }
  }

  /**
   * Test data loc path
   * @param {String} loc Path to data location folder or volume
   * @return {Promise<Boolean>} Valid path or id
   */
  async testLoc(loc) {
    const ctr = this;
    const dockVol = ctr.getState('docker_volume');
    if (loc === dockVol) {
      return true;
    } else {
      return await ctr.checkPathWritable(loc);
    }
  }
  async checkPathWritable(path) {
    try {
      await fs.access(path, fs_constants.W_OK);
      return true;
    } catch (e) {
      return false;
    }
  }
  async checkPathExists(path) {
    try {
      await fs.access(path, fs_constants.F_OK);
      return true;
    } catch (e) {
      return false;
    }
  }
  /**
   * Init, set options
   */
  async init(opt) {
    const ctr = this;
    if (ctr._init) {
      return;
    }
    ctr._init = true;

    if (opt) {
      ctr.initState(opt);
    }

    /**
     * Cleanup auto
     * -> do not use process.exit
     */
    exitHook(() => {
      ctr.stopSync();
    });

    /**
     * Create browser window
     */
    app.on('ready', () => {
      ctr.log('Create window');
      ctr.createWindow();
    });
    app.on('activate', () => {
      ctr.log('Activate');
      ctr.createWindow();
    });

    app.on('window-all-closed', () => {
      ctr.log('All closed');
      app.quit();
    });

    ipcMain.once('ready', async () => {
      ctr.log('Browser ready');
      await ctr.start();
    });

    ipcMain.handle('request', ctr.handleRequest.bind(ctr));
  }

  createWindow() {
    const ctr = this;

    if (ctr._mainWindow) {
      return;
    }

    ctr._mainWindow = new BrowserWindow({
      width: 1200,
      height: 800,
      backgroundColor: '#fff',
      webPreferences: {
        nodeIntegration: false,
        preload: path.join(__dirname, 'preload.js')
      }
    });
    ctr.showPage('splash.html');

    /**
     * Load navigation link in external window
     */

    ctr._mainWindow.webContents.on('will-navigate', (e, url) => {
      if (url !== e.sender.getURL()) {
        e.preventDefault();
        shell.openExternal(url);
      }
    });

    ctr._mainWindow.maximize();
    if (isDev) {
      ctr._mainWindow.openDevTools();
    }

    ctr._mainWindow.on('close', (e) => {
      if (isDev) {
        //return;
      }
      const choice = dialog.showMessageBoxSync(ctr._mainWindow, {
        type: 'question',
        buttons: ['Yes', 'No'],
        title: 'Confirm',
        message: 'Are you sure you want to quit?'
      });
      if (choice === 1) {
        e.preventDefault();
      }
    });

    ctr._mainWindow.on('closed', () => {
      setTimeout(() => {
        // fired before messages are sent. Bug ? -> set timeout as workaround.
        ctr._mainWindow = null;
      }, 1000);
      app.quit();
    });

    /**
     * Custom menu (none)
     */
    const menu = Menu.buildFromTemplate([
      ...(isMac
        ? [
            {
              label: app.name,
              submenu: [
                {role: 'about'},
                {type: 'separator'},
                {role: 'services'},
                {type: 'separator'},
                {role: 'hide'},
                {role: 'hideothers'},
                {role: 'unhide'},
                {type: 'separator'},
                {role: 'quit'}
              ]
            }
          ]
        : []),
      {
        label: 'File',
        submenu: [isMac ? {role: 'close'} : {role: 'quit'}]
      },
      {
        label: 'Project',
        submenu: [
          {
            label: 'Direct import...',
            click: ctr.dialogProjectUpload.bind(ctr)
          }
        ]
      },
      {
        label: 'View',
        submenu: [
          {role: 'reload'},
          {role: 'forceReload'},
          {role: 'toggleDevTools'},
          {type: 'separator'},
          {role: 'resetZoom'},
          {role: 'zoomIn'},
          {role: 'zoomOut'},
          {type: 'separator'},
          {role: 'togglefullscreen'}
        ]
      },
      {
        label: 'Window',
        submenu: [
          {role: 'minimize'},
          {role: 'zoom'},
          ...(isMac
            ? [
                {type: 'separator'},
                {role: 'front'},
                {type: 'separator'},
                {role: 'window'}
              ]
            : [{role: 'close'}])
        ]
      },
      {
        label: 'Edit',
        submenu: [
          {label: 'Undo', accelerator: 'CmdOrCtrl+Z', selector: 'undo:'},
          {label: 'Redo', accelerator: 'Shift+CmdOrCtrl+Z', selector: 'redo:'},
          {type: 'separator'},
          {label: 'Cut', accelerator: 'CmdOrCtrl+X', selector: 'cut:'},
          {label: 'Copy', accelerator: 'CmdOrCtrl+C', selector: 'copy:'},
          {label: 'Paste', accelerator: 'CmdOrCtrl+V', selector: 'paste:'},
          {
            label: 'Select All',
            accelerator: 'CmdOrCtrl+A',
            selector: 'selectAll:'
          }
        ]
      }
    ]);

    Menu.setApplicationMenu(menu);
  }

  /**
   * Handle request
   */
  async handleRequest(e, config) {
    const ctr = this;
    let result = null;
    if (!e || !config || !config.type || !config.data) {
      return;
    }
    const d = config.data;
    switch (config.type) {
      case 'set_state':
        ctr.setState(d.key, d.value);
        result = ctr.getState(d.key);
        break;
      case 'get_state':
        result = ctr.getState(d.key);
        break;
      case 'set_version':
        ctr.log(d.version);
        result = await ctr.setVersion(d.version);
        break;
      case 'list_versions':
        result = await ctr.listVersions();
        break;
      case 'dialog_data_location':
        ctr.log('');
        result = await ctr.initDataLocation({reset: true});
        break;
      default:
        ctr.log('Unknown command', config);
    }
    return result;
  }

  /**
   * Stop docker
   *
   *  ASYNC NOT SUPPORTED BY exitHook
   *  All methods should by sync
   *
   */
  stopSync() {
    const ctr = this;
    try {
      ctr.setState('stopped', true);
      ctr.showPage('splash.html');
      ctr.sendMessageCodeClient('msg-info', 'stop');
      ctr.cleanAllContainers(); // ⚠️ ASYNC code in sync function..
    } catch (e) {
      ctr.dialogShowError(e);
    }
  }

  /**
   * Restart
   */
  async restart() {
    const ctr = this;
    try {
      app.relaunch();
      app.exit();
    } catch (e) {
      ctr.dialogShowError(e);
    }
  }
  async reload() {
    const ctr = this;
    try {
      ctr._mainWindow.reload();
    } catch (e) {
      ctr.dialogShowError(e);
    }
  }

  /**
   * Start docker
   */
  async start() {
    const ctr = this;
    const language = ctr.getState('language');
    try {
      ctr.log('language', language);
      ctr.log({msg: tl('loading_docker', language)});
      ctr.clearCache();
      ctr.setState('stopped', false);
      ctr.sendMessageCodeClient('msg-info', 'start');
      await ctr.wait(1000);
      /**
       * Link docker-compose and docker
       */
      ctr._docker = await ctr.initDocker();

      /**
       * If no docker instance, msg
       */
      if (!ctr._docker) {
        ctr.log('No docker');
        const msgNoDocker = tl('no_docker', language, {
          link: 'https://docs.docker.com/get-docker'
        });
        ctr.sendMessageCodeClient('msg-info', msgNoDocker);
        return;
      }
      /**
       * Test for network
       */
      const hasNet = await ctr.hasInternet();
      ctr.setState('offline', !hasNet);
      if (!hasNet) {
        ctr.sendMessageCodeClient('msg-info', 'offline');
        await ctr.wait(1000);
      }

      /**
       * Test if docker image exists
       */
      const hasV = await ctr.hasVersion();
      if (!hasV) {
        ctr.sendMessageCodeClient('msg-info', 'docker_load_file');
        await ctr.wait(2000);
        await ctr.loadImage();
      }

      if (hasNet && !isDev) {
        ctr.sendMessageCodeClient('msg-info', 'docker_check_for_update');
        await ctr.wait(2000);
        await ctr.updateLatest();
      }

      ctr.sendMessageCodeClient('msg-info', 'data_loc_check');
      await ctr.initDockerVolumes();
      await ctr.initDataLocation();

      ctr.sendMessageCodeClient('msg-info', 'loading_docker');

      ctr._container = await ctr.initContainer();
      await ctr._container.start();
      await ctr.waitForReady();

      ctr.sendMessageCodeClient('msg-info', 'loaded_docker');

      await ctr.wait(500);
      /**
       * Connect / load page
       */
      ctr.showPage('app');
    } catch (e) {
      ctr.dialogShowError(e);
    }
  }

  /**
   * Clear session cache
   */
  async clearCache() {
    const ses = session.defaultSession;
    /*
     * HTTP cache
     */

    await ses.clearCache();
    /**
     * Data storage :
     * -> appcache, cookies, filesystem, indexdb, localstorage, shadercache, websql, serviceworkers, cachestorage.
     * -> https://www.electronjs.org/docs/api/session#sesclearstoragedataoptions
     */
    if (0) {
      await ses.clearStorageData();
    }
  }

  /**
   * Display a default page, by name / id
   * @param {String} name Page name (in pages folder)
   */
  showPage(name) {
    const ctr = this;
    if (!ctr._mainWindow || ctr._mainWindow.isDestroyed()) {
      return;
    }
    switch (name) {
      case 'app':
        const url = ctr.getState('url_guest');
        const port = ctr.getState('port_host');
        ctr._mainWindow.loadURL(`${url}:${port}`);
        break;
      default:
        ctr._mainWindow.loadURL(`file://${__dirname}/pages/${name}`);
    }
  }

  /**
   * Messages
   * handled in preload.js
   */
  log(...msg) {
    const ctr = this;
    try {
      if (ctr._mainWindow && !ctr._mainWindow.isDestroyed()) {
        ctr._mainWindow.webContents.send('msg-log', msg);
      }
      if (isDev) {
        console.log(msg);
      }
    } catch (e) {
      ctr.dialogShowError(e);
    }
  }

  sendMessageClient(type, msg) {
    const ctr = this;
    try {
      if (!ctr._mainWindow || ctr._mainWindow.isDestroyed()) {
        return;
      }
      ctr._mainWindow.webContents.send(type || 'msg-info', msg || '');
    } catch (e) {
      ctr.dialogShowError(e);
    }
  }

  sendMessageCodeClient(type, code, data) {
    const ctr = this;
    try {
      const language = ctr.getState('language');
      const msg = tl(code, language, data);
      ctr.sendMessageClient(type, msg);
    } catch (e) {
      ctr.dialogShowError(e);
    }
  }

  dialogShowError(e) {
    const ctr = this;
    const language = ctr.getState('language');
    try {
      if (!ctr._mainWindow || ctr._mainWindow.isDestroyed()) {
        console.error(e);
        return;
      }
      dialog.showMessageBoxSync(ctr._mainWindow, {
        type: 'error',
        title: tl('error_dialog_title', language),
        message: tl('error_generic', language, {
          err: e.err || e.message || e
        })
      });
    } catch (e) {
      console.error('dialogShowError failed', e);
    }
  }

  /**
   * waiting
   */
  wait(n) {
    const ctr = this;
    return new Promise((resolve) => {
      ctr.log('wait', n);
      setTimeout(() => {
        resolve(true);
      }, n || 5000);
    });
  }

  async waitForReady() {
    const ctr = this;
    const ok = await ctr.isReady();
    if (ok) {
      ctr.log('Ready!');
      return true;
    }
    await ctr.wait(1000);
    await ctr.waitForReady();
  }

  async isReady() {
    const ctr = this;
    let ready;
    ready = ctr.getState('listening', false);
    if (ready) {
      return true;
    }
    const log = await ctr.getLogs();
    const port = ctr.getState('port_guest');
    /**
     * Look for log with 'Listening on http://0.0.0.0:3434' in it
     */
    const rexp = new RegExp(`0\\.0\\.0\\.0\\:${port}`);
    ready = !!log.match(rexp);
    if (isDev) {
      ctr.log('ready test logs', log);
    }
    return ready;
  }

  /**
   * Get container logs
   */

  async getLogs() {
    const ctr = this;
    const logBuf = await ctr._container.logs({
      stdout: true,
      stderr: true,
      follow: false
    });
    return ctr.dockerBufferToString(logBuf);
  }

  /**
   * Remove control characters, not properly formated
   * eg. "\u0002\u0000\u0000\u0000\u0000\u0000\u0000!Attaching package: ‘R.utils’"
   * -> Attaching package: ‘R.utils'
   * NOTE: this is probably linked to how docker encode streams:
   * See https://docs.docker.com/engine/api/v1.37/#operation/ContainerAttach
   * -> this add 8 first bit as header. As it happen to each line, we can't just
   * remove the first 8 bit in most case.
   */
  dockerBufferToString(buffer) {
    return buffer
      .filter((b) => ![0, 1, 2, 3, 14, 17, 20, 23, 26, 27, 30, 31].includes(b))
      .toString('utf8');
  }

  /**
   * Projects helpers
   */
  async getProjectsList() {
    const ctr = this;
    const dbPath = ctr.getState('grass_db_location');
    const opt = {
      Cmd: ['ls', dbPath],
      Cmd: [
        'Rscript',
        '-e',
        `library(jsonlite);print(toJSON(list.files('${dbPath}')))`
      ],
      AttachStdout: true,
      AttachStderr: true
    };
    const cmdExec = await ctr._container.exec(opt);
    const stream = await cmdExec.start();
    const buf = await streamToPromise(stream);
    const str = ctr.dockerBufferToString(buf);
    const strClean = str.match(/\[.*\]/g);
    if (!strClean) {
      return [];
    }
    const data = JSON.parse(strClean[0]);
    return data;
  }

  async projectExists(name) {
    const ctr = this;
    const projectList = await ctr.getProjectsList();
    return projectList.includes(name);
  }

  async dialogProjectUpload() {
    const ctr = this;
    try {
      const projectName = await prompt(
        {
          modal: true,
          title: 'New project imporation',
          header: 'Project direct import',
          description:
            'This tool will load an archived project (*.am5p ) into the database. It should be faster and more reliable than the classic uploader, especially for large projects.\nThe name should have at least 4 alphanumeric characters and start with a letter. No special characters allowed except underscore.',
          height: 400,
          fields: [
            {
              id: 'pname',
              label: 'Project name',
              type: 'input',
              attrs: {
                placeholder: 'Name',
                required: true
              }
            }
          ],
          validator: async (args) => {
            const exists = await ctr.projectExists(args.pname);
            if (exists) {
              throw new Error('Project already exists');
            }
            const valid =
              args.pname.length > 3 &&
              args.pname.match(/^[a-zA-Z]/) &&
              !args.pname.match(/[^\w_]+/);
            if (!valid) {
              throw new Error('Name invalid');
            }
          }
        }
        //ctr._mainWindow
      );

      if (!projectName) {
        return;
      }

      const projectFiles = await dialog.showOpenDialog(ctr._mainWindow, {
        title: 'Select project file',
        properties: ['openFile'],
        filters: [
          {
            name: 'AccessMod Archive Project',
            extensions: ['am5p']
          }
        ]
      });

      if (!projectFiles || projectFiles.canceled) {
        return;
      }
      /**
       * Import
       */
      const file = projectFiles.filePaths[0];
      const fileBaseName = path.basename(file);
      const fileStat = await fs.stat(file);
      const fileSize = Math.round(fileStat.size / (1024 * 1024));
      const large = fileSize > 900;
      const confirmImport = await dialog.showMessageBox(ctr._mainWindow, {
        type: 'question',
        buttons: ['Cancel', 'Import'],
        title: 'Confirmation',
        message: `
      Archive: '${fileBaseName}' ~ ${fileSize}MB \n
      Project: '${projectName.pname}' \n
      The importation should take less than ${large ? 5 : 1} minute${
          large ? 's' : ''
        }. A message will be displayed at the end of the process`,
        defaultId: 0
      });

      if (confirmImport.response === 0) {
        return;
      }

      const res = await ctr.importProject(
        projectFiles.filePaths[0],
        projectName.pname
      );

      if (res.StatusCode !== 0) {
        ctr.dialogShowError(
          `An error occured during importation: ${res.Error}`
        );
        return;
      }

      const confirmRestart = await dialog.showMessageBox(ctr._mainWindow, {
        type: 'question',
        buttons: ['Cancel', 'Reload'],
        title: 'Restart now ?',
        message: 'The project has been imported. Reload now?',
        defaultId: 0
      });

      if (confirmRestart.response === 0) {
        return;
      }
      ctr.reload();
    } catch (e) {
      ctr.dialogShowError(e);
    }
  }

  /*
   * Init docker
   * @return {Docker|Boolean} Docker instance or false
   */
  async initDocker() {
    const ctr = this;
    try {
      const socketPath =
        process.platform === 'win32'
          ? '//./pipe/docker_engine'
          : '/var/run/docker.sock';

      const isPathOk = await ctr.testSocket(socketPath);
      ctr.log({isPathOk, socketPath});
      if (!isPathOk) {
        throw new Error(`SocketPath can't be reached`);
      }
      const docker = new Docker({
        socketPath: socketPath
      });
      ctr.log('Has docker', !!docker);
      return docker;
    } catch (e) {
      return false;
    }
  }

  async hasVersion(version) {
    const ctr = this;
    const tag = ctr.getRepoTag(version);
    const versions = await ctr.listRepoTags();
    return versions.length > 0 && versions.includes(tag);
  }

  getRepoTag(tag) {
    const ctr = this;
    const version = tag || ctr.getState('version');
    const image_name = ctr.getState('image_name');
    return `${image_name}:${version}`;
  }

  async listRepoTags() {
    const ctr = this;
    const imgs = await ctr.listImages();
    const tags = [];
    for (let img of imgs) {
      if (img.RepoTags) {
        tags.push(...img.RepoTags);
      }
    }
    return tags;
  }

  async listVersions() {
    const ctr = this;
    const rTag = await ctr.listRepoTags();
    const versions = rTag.map((r) => r.split(':')[1]);
    console.log(versions);
    return versions;
  }

  async listImages() {
    const ctr = this;
    const image_name = ctr.getState('image_name');
    const ref = {};
    ref[image_name] = true;

    const imgs = await ctr._docker.listImages({
      filters: {reference: ref}
    });
    return imgs;
  }

  async setVersion(version) {
    const ctr = this;
    const hasVersion = ctr.hasVersion(version);
    if (hasVersion) {
      ctr.setState('version', version);
      await ctr.restart();
    }
  }

  async loadImage() {
    const ctr = this;
    const imagePath = ctr.getState('image_path');
    await ctr._docker.loadImage(imagePath);
  }

  async updateLatest() {
    const ctr = this;
    try {
      const latest = ctr.getRepoTag('latest');
      await ctr._docker.pull(latest);
      return true;
    } catch (e) {
      return false;
    }
  }

  /**
   * Network
   */
  async hasInternet() {
    try {
      await internetAvailable({
        timeout: 2000,
        retries: 3
      });
      return true;
    } catch (r) {
      return false;
    }
  }

  testSocket(path) {
    return new Promise((resolve) => {
      http
        .get({socketPath: path}, () => {
          resolve(true);
        })
        .on('error', () => resolve(false));
    });
  }

  /**
   * Misc helpers;
   */

  randomString(prefix, suffix) {
    if (!prefix) {
      prefix = '';
    }
    if (!suffix) {
      suffix = '';
    }
    const r = Math.random()
      .toString(32)
      .split('.')[1];
    return prefix + r + suffix;
  }
}

module.exports.Controller = Controller;
