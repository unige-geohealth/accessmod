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
    try {
      const ctr = this;
      if (ctr._state) {
        return;
      }
      const schema = await getSchema();
      ctr._state = new Store({schema});
      ctr._state.store = Object.assign({}, ctr._state.store, state);
    } catch (e) {
      console.error(e);
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
          `${volume}:/data/dbgrass/`
        ]
      }
    });
    return container;
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
        if (cOld) {
          await cOld.stop();
          await cOld.remove();
        }
        console.log('Remove old container');
      }
    } catch (e) {
      ctr.log('Error removing containers', e);
    }
  }

  async initDockerVolume() {
    const ctr = this;
    const volume_name = ctr.getState('docker_volume');
    const app_name = ctr.getState('app_name');
    const ref = {};
    ref[volume_name] = true;
    const res = await ctr._docker.listVolumes({filters: {name: ref}});
    if (!res.Volumes.length) {
      /**
       * Missing volume, create it
       */
      await ctr._docker.createVolume({
        name: volume_name,
        labels: {app: app_name}
      });
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
   * @return {Boolean} Valid path or id
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
      if (ctr._mainWindow === null) {
        ctr.createWindow();
      }
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
    ctr._mainWindow.on('closed', () => {
      ctr._mainWindow = null;
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
    const language = ctr.getState('language');
    try {
      ctr.cleanAllContainers(); // ⚠️ ASYNC code in sync function..
      ctr.setState('stopped', true);
      ctr.showPage('splash.html');
      ctr.sendMessageCodeClient('msg-info', 'stop');
    } catch (err) {
      dialog.showMessageBox(ctr._mainWindow, {
        type: 'error',
        title: tl('error_dialog_title', language),
        message: err.message
      });
    }
  }

  /**
   * Restart
   * Alternative : app.relaunch() ?
   */
  async restart() {
    try {
      app.relaunch();
    } catch (e) {
      console.error(e);
    }
  }

  /**
   * Start docker
   */
  async start() {
    /*
     * ctr not in try/catch, used for messages
     */

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
      await ctr.initDockerVolume();
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
      dialog.showMessageBox(ctr._mainWindow, {
        type: 'error',
        title: tl('error_dialog_title', language),
        message: tl('error_generic', language, {
          err: e.err || e.message || e
        })
      });
      if (isDev) {
        throw new Error(e);
      }
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
    if (ctr._mainWindow) {
      ctr._mainWindow.webContents.send('msg-log', msg);
    }
    if (isDev) {
      console.log(msg);
    }
  }
  sendMessageClient(type, msg) {
    const ctr = this;
    ctr._mainWindow.webContents.send(type || 'msg-info', msg || '');
  }
  sendMessageCodeClient(type, code, data) {
    const ctr = this;
    const language = ctr.getState('language');
    const msg = tl(code, language, data);
    ctr.sendMessageClient(type, msg);
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
    const log = logBuf.toString('utf8');
    return log;
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
    return imgs.reduce((a, i) => {
      if (i.RepoTags) {
        a.push(...i.RepoTags);
      }
      return a;
    }, []);
  }

  async listVersions() {
    const ctr = this;
    const rTag = await ctr.listRepoTags();
    return rTag.map((r) => r.split(':')[1]);
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
}

module.exports.Controller = Controller;
