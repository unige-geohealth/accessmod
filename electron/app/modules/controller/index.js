const fs = require('fs').promises;
const {constants: fs_constants} = require('fs');
const path = require('path');
const http = require('http');
const YAML = require('yaml');
const Docker = require('dockerode');
const compose = require('docker-compose');
const internetAvailable = require('internet-available');
const {dialog} = require('electron');
const {spawnSync} = require('child_process');
const {tl} = require('@am5/translate');
const {app, BrowserWindow, Menu, shell} = require('electron');
const {ipcMain} = require('electron');
const Store = require('electron-store');
const exitHook = require('exit-hook');
const {getSchema} = require('@am5/controller/state_schema.js');
const isDev = !app.isPackaged;

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

  async initDataLocation(config) {
    const ctr = this;
    config = Object.assign({}, {reset: false}, config);
    const hasLoc = ctr.getState('data_location');
    const language = ctr.getState('language');
    if (!hasLoc || config.reset === true) {
      let dataLoc = null;
      let writable = false;
      const choice = await dialog.showMessageBox(ctr._mainWindow, {
        type: 'question',
        buttons: [
          tl('data_loc_opt_docker_volume', language),
          tl('data_loc_opt_directory', language),
          tl('data_loc_opt_app_data', language)
        ],
        title: tl('data_loc_options_title', language),
        message: tl('data_loc_options', language),
        defaultId: 0
      });

      console.log(choice);

      switch (choice.response) {
        case 2:
          console.log('choice is userdata');
          dataLoc = app.getPath('userData');
          break;
        case 1:
          console.log('choice is select folder');
          let resp = await dialog.showOpenDialog({
            properties: ['openDirectory']
          });
          dataLoc = resp.filePaths[0];
          break;
        case 0:
          console.log('choice is docker volume');
          dataLoc = 'dbgrass';
          break;
        default:
          console.log('choice is default');
          dataLoc = 'dbgrass';
      }

      if (dataLoc === 'dbgrass') {
        writable = true;
      } else {
        writable = await ctr.checkPathWritable(dataLoc);
        if (!writable) {
          await ctr.initDataLocation(config);
        }
      }

      console.log('data loc is', dataLoc, ' and writable', writable);

      if (writable) {
        ctr.setState('data_location', dataLoc);
      }
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
      console.log('App stopped');
      ctr.stop();
    });

    /**
     * Create browser window
     */
    app.on('ready', () => {
      console.log('Create window');
      ctr.createWindow();
    });
    app.on('activate', () => {
      console.log('Activate');
      if (ctr._mainWindow === null) {
        ctr.createWindow();
      }
    });

    app.on('window-all-closed', () => {
      console.log('All closed');
      app.quit();
    });

    ipcMain.once('ready', () => {
      console.log('Browser ready');
      ctr.start();
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
    if(isDev){
      ctr._mainWindow.openDevTools();
    }
    ctr._mainWindow.on('closed', () => {
      ctr._mainWindow = null;
      app.quit();
    });

    /**
     * Custom menu (none)
     */
    Menu.setApplicationMenu(new Menu());
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
        result = ctr.setVersion(d.version);
        break;
      default:
        console.log('Unknown command', config);
    }
    return result;
  }

  /**
   * ⚠️ Can't be async, exitHook does not support async :(
   */
  stop(config) {
    const ctr = this;
    config = Object.assign({}, {dialog: true}, config);
    console.log('Stop docker', !!ctr);
    try {
      let exit = true;
      if (config.dialog === true && !isDev) {
        const language = ctr.getState('language');
        const choice = dialog.showMessageBoxSync(ctr._mainWindow, {
          type: 'question',
          buttons: [
            tl('quit_and_close_docker', language),
            tl('quit_and_keep_docker', language)
          ],
          title: 'AccessMod',
          message: tl('quit_message', language),
          defaultId: 0
        });
        exit = choice === 0;
      }

      ctr.showPage('splash.html');
      ctr.sendMessageCodeClient('msg-info', 'stop');

      if (exit && !isDev) {
        spawnSync('docker-compose', ['down'], {
          cwd: ctr.getState('compose_folder')
        });
        return;
      }
    } catch (err) {
      dialog.showMessageBox(ctr._mainWindow, {
        type: 'error',
        title: tl('error_dialog_title', language),
        message: err.message
      });
    }
  }
  /**
   * Start docker
   */

  async start() {
    const ctr = this;
    const language = ctr.getState('language');
    console.log('language', language);
    console.log({msg: tl('loading_docker', language)});
    try {
      ctr.sendMessageCodeClient('msg-info', 'start');
      await ctr.wait(2000);

      /**
       * Link docker-compose and docker
       */
      ctr._compose = compose;
      ctr._docker = await ctr.initDocker();
      ctr.updateAccessModVersionsList();
      /**
       * If no docker instance, msg
       */

      if (!ctr._docker) {
        dialog.showMessageBox(ctr._mainWindow, {
          type: 'error',
          title: tl('error_dialog_title', language),
          message: tl('no_docker', language, {
            link: 'https://docs.docker.com/get-docker'
          })
        });
        return;
      }
      /**
       * Test for network
       */
      const hasNet = await ctr.hasInternet();
      if (!hasNet) {
        ctr.setState('offline', true);
        ctr.sendMessageCodeClient('msg-info', 'offline');
      }
      await ctr.wait(100);

      /**
       * Test if docker accessmod image exists
       */
      const hasAm = await ctr.hasAccessMod();

      if (!hasAm) {
        ctr.sendMessageCodeClient('msg-info', 'docker_load_file');
        await ctr.wait(2000);
        await ctr.importAccessMod();
      }

      if (hasNet) {
        /**
         * Dont wait, pull in background
         */

        ctr.updateAccessMod();
      }

      ctr.sendMessageCodeClient('msg-info', 'data_loc_check');
      await ctr.initDataLocation();
      ctr.sendMessageCodeClient('msg-info', 'loading_docker');
      await ctr.updateComposeFile();
      await ctr._compose.upAll({cwd: ctr.getState('compose_folder')});
      await ctr.waitForReady();
      ctr.sendMessageCodeClient('msg-info', 'loaded_docker');
      await ctr.wait(100);
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
    }
  }

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
   * Events
   */
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
    return new Promise((resolve) => {
      console.log('wait', n);
      setTimeout(() => {
        resolve(true);
      }, n || 5000);
    });
  }

  async waitForReady() {
    const ctr = this;
    const ok = await ctr.isReady();
    if (ok) {
      console.log('Ready!');
      return true;
    }
    await ctr.wait(1000);
    await ctr.waitForReady();
  }

  async isReady() {
    let ready;
    const ctr = this;
    ready = ctr.getState('listening', false);
    if (ready) {
      return true;
    }
    const log = await ctr._compose.logs('am5', {
      cwd: ctr.getState('compose_folder')
    });
    const port = ctr.getState('port_guest');
    const rexp = new RegExp(`0\\.0\\.0\\.0\\:${port}`);
    ready = !!log.out.match(rexp);
    return ready;
  }

  /*
   * Init docker
   */

  async initDocker() {
    const ctr = this;
    try {
      const socketPath =
        process.platform === 'win32'
          ? '//./pipe/docker_engine'
          : '/var/run/docker.sock';

      const isPathOk = await ctr.testSocket(socketPath);

      if (!isPathOk) {
        throw new Error(`SocketPath can't be reached`);
      }
      ctr.docker = new Docker({
        socketPath: socketPath
      });

      return ctr.docker;
    } catch (e) {
      return false;
    }
  }

  async updateComposeFile() {
    const ctr = this;
    const image = ctr.getState('image_name');
    const version = ctr.getState('version');
    const portGuest = ctr.getState('port_guest');
    const portHost = ctr.getState('port_host');
    const composeFolder = ctr.getState('compose_folder');
    const hasVersion = ctr.hasVersion(version);
    let dataLoc = ctr.getState('data_location');
    const isVolume = dataLoc === 'dbgrass';
    const isWritable = isVolume || (await ctr.checkPathWritable(dataLoc));

    if (!hasVersion) {
      console.log('no version, cancel compose file write');
      return;
    }

    if (!isWritable) {
      throw new Error('Data loc not writable, use Controller>initDataLocation');
    }

    if (!isVolume) {
      /**
       * Add a sub-directory 'dbgrass'
       */

      dataLoc = path.join(dataLoc, 'dbgrass');
      const exists = await ctr.checkPathExists(dataLoc);
      if (!exists) {
        await fs.mkdir(dataLoc, {recursive: true});
      }
    }

    const composeStr = YAML.stringify({
      version: '3.8',
      services: {
        am5: {
          image: `${image}:${version}`,
          ports: [`${portHost}:${portGuest}`],
          volumes: [
            '/var/run/docker.sock:/var/run/docker.sock',
            `${dataLoc}:/data/dbgrass`
          ]
        }
      },
      volumes: {
        dbgrass: null
      }
    });

    await fs.writeFile(
      path.join(composeFolder, 'docker-compose.yml'),
      composeStr
    );
  }

  async hasAccessMod() {
    const ctr = this;
    const versions = ctr.getState('versions');
    return versions.length > 0;
  }

  /**
   * Version
   */
  async updateAccessModVersionsList() {
    const ctr = this;
    const versions = ctr.getState('versions');
    versions.length = 0;
    const imgs = await ctr._docker.listImages();
    const image = ctr.getState('image_name');
    for (let img of imgs) {
      for (let tag of img.RepoTags || []) {
        let t = tag.split(':');
        let isAccessMod = t[0] === image;
        const hasTag = !!t[1];
        if (isAccessMod && hasTag) {
          versions.push({
            local: true,
            tag: t[1],
            size: img.Size | 0
          });
        }
      }
    }
    ctr.setState('versions', versions);
    return versions;
  }

  hasVersion(version) {
    const ctr = this;
    const versions = ctr.getState('versions', []);
    const item = versions.find((v) => v.tag === version);
    return !!item;
  }

  async setVersion(version) {
    const ctr = this;
    const hasVersion = ctr.hasVersion(version);
    if (hasVersion) {
      ctr.setState('version', version);
      ctr.stop({dialog: false});
      await ctr.updateComposeFile();
      await ctr.start();
    }
  }

  async importAccessMod() {
    const ctr = this;
    const imagePath = ctr.getState('image_path');
    console.log('import', imagePath);
    try {
      await ctr._docker.importImage(imagePath);
      return true;
    } catch (e) {
      return false;
    }
  }

  async updateAccessMod() {
    const ctr = this;
    try {
      await ctr._compose.pullOne('am5', {cwd: ctr.getState('compose_folder')});
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
