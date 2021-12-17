const {Versions} = require('@am5/versions');
const {Classes} = require('@am5/classes');
const {tl} = require('@am5/translate');
const path = require('path');
const {dialog} = require('electron');
const {app, session, BrowserWindow, shell} = require('electron');
const {ipcMain} = require('electron');
const http = require('http');
const dns = require('dns').promises;
//const exitHook = require('exit-hook');
//const exitHook = require('async-exit-hook');
//const isWin = process.platform === 'win32';
//const onExit = require('signal-exit');
//const prexit = require('prexit');
const unload = require('unload');

/**
 * Sub classes
 */

const {ProjectsTools} = require('./projects_tools');
const {StateTools} = require('./state_tools.js');
const {DataLocationTools} = require('./data_loc_tools.js');
const {ClientCom} = require('./client_com.js');
const {DockerTools} = require('./docker_tools');
const {MenuTools} = require('./menu_tools');

/**
 * Main controller object
 */

class Controller extends Classes([
  ProjectsTools,
  StateTools,
  DataLocationTools,
  ClientCom,
  DockerTools,
  MenuTools
]) {
  constructor(opt) {
    super();
    const ctr = this;
    ctr._opt = opt;
    ctr._versions = new Versions(ctr);
    /**
     * bind (collbacked)
     */

    ctr.dialogShowError = ctr.dialogShowError.bind(ctr);
    ctr.dialogProjectUpload = ctr.dialogProjectUpload.bind(ctr);
    ctr.dialogShowError = ctr.dialogShowError.bind(ctr);
  }

  /**
   * Check if is dev;
   */
  get isDev() {
    return !app.isPackaged;
  }

  /**
   * Init, set options
   */
  async init(opt) {
    const ctr = this;

    if (!opt) {
      opt = ctr._opt;
    }

    if (ctr._init) {
      return;
    }
    ctr._init = true;
    ctr._destroyed = false;
    ctr._destroying = false;

    if (opt) {
      ctr.initState(opt);
    }

    /**
     * Cleanup auto
     */
    unload.add(async () => {
      await ctr.stop();
    });
    /**
     * Create browser window
     */
    await ctr.waitAppReady();
    await ctr.createWindow();
    await ctr.showPage('splash.html');

    app.on('window-all-closed', async () => {
      await ctr.destroy();
    });

    app.on('before-quit', async (e) => {
      if (!ctr._destroying) {
        e.preventDefault();
        const res = ctr.dialogConfirmQuit();
        if (res) {
          await ctr.destroy();
        }
      }
    });

    await ctr.waitIpc();
    ipcMain.handle('request', ctr.handleRequest.bind(ctr));

    await ctr.start();
  }

  async destroy() {
    const ctr = this;
    if (ctr._destroying) {
      return;
    }
    ctr._destroying = true;
    ctr._mainWindow.close();
    await ctr.stop();
    app.quit();
    ctr._destroyed = true;
  }

  async createWindow() {
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
      },
      closable: true
    });

    /**
     * Load navigation link in external window
     */
    ctr._mainWindow.webContents.on('will-navigate', async (e, url) => {
      try {
        if (url !== e.sender.getURL()) {
          e.preventDefault();
          const hasNet = await ctr.hasInternet();
          if (!hasNet) {
            ctr.dialogNoNetwork();
          } else {
            shell.openExternal(url);
          }
        }
      } catch (e) {
        ctr.dialogShowError(e);
      }
    });

    /**
     * Wndow size and dev mode
     */

    ctr._mainWindow.maximize();
    if (ctr.isDev) {
      ctr._mainWindow.openDevTools();
    }

    ctr._mainWindow.on('close', (e) => {
      if (ctr._destroying) {
        return;
      }
      e.preventDefault();
      app.quit();
    });

    await ctr.updateMenu({
      versionsAdd: false // later...
    });
  }

  dialogConfirmQuit() {
    const ctr = this;
    if (ctr.isDev) {
      //return true;
    }
    const choice = dialog.showMessageBoxSync(ctr._mainWindow, {
      type: 'question',
      buttons: ['Yes', 'No'],
      title: 'Confirm',
      message: 'Are you sure you want to quit?'
    });
    ctr.log('dialog confirm quit ', choice);
    return choice === 0;
  }

  /**
   * Stop docker gracefully
   *
   */
  async stop() {
    const ctr = this;
    ctr.setState('stopped', true);
    await ctr.showPage('splash.html');
    ctr.sendMessageCodeClient('msg-info', 'stop');
    await ctr.containersCleanAll();
  }

  /**
   * Restart
   */
  async restart() {
    const ctr = this;
    try {
      await ctr.stop();
      await ctr.start();
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
      ctr.log('Start!');
      
      ctr.clearCache();
      ctr.setState('stopped', false);
      ctr.sendMessageCodeClient('msg-info', 'start');
      await ctr.wait(1000, 'start msg-info ');
      /**
       * Link docker-compose and docker
       */
      await ctr.initDocker();

      /**
       * If no docker instance, msg
       */
      if (!ctr.hasDocker()) {
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
        await ctr.wait(1000, 'start msg-info offline');
      }

      /**
       * Update versions in menu
       */
      await ctr.updateMenu({
        versionsAdd: true,
        versionsForceFetch: true
      });

      /**
       * Test if docker image exists
       */
      const hasV = await ctr._versions.hasVersionLocal();

      if (!hasV) {
        ctr.sendMessageCodeClient('msg-info', 'docker_load_file');
        await ctr.wait(2000, 'start docker_load_file');
        await ctr.loadImage();
      }

      ctr.sendMessageCodeClient('msg-info', 'data_loc_check');
      await ctr.initDockerVolumes();
      await ctr.initDataLocation();

      ctr.sendMessageCodeClient('msg-info', 'loading_docker');
      await ctr.initContainer();
      await ctr.containersStartAll();
      await ctr.waitForReadyAll();
      ctr.sendMessageCodeClient('msg-info', 'loaded_docker');
      /**
       * Connect / load page
       */
      await ctr.showPage('app');
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
  async showPage(name) {
    const ctr = this;
    if (!ctr._mainWindow || ctr._mainWindow.isDestroyed()) {
      return;
    }
    if (ctr._page_name === name) {
      return;
    }
    ctr._page_name = name;
    switch (name) {
      case 'app':
        await ctr.loadAppURL();
        break;
      default:
        ctr._mainWindow.loadURL(`file://${__dirname}/pages/${name}`);
    }
  }

  async loadAppURL() {
    const ctr = this;
    const url = ctr.getState('url_guest');
    const port = ctr.getState('port_host');
    const urlPort = `${url}:${port}`;
    const ok = await ctr.testUrl(urlPort);

    if (ok) {
      ctr._load_app_ntry = 0;
      ctr._mainWindow.loadURL(urlPort);
      return;
    }

    if (!ctr._load_app_ntry) {
      ctr._load_app_ntry = 1;
    } else {
      ctr._load_app_ntry++;
    }

    if (ctr._load_app_ntry >= 10) {
      ctr._load_app_ntry = 0;
      ctr.dialogShowError('App failed to load, try another version.');
      return;
    }
    await ctr.wait(1000, 'load app url');
    await ctr.loadAppURL();
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
      if (ctr.isDev) {
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
    if (ctr.isDev) {
      console.error(e);
    }
    try {
      if (!ctr._mainWindow || ctr._mainWindow.isDestroyed()) {
        return;
      }
      dialog.showMessageBoxSync(ctr._mainWindow, {
        type: 'error',
        title: tl('error_dialog_title', language),
        message: tl('error_generic', language, {
          err: e.err || e.message || e
        })
      });
    } catch (errorDialog) {
      console.error('dialogShowError failed', errorDialog);
    }
  }

  /**
   * waiting
   */
  wait(n, txt) {
    const ctr = this;
    return new Promise((resolve) => {
      ctr.log('wait', txt, n);
      setTimeout(() => {
        resolve(true);
      }, n || 5000);
    });
  }
  async waitForReadyAll() {
    const ctr = this;
    let readyAll = true;
    for (let n in ctr._containers) {
      if (readyAll) {
        readyAll = await ctr.isContainerReady(n);
      }
    }
    if (readyAll) {
      return true;
    }
    await ctr.wait(2000, 'waitForReadyAll');
    return ctr.waitForReadyAll();
  }
  waitIpc() {
    return new Promise((resolve) => {
      ipcMain.once('ready', () => {
        resolve('ok');
      });
    });
  }
  waitAppReady() {
    return new Promise((resolve) => {
      app.on('ready', () => {
        resolve('ok');
      });
    });
  }

  /**
   * Dialogs
   *
   */
  dialogNoNetwork() {
    const ctr = this;
    dialog.showMessageBoxSync(ctr._mainWindow, {
      type: 'warning',
      buttons: ['ok'],
      title: 'No network',
      message: 'This operation requires a working internet connection'
    });
  }

  /**
   * Test for internet
   */
  async hasInternet() {
    try {
      await dns.lookup('google.com');
      return true;
    } catch (e) {
      return false;
    }
  }

  /**
   * Check if request is ok is available
   */
  testUrl(url) {
    return new Promise((resolve) => {
      http
        .request(url, {method: 'HEAD'}, (res) => {
          const {statusCode} = res;
          if (statusCode === 200) {
            resolve(true);
          } else {
            resolve(false);
          }
        })
        .on('error', () => {
          resolve(false);
        })
        .end();
    });
  }

  /**
   * Check for socket
   */
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
