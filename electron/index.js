const {shell, app, BrowserWindow, dialog} = require('electron');
const {stop, start} = require('./app/docker.js');
const {ipcMain} = require('electron');
const exitHook = require('exit-hook');
const path = require('path');
const am = {};



/**
* Darwin: PATH is not inherited from session PATH. Fix this.
* -> Spanwned processes will not work when packed
*/
const fixPath = require('fix-path');
fixPath();

/**
 * Electron complain about ressource loaded through localhost...
 * -> Except am5 tiles in the map tool, no external things should be loaded.
 */
process.env['ELECTRON_DISABLE_SECURITY_WARNINGS'] = 'true';

/**
 * Cleanup auto
 * -> do not use process.exit
 */
exitHook(() => {
  console.log('App stopped');
  stop(am);
});

/**
 * Create browser window
 */
app.on('ready', createWindow);
app.on('activate', () => {
  if (am.mainWindow === null) {
    createWindow();
  }
});

app.on('window-all-closed', () => {
  app.quit();
});


ipcMain.once('ready',() => {
  start(am);
});


function createWindow() {
  am.mainWindow = new BrowserWindow({
    width: 1200,
    height: 800,
    backgroundColor:'#fff',
    webPreferences: {
      nodeIntegration : false,
      preload: path.join(__dirname, 'app', 'preload.js')
    }
  });
  am.mainWindow.maximize();;
  am.mainWindow.loadURL(`file://${__dirname}/app/pages/start.html`);
  //am.mainWindow.openDevTools();
  am.mainWindow.on('closed', () => {
    am.mainWindow = null;
    app.quit();
  });
}

exports.am = am;
