const {app, Menu} = require('electron');
const isMac = process.platform === 'darwin';

class MenuTools {
  constructor() {}

  async updateMenu(opt) {
    const ctr = this;
    opt = Object.assign({}, {versionsAdd: true, versionsForceFetch: true}, opt);
    const after_5_7_15 = await ctr._versions.currentGreaterThan('5.7.15');
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
      ...(after_5_7_15
        ? [
            {
              label: 'Project',
              submenu: [
                {
                  label: 'Direct import...',
                  click: () =>
                    ctr.dialogProjectUpload().catch(ctr.dialogShowError)
                }
              ]
            }
          ]
        : []),
      {
        label: 'Versions',
        submenu: opt.versionsAdd
          ? await ctr._versions
              .getVersionsForMenu(opt.versionsForceFetch)
              .catch(ctr.dialogShowError)
          : []
      },
      {
        label: 'Database',
        submenu: [
          {
            label: 'Change database location...',
            click: () =>
              ctr
                .initDataLocation({reset: true, cancelable: true})
                .catch(ctr.dialogShowError)
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
}

module.exports.MenuTools = MenuTools;
