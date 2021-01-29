const compose = require('docker-compose');
const Docker = require('dockerode');

const internetAvailable = require('internet-available');
const {dialog, ipcMain} = require('electron');
const {spawnSync, spawn} = require('child_process');
const path = require('path');
const composeFolder = path.join(__dirname, './');
const imagePath = path.join(__dirname,'./docker_images/accessmod.docker.gz')
const {tl} = require(path.join(__dirname, './translate'));
const dockerComposeOpt = {
  //env: process.env,
  cwd: composeFolder
};

const state = {
  ready: false,
  docker: null
};

function stop(am) {
  try {
    console.log('Stop requested ');
    const choice = dialog.showMessageBoxSync(am.mainWindow, {
      type: 'question',
      buttons: ['Exit', 'Exit and keep the server running'],
      title: 'AccessMod',
      message: 'Keep the server running ?',
      defaultId: 0
    });
    const exit = choice === 0;
    if (exit) {
      spawnSync('docker-compose', ['down'], dockerComposeOpt);
      return;
    }
    console.log('Stop done');
  } catch (err) {
    dialog.showMessageBox(am.mainWindow, {
      type: 'error',
      title: tl('eltrn_error_dialog_title', 'en'),
      message: err.message
    });
  }
}

async function start(am) {
  try {
    console.log({msg: tl('eltrn_loading_docker', 'en')});
    await wait(100);
    am.mainWindow.webContents.send('msg-info', tl('eltrn_start', 'en'));

    /**
     * Test if docker is running
     */
    state.docker = await initDocker();

    if (!state.docker) {
      dialog.showMessageBox(am.mainWindow, {
        type: 'error',
        title: tl('eltrn_error_dialog_title', 'en'),
        message: tl('eltrn_no_docker', 'en', {
          link: '<a target="_blank" href="https://docs.docker.com/get-docker/">'
        })
      });
      return;
    }

    /**
     * Test for network
     */
    const hasNet = await hasInternet();
    if (hasNet) {
      am.offline = false;
    } else {
      am.mainWindow.webContents.send('msg-info', tl('eltrn_offline', 'en'));
      am.offline = true;
    }
    await wait(100);

    /**
     * Test if docker accessmod image exists
     */
    const hasAm = await hasAccessMod();

    if (!hasAm) {
      am.mainWindow.webContents.send(
        'msg-info',
        'First time ? Initialization of the base image :)'
      );
      await wait(2000);
      await importAccessmod();
    }

    if (hasNet) {
      am.mainWindow.webContents.send('msg-info', 'Download latest changes');
      await updateAccessmod();
    }

    am.mainWindow.webContents.send(
      'msg-info',
      tl('eltrn_loading_docker', 'en')
    );
    await compose.upAll(dockerComposeOpt);
    await waitForReady();
    am.mainWindow.webContents.send('msg-info', tl('eltrn_loaded_docker', 'en'));
    await wait(100);
    am.mainWindow.loadURL('http://localhost:8833');
  } catch (e) {
    dialog.showMessageBox(am.mainWindow, {
      type: 'error',
      title: tl('eltrn_error_dialog_title', 'en'),
      message: tl('eltrn_error_generic', 'en', {err: e.err || e.message || e})
    });
  }
}

function wait(n) {
  return new Promise((resolve) => {
    console.log('wait', n);
    setTimeout(() => {
      resolve(true);
    }, n || 5000);
  });
}

async function waitForReady() {
  const ok = await isReady();
  if (ok) {
    console.log('Ready!');
    return true;
  }
  await wait(1000);
  await waitForReady();
}

async function isReady() {
  if (state.ready === true) {
    return true;
  }
  const log = await compose.logs('am5', dockerComposeOpt);
  state.ready = !!log.out.match(/0\.0\.0\.0\:3939/);
  return state.ready;
}

async function initDocker() {
  try {
    state.docker = new Docker({socketPath: '/var/run/docker.sock'});
    return state.docker;
  } catch (e) {
    return false;
  }
}

async function hasAccessMod(){
  const imgs = await state.docker.listImages();
  return !!imgs.find(i=>i.RepoTags.filter(tag=>tag.match('fredmoser/accessmod')));
}

async function hasInternet() {
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

async function importAccessmod() {
  try {
    await state.docker.importImage(imagePath);
    return true;
  } catch (e) {
    return false;
  }
}

async function updateAccessmod() {
  try {
    await compose.pullOne('am5', dockerComposeOpt);
    return true;
  } catch (e) {
    return false;
  }
}





exports.start = start;
exports.stop = stop;
