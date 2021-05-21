window.ipc = require('electron').ipcRenderer;

class Com {
  constructor() {
    const cm = this;
    cm.init();
  }

  init() {
    const cm = this;
    if (cm._init) {
      return;
    }
    cm._init = true;
    cm._ipc = window.ipc;
    cm._ipc.on('msg-log', (e, m) => {
      console.log(m);      
    });
    cm._ipc.on('msg-info', (e, m) => {
      cm.handleMessage(m, 'info', e);
    });
    cm._ipc.on('msg-error', (e, m) => {
      cm.handleMessage(m, 'error', e);
    });
    cm._ipc.on('msg-state', (e, m) => {
      cm.handleMessage(m, 'state', e);
    });
    cm.send('ready');
  }

  send(type, data) {
    const cm = this;
    cm._ipc.send(type, data);
  }

  async request(type, data) {
    const cm = this;
    const promInvoke = cm._ipc.invoke('request', {type, data});
    const result = await Promise.race([promInvoke, cm.wait(3000)]);
    return result;
  }

  async getState(key) {
    const cm = this;
    const res = await cm.request('get_state', {key});
    return res;
  }

  async setState(key, value) {
    const cm = this;
    const res = await cm.request('set_state', {key, value});
    return res;
  }

  async wait(time) {
    return new Promise((resolve) => {
      setTimeout(() => {
        resolve(false);
      }, time);
    });
  }

  handleMessage(msg, type) {
    const elInfo = document.getElementById('msgInfo');
    const elError = document.getElementById('msgError');

    if (!elInfo || !elError) {
      console.log({msg, type});
    }

    switch (type) {
      case 'error':
        const elLi = document.createElement('li');
        elLi.innerHTML = msg;
        elError.appendChild(elLi);
        elInfo.innerHTML = '...';
        break;
      case 'info':
        elInfo.innerHTML = msg;
        break;
      default:
        console.log(msg);
    }
  }
}

window.amcom = new Com();
