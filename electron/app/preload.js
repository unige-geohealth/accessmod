window.ipc = require('electron').ipcRenderer;

const elMsg = document.getElementById('msg');

window.ipc.on('msg-info', (event, m) => {
  updateMessage(m, 'info');
});
window.ipc.on('msg-error', (event, m) => {
  updateMessage(m, 'error');
});
window.ipc.on('msg-state', (event, m) => {
  updateMessage(m, 'state');
});

function updateMessage(msg, type) {
  const elInfo = document.getElementById('msgInfo');
  const elError = document.getElementById('msgError');

  if(!elInfo || !elError){
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

window.ipc.send('ready');
