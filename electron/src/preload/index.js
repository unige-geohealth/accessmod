import { ipcRenderer } from "electron";

class Com {
  constructor() {
    const cm = this;
    cm.init();
  }

  init() {
    const cm = this;

    if (cm._init) return;
    cm._init = true;
    cm._ipc = ipcRenderer;
    cm._ipc.on("msg-log", (_, m) => {
      console.log(m);
    });
    cm._ipc.on("msg-info", (e, m) => {
      cm.handleMessage(m, "info", e);
    });
    cm._ipc.on("msg-error", (e, m) => {
      cm.handleMessage(m, "error", e);
    });
    cm._ipc.on("msg-state", (e, m) => {
      cm.handleMessage(m, "state", e);
    });
    cm.send("ready");
  }

  send(type, data) {
    const cm = this;
    cm._ipc.send(type, data);
  }

  async request(type, data) {
    const cm = this;
    const promInvoke = cm._ipc.invoke("request", {
      type,
      data,
    });

    return await Promise.race([promInvoke, cm.wait(3000)]);
  }

  async getState(key) {
    const cm = this;

    return await cm.request("get_state", {
      key,
    });
  }

  async setState(key, value) {
    const cm = this;

    return await cm.request("set_state", {
      key,
      value,
    });
  }

  async wait(time) {
    return new Promise((resolve) => {
      setTimeout(() => {
        resolve(false);
      }, time);
    });
  }

  handleMessage(msg, type) {
    const elInfo = document.getElementById("msgInfo");
    const elError = document.getElementById("msgError");

    switch (type) {
      case "error":
        {
          const elLi = document.createElement("li");
          elLi.innerHTML = msg;
          elError.appendChild(elLi);
          elInfo.innerHTML = "...";
        }
        break;

      case "info":
        elInfo.innerHTML = msg;
        break;

      default:
    }
  }
}

window.amcom = new Com();
