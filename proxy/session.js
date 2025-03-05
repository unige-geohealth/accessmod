import { randomUUID } from "crypto";
import { spawn } from "child_process";
import { createProxyMiddleware } from "http-proxy-middleware";
import path from "path";
import { getPort, wait } from "./utils.js";
export const sessions = new Map();

export class Session {
  constructor(socket, entrypoint) {
    this._socket = socket;
    this._sessions = sessions;
    this._entrypoint = entrypoint;
    this._id = randomUUID();
  }

  get entrypoint() {
    return this._entrypoint;
  }
  get socket() {
    return this._socket;
  }
  get id() {
    return this._id;
  }
  get port() {
    return this._port;
  }
  get url() {
    return this._url;
  }
  get process() {
    return this._process;
  }
  get proxy() {
    return this._proxy;
  }

  async init() {
    this._port = await getPort();
    this._url = `http://0.0.0.0:${this.port}`;
    const ok = await this.run();
    if (ok) {
      this.register();
      this.init_proxy();
      this.init_routine();
      this.socket.emit("init", `/app/${this.id}/`);
    }
    return ok;
  }

  destroy() {
    if (this._destroyed) {
      return;
    }
    this.stop();
    this._destroyed = true;
  }

async stop() {
  if (this.process) {
    this.process.kill("SIGTERM");
  }
  this.unregister();
}

  register() {
    const p = this._sessions.get(this.id);
    if (!p) {
      this._sessions.set(this.id, this);
    }
  }


init_proxy() {
  this._proxy = createProxyMiddleware({
    target: `http://localhost:${this.port}`,
    ws: true,
    changeOrigin: true,
    pathRewrite: {
      [`^/app/${this.id}`]: "",
    },
    onProxyReq: (proxyReq, req, res) => {
      // Add session ID as query parameter for Shiny
      const hasQuery = proxyReq.path.includes("?");
      proxyReq.path =
        proxyReq.path + (hasQuery ? "&" : "?") + `sm_session_id=${this.id}`;
    },
    onProxyRes: (proxyRes, req, res) => {
      // Handle websocket connection errors
      if (proxyRes.headers.upgrade === 'websocket') {
        console.log(`Websocket connection established for session ${this.id}`);
      }
    },
    onError: (err, req, res) => {
      console.error(`Proxy error for session ${this.id}:`, err);
      if (!res.headersSent) {
        res.status(502).send("Proxy error");
      }
    },
    logLevel: 'warn'
  });
}

  unregister() {
    this._sessions.delete(this.id);
    delete this._process;
  }

  async run() {
    this._process = spawn("Rscript", [this.entrypoint, this.port], {
      cwd: path.dirname(this.entrypoint),
    });

    this._process.stdout.on("data", (data) => {
      console.log(`Shiny stdout [${this.id}]: ${data}`);
    });

    this._process.stderr.on("data", (data) => {
      console.log(`Shiny stderr [${this.id}]: ${data}`);
    });
    this._process.on("close", async () => {
      await this.stop();
    });
    this._process.on("error", (error) => {
      console.error(error);
    });

    const ok = await this.healthy();
    if (!ok) {
      console.error(
        `Process cannot start on port ${this.port} for entry point ${this.entrypoint}`
      );
      this.destroy();
    }
    return ok;
  }

  init_routine() {
    if (this._routine_started) {
      return;
    }
    this._routine_started = true;
    return this._routine_health();
  }

  _routine_health() {
    setTimeout(async () => {
      const is_probably_healthy = await this._health_check(false);
      if (!is_probably_healthy) {
        return this.destroy();
      } else {
        return this._routine_health();
      }
    }, 1e3 * 60 * 15);
  }

  async healthy() {
    let ok = false;
    const max = 10;
    for (let i = 0; i < max; i++) {
      console.log(`Attempt ${i}/${max}`);
      await wait(1000);
      ok = await this._health_check();
      if (ok) {
        break;
      }
    }
    return ok;
  }
  async _health_check(fail_if_timeout = true) {
    try {
      const promFetch = fetch(`${this._url}/x/health`);
      const promTimeout = wait(3000);
      const response = await Promise.race([promFetch, promTimeout]);
      const is_timeout = response === "timeout";
      if (is_timeout && fail_if_timeout) {
        throw new Error("timeout");
      }
      if (is_timeout && !fail_if_timeout) {
        console.warn("Health check : R probably busy");
        return true;
      }
      const text = await response.text();
      return /^ok/.test(text);
    } catch (error) {
      return false;
    }
  }
}
