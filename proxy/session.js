import { randomUUID } from "crypto";
import { spawn } from "child_process";
import path from "path";
import { getPort, wait } from "./utils.js";
export const sessions = new Map();

export class Session {
  constructor(socket, proxy, entrypoint) {
    this._socket = socket;
    this._proxy = proxy;
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
  get url() {
    return this._url;
  }
  get path() {
    return this._path;
  }

  async init() {
    this._port = await getPort();
    this._url = `http://0.0.0.0:${this.port}`;
    this._path = `/app/${this.id}/`;
    const ok = await this.run();
    if (ok) {
      this.register();
      this.init_routine();
      this.socket.emit("init", this.path);
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

  stop() {
    if (this.process) {
      this.process.kill("SIGTERM");
      setTimeout(() => {
        try {
          this.process.kill("SIGKILL");
        } catch (e) {}
      }, 3000);
      this._process = null;
    }
    this.unregister();
  }

  register() {
    const p = this._sessions.get(this.id);
    if (!p) {
      this._sessions.set(this.id, this);
    }
    this.proxy.register(this.path, this.url);
  }

  unregister() {
    this._sessions.delete(this.id);
    this.proxy.unregister(this.path);
    delete this._process;
  }

  async run() {
    // Spawn the R process with options to better handle child processes
    this._process = spawn("Rscript", [this.entrypoint, this.port], {
      cwd: path.dirname(this.entrypoint),
      // Set detached to false to ensure child processes are in the same process group
      detached: false,
    });

    // Log process ID for debugging
    console.log(
      `Started R process with PID ${this._process.pid} for session ${this.id}`
    );

    this._process.stdout.on("data", (data) => {
      console.log(`Shiny stdout [${this.id}]: ${data}`);
    });

    this._process.stderr.on("data", (data) => {
      console.log(`Shiny stderr [${this.id}]: ${data}`);
    });

    this._process.on("close", (code) => {
      this.stop();
    });

    this._process.on("exit", (code) => {});

    this._process.on("error", (error) => {
      console.error(`Error in process for session ${this.id}:`, error);
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

// Function to clean up all active sessions
const cleanupAllSessions = () => {
  console.log(`Cleaning up ${sessions.size} active sessions`);
  for (const [sessionId, session] of sessions.entries()) {
    try {
      console.log(`Destroying session ${sessionId}`);
      session.destroy();
    } catch (error) {
      console.error(`Error destroying session ${sessionId}:`, error);
    }
  }
};

// Add server shutdown handler
process.on('SIGINT', () => {
  console.log('Server shutting down...');
  cleanupAllSessions();
  process.exit(0);
});

process.on('SIGTERM', () => {
  console.log('Server shutting down...');
  cleanupAllSessions();
  process.exit(0);
});