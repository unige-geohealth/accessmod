import Docker from "dockerode";
import process from "node:process";
import { homedir } from "os";
import { createReadStream, existsSync } from "fs";
import http from "http";
import { meta } from "./../../docker/index.js";

export class DockerTools {
  constructor() {}

  /*
   * Initialize Docker client
   * @return {Docker|null} Docker instance or null if initialization fails
   */
  async initDocker() {
    try {
      const socketPath = await this.findDockerSocket();
      if (!socketPath) {
        console.error(`Socket path can't be reached`);
        return null;
      }

      const docker = new Docker({ socketPath });
      this._docker = docker;
      this.log("Has docker", !!docker);
      return docker;
    } catch (e) {
      console.error(e);
      return null;
    }
  }

  // Find a valid Docker socket
  async findDockerSocket() {
    const isWindows = process.platform === "win32";
    const paths = isWindows
      ? ["//./pipe/docker_engine"]
      : [
          "/var/run/docker.sock",
          "/private/var/run/docker.sock",
          `${homedir()}/.docker/run/docker.sock`,
        ];

    for (const path of paths) {
      if (await this.testSocket(path)) {
        return path; // Return the first valid path
      }
    }
    return null; // No valid socket found
  }

  // Test if a socket path is accessible
  async testSocket(path) {
    return new Promise((resolve) => {
      http
        .get({ socketPath: path }, () => resolve(true))
        .on("error", () => resolve(false));
    });
  }

  /**
   * Check of docker exists
   * @return {Boolean}
   */
  hasDocker() {
    const ctr = this;
    return ctr._docker instanceof Docker;
  }

  /*
   * Load image ( during start() + no image )
   */
  async loadImage() {
    const ctr = this;
    const imagePath = ctr.getState("image_path");
    const exists = existsSync(imagePath);
    if (!exists) {
      throw new Error(`No image found. Path: ${imagePath}`);
    }
    const data = createReadStream(imagePath);
    await ctr._docker.loadImage(data, {
      repo: meta.image_name,
      tag: meta.tag,
    });
    return meta;
  }

  async isContainerReady(name) {
    const ctr = this;
    const logs = await ctr.getContainerLogs(name);

    /**
     * Just look at the logs and check that the app is listening.
     *
     * Should be compatible with previous version, where
     * no /status or /health check is available.
     * TODO: Map strategies used in version < 5.7.17 and
     * proceed accordingly.
     */
    const rexp = RegExp(`http://0\\.0\\.0\\.0`);
    const ready = rexp.test(logs);

    ctr.log(`Container ${name} ready`, ready);
    return ready;
  }

  /**
   * Get container logs
   */
  async getContainerLogs(name) {
    const ctr = this;
    const cont = ctr.getContainerByName(name);
    const logBuf = await cont.logs({
      stdout: true,
      stderr: true,
      follow: false,
    });

    return ctr.dockerBufferToString(logBuf);
  }

  getContainerByName(name) {
    const ctr = this;
    return ctr._containers[name];
  }

  /**
   * Remove control characters, not properly formated
   * eg. "\u0002\u0000\u0000\u0000\u0000\u0000\u0000!Attaching package: ‘R.utils’"
   * -> Attaching package: ‘R.utils'
   * NOTE: this is probably linked to how docker encode streams:
   * See https://docs.docker.com/engine/api/v1.37/#operation/ContainerAttach
   * -> this add 8 first bit as header. As it happen to each line, we can't just
   * remove the first 8 bit in most case.
   */
  dockerBufferToString(buffer) {
    return buffer
      .filter((b) => ![0, 1, 2, 3, 14, 17, 20, 23, 26, 27, 30, 31].includes(b))
      .toString("utf8");
  }

  async initContainer() {
    const ctr = this;
    const tag = ctr._versions.getRepoTag();
    const port_guest = ctr.getState("port_guest");
    const port_host = ctr.getState("port_host");
    const port_guest_http = ctr.getState("port_guest_http");
    const port_host_http = ctr.getState("port_host_http");
    const name = ctr.getState("container_name");

    //const nameHttp = ctr.getState('container_name_http');
    const volume = ctr.getState("data_location");
    const volumeTmp = ctr.getState("docker_volume_tmp");
    const dbgrass = ctr.getState("grass_db_location");
    const optBindPort = {};

    const optExposedPort = {};

    optBindPort[`${port_guest}/tcp`] = [
      {
        HostPort: String(port_host),
      },
    ];
    optExposedPort[`${port_guest}/tcp`] = {};
    optBindPort[`${port_guest_http}/tcp`] = [
      {
        HostPort: String(port_host_http),
      },
    ];
    optExposedPort[`${port_guest_http}/tcp`] = {};

    await ctr.containersCleanAll();

    ctr._containers = {};

    ctr._containers[name] = await ctr._docker.createContainer({
      name: name,
      Image: tag,
      ExposedPorts: optExposedPort,
      Cmd: [
        "Rscript",
        "--vanilla",
        "run.r",
        String(port_guest),
        String(port_guest_http),
        String(port_host_http),
      ],
      Healthcheck: {
        test: [
          "CMD",
          "wget",
          "--spider",
          `http://localhost:${port_guest}/status`,
        ],
        interval: 5e9,
        timeout: 60e9,
        retries: 10,
        start_period: 10e9,
      },
      HostConfig: {
        PortBindings: optBindPort,
        Binds: [
          `${volume}:${dbgrass}`,
          `${volumeTmp}:/tmp`,
          `/var/run/docker.sock:/var/run/docker.sock`,
        ],
        RestartPolicy: {
          Name: "on-failure",
          MaximumRetryCount: 10,
        },
      },
    });
  }

  /**
   * Start all containers already created
   */
  async containersStartAll() {
    const ctr = this;

    for (const n in ctr._containers) {
      const cont = ctr.getContainerByName(n);
      await cont.start();
    }
  }

  async workerRun(opt) {
    opt = {
      binds: [],
      cmd: ["ls"],
      ...opt,
    };
    const ctr = this;

    try {
      const tag = ctr._versions.getRepoTag();
      const data = await ctr._docker.run(
        tag,
        opt.cmd,
        [process.stdout, process.stderr],
        {
          Tty: false,
          HostConfig: {
            AutoRemove: true,
            Cmd: opt.cmd,
            Binds: opt.binds,
          },
        }
      );

      return data[0];
    } catch (e) {
      ctr.dialogShowError(e);
    }
  }

  async containersCleanByName(name) {
    const ctr = this;

    try {
      const refName = {};

      refName[name] = true;
      const containerOld = await ctr._docker.listContainers({
        all: true,
        filters: {
          name: refName,
        },
      });

      for (const c of containerOld) {
        const cOld = await ctr._docker.getContainer(c.Id);

        if (cOld) {
          if (c.State === "running") {
            ctr.log(`Stop container ${c.Id}`);
            await cOld.stop();
          }

          ctr.log(`Remove container ${c.Id}`);
          await cOld.remove();
        }
      }
    } catch (e) {
      ctr.log("Error removing containers", e);
    }
  }

  async containersCleanAll() {
    const ctr = this;

    if (!ctr.hasDocker()) {
      return;
    }

    const name = ctr.getState("container_name");
    await ctr.containersCleanByName(name);
  }

  async initDockerVolumes() {
    const ctr = this;
    const app_name = ctr.getState("app_name");
    const volumes = [
      ctr.getState("docker_volume"),
      ctr.getState("docker_volume_tmp"),
    ];

    for (let v of volumes) {
      const ref = {};

      ref[v] = true;
      const res = await ctr._docker.listVolumes({
        filters: {
          name: ref,
        },
      });

      if (!res.Volumes.length) {
        /**
         * Missing volume, create it
         */
        await ctr._docker.createVolume({
          name: v,
          labels: {
            app: app_name,
          },
        });
      }
    }
  }
}
