import getPort from "get-port";
import { Controller } from "./modules/controller/index.js";
import fixPath from "fix-path";
import squirrel_startup from "electron-squirrel-startup";
import { getAbsolutePath } from "./helpers.js";
import { meta } from "./docker/index.js";

const imagePath = getAbsolutePath(
  "./../../docker/accessmod-docker.tar.gz",
  import.meta.url
);

/**
 * Darwin: PATH is not inherited from session PATH. Fix this.
 * -> Spanwned processes will not work when packed
 */
fixPath();

/**
 * Start
 */
start();

async function start() {
  try {
    if (squirrel_startup) {
      /**
       * Default Squirrel.Windows event handler for an Electron apps.
       * https://www.npmjs.com/package/electron-squirrel-startup
       */
      return;
    }

    const ctrl = new Controller({
      image_path: imagePath,
      image_name: meta.image_name,
      url_guest: "http://localhost",
      repo_url_api: "https://hub.docker.com/v2/",
      port_guest: 3000,
      port_host: await getPort(),
      port_guest_http: 5000,
      port_host_http: await getPort(),
    });

    await ctrl.init();
  } catch (e) {
    console.error(e);
  }
}
