import getPort from "get-port";
import { Controller } from "./modules/controller/index.js";
import squirrel_startup from "electron-squirrel-startup";
import fixPath from "fix-path";
/**
 * Darwin: PATH is not inherited from session PATH. Fix this.
 * -> Spanwned processes will not work when packed
 */
fixPath();
import { meta } from "./docker/index.js";


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
      image_path: meta.image_path,
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
