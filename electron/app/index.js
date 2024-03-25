const moduleAlias = require("module-alias");
const fs = require("fs");
const path = require("path");
const { Controller } = require("@am5/controller");
const getPort = require("get-port");
const fixPath = require("fix-path");
const squirrel_startup = require("electron-squirrel-startup");

/**
 * Darwin: PATH is not inherited from session PATH. Fix this.
 * -> Spanwned processes will not work when packed
 */
fixPath();

/** 
* Default Squirrel.Windows event handler for an Electron apps.
* https://www.npmjs.com/package/electron-squirrel-startup
*/
if (squirrel_startup) {
  return;
}

moduleAlias.addAliases({
  "@am5": `${__dirname}/modules/`,
  "@root": `${__dirname}`,
  "@docker": `${__dirname}/docker`,
});


/**
 * Electron complain about ressource loaded through localhost...
 * -> Except am5 tiles in the map tool, no external things should be loaded.
 */
process.env["ELECTRON_DISABLE_SECURITY_WARNINGS"] = "true";

/**
 * Read the file that should describe the image includded
 */

const metaDockerPath = path.join(__dirname, "docker/meta.json");
const metaDocker = JSON.parse(fs.readFileSync(metaDockerPath));

const ctrl = new Controller({
  image_path: path.join(__dirname, "docker/accessmod-docker.tar.gz"),
  image_name: metaDocker.image_name,
  url_guest: "http://localhost",
  repo_url_api: "https://hub.docker.com/v2/",
  port_guest: 3000,
  port_host: await getPort(),
  port_guest_http: 5000,
  port_host_http: await getPort(),
});
await ctrl.init();
