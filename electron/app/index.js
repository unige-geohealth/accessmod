const moduleAlias = require('module-alias');

moduleAlias.addAliases({
  '@am5': `${__dirname}/modules/`,
  '@root': `${__dirname}`,
  '@docker': `${__dirname}/docker`
});

const fs = require('fs');
const path = require('path');
const {Controller} = require('@am5/controller');
const getPort = require('get-port');

/**
 * Darwin: PATH is not inherited from session PATH. Fix this.
 * -> Spanwned processes will not work when packed
 */
const fixPath = require('fix-path');
fixPath();

/**
 * Electron complain about ressource loaded through localhost...
 * -> Except am5 tiles in the map tool, no external things should be loaded.
 */
process.env['ELECTRON_DISABLE_SECURITY_WARNINGS'] = 'true';

/**
 * Read the file that should describe the image includded
 */

const metaDockerPath = path.join(__dirname, 'docker/meta.json');
const metaDocker = JSON.parse(fs.readFileSync(metaDockerPath));

(async () => {
  const ctrl = new Controller({
    image_path: path.join(__dirname, 'docker/accessmod-docker.tar.gz'),
    image_name: metaDocker.image_name,
    url_guest: 'http://localhost',
    repo_url_api :'https://hub.docker.com/v2/',
    port_guest: 3000,
    port_host: await getPort(),
    port_guest_http: 5000,
    port_host_http: await getPort() 
  });
  await ctrl.init();
})().catch(console.error);
