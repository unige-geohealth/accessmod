const moduleAlias = require('module-alias');

moduleAlias.addAliases({
  '@am5': `${__dirname}/modules/`,
  '@root': `${__dirname}`,
  '@docker': `${__dirname}/docker`
});

const fs = require('fs');
const path = require('path');
const {Controller} = require('@am5/controller');

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
const metaDockerPath = path.join(__dirname,'docker/meta.json');
const metaDocker = JSON.parse(fs.readFileSync(metaDockerPath));

const ctrl = new Controller({
  image_path: path.join(__dirname, 'docker/accessmod.docker.gz'),
  image_name: metaDocker.image_name,
  url_guest: 'http://localhost',
  port_guest: 3939,
  //port_host: 8833, // auto 
  version: metaDocker.tag
});

ctrl.init();

