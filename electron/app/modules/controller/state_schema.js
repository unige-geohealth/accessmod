const getPort = require('get-port');
const {app} = require('electron');

async function getSchema() {
  const port = await getPort();
  const appData = app.getPath('userData');
  return {
    image_path: {
      type: 'string',
      default: __dirname
    },
    compose_folder: {
      type: 'string',
      default: appData
    },
    url_guest: {
      type: 'string'
    },
    port_host: {
      type: 'number',
      minimum: 0,
      maximum: 65535,
      default: port
    },
    data_location: {
      type: 'string',
      default: ''
    },
    port_guest: {
      type: 'number',
      minimum: 0,
      maximum: 65535,
      default: 3939
    },
    version: {
      type: 'string',
      default: 'latest'
    },
    versions: {
      type: 'array',
      default: []
    },
    stopped: {
      type: 'boolean',
      default: false
    },
    image_name: {
      type: 'string',
      default: ''
    },
    language: {
      type: 'string',
      minLength: 2,
      maxLength: 2,
      default: 'en'
    },
    offline: {
      type: 'boolean',
      default: false
    },
    docker_volume: {
      type: 'string',
      default: 'dbgrass'
    }
  };
}

module.exports.getSchema = getSchema;
