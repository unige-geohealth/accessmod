const getPort = require('get-port');
const meta = require('@docker/meta.json');
async function getSchema() {
  const port = await getPort();
  return {
    image_path: {
      type: 'string',
      default: __dirname
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
    port_guest: {
      type: 'number',
      minimum: 0,
      maximum: 65535,
      default: 3939
    },
    data_location: {
      type: 'string',
      default: 'docker_volume'
    },
    grass_db_location: {
      type: 'string',
      default: '/data/dbgrass'
    },
    docker_volume: {
      type: 'string',
      default: 'acessmod_storage'
    },
    docker_volume_tmp: {
      type: 'string',
      default: 'accessmod_temporary'
    },
    image_name: {
      type: 'string',
      default: meta.image_name
    },
    container_name: {
      type: 'string',
      default: 'accessmod_worker'
    },
    app_name: {
      type: 'string',
      default: 'accessmod'
    },
    version: {
      type: 'string',
      default: meta.tag
    },
    stopped: {
      type: 'boolean',
      default: false
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
    }
  };
}
debugger;
module.exports.getSchema = getSchema;
