const compose = require('docker-compose');

init();

async function init() {
  try {
    await compose.upOne('am5');
    const log = await compose.logs('am5');
    await compose.stopOne('am5');
    console.log(log);
  } catch (e) {
    console.error(e);
  }
}
