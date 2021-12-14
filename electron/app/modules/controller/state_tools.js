const {getSchema} = require('@am5/controller/state_schema.js');
const Store = require('electron-store');

class StateTools {
  constructor() {}

  setState(key, value) {
    const ctr = this;
    ctr._state.set(key, value);
  }

  getState(key) {
    const ctr = this;
    return ctr._state.get(key);
  }

  initState(state) {
    const ctr = this;
    try {
      if (ctr._state) {
        return;
      }
      const schema = getSchema();
      ctr._state = new Store({schema});
      ctr._state.store = Object.assign({}, ctr._state.store, state);
    } catch (e) {
      ctr.dialogShowError(e);
    }
  }
}

module.exports.StateTools = StateTools;
