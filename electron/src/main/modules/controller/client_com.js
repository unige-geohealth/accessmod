export class ClientCom {
    constructor() {}
    
    /**
   * Handle request
   */
    async handleRequest(e, config) {
        const ctr = this;
        let result = null;
        
        if (!e || !config?.type)
            return;
        
        const versions = ctr._versions;
        const d = config.data || {};
        
        switch(config.type) {
        case 'set_state':
            ctr.setState(d.key, d.value);
            result = ctr.getState(d.key);
            break;
        
        case 'stop':
            await ctr.stop();
            result = 'ok';
            break;
        
        case 'restart':
            await ctr.restart();
            result = 'ok';
            break;
        
        case 'get_state':
            result = ctr.getState(d.key);
            break;
        
        case 'set_version':
            ctr.log(d.version);
            result = await versions.setVersion(d.version);
            break;
        
        case 'list_versions':
            result = await versions.listLocal();
            break;
        
        case 'dialog_data_location':
            ctr.log('');
            result = await ctr.initDataLocation({
                reset: true,
                cancelable: true,
            });
            break;
        
        default:
            ctr.log('Unknown command', config);
        }
        
        
        return result;
    }
}
