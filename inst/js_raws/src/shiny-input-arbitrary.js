import { logger, document_ready  } from './utils.js';

const register_set_input = (Shiny, debug = false) => {

  let registered = false;
  const log = logger(debug);

  document_ready( () => {
    if(registered){ return; }
    registered = true;
    log("[Dipsaus]: Register assign input widget");

    Shiny.addCustomMessageHandler('dipsaus-set-input', (params) => {
      const inputid = params.inputId,
            proxy = params.proxy || 'serialize',
            priority = params.priority;

      if(!inputid){ return; }

      log('Setting input ' + inputid);

      // inputid is string, we don't check it
      Shiny.setInputValue(
        inputid + ':dipsaus_asis',
        {
          'proxy' : proxy,
          'value' : params.value
        },
        {
          'priority': priority
        }
      );

    });
  } );

};


export{ register_set_input };
