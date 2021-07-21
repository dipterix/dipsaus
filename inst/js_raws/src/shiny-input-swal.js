import swal from 'sweetalert';
import { logger, document_ready  } from './utils.js';

const close_current = () => {
  swal.stopLoading();
  swal.close();
}

const register_swal = (Shiny, debug = false) => {

  let registered = false;
  const log = logger(debug);

  document_ready( () => {
    if(registered){ return; }
    registered = true;
    log("[Dipsaus]: Register swal-alert widget");

    Shiny.addCustomMessageHandler('dipsaus-swal-close', () => {
      const state = swal.getState();
      if( state && state.isOpen ) {
        close_current();
      }
    });

    Shiny.addCustomMessageHandler('dipsaus-swal-show', (params) => {
      const inputId = params.inputId
      if(!inputId){ return; }

      const state = swal.getState();
      if( state && state.isOpen ) {
        close_current();
      }

      const buttons = params.buttons === undefined ? false : params.buttons;
      const autoClose = params.autoClose === undefined ? (buttons === false) : params.autoClose;
      const swal_params = {
        className: "dipsaus-swal-modal",
        title : params.title || "Hi",
        text : params.text || "",
        icon : params.icon || "info",
        buttons : buttons,
        closeOnClickOutside : autoClose,
        closeOnEsc : autoClose,
        dangerMode : params.dangerMode === true,
      }

      // debug
      swal(swal_params)
        .then((value) => {
          // inputId is string, we don't check it
          Shiny.setInputValue(
            inputId + ':dipsaus_swal',
            {
              'inputId' : inputId,
              'value' : value
            }
          );


        });

    });
  } );

};


export{ register_swal };
