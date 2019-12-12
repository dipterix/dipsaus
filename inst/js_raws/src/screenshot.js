import { logger, document_ready  } from './utils.js';
import html2canvas from 'html2canvas';

const register_screenshot = (Shiny, debug = false) => {

  let registered = false;
  const log = logger(debug);

  document_ready( () => {
    if(registered){ return; }
    registered = true;
    log("[Dipsaus]: Register screenshot widget");

    Shiny.addCustomMessageHandler('dipsaus-screeshot', (params) => {
      const inputid = params.inputId || "..dipsaus_screenshot..";

      html2canvas(document.body).then((canvas) => {

        Shiny.onInputChange(
          inputid,
          canvas.toDataURL()
        );
      });

    });
  } );

};


export{ register_screenshot };
