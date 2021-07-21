import { has_jquery, logger, document_ready, check_shiny  } from './utils.js';

function register_actionButtonStyled ( Shiny, debug = false ){
  check_shiny( Shiny, 'Register actionButtonStyled (dipsaus)' );
  const log = logger( debug );
  const shiny_binding = Shiny.inputBindings.bindingNames["shiny.actionButtonInput"].binding;
  const btn_types = ['default', 'info', 'success', 'primary', 'warning', 'danger'];

  document_ready( () => {
    Shiny.addCustomMessageHandler('dipsaus.updateActionButtonStyled', ( msg ) => {
      const inputId = msg.inputId,
            el = document.getElementById( inputId );
      log( msg );
      if( !el ){ return; }

      if( msg.type && btn_types.includes( msg.type ) ){

        btn_types.forEach((_v) => {
          if( _v === msg.type ){
            el.classList.add('btn-' + _v);
          }else{
            el.classList.remove('btn-' + _v);
          }
        });

      }

      el.disabled = msg.disabled === true;

    });
  });
}

export { register_actionButtonStyled };

