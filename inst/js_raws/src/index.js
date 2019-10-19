import { register_compoundInput2 } from './shiny-input-compound.js';
import { register_actionButtonStyled } from './shiny-input-actionbutton2.js';


const Shiny = window.Shiny;
const DEBUG = true;

register_actionButtonStyled( Shiny, DEBUG );
register_compoundInput2( Shiny, DEBUG );