import { register_compoundInput2 } from './shiny-input-compound.js';
import { register_actionButtonStyled } from './shiny-input-actionbutton2.js';
import { register_set_input } from './shiny-input-arbitrary.js';
import { register_screenshot } from './screenshot.js';
import html2canvas from 'html2canvas';

const Shiny = window.Shiny;
const DEBUG = true;

register_actionButtonStyled( Shiny, DEBUG );
register_compoundInput2( Shiny, DEBUG );
register_screenshot( Shiny, DEBUG );
register_set_input( Shiny, DEBUG );

