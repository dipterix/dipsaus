/******/ (function(modules) { // webpackBootstrap
/******/ 	// The module cache
/******/ 	var installedModules = {};
/******/
/******/ 	// The require function
/******/ 	function __webpack_require__(moduleId) {
/******/
/******/ 		// Check if module is in cache
/******/ 		if(installedModules[moduleId]) {
/******/ 			return installedModules[moduleId].exports;
/******/ 		}
/******/ 		// Create a new module (and put it into the cache)
/******/ 		var module = installedModules[moduleId] = {
/******/ 			i: moduleId,
/******/ 			l: false,
/******/ 			exports: {}
/******/ 		};
/******/
/******/ 		// Execute the module function
/******/ 		modules[moduleId].call(module.exports, module, module.exports, __webpack_require__);
/******/
/******/ 		// Flag the module as loaded
/******/ 		module.l = true;
/******/
/******/ 		// Return the exports of the module
/******/ 		return module.exports;
/******/ 	}
/******/
/******/
/******/ 	// expose the modules object (__webpack_modules__)
/******/ 	__webpack_require__.m = modules;
/******/
/******/ 	// expose the module cache
/******/ 	__webpack_require__.c = installedModules;
/******/
/******/ 	// define getter function for harmony exports
/******/ 	__webpack_require__.d = function(exports, name, getter) {
/******/ 		if(!__webpack_require__.o(exports, name)) {
/******/ 			Object.defineProperty(exports, name, { enumerable: true, get: getter });
/******/ 		}
/******/ 	};
/******/
/******/ 	// define __esModule on exports
/******/ 	__webpack_require__.r = function(exports) {
/******/ 		if(typeof Symbol !== 'undefined' && Symbol.toStringTag) {
/******/ 			Object.defineProperty(exports, Symbol.toStringTag, { value: 'Module' });
/******/ 		}
/******/ 		Object.defineProperty(exports, '__esModule', { value: true });
/******/ 	};
/******/
/******/ 	// create a fake namespace object
/******/ 	// mode & 1: value is a module id, require it
/******/ 	// mode & 2: merge all properties of value into the ns
/******/ 	// mode & 4: return value when already ns object
/******/ 	// mode & 8|1: behave like require
/******/ 	__webpack_require__.t = function(value, mode) {
/******/ 		if(mode & 1) value = __webpack_require__(value);
/******/ 		if(mode & 8) return value;
/******/ 		if((mode & 4) && typeof value === 'object' && value && value.__esModule) return value;
/******/ 		var ns = Object.create(null);
/******/ 		__webpack_require__.r(ns);
/******/ 		Object.defineProperty(ns, 'default', { enumerable: true, value: value });
/******/ 		if(mode & 2 && typeof value != 'string') for(var key in value) __webpack_require__.d(ns, key, function(key) { return value[key]; }.bind(null, key));
/******/ 		return ns;
/******/ 	};
/******/
/******/ 	// getDefaultExport function for compatibility with non-harmony modules
/******/ 	__webpack_require__.n = function(module) {
/******/ 		var getter = module && module.__esModule ?
/******/ 			function getDefault() { return module['default']; } :
/******/ 			function getModuleExports() { return module; };
/******/ 		__webpack_require__.d(getter, 'a', getter);
/******/ 		return getter;
/******/ 	};
/******/
/******/ 	// Object.prototype.hasOwnProperty.call
/******/ 	__webpack_require__.o = function(object, property) { return Object.prototype.hasOwnProperty.call(object, property); };
/******/
/******/ 	// __webpack_public_path__
/******/ 	__webpack_require__.p = "";
/******/
/******/
/******/ 	// Load entry module and return exports
/******/ 	return __webpack_require__(__webpack_require__.s = 0);
/******/ })
/************************************************************************/
/******/ ([
/* 0 */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
__webpack_require__.r(__webpack_exports__);

// CONCATENATED MODULE: ./src/utils.js


function has_jquery() {
  return( (window.jQuery || jQuery) !== undefined );
}

function parse_html ( s ) {
  const _el = document.createElement('div');
  _el.innerHTML = s;
  return( _el.children[ 0 ] );
}

function logger ( debug = true, level = 'log' ){
  this.debug = debug;
  return (
    ( s, d ) => {
      if( d ){
        this.debug = s === true;
      }else if ( this.debug ){
        console[level](s);
      }
    }
  );
}



var readyList = [];
var readyFired = false;
var readyEventHandlersInstalled = false;

const ready = function (){
  if (!readyFired) {
    readyFired = true;
    for (var i = 0; i < readyList.length; i++) {
        readyList[i].fn.call(window, readyList[i].ctx);
    }
    readyList.length = 0;
  }
};


const readyStateChange = function() {
    if ( document.readyState === "complete" ) {
        ready();
    }
};

const document_ready = function(callback, context) {
  if (typeof callback !== "function") {
      throw new TypeError("callback for document_ready(fn) must be a function");
  }
  if (readyFired) {
      setTimeout(function() {callback(context);}, 1);
      return;
  } else {
      // add the function and context to the list
      readyList.push({fn: callback, ctx: context});
  }
  // if document already ready to go, schedule the ready function to run
  if (document.readyState === "complete") {
      setTimeout(ready, 1);
  } else if (!readyEventHandlersInstalled) {
      // otherwise if we don't have event handlers installed, install them
      if (document.addEventListener) {
          // first choice is DOMContentLoaded event
          document.addEventListener("DOMContentLoaded", ready, false);
          // backup is window load event
          window.addEventListener("load", ready, false);
      } else {
          // must be IE
          document.attachEvent("onreadystatechange", readyStateChange);
          window.attachEvent("onload", ready);
      }
      readyEventHandlersInstalled = true;
  }
}



// CONCATENATED MODULE: ./src/shiny-input-compound.js


class shiny_input_compound_CompountInputItem{

  constructor( el, binding, Shiny ){
    this._id = el.id;
    this._el = el;
    this._listeners = [];
    this.items = [];
    this.binding = binding;
    this._results = {};
    this._initialized = false;
    this.shiny = Shiny;

    // Get initialisation data
    this.header = el.getElementsByClassName('dipsaus-compound-input-header')[0];
    const g = JSON.parse( this.header.innerText );

    g.template = g.template[0];
    g.initial_ncomp = g.initial_ncomp[0];
    g.min_ncomp = g.min_ncomp[0];
    g.max_ncomp = g.max_ncomp[0];

    this.sub_ids = Object.keys( g.bind_infos );

    if( typeof g.initial_ncomp !== 'number' || g.initial_ncomp < 0 ){
      g.initial_ncomp = 0;
    }
    this.construct_params = g;

    // get body, add components
    this.body = el.getElementsByClassName('dipsaus-compound-input-body')[0];

    // Get footer, add + and - buttons
    this.footer = el.getElementsByClassName('dipsaus-compound-input-foot')[0];
    this.ctrl_div = el.getElementsByClassName('dipsaus-compound-input-foot-ctrl')[0];
    this.add_btn = document.createElement('button'),
    this.minus_btn = document.createElement('button');

    this.add_btn.className = 'btn btn-default';
    this.add_btn.innerText = '+';
    this.minus_btn.className = 'btn btn-default';
    this.minus_btn.innerText = '-';

    this.ctrl_div.appendChild(this.add_btn);
    this.ctrl_div.appendChild(this.minus_btn);
    this.footer.appendChild(this.ctrl_div);

    // create groups
    this.set_item_size( this.construct_params.initial_ncomp );

    // set values
    const vals = g.initial_value;
    if( Array.isArray(g.initial_value) ){
      g.initial_value.forEach(( v, ii ) => {
        v['.__item'] = ii + 1;
      });
    }else{
      g.initial_value = [];
    }
    this.receive_message({ value: g.initial_value });

    // Add listeners
    this.add_btn.addEventListener('click', () => {
      this.set_item_size( this._n + 1 );
      this.send_to_shiny( true );
    });

    this.minus_btn.addEventListener('click', () => {
      this.set_item_size( this._n - 1 );
      this.send_to_shiny( true );
    });

    // Add shiny custom message callbacks

    this._finish_init();
  }

  _finish_init(){
    if( !this._initialized ){
      this._initialized = true;
      this.send_to_shiny( true );
    }
  }

  send_to_shiny( defered ){
    if( this._initialized ){
      const evt = new CustomEvent('dipsaus.compoundInputChanged', { defered: defered || false });
      this._el.dispatchEvent( evt );
    }
  }

  // Shiny subscribe
  subscribe( callback ){
    const _f = (evt) => { callback(evt.defered) };
    this._listeners.push( _f );
    this._el.addEventListener( 'dipsaus.compoundInputChanged', _f );
  }
  unsubscribe(){
    this._listeners.forEach((_f) => {
      this._el.removeEventListener( 'dipsaus.compoundInputChanged', _f );
    });
    this._listeners.length = 0;
  }

  set_item_size( n ){
    const g = this.construct_params;
    let _n = n;
    if( _n < g.min_ncomp ){ _n = g.min_ncomp; }
    if( _n > g.max_ncomp ){ _n = g.max_ncomp; }
    if( this._n === _n ){ return(null); }

    for( let ii = 0; ii < g.max_ncomp ; ii++ ){
      if( ii >= this.items.length ){
        this.add_item();
      }
      if( ii < _n ){
        this.show_item( ii );
      }else{
        this.hide_item( ii );
      }
    }
    this._n = _n;
  }

  show_item( ii ){
    if( this.items.length > ii ){
      this.items[ ii ].el.style.display = 'block';
    }
  }
  hide_item( ii ){
    if( this.items.length > ii ){
      this.items[ ii ].el.style.display = 'none';
    }
  }

  add_item(){
    const idx = this.items.length + 1;

    const _i = {
      value : {}
    };


    let txt = this.construct_params.template.split('${{ind}}').join( idx );
    const legend_color = this.construct_params.label_color[ idx - 1 ];
    txt = txt.split('${{label_color}}').join( legend_color );

    // _i.el = dom_parser.parseFromString(txt, "text/html").children[0];
    _i.el = parse_html(txt);
    this.body.appendChild( _i.el );

    this.sub_ids.forEach((input_id) => {
      const nested_id = this._id + '_' + input_id + '_' + idx;
      const shiny_binding_info = this.construct_params.bind_infos[ input_id ];
      if( !shiny_binding_info ){
        console.log('Cannot find shiny inputbinding for type ' + input_id);
        return;
      }
      const shiny_binding = shiny_binding_info.binding;
      _i.value[input_id] = null;
      if( this.shiny.inputBindings.bindingNames.hasOwnProperty( shiny_binding ) ){

        const _b = this.shiny.inputBindings.bindingNames[shiny_binding].binding;

        const sl = _b.find( _i.el );
        const _f = (_el) => {
          if( _b.getId(_el) === nested_id ) {
            _b.initialize( _el );
            // _b.unsubscribe( _el );

            _b.subscribe( _el, ( defered ) => {
              _i.value[input_id] = _b.getValue( _el );
              this.send_to_shiny( defered );
            });


            _i.value[input_id] = _b.getValue( _el );
          }
        };

        if( jQuery !== undefined ){
          $(sl).each((ii, v) => { _f(v); });
        }else if( Array.isArray(sl) ){
          sl.forEach(_f);
        }

      }else{
        console.log('Cannot find input binding - ' + shiny_binding + ' - ' + nested_id);
      }

    });

    this.items.push( _i );
    this._n = idx;
  }

  get_value(){
    if( this._n > this.items.length ){
      console.log('Compoundinput number error');
      return(null);
    }
    this._results = {};
    for( let ii = 0; ii < this._n; ii++ ){
      this._results[ String(ii + 1) ] = this.items[ ii ].value;
    }
    this._results.meta = this.construct_params;
    return( this._results );
  }

  // get which^th item, which starts from 0
  get_sub_binding(which, inputid){
    if( which >= this.construct_params.max_ncomp || which < 0 ){ return; }

    const bind_info = this.construct_params.bind_infos[ inputid ];

    if( !bind_info ){ return; }
    const bind_name = bind_info.binding;

    if( !Array.isArray(bind_name) || bind_name.length < 1) { return; }

    if( !this.shiny.inputBindings.bindingNames.hasOwnProperty(bind_name[0]) ){ return; }

    const _b = this.shiny.inputBindings.bindingNames[ bind_name[0] ].binding;
    const scope = this.items[ which ].el;
    const el = _b.find( scope );

    for( let ii in el ){
      if( _b.getId( el[ ii ] ) === (this._id + '_' + inputid + '_' + (which + 1)) ){
        return({
          shiny_binding : _b,
          scope : scope,
          el : el[ ii ]
        });
      }
    }

    return;
  }

  receive_message( data ){
    data.value.forEach((_i) => {
      if( _i && typeof(_i) === 'object' ){
        let idx = _i['.__item'];
        if( idx ){ idx = idx - 1; }

        if( idx < this.construct_params.max_ncomp ){
          for( let k in _i ){
            const binding = this.get_sub_binding( idx, k );
            if( binding ){
              binding.shiny_binding.setValue( binding.el, _i[ k ] );
            }
          }
        }
      }
    });
    if( typeof data.ncomp === 'number' ){
      this.set_item_size( data.ncomp );
    }
  }
}

function register_compoundInput2 ( Shiny, debug = false ){
  const log = (s) => { if( !debug ){ console.log(s); } };
  if(Shiny === undefined){
    log('Shiny is not found.');
    return;
  }
  log('Register compountInput2 (dipsaus)');

  // Register!

  // Create binding skeleton for compound inputs
  const binding = new Shiny.InputBinding();
  const els = {};
  let window_loaded = false;

  // add bindings
  binding.find = (scope) => {
    if( has_jquery() ){
      return( $(scope).find('.dipsaus-compound-input') );
    }
    return( scope.getElementsByClassName('dipsaus-compound-input') );
  };

  // this method will be called on initialisation
  binding.initialize = (el) => {
    let _el = el;
    if( has_jquery() ){
      _el = $(_el)[0];
    }
    els[ _el.id ] = new shiny_input_compound_CompountInputItem( _el, binding, Shiny );
  };

  binding.getValue = (el) => {
    let _el = el;
    if( has_jquery() ){
      _el = $(_el)[0];
    }
    if( !els[ _el.id ] ){ return; }
    return(els[ _el.id ].get_value());
  };
  binding.subscribe = (el, callback) => {
    let _el = el;
    if( has_jquery() ){
      _el = $(_el)[0];
    }
    const _i = els[ _el.id ];
    if( _i ){
      _i.subscribe( callback );
    }
  };
  binding.unsubscribe = (el) => {
    let _el = el;
    if( has_jquery() ){
      _el = $(_el)[0];
    }
    const _i = els[ _el.id ];
    if( _i ){
      _i.unsubscribe();
    }
  };
  binding.getRatePolicy = () => {
    return {
      policy: 'debounce',
      delay: 250
    };
  };
  binding.getType = (el) => {
    return( "dipsaus.compoundInput2" );
  };
  binding.receiveMessage = (el, data) => {
    let _el = el;
    if( has_jquery() ){
      _el = $(_el)[0];
    }
    const _i = els[ _el.id ];
    if( _i ){
      _i.receive_message( data );
    }
  };

  // Shiny priority system is shit
  Shiny.inputBindings.register(binding, 'dipsaus.compoundInput2', 0);
}



// CONCATENATED MODULE: ./src/shiny-input-actionbutton2.js


function register_actionButtonStyled ( Shiny, debug = false ){
  const log = logger( debug );
  const shiny_binding = Shiny.inputBindings.bindingNames["shiny.actionButtonInput"].binding;

  document_ready( () => {
    Shiny.addCustomMessageHandler('dipsaus.updateActionButtonStyled', ( msg ) => {
      const inputId = msg.inputId;
      log( inputId );
    });
  });
}



/*
var actionButtonInputBinding = new InputBinding();
$.extend(actionButtonInputBinding, {
  find: function(scope) {
    return $(scope).find(".action-button");
  },
  getValue: function(el) {
    return $(el).data('val') || 0;
  },
  setValue: function(el, value) {
    $(el).data('val', value);
  },
  getType: function(el) {
    return 'shiny.action';
  },
  subscribe: function(el, callback) {
    $(el).on("click.actionButtonInputBinding", function(e) {
      var $el = $(this);
      var val = $el.data('val') || 0;
      $el.data('val', val + 1);

      callback();
    });
  },
  getState: function(el) {
    return { value: this.getValue(el) };
  },
  receiveMessage: function(el, data) {
    var $el = $(el);

    // retrieve current label and icon
    var label = $el.text();
    var icon = '';

    // to check (and store) the previous icon, we look for a $el child
    // object that has an i tag, and some (any) class (this prevents
    // italicized text - which has an i tag but, usually, no class -
    // from being mistakenly selected)
    if ($el.find('i[class]').length > 0) {
      var icon_html = $el.find('i[class]')[0];
      if (icon_html === $el.children()[0]) {     // another check for robustness
        icon = $(icon_html).prop('outerHTML');
      }
    }

    // update the requested properties
    if (data.hasOwnProperty('label')) label = data.label;
    if (data.hasOwnProperty('icon')) {
      icon = data.icon;
      // if the user entered icon=character(0), remove the icon
      if (icon.length === 0) icon = '';
    }

    // produce new html
    $el.html(icon + ' ' + label);
  },
  unsubscribe: function(el) {
    $(el).off(".actionButtonInputBinding");
  }
});
inputBindings.register(actionButtonInputBinding, 'shiny.actionButtonInput');


$(document).on('click', 'a.action-button', function(e) {
  e.preventDefault();
});
*/
// CONCATENATED MODULE: ./src/index.js




const src_Shiny = window.Shiny;
const DEBUG = true;

register_actionButtonStyled( src_Shiny, DEBUG );
register_compoundInput2( src_Shiny, DEBUG );

/***/ })
/******/ ]);
//# sourceMappingURL=dipsaus-dipterix-lib.js.map