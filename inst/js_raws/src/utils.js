

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


export { has_jquery , parse_html, logger, document_ready };