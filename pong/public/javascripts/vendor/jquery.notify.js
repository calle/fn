/**
*  jQuery Notify Plugin v0.1
*
*  Copyright (c) 2010 Jakob Stenqvist
*  Dual licensed under the MIT or GPL Version 2 licenses.
*  http://jquery.org/license
* 
* Use:
* $.notify.error("Error flash", "This error message will animate in and out.")
* $.notify.error("Error title", "Create a sticky, this error message will not be animated out, but will be removed on click.", true)
* $.notify.info("Title", "Info message, can be used as sticky or use normal in/out animation.")
*
*/

(function( $ , win ){
  if( !$ ) return;
  function Notify(m){
    if( m ) this.modal = m;
    var self = this,
      messages = [],
      article = $("<article><h2/><h1/></article>");
  
    return {
      notify:  function( n ){
        var a = article.clone().addClass(n.type);
        self.modal.append( a );
        if( n.title ) $("h1", a).text(n.title);
        if( n.type ) $("h2", a).text(n.type);
        if( n.message ) $("<p />").appendTo(a).html(n.message);
      
        if( !n.sticky ) 
          a.slideUp(0).slideDown("fast").delay( n.delay ? n.delay : 4000 ).slideUp("fast", function(){
            $(this).remove();
          });
        a.click( function(){
          $(this).stop(true, false).slideUp("fast", function(){
            $(this).remove();
          });
        });
        return a;
      },
      error: function( title, message, sticky, delay ) {
        return this.notify( {title: title, message: message, type: "error", sticky: sticky, delay: delay });
      },
      info: function( title, message, sticky, delay ) {
        return this.notify( {title: title, message: message, type: "info", sticky: sticky, delay: delay });
      }
    }
  }
  win.Notify = Notify;
  $.notify = Notify();
})( jQuery, window );