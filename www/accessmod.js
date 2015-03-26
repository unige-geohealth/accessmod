

Shiny.addCustomMessageHandler("jsCode",
    function(message) {
      /*console.log(message);*/
      eval(message.code);
    }
    );



/*http://stackoverflow.com/questions/1495219/how-can-i-prevent-the-backspace-key-from-navigating-back*/
$(function(){
  /*
   *      * this swallows backspace keys on any non-input element.
   *           * stops backspace -> back
   *                */
  var rx = /INPUT|SELECT|TEXTAREA/i;

  $(document).bind("keydown keypress", function(e){
    if( e.which == 8 ){ // 8 == backspace
      if(!rx.test(e.target.tagName) || e.target.disabled || e.target.readOnly ){
        e.preventDefault();
      }
    }
  });
});

