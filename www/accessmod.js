

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



/* Check if shiny is busy and add progress cursor if necessary*/
amCheckBusy = function(){
  if ($('html').hasClass('shiny-busy')){
    $("body").css("cursor", "progress");
  } else {
    $("body").css("cursor", "auto");
  }
};
setInterval(amCheckBusy,100);



amAddBusy = function () {
  $('html').addClass('shiny-busy')
}

amRemoveBusy = function(){
  $('html').removeClass('shiny-busy')
}

window.downloadFile = function (sUrl) {

  //iOS devices do not support downloading. We have to inform user about this.
  if (/(iP)/g.test(navigator.userAgent)) {
    alert('Your device does not support files downloading. Please try again in desktop browser.');
    return false;
  }

  //If in Chrome or Safari - download via virtual link click
  if (window.downloadFile.isChrome || window.downloadFile.isSafari) {
    //Creating new link node.
    var link = document.createElement('a');
    link.href = sUrl;

    if (link.download !== undefined) {
      //Set HTML5 download attribute. This will prevent file from opening if supported.
      var fileName = sUrl.substring(sUrl.lastIndexOf('/') + 1, sUrl.length);
      link.download = fileName;
    }

    //Dispatching click event.
    if (document.createEvent) {
      var e = document.createEvent('MouseEvents');
      e.initEvent('click', true, true);
      link.dispatchEvent(e);
      return true;
    }
  }

  // Force file download (whether supported by server).
  if (sUrl.indexOf('?') === -1) {
    sUrl += '?download';
  }

  window.open(sUrl, '_blank');
  return true;
}

window.downloadFile.isChrome = navigator.userAgent.toLowerCase().indexOf('chrome') > -1;
window.downloadFile.isSafari = navigator.userAgent.toLowerCase().indexOf('safari') > -1;
