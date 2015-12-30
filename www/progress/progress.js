





$(document).ready(function(){






  $('[data-toggle="tooltip"]').tooltip();   

  $pan = $("#panel-for-sup");
  $pan.toggleClass("progress-hidden");



  var progressOld = {};

  function progressUpdate(m){
    var el = [],
        progOld = progressOld,
        text = "",
        $pPanel = $("#progress-panel-for-"+m.id),
        $pExit  = $("#pBarExit"),
        $pContent = $("#pBarContent") ;


    

      // conditional
    if(typeof(m.visible) != "undefined" && m.visible && !progOld.visible){
      $pPanel.removeClass("pbar-hidden").fadeIn(200,"linear");
    }else{
      $pPanel.addClass('pbar-hidden');
      Shiny.onInputChange('cleanExit',false);
      $pExit.addClass('pbar-hidden');
    }

    if(typeof(m.title) != "undefined" && m.title != progOld.title){
      txt = decodeURIComponent(escape(window.atob(m.title)));
      $("#progress-title-for-"+m.id).html(txt);
    }
    if(typeof(m.title) != "undefined" && m.text != progOld.text){
      txt = decodeURIComponent(escape(window.atob(m.text)));
      $("#progress-text-for-"+m.id).html(txt);
    }
    if(typeof(m.tooltip) != "undefined" && m.tooltip != progOld.tooltip){
      txt = decodeURIComponent(escape(window.atob(m.tooltip)));
      el = $("#progress-tooltip-for-"+m.id);
      el.attr("data-original-title",txt);
    }
    if(typeof(m.percent) != "undefined" && m.percent != progOld.percent ){
      $("#progress-for-"+m.id).width(m.percent+"%");
    }
    progOld = m;
  }


  Shiny.addCustomMessageHandler("progressUpdate",progressUpdate);

});
