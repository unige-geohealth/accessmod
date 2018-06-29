


var stopProcess =  function(stop){
  var action = false;
  if( stop ){
  action = true;
  }
  console.log("stopProcess= "+action);
  Shiny.onInputChange('cleanExit',action);  
};




/**
 * Create and manage multiple progression bar
 * @param {boolean} enable Enable the screen 
 * @param {string} id Identifier of the given item
 * @param {number} percent Progress bar percentage
 * @param {string} text Optional text
 * @param {function} stopFunction Display a button and launch this function if provided
 */
var progressScreen = function(enable, id, percent, text, stopFunction ) {

  var lScreen = document.getElementsByClassName("loading-screen")[0],
  lItem = document.getElementById(id),
  lBusy = document.getElementsByClassName("shiny-busy-panel")[0],
  lBody = document.getElementsByTagName("body")[0]; 


  if (!enable) {
    if (lScreen) lScreen.remove();
    return;
  }

  if (!id || !percent || !text) return;

  if (!lScreen && enable) {
    lScreen = document.createElement("div");
    lScreen.className = "loading-screen";
    lScreenContainer = document.createElement("div");
    lScreenContainer.className = "loading-container";
    lScreen.appendChild(lScreenContainer);
    if(!lBusy){ 
      lBody.appendChild(lScreen);
    }else{
      lBody.insertBefore(lScreen,lBusy);
    }
  }

  if (!lItem) {
    //
    lItem = document.createElement("div");
    btnStop = document.createElement("i");
    pBarIn = document.createElement("div");
    pBarOut = document.createElement("div");
    pBarTxt = document.createElement("div");
    pBarTxtSpan =  document.createElement("span");
    //
    lItem.className = "loading-item";
    lItem.setAttribute("id", id);
    pBarIn.className = "loading-bar-in";
    pBarOut.className = "loading-bar-out";
    pBarTxt.className = "loading-bar-txt";
    pBarTxtSpan.className = "loading-bar-txt-content";
    //
    pBarOut.appendChild(pBarIn);
    lItem.appendChild(pBarOut);
    lItem.appendChild(pBarTxt);
    lScreenContainer.appendChild(lItem);

    if (stopFunction){
      fun = function(){
        pBarTxtSpan.innerText = "Stop as soon as possible...";
        stopFunction(true);
      };
      btnStop.setAttribute("class","fa fa-stop-circle");
      btnStop.addEventListener('click', fun, false);
    }

    pBarTxt.appendChild(btnStop);
    pBarTxt.appendChild(pBarTxtSpan);  

  } else {
    pBarIn = lItem.getElementsByClassName("loading-bar-in")[0];
    pBarTxtSpan = lItem.getElementsByClassName("loading-bar-txt-content")[0];
  }

  if (percent >= 100) {
    if (lItem) lItem.remove();
    stopProcess(false);
  } else {
    pBarIn.style.width = percent + "%";
    pBarTxtSpan.innerHTML = text;
  }

  lItems = lScreenContainer.getElementsByClassName("loading-item");

  if (lItems.length === 0) progressScreen(false);

};


$(document).ready(function(){

  /* create panel busy*/
  var body = document.getElementsByTagName("body")[0];
  var panelBusy = document.createElement("div");
  var panelBusyContent = document.createElement("div");
  var panelBusyText =  document.createElement("p");

  panelBusyText.innerHTML = "Loading, please wait";
  panelBusy.setAttribute("class","shiny-busy-panel");
  panelBusyContent.setAttribute("class","shiny-busy-panel-content");

  panelBusyContent.appendChild(panelBusyText);
  panelBusy.appendChild(panelBusyContent);
  body.appendChild(panelBusy);

  //var progressOld = {};

  function progressUpdate(m){
    m.title = decodeURIComponent(escape(window.atob(m.title)));
    m.text = decodeURIComponent(escape(window.atob(m.text)));
    text = m.title + ": " + m.text;
    progressScreen(true,"shinyProgressBar",m.percent,text,stopProcess);
  }

  Shiny.addCustomMessageHandler("progressUpdate",progressUpdate);

});
