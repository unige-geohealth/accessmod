/**
*
*        ___                                  __  ___            __   ______
*       /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
*      / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
*     / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
*    /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
*
*   AccessMod 5 Supporting Universal Health Coverage by modelling physical accessibility to health care
*   
*   Copyright (c) 2014-2020  WHO, Frederic Moser (GeoHealth group, University of Geneva)
*   
*   This program is free software: you can redistribute it and/or modify
*   it under the terms of the GNU General Public License as published by
*   the Free Software Foundation, either version 3 of the License, or
*   (at your option) any later version.
*   
*   This program is distributed in the hope that it will be useful,
*   but WITHOUT ANY WARRANTY; without even the implied warranty of
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*   GNU General Public License for more details.
*   
*   You should have received a copy of the GNU General Public License
*   along with this program.  If not, see <https://www.gnu.org/licenses/>.
*/



var stopProcess = function(stop) {
  var action = false;
  if (stop) {
    action = true;
  }
  Shiny.onInputChange('cleanExit', action);
};

/**
 * Create and manage multiple progression bar
 * @param {boolean} enable Enable the screen
 * @param {string} id Identifier of the given item
 * @param {number} percent Progress bar percentage
 * @param {string} title Optional title
 * @param {string} text Optional text
 * @param {function} stopFunction Display a button and launch this function if provided
 */
var progressScreen = function(enable, id, percent, title, text, stopFunction) {
  var lScreen = document.getElementsByClassName('loading-screen')[0],
    lItem = document.getElementById(id),
    lBusy = document.getElementsByClassName('shiny-busy-panel')[0],
    lBody = document.getElementsByTagName('body')[0];

  if (!enable) {
    if (lScreen) {
      lScreen.remove();
    }
    return;
  }

  if (!id || !text) {
    return;
  }

  if (!lScreen && enable) {
    lScreen = document.createElement('div');
    lScreen.className = 'loading-screen';
    lScreenContainer = document.createElement('div');
    lScreenContainer.className = 'loading-container';
    lScreen.appendChild(lScreenContainer);
    if (!lBusy) {
      lBody.appendChild(lScreen);
    } else {
      lBody.insertBefore(lScreen, lBusy);
    }
  }

  if (!lItem) {
    //
    lItem = document.createElement('div');
    btnStop = document.createElement('i');
    pBarIn = document.createElement('div');
    pBarOut = document.createElement('div');
    pBarTxt = document.createElement('div');
    pBarTitleSpan = document.createElement('span');
    pBarTxtSpan = document.createElement('span');
    //
    lItem.className = 'loading-item';
    lItem.setAttribute('id', id);
    pBarIn.className = 'loading-bar-in';
    pBarOut.className = 'loading-bar-out';
    pBarTxt.className = 'loading-bar-txt';
    pBarTxtSpan.className = 'loading-bar-txt-content';
    pBarTitleSpan.className = 'loading-bar-title-content';
    pBarTxtSpan.style.fontSize = '0.9em';
    pBarTxtSpan.style.marginLeft = '5px';
    //
    pBarOut.appendChild(pBarIn);
    lItem.appendChild(pBarOut);
    lItem.appendChild(pBarTxt);
    lScreenContainer.appendChild(lItem);

    if (stopFunction) {
      fun = function() {
        pBarTxtSpan.innerText = 'Stop as soon as possible...';
        stopFunction(true);
      };
      btnStop.setAttribute('class', 'fa fa-stop-circle');
      btnStop.addEventListener('click', fun, false);
    }

    pBarTxt.appendChild(btnStop);
    pBarTxt.appendChild(pBarTitleSpan);
    pBarTxt.appendChild(pBarTxtSpan);
  } else {
    pBarIn = lItem.getElementsByClassName('loading-bar-in')[0];
    pBarTxtSpan = lItem.getElementsByClassName('loading-bar-txt-content')[0];
    pBarTitleSpan = lItem.getElementsByClassName(
      'loading-bar-title-content'
    )[0];
  }

  if (percent >= 100) {
    if (lItem) {
      lItem.remove();
    }
    stopProcess(false);
  } else {
    pBarIn.style.width = percent + '%';
    pBarTitleSpan.innerHTML = title;
    pBarTxtSpan.innerHTML = ' – ' + text;
  }

  lItems = lScreenContainer.getElementsByClassName('loading-item');

  if (lItems.length === 0) {
    progressScreen(false);
  }
};

$(document).ready(function() {
  /* create panel busy*/
  var body = document.getElementsByTagName('body')[0];
  var panelBusy = document.createElement('div');
  var panelBusyContent = document.createElement('div');
  var panelBusyText = document.createElement('p');

  panelBusyText.innerHTML = 'Loading, please wait';
  panelBusy.setAttribute('class', 'shiny-busy-panel');
  panelBusyContent.setAttribute('class', 'shiny-busy-panel-content');

  panelBusyContent.appendChild(panelBusyText);
  panelBusy.appendChild(panelBusyContent);
  body.appendChild(panelBusy);

  //var progressOld = {};

  function progressUpdate(m) {
    m.title = decodeURIComponent(escape(window.atob(m.title)));
    m.text = decodeURIComponent(escape(window.atob(m.text)));
    progressScreen(
      m.visible,
      'shinyProgressBar',
      m.percent,
      m.title,
      m.text,
      stopProcess
    );
  }

  Shiny.addCustomMessageHandler('progressUpdate', progressUpdate);
});
