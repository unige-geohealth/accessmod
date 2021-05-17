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

$(document).on('shiny:connected', function() {
  Shiny.addCustomMessageHandler('amJsCode', amEvaluateJsCode);
  Shiny.addCustomMessageHandler('amJsDebug', amDebugJs);
  Shiny.addCustomMessageHandler('amBtnDisable', amDisableBtn);
  Shiny.addCustomMessageHandler('amInputDisable', amDisableInput);
  Shiny.addCustomMessageHandler('amSelectizeDisable', amDisableSelectize);
  Shiny.addCustomMessageHandler('amLinkDisable', amDisableLink);
  Shiny.addCustomMessageHandler('amUpdateText', amUpdateText);
  Shiny.addCustomMessageHandler('amUpdateSortable', amUpdateSortable);
  Shiny.addCustomMessageHandler('amGetClientTime', amGetClientTime);
  Shiny.addCustomMessageHandler('amSetCookie', amSetCookies);
  Shiny.addCustomMessageHandler('amSetLanguage', amSetLanguage);
  Shiny.addCustomMessageHandler('amUiClassList', amUiClassList);
  Shiny.addCustomMessageHandler('amWriteMarkdown', amWriteMarkdown);

  /*
   *  this swallows backspace keys on any non-input element.
   *  NOTE; http://stackoverflow.com/questions/1495219/how-can-i-prevent-the-backspace-key-from-navigating-back
   * stops backspace -> back
   */
  var rx = /INPUT|SELECT|TEXTAREA/i;
  $(document).bind('keydown keypress', function(e) {
    if (e.which === 8) {
      // 8 == backspace
      if (
        !rx.test(e.target.tagName) ||
        e.target.disabled ||
        e.target.readOnly
      ) {
        e.preventDefault();
      }
    }
  });

  /**
   * Register timezone offset ( minutes )
   */
  var offset = new Date().getTimezoneOffset();
  Shiny.onInputChange('timeOffset', offset || 1);

  /**
   * Add modal observer for changes, convert marked
   * class to html
   */
  mutationObserveMarked('amModal');
});

/*
 * Ask confirmation before reload
 */
window.onbeforeunload = function() {
  return;
  /*  var dialogText = 'Are you sure you want to quit?';*/
  //e.returnValue = dialogText;
  /*return dialogText;*/
};

/**
 * Shiny input bindings
 */
var doubleSortableBinding = new Shiny.InputBinding();
$.extend(doubleSortableBinding, {
  find: function(scope) {
    return $(scope).find('.am_dbl_srt_input');
  },
  getValue: function(el) {
    attr = 'data-input';
    var res = [];
    $(el)
      .children()
      .each(function() {
        res.push($(this).attr('data-input'));
      });
    return res;
  },
  setValue: function(el, value) {
    $(el).innerHTML = value;
  },
  subscribe: function(el, callback) {
    $(el).on('change.doubleSortableBinding', function() {
      callback();
    });
  },
  unsubscribe: function(el) {
    $(el).off('change.doubleSortableBinding');
  }
});
Shiny.inputBindings.register(doubleSortableBinding);

/*
 * Read cookie as input
 */
var shinyCookieInputBinding = new Shiny.InputBinding();
$.extend(shinyCookieInputBinding, {
  find: function(scope) {
    return $(scope).find('.shinyCookies');
  },
  getValue: function() {
    return readCookie();
  }
});
Shiny.inputBindings.register(shinyCookieInputBinding);

/*
 *  Generic read cookie function
 */
function readCookie() {
  var cookies = document.cookie.split('; ');
  var values = {};
  for (var i = 0; i < cookies.length; i++) {
    var spcook = cookies[i].split('=');
    values[spcook[0]] = spcook[1];
  }
  return values;
}

/**
 * Delete all cookie value NOTE: cookie path rewriting
 * http://stackoverflow.com/questions/595228/how-can-i-delete-all-cookies-with-javascript#answer-11095647
 */
function clearListCookies() {
  var cookies = document.cookie.split(';');
  for (var i = 0; i < cookies.length; i++) {
    var spcook = cookies[i].split('=');
    deleteCookie(spcook[0]);
  }
  function deleteCookie(cookiename) {
    var d = new Date();
    d.setDate(d.getDate() - 1);
    var expires = ';expires=' + d;
    var name = cookiename;
    var value = '';
    document.cookie = name + '=' + value + expires + '; path=/';
  }
}

function amEvaluateJsCode(message) {
  eval(message.code);
}
function amDebugJs(m) {
  console.log(m);
}
function amDisableBtn(m) {
  if (m.disable) {
    $('#' + m.id)
      .addClass('btn-danger')
      .removeClass('btn-default')
      .prop('disabled', true)
      .children()
      .prop('disabled', true);
  } else {
    $('#' + m.id)
      .addClass('btn-default')
      .removeClass('btn-danger')
      .attr('disabled', false)
      .children()
      .prop('disabled', false);
  }
}
function amDisableInput(m) {
  var elInput = document.getElementById(m.id);
  if (m.disable) {
    elInput.setAttribute('disabled', true);
  } else {
    elInput.removeAttribute('disabled');
  }
}
function amDisableSelectize(m) {
  var selectize = $(document.getElementById(m.id))[0].selectize;
  if (!selectize) {
    return;
  }
  if (m.disable) {
    selectize.disable();
  } else {
    selectize.enable();
  }
}
function amDisableLink(m) {
  if (m.disable) {
    $('#' + m.id)
      .css({color: 'red', display: 'inline'})
      .addClass('btn btn-txt-left')
      .prop('disabled', true)
      .children()
      .prop('disabled', true);
  } else {
    $('#' + m.id)
      .css('color', '')
      .prop('disabled', false)
      .removeClass('btn btn-txt-left')
      .children()
      .prop('disabled', false);
  }
}

function amUiClassList(o) {
  o = o || {};
  var i = 0;
  var elDiv = document.getElementById(o.id);

  if (elDiv) {
    if (o.add instanceof Array) {
      for (i = 0, iL = o.add.length; i < iL; i++) {
        elDiv.classList.add(o.add[i]);
      }
    }
    if (o.remove instanceof Array) {
      for (i = 0, iL = o.remove.length; i < iL; i++) {
        elDiv.classList.remove(o.remove[i]);
      }
    }
  }
}

function amWriteMarkdown(m) {
  var el = document.getElementById(m.id);
  var elDest = document.createElement('p');
  if (el instanceof Element && m.text) {
    if (m.text instanceof Array) {
      m.text = m.text.join('\n');
    }
    el.parentElement.replaceChild(elDest, el);
    elDest.style.maxHeight = '400px';
    elDest.style.overflowY = 'auto';
    elDest.innerHTML = marked(m.text);
  }
}

function mutationObserveMarked(id) {
  var targetNode = document.getElementById(id);
  if (!targetNode) {
    return;
  }
  var config = {childList: true, subtree: true};
  var mutation;
  var elsMarked;
  var callback = function(mutationsList) {
    for (var i = 0, iL = mutationsList.length; i < iL; i++) {
      mutation = mutationsList[i];
      if (mutation.type === 'childList') {
        elsMarked = targetNode.querySelectorAll('.markdown');
        for (var j = 0, jL = elsMarked.length; j < jL; j++) {
          var elMark = elsMarked[j];
          var str = elMark.innerText;
          if (elMark.classList.contains('base64')) {
            str = b64_to_utf8(str);
          }
          elMark.innerHTML = marked(str);
          elMark.style.maxHeight = '400px';
          elMark.style.overflowY = 'auto';
          elMark.classList.remove('markdown');
        }
      }
    }
  };
  var observer = new MutationObserver(callback);
  observer.observe(targetNode, config);
}

function amUpdateText(m) {
  el = document.getElementById(m.id);
  if (typeof el !== 'undefined' && el !== null) {
    el.innerHTML = b64_to_utf8(m.txt.toString());
    if (m.addId) {
      setUniqueItemsId();
    }
  }
}

function amUpdateSortable(m) {
  $('#' + m).change();
}

function amGetClientTime(s) {
  var d = new Date();
  var clientPosix = parseInt(d.getTime() / 1000);
  var clientTimeZone = -(d.getTimezoneOffset() / 60);
  var res = {
    serverPosix: s.serverPosix,
    serverTimeZone: s.serverTimeZone,
    clientPosix: clientPosix,
    clientTimeZone: clientTimeZone
  };
  Shiny.onInputChange('clientTime', res);
}

function amSetCookies(m) {
  if (m.deleteAll) {
    clearListCookies();
  } else {
    for (var i in m.cookies) {
      if (true) {
        setCookie(i, m.cookies[i], m.expires);
      }
    }
  }
  if (m.reload) {
    window.location.reload();
  }
}
function setCookie(cname, cvalue, exdays) {
  var d = new Date();
  d.setTime(d.getTime() + exdays * 24 * 60 * 60 * 1000);
  var expires = 'expires=' + d.toUTCString();
  document.cookie = cname + '=' + cvalue + '; ' + expires;
}
function b64_to_utf8(str) {
  str = str.replace(/\s/g, '');
  return decodeURIComponent(escape(window.atob(str)));
}
function utf8_to_b64(str) {
  return window.btoa(unescape(encodeURIComponent(str)));
}
function isNotEmpty(str) {
  var r =
    typeof str !== undefined && str.length > 0 && str.indexOf('NO DATA') === -1;
  return r;
}

/**
 * Download file
 */
window.downloadFile = function downloadFile(sUrl) {
  //iOS devices do not support downloading. We have to inform user about this.
  if (/(iP)/g.test(navigator.userAgent)) {
    alert(
      'Your device does not support files downloading. Please try again in desktop browser.'
    );
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
};

window.downloadFile.isChrome =
  navigator.userAgent.toLowerCase().indexOf('chrome') > -1;
window.downloadFile.isSafari =
  navigator.userAgent.toLowerCase().indexOf('safari') > -1;
