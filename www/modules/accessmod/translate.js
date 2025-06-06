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
*   Copyright (c) 2014-present WHO, Frederic Moser (GeoHealth group, University of Geneva)
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


function amSetLanguage(opt) {
  const elRoot = opt.elRoot || document;
  const els = elRoot.querySelectorAll('[data-amt_id]');
  if(am.settings.language === opt.lang){
     return;
  }
  am.settings.language = opt.lang || opt.langDefault || am.settings.language || 'en';
  els.forEach(function(el) {
    const id = el.dataset.amt_id;
    el.innerText = amSearchDict(id, opt.lang, opt.langDefault);
  });
  const elsInput = elRoot.querySelectorAll('input');
  elsInput.forEach(function(el) {
  if(el.placeholder){
    el.placeholder = amSearchDict('placeholder_enter_value',opt.lang,opt.langDefault);
  }
  });
}

function amSearchDict(id, lang, langDefault) {
  lang = lang || langDefault ||  am.settings.language;
  langDefault = langDefault ||  am.settings.language;
  const dict = am.dictionary;
  if (dict && dict instanceof Array) {
    const trad = dict.find(function(item) {
      return item.id === id;
    });
    if (trad && trad[lang]) {
      return trad[lang];
    }
    if (trad && trad[langDefault]) {
      return trad[langDefault];
    }
  }
  return id;
}
