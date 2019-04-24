$.getJSON('dict/main.json', function(dict) {
  window.dict = dict;
});

window.am5 = {
  settings: {
    language: 'en'
  }
};

function amSetLanguage(opt) {
  var elRoot = opt.elRoot || document;
  var els = elRoot.querySelectorAll('[data-amt_id]');
  am5.settings.language = opt.lang || opt.langDefault || am5.settings.language || 'en';
  els.forEach(function(el) {
    var id = el.dataset.amt_id;
    el.innerText = amSearchDict(id, opt.lang, opt.langDefault);
  });
  var elsInput = elRoot.querySelectorAll('input');
  elsInput.forEach(function(el) {
  if(el.placeholder){
    el.placeholder = amSearchDict('placeholder_enter_value',opt.lang,opt.langDefault);
  }
  });
}

function amSearchDict(id, lang, langDefault) {
  lang = lang || langDefault ||  am5.settings.language;
  langDefault = langDefault ||  am5.settings.language;

  if (dict && dict instanceof Array) {
    var trad = dict.find(function(item) {
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
