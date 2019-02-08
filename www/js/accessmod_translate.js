
$.getJSON("/dict/main.json", function(dict) {
  window.dict = dict;
});

function amSetLanguage(opt){
  var els = document.querySelectorAll("[data-amt_id]");
  els.forEach(function(el){
    var id = el.dataset.amt_id;
    el.innerText = amSearchDict(id,opt.lang,opt.langDefault);
  });
}

function amSearchDict(id,lang,langDefault){
  if(dict && dict instanceof Array){
    var trad = dict.find(function(item){return item.id == id;});
    if(trad && trad[lang]) return trad[lang];
    if(trad && trad[langDefault]) return trad[langDefault];
  }
  return id;
}
