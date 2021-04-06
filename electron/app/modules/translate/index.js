const path = require('path');
const fs = require('fs');
const dict = JSON.parse(fs.readFileSync(path.join(__dirname, './json/dict.json')));

function tl(id, lang, obj, def) {
  let item = ((dict.find((d) => d.id === id) || {})[lang || 'en']) || def || '';
  if (typeof item === 'string') {
    if(obj){
      for (let k in obj) {
        item = item.replace(`{{${k}}}`, obj[k]);
      }
    }
    if(!item){
     item = tl(id, 'en', obj, id);
    }
    return item;
  } else {
    console.log({obj,item});
    return id;
  }
}

exports.tl = tl;
