const path = require('path');
const fs = require('fs');
const dict = JSON.parse(fs.readFileSync(path.join(__dirname, './dict.json')));

function tl(id, lang, obj) {
  let item = ((dict.find((d) => d.id === id) || {})[lang || 'en']) || '';
  //console.warn({item});
  if (typeof item === 'string') {
    if(obj){
      for (let k in obj) {
        //console.log({typofItem:`${typeof item}`});
        item = item.replace(`{{${k}}}`, obj[k]);
      }
    }
    return item;
  } else {
    console.log({obj,item});
    return id;
  }
}

exports.tl = tl;
