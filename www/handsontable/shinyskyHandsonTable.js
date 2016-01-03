




/* handson table from shiny sky*/

function isNumber(n) {
  return !isNaN(parseFloat(n)) && isFinite(n);
}

//input binding
var hotable = new Shiny.InputBinding();
$.extend(hotable, {
  find: function(scope) {
    return $(scope).find(".hotable");
  },
  getValue: function(el) {
    var ht = $(el).handsontable("getInstance");
   
    if( ht === undefined){
        return (null);
      } else {
        var ht1 = ht.getData();
          ht2 = ht1;
          return ({
            colHeaders: ht.getColHeader(),
            data: ht1
          });
      }
  },
  setValue: function(el, value) {},
  subscribe: function(el, callback) {
    $(el).on("afterChange", function(e) {
      callback();
    });
  },
  unsubscribe: function(el) {
    $(el).off(".hotable");
  }
});
Shiny.inputBindings.register(hotable);

//output binding
var hotableOutput = new Shiny.OutputBinding();
$.extend(hotableOutput, {
  find: function(scope) {
    return $(scope).find('.hotable');
  },
  renderValue: function(el, json) {
    if (json === null) return;
    if (!json.hasOwnProperty("data")) return;

    // define handsontable
    $(el).handsontable({
      columns: json.columns,
      manualColumnResize: true,
      minSpareRows: json.nSpareRow, // at least one empty row
      maxRows : json.maxRows, // if no thing is given, set as the nrows(df)
      colHeaders: json.colHeaders,
      handlebar: false,
      fixedColumnsLeft: json.fixedCols,
      stretchH:json.stretched,
// contextMenu: true,
      columnSorting: true
    });

    var ht = $(el).handsontable('getInstance');
    var obj_of_arr = json.data;
    // console.debug(obj_of_arr);

    // parse an object of arrays into array of object
    var keys = json.colHeaders; // obtain the keys from the colHeaders - This retains the order of the columns
    var arr_of_obj = [];

    if (typeof obj_of_arr[keys[0]] === "object") {
      var l = obj_of_arr[keys[0]].length;
      for (var i = 0; i < l; i++) {
        var tmpobj = [];
        keys.map(function(key) {
          tmpobj.push(obj_of_arr[key][i]);
        });
        arr_of_obj.push(tmpobj);
      }
    } else {
      var l = 1;
      var tmpobj = [];
      keys.map(function(key) {
        tmpobj.push(obj_of_arr[key]);
      });
      arr_of_obj = [tmpobj];
    }
    //console.debug(arr_of_obj);

    ht.loadData(arr_of_obj);
    ht.addHook("afterChange", function() {
      $(el).trigger("afterChange");
    });
    $(el).trigger("afterChange");
  }
});
Shiny.outputBindings.register(hotableOutput, "hotable");



// id : element id (string)
// col : col name (string)
// val : value to update with
function  hotableSetColValues(id,col,val){
  $tbl  = $("#"+id);
  res   = [];
  if($tbl !== undefined){
    ht = $tbl.handsontable("getInstance");
    rc = ht.countRows();
    cc = ht.countCols();
    if(rc > 0 && cc > 0){
      // search 
      hed = ht.getColHeader();
      pos = hed.indexOf(col);
      if( pos !== undefined){

        for(i = 0; i < rc; i++){
          res.push([i,pos,val]);
        }
        ht.setDataAtCell(res);
      }
    }
  }
}



