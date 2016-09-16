

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

      var data = ht.getData();
      var res = {};
      var cols = ht.getColHeader();

      for(var c= 0; c<cols.length; c++){
        var col = cols[c];
        res[col] = [];
        for(var l=0;l<data.length;l++){
          res[col].push(data[l][c]);
        }
      }

      return ({
        colHeaders: cols,
        data: JSON.stringify(res)
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
      stretchH:json.stretched,
      columnSorting: true,
      data:json.data
    });
    var ht = $(el).handsontable("getInstance");
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


function newWorker(fun){
  // convert input function to string
  fun = fun.toString();
  fun = fun
    .substring(
        fun.indexOf("{")+1, 
        fun.lastIndexOf("}")
        );
  // Make a blob
  var blob = new Blob(
      [fun],
      {type: "application/javascript"}
      );
  // convert as url for new worker
  var blobUrl = URL.createObjectURL(blob);

  // return new worker
  return(new Worker(blobUrl));
}

function workerSetColCond(){
  // Inital message
  postMessage({
    progress: 0,
    message: "start"
  });


  // handle message send from the main thread
  onmessage = function(e) {
    var data = e.data;
    var res = [],
    a1 = data.targetArray,
    a2 = data.filterArray,
    v1_true = data.targetValueTrue,
    v1_false = data.targetValueFalse,
    v2 = data.filterValue,
    c1 = data.targetCol,
    c2 = data.filterCol,
    nRow = a1.length;

    for(var i = 0; i < nRow ; i++){
      progress =  ((i+1)/nRow)*100;
      if(progress === 0 || progress == 100 || i%1000 === 0){ 
        postMessage({
          progress : progress,
          message : (i+1)+"/"+nRow
        });
      }
      if( a2[i] === v2 ){
        res.push([i,c1,v1_false]);
      }else{
        res.push([i,c1,v1_true]);
      }
    }
    postMessage({
      result : res
    });
    close();
  };

}





function hotableSetColValuesByCond(id,col,val,colCond,valCond){
  $tbl  = $("#"+id);
  if($tbl !== undefined){
    var ht = $tbl.handsontable("getInstance"),
      header = ht.getColHeader(),
      posColCond = header.indexOf(colCond),
      posCol = header.indexOf(col),
      valCondAll = ht.getDataAtCol(posColCond),
      valAll = ht.getDataAtCol(posCol);

    var w = newWorker(workerSetColCond);
    // handle message received
    w.onmessage = function(e) {
      var m = e.data;
      if ( m.progress ) {
        console.log(m.progress);
        progressScreen(
            true,
            id+"_progress",
            m.progress,
            "Filtering" + m.message
            );
      }
      if( m.result ){
        ht.setDataAtCell(m.result);
      }
    };
    // launch process
    w.postMessage({
      targetArray : valAll,
      filterArray : valCondAll,
      targetValueTrue : true,
      targetValueFalse : false,
      filterValue : valCond,
      targetCol :posCol,
      filterCol : posColCond
    });
  }
}


