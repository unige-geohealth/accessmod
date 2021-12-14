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

window.tables = {};

//input binding
var hotable = new Shiny.InputBinding();

$.extend(hotable, {
  find: function(scope) {
    return $(scope).find('.hotable');
  },
  getValue: function(el) {
    var ht = window.tables[el.id];

    if (ht === undefined) {
      return null;
    } else {
      var data = ht.getData();
      var res = {};
      var cols = ht.getColHeader();

      for (var c = 0; c < cols.length; c++) {
        var col = cols[c];
        res[col] = [];
        for (var l = 0; l < data.length; l++) {
          res[col].push(data[l][c]);
        }
      }

      return {
        colHeaders: cols,
        data: JSON.stringify(res)
      };
    }
  },
  setValue: function() {},
  subscribe: function(el, callback) {
    $(el).on('afterChange', callback);
  },
  unsubscribe: function(el) {
    $(el).off('.hotable');
  }
});

Shiny.inputBindings.register(hotable);

//output binding
var hotableOutput = new Shiny.OutputBinding();
$.extend(hotableOutput, {
  find: function(scope) {
    return $(scope).find('.hotable');
  },
  renderValue: function(el, opt) {
    if (opt === null) {
      return;
    }
    if (!opt.hasOwnProperty('data')) {
      return;
    }

    var data = opt.data;

    var settings = {
      columns: opt.columns,
      manualColumnResize: true,
      minSpareRows: opt.nSpareRow, // at least one empty row
      maxRows: opt.maxRows, // if no thing is given, set as the nrows(df)
      colHeaders: opt.colHeaders,
      handlebar: false,
      stretchH: opt.stretched,
      fixedColumnsLeft: opt.fixedCols,
      columnSorting: true,
      data: data,
      licenseKey: 'non-commercial-and-evaluation'
    };

    if (!window.tables[el.id]) {
      window.tables[el.id] = new Handsontable(el, settings);

      window.tables[el.id].addHook('afterChange', function() {
        /**
         * Link ht afterChange to a shiny/jquery afterChange event
         */
        $(el).trigger('afterChange');
      });
    } else {
      var ht = window.tables[el.id];
      if ( opt.toolsConditionalColumn) {
        /**
         * Copy conditional selected in the updated table, if needed
         */
        var headers = ht.getColHeader();
        var tOpt = opt.toolsConditionalColumn;
        var posCol = headers.indexOf(tOpt.column);
        var posId =  headers.indexOf(tOpt.idColumn);
        if (posCol > -1 && posId >- 1) {
          var oldCondResult = ht.getDataAtCol(posCol);
          var oldIds = ht.getDataAtCol(posId);
          var sameLength = oldCondResult.length === data.length;
          /**
          * It could be the same length but different values...
          */
          if (sameLength) {
            data.forEach(function(r) {
              var id = r[tOpt.idColumn];
              if(id){
                var posSync = oldIds.indexOf(id);
                var hasOld = typeof oldCondResult[posSync] !== 'undefined';
                if(hasOld){
                  r[tOpt.column] = oldCondResult[posSync];
                }
              }
            });
          }
        }
      }

      ht.updateSettings(settings);
    }

    $(el).trigger('afterChange');

    /**
     * TODO: add destroy / update method instad of recreating it
     */
    if (opt.toolsConditionalColumn) {
      hotableMakeToolsConditionalColumn(el, opt);
    }

    hotableDownloadButton(el);
  }
});
Shiny.outputBindings.register(hotableOutput, 'hotable');

function hotableDownloadButton(elTable) {
  var id = elTable.id;
  var hot = window.tables[id];
  var exportPlugin = hot.getPlugin('exportFile');
  var elContainer = elTable.parentElement;
  var classDlButtonContainer = 'handson_tbl_dl_button_container';
  var elDlButtonContainer = elContainer.querySelector(
    '.' + classDlButtonContainer
  );

  if (elDlButtonContainer) {
    while (elDlButtonContainer.firstElementChild) {
      elDlButtonContainer.firstElementChild.remove();
    }
  } else {
    elDlButtonContainer = elCreate('div');
    elDlButtonContainer.classList.add(classDlButtonContainer);
    elContainer.appendChild(elDlButtonContainer);
  }

  var elDlButton = elCreate('button');
  var elIcon = elCreate('i');
  elDlButton.className = 'btn btn-xs btn-default';
  elIcon.className = 'fa fa-download';
  elDlButton.appendChild(elIcon);
  

  elDlButton.addEventListener('click', dl);
  elDlButtonContainer.appendChild(elDlButton);

  function dl() {
    exportPlugin.downloadFile('csv', {
      bom: false,
      columnDelimiter: ',',
      columnHeaders: true,
      exportHiddenColumns: false,
      exportHiddenRows: true,
      fileExtension: 'csv',
      filename: 'am5_export_table_' + id + '_[YYYY]-[MM]-[DD]',
      mimeType: 'text/csv',
      rowDelimiter: '\r\n',
      rowHeaders: false
    });
  }
}

/**
 * Create ui for a conditional update select componant
 * @param {Element} el Table
 * @param {Object} Config
 */
function hotableMakeToolsConditionalColumn(elTable, config) {
  var options = config.toolsConditionalColumn;

  if (!options) {
    options = {
      valueSet: true,
      valueUnset: false,
      column: 'amSelect',
      columnsSelectInput: ['type', 'displayClass', 'tags']
    };
  }

  var id = elTable.id;
  var elCol, colOpt, colType, colId, colData;
  var elNumericInput, elSelectValues;
  var elProgress = elCreate('span');
  var opsNum = ['==', '>=', '<=', '>', '<', '!='];
  var opsString = ['==', '!='];
  var hot = window.tables[id];

  var elContainer = elTable.parentElement.parentElement;

  var classToolsContainer = 'handson_tbl_tools_container';
  var classTools = 'handson_tbl_tools_conditional';

  var elTools = elContainer.querySelector('.' + classTools);
  var elToolsContainer = elContainer.querySelector('.' + classToolsContainer);

  if (elTools && elTools._destroy) {
    elTools._destroy();
  } else {
    elTools = elCreate('div');
    elToolsContainer.appendChild(elTools);
  }

  /**
   * Build
   */
  var columnsHeaders = getHeaderObj(hot);
  //var colSelect = [];
  /* var colItem ;*/
  //for(var i=0,l=columns.length;i<l;i++){
  //colItem = columns[i];

  /*}*/
  var elSelectColHeader = selectCreate(columnsHeaders);
  var elSelectOpts = elCreate('div');
  var elBtnAll = elCreate('a');
  var elBtnNone = elCreate('a');
  var elBtnAdd = elCreate('a');
  var elBtnRemove = elCreate('a');
  var elCount = elCreate('span');

  // elSelectOpts content, set later
  var elSelectOpsNum = selectCreate(opsNum);
  var elSelectOpsString = selectCreate(opsString);

  elTools.classList.add(classTools);
  elCount.innerText = '0/0';
  elBtnAll.dataset.amt_id =
    options.labelSetAll || 'handson_tbl_ctrl_select_all';
  elBtnNone.dataset.amt_id =
    options.labelSetNone || 'handson_tbl_ctrl_select_none';
  elBtnAdd.dataset.amt_id = options.labelSet || 'handson_tbl_ctrl_select';
  elBtnRemove.dataset.amt_id =
    options.labelUnset || 'handson_tbl_ctrl_unselect';
  elTools.appendChild(elBtnAll);
  elTools.appendChild(elBtnNone);
  elTools.appendChild(elSelectColHeader);
  elTools.appendChild(elSelectOpts);
  elTools.appendChild(elBtnAdd);
  elTools.appendChild(elBtnRemove);
  elTools.appendChild(elCount);
  amSetLanguage({elRoot: elTools});
  updateSelect();

  /**
   * Events
   */
  elSelectColHeader.addEventListener('change', updateSelect);
  elBtnAdd.addEventListener('click', cmdSet);
  elBtnRemove.addEventListener('click', cmdUnset);
  elBtnAll.addEventListener('click', cmdSetAll);
  elBtnNone.addEventListener('click', cmdSetNone);
  hot.addHook('afterChange', updateCount);
  updateCount();

  elTools._destroy = function() {
    elSelectColHeader.removeEventListener('change', updateSelect);
    elBtnAdd.removeEventListener('click', cmdSet);
    elBtnRemove.removeEventListener('click', cmdUnset);
    elBtnAll.removeEventListener('click', cmdSetAll);
    elBtnNone.removeEventListener('click', cmdSetNone);
    hot.removeHook('afterChange', updateCount);
    while (elTools.firstElementChild) {
      elTools.firstElementChild.remove();
    }
  };

  /**
   * Helpers
   */
  function updateCount() {
    var pos = hot.getColHeader().indexOf(options.column);
    var data = hot.getDataAtCol(pos);
    var n = data.length;
    var s = 0;
    for (var i = 0; i < n; i++) {
      if (data[i] === options.valueSet) {
        s++;
      }
    }
    elCount.innerText = s + '/' + n;
  }

  function updateSelect() {
    while (elSelectOpts.firstChild) {
      elSelectOpts.removeChild(elSelectOpts.firstChild);
    }
    elCol = elSelectColHeader;
    colOpt = elCol.options[elCol.selectedIndex].dataset.opt;
    colId = JSON.parse(colOpt).value;
    if (typeof colId !== 'undefined' && colId !== null) {
      colInfo = columnsHeaders[colId];
      colData = hot.getDataAtCol(colId);
      colType = 'string';

      config.columns.forEach(function(c) {
        if (c.header === colInfo.label && c.type) {
          colType = c.type;
        }
      });

      if (colType === 'numeric') {
        colData.forEach(function(d) {
          if (d === null) {
            d = 0;
          }
        });
        elSelectOpts.appendChild(elSelectOpsNum);
        elNumericInput = elCreate('input');
        elNumericInput.type = 'number';
        elSelectOpts.appendChild(elNumericInput);
      } else {
        colData.forEach(function(d) {
          if (d === null) {
            d = '';
          }
        });
        elSelectOpts.appendChild(elSelectOpsString);
        elSelectValues = selectCreate(colData);
        elSelectOpts.appendChild(elSelectValues);
      }
    }
  }

  function applySelection(cmd) {
    var val;
    var data;
    var set = cmd === 'set' ? options.valueSet : options.valueUnset;

    var isNum = colType === 'numeric';

    var col = elSelectColHeader.value;
    var op = isNum ? elSelectOpsNum.value : elSelectOpsString.value;

    if (isNum) {
      val = elNumericInput.value * 1;
    } else {
      data = elSelectValues.selectedOptions[0].dataset;
      if (data.opt) {
        val = JSON.parse(data.opt).value;
      }
    }

    hotableSetColValuesByCond(id, {
      col: options.column,
      set: set,
      whereCol: col,
      whereVal: val,
      whereOp: op,
      elProgress: elProgress
    });
  }
  function cmdSetAll() {
    hotableSetColValues(id, {col: options.column, set: options.valueSet});
  }
  function cmdSetNone() {
    hotableSetColValues(id, {col: options.column, set: options.valueUnset});
  }

  function cmdSet() {
    return applySelection('set');
  }
  function cmdUnset() {
    return applySelection('unset');
  }
}

/**
 * Update a column of an handsontable using a given value
 * @param {String} id Id of the table stored in window.tables
 * @param {Object} options
 * @param {String} options.col Column to update
 * @param {*} options.set Value to update the column with
 */
function hotableSetColValues(id, options) {
  var o = options || {};
  var res = [];
  var ht = window.tables[id];
  if (!ht) {
    return;
  }

  rc = ht.countRows();
  cc = ht.countCols();
  if (rc > 0 && cc > 0) {
    // search
    hed = ht.getColHeader();
    pos = hed.indexOf(o.col);
    if (pos !== undefined) {
      for (i = 0; i < rc; i++) {
        res.push([i, pos, o.set]);
      }
      ht.setDataAtCell(res);
    }
  }
}

/**
 * Create a new worker from a function
 * @param {Function} fun function to execute in the worker
 */
function newWorker(fun) {
  // convert input function to string
  fun = fun.toString();
  fun = fun.substring(fun.indexOf('{') + 1, fun.lastIndexOf('}'));
  // Make a blob
  var blob = new Blob([fun], {
    type: 'application/javascript'
  });
  // convert as url for new worker
  var blobUrl = URL.createObjectURL(blob);

  // return new worker
  return new Worker(blobUrl);
}

/*
 * Worker for the conditional search
 */
function workerSetColCond() {
  // Inital message
  postMessage({
    progress: 0,
    message: 'start'
  });

  // handle message send from the main thread
  onmessage = function(e) {
    var data = e.data;
    var res = [],
      a1 = data.targetArray,
      a2 = data.filterArray,
      set = data.setValue,
      v2 = data.filterValue,
      c1 = data.targetCol,
      //c2 = data.filterCol,
      nRow = a1.length,
      op = data.operator || '==';

    var fun = {
      '==': function(a, b) {
        return a === b;
      },
      '>=': function(a, b) {
        return a >= b;
      },
      '<=': function(a, b) {
        return a <= b;
      },
      '>': function(a, b) {
        return a > b;
      },
      '<': function(a, b) {
        return a < b;
      },
      '!=': function(a, b) {
        return a !== b;
      },
      '': function(a, b) {
        return a === b;
      }
    };

    for (var i = 0; i < nRow; i++) {
      progress = ((i + 1) / nRow) * 100;
      if (progress === 0 || progress === 100 || i % 1000 === 0) {
        postMessage({
          progress: progress,
          message: i + 1 + '/' + nRow
        });
      }

      if (fun[op](a2[i], v2)) {
        res.push([i, c1, set]);
      } else {
        res.push([i, c1, a1[i]]);
      }
    }
    postMessage({
      result: res
    });
    close();
  };
}

Shiny.addCustomMessageHandler(
  'hotableSetColValuesByCond',
  hotableSetColValuesByCondWrapper
);

function hotableSetColValuesByCondWrapper(o) {
  hotableSetColValuesByCond(o.id, o);
}

/**
 * Set value of a column based on a test on another column
 * @param {String} id Id of a table stored in window.tables
 * @param {Object} options
 * @param {String} options.col Name or index of column to alter
 * @param {String} options.colWhere Name or index of column to query
 * @param {*} options.set Value to update the column with
 * @param {String} options.whereOp Operator to use in the compare function.
 * @param {Element} options.elProgress Element to update with the progress percentage
 */
function hotableSetColValuesByCond(id, options) {
  var o = options || {};

  var ht = window.tables[id];

  if (!ht) {
    return;
  }

  var header = ht.getColHeader(),
    posColCond = header.indexOf(o.whereCol),
    posCol = header.indexOf(o.col),
    valCondAll = ht.getDataAtCol(posColCond),
    valAll = ht.getDataAtCol(posCol);
  o.whereOp = o.whereOp || '==';

  posColCond = ifNotEmpty(posColCond, posColCond, o.whereCol);
  posCol = ifNotEmpty(posCol, posCol, o.col);

  var w = newWorker(workerSetColCond);
  // handle message received
  w.onmessage = function(e) {
    var m = e.data;
    if (m.progress && o.elProgress) {
      o.elProgress.innerText = Math.round(m.progress) + '%';
    }
    if (m.result) {
      if (o.elProgress) {
        o.elProgress.innerText = '';
      }
      ht.setDataAtCell(m.result);
    }
  };
  // launch process
  w.postMessage({
    targetArray: valAll,
    filterArray: valCondAll,
    setValue: o.set,
    filterValue: o.whereVal,
    targetCol: posCol,
    filterCol: posColCond,
    operator: o.whereOp
  });
}

/**
 * Test if number
 * @param {*} n Test if n is a number
 */
function isNumber(n) {
  return !isNaN(parseFloat(n)) && isFinite(n);
}

/**
 * Test for empty, return b if true, if not
 * @param {Boolean} a Condition
 * @param {*} b Object to return if true
 * @param {*} b Object to return if false
 */
function ifNotEmpty(a, b, c) {
  var tA = typeof a;
  if (tA !== 'undefined' || isNumber(a) || (tA === 'string' && a.length > 0)) {
    return b;
  } else {
    return c;
  }
}

/**
 * Get object with label and value key from table headers
 * @param {Object}  hot handsontable object
 */
function getHeaderObj(hot) {
  var out = [];
  var headers = hot.getColHeader();
  for (var i = 0, iL = headers.length; i < iL; i++) {
    out.push({
      label: headers[i],
      value: i
    });
  }
  return out;
}

/**
 * Create an element
 * @param  {String} t Type of element to create
 */
function elCreate(t) {
  return document.createElement(t);
}

function hasValue(x) {
  return x || x === 0;
}

function filterArray(arr) {
  var index = -1,
    arrLength = arr ? arr.length : 0,
    resIndex = -1,
    result = [];

  while (++index < arrLength) {
    var value = arr[index];
    if (hasValue(value)) {
      result[++resIndex] = value;
    }
  }

  return result;
}

/**
 * Create a select drop down list based on an array or an array of object
 * @param {Array} arr Array of number, string or object with value / label keys
 * @param {String} id optional id for the select element
 */
function selectCreate(arr, id) {
  var opt, item, value, label;
  arr = filterArray(arr);

  /**
   * Sort by label or value
   */

  arr = arr.sort(function(a, b) {
    if (
      ifNotEmpty(a && a.label, a.label, a) <
      ifNotEmpty(b && b.label, b.label, b)
    ) {
      return -1;
    }
    if (
      ifNotEmpty(a && a.label, a.label, a) >
      ifNotEmpty(b && b.label, b.label, b)
    ) {
      return 1;
    }
    return 0;
  });

  var sel = elCreate('select');
  if (id) {
    sel.id = id;
  }
  var seen = [];

  /**
   * Populate options
   */
  for (var i = 0, iL = arr.length; i < iL; i++) {
    item = arr[i];
    if (seen.indexOf(item) === -1) {
      seen.push(item);
      opt = elCreate('option');
      label = ifNotEmpty(item.label, item.label, item);
      value = ifNotEmpty(item.value, item.value, item);
      opt.innerText = label;
      opt.dataset.opt = JSON.stringify({
        value: value
      });
      sel.appendChild(opt);
    }
  }
  return sel;
}
