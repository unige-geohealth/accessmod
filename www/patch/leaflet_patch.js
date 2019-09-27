(function() {
  /**
   * Add scale handler
   */
  LeafletWidget.methods.addScale = function() {
    (function() {
      L.control.scale().addTo(this);
    }.call(this));
  };
  /**
   * PNG handler
   */
  LeafletWidget.methods.addPng = function(
    layerId,
    group,
    imgUrl,
    lat1,
    lng1,
    lat2,
    lng2,
    options
  ) {
    (function() {
      var pngLayer = L.imageOverlay(
        imgUrl,
        [[lat1, lng1], [lat2, lng2]],
        options
      );

      this.layerManager.addLayer(pngLayer, 'image', layerId, group);
    }.call(this));
  };

  LeafletWidget.methods.setPngOpacity = function(layerId, opacity) {
    (function() {
      var layer = this.layerManager.getLayer('image', layerId);
      if (layer) {
        layer.setOpacity(opacity);
      }
    }.call(this));
  };

  /**
   * Relocate handler
   */
  var amRelocateConfig = {
    enabled: false,
    id: null,
    items: [],
    history: [],
    visited: [],
    controls: null
  };

  LeafletWidget.methods.removeMarkersRelocate = function(layerId) {
    (function() {
      //var layer = this.layerManager.getLayer('marker',layerId);
      //if(layer){
      this.layerManager.removeLayer('marker', layerId);
      //}
      var c = amRelocateConfig;
      var that = this;

      c.enabled = [];
      c.history = [];
      c.visited = [];

      initRelocateStatus(that, false);
    }.call(this));
  };

  LeafletWidget.methods.addMarkersRelocate = function(layerId, group, data) {
    (function() {
      var that = this;
      var i, iL, lat, lng, marker, value, id, label;
      var ids = data.id || [];
      var markers = [];
      var hasArrayValue = data.value instanceof Array;
      var hasArrayLabel = data.label instanceof Array;

      initRelocateStatus(that, true);
      for (i = 0, iL = ids.length; i < iL; i++) {
        var options = {};
        id = ids[i];
        lat = data.lat[i];
        lng = data.lng[i];
        value = hasArrayValue ? data.value[i] : null;
        label = hasArrayLabel ? data.label[i] : value;
        
        options = {
          icon: getColorIconIfValue(value),
          opacity: value ? 0.9 : 1,
          title: label || i,
          clickable: true,
          draggable: true,
          riseOnHover: true,
          riseOffset: 30
        };
        marker = L.marker({lat: lat, lng: lng}, options);
        marker._id = id;
        marker.on('dragend', triggerUpdate);
        markers.push(marker);
        addRelocateItem(id, value, lat, lng, marker);
      }

      updateControlsState();
      markers = L.layerGroup(markers);
      this.layerManager.addLayer(markers, 'marker', layerId, group);
    }.call(this));
  };

  LeafletWidget.methods.updateMarkerRelocate = function(
    layerId,
    group,
    markerId,
    newValue,
    newLat,
    newLng
  ) {
    (function() {
      updateRelocateItem(markerId, newValue, newLat, newLng, true);
    }.call(this));
  };

  var RelocateControls = L.Control.extend({
    options: {
      position: 'topright'
    },
    onAdd: function(map) {
      var elBar = L.DomUtil.create(
        'div',
        'leaflet-control-zoom leaflet-bar leaflet-control'
      );

      this._elBtnEdit = createButton({
        html: '<span></span>',
        title: 'Edit',
        className: 'fa fa-pencil',
        container: elBar,
        fn: toggleEdit.bind(map),
        context: this
      });

      this._elBtnNext = createButton({
        html: '<span></span>',
        title: 'Next',
        className: 'fa fa-step-forward',
        container: elBar,
        fn: findNext.bind(map),
        context: this
      });

      this._elBtnUndo = createButton({
        html: '<span></span>',
        title: 'Undo',
        className: 'fa fa-undo',
        container: elBar,
        fn: undoLastRelocate.bind(map),
        context: this
      });

      //this._elBtnSave = createButton({
      //html: '<span></span>',
      //title: 'Save',
      //className: 'fa fa-floppy-o',
      //container: elBar,
      //fn: sendChangesToShiny,
      //context: this
      /*});*/

      amRelocateConfig.controls = this;

      return elBar;
    }
  });

  function updateControlsState() {
    var c = amRelocateConfig;
    var ctx = c.controls;
    var isEnabled = c.enabled === true;
    var hasItems = c.items && c.items.length > 0;
    var hasHistory = c.history.length > 0;

    sendStateToShiny({
      isEnabled: isEnabled,
      hasItems: hasItems,
      hasHistory: hasHistory,
      changes: getChanges()
    });

    if (!hasItems) {
      c.enabled = false;
      ctx._elBtnEdit.disable();
      //ctx._elBtnSave.disable();
      ctx._elBtnUndo.disable();
      ctx._elBtnNext.disable();
    } else {
      ctx._elBtnEdit.enable();

      if (!isEnabled) {
        ctx._elBtnUndo.disable();
        ctx._elBtnNext.disable();
        setMarkersDraggable(false);
      } else {
        ctx._elBtnUndo.enable();
        ctx._elBtnNext.enable();
        setMarkersDraggable(true);

        if (hasHistory) {
          ctx._elBtnUndo.enable();
        } else {
          ctx._elBtnUndo.disable();
        }
      }
    }
  }

  function setMarkersDraggable(enable) {
    var c = amRelocateConfig;
    var items = c.items;
    items.forEach(function(i) {
      /**
       * set marker draggable could be invoked
       * while the marker is not yet rendered
       * In that case, marker.dragging does not work,
       * we update simply options.draggable instead
       */
      i.marker.options.draggable = enable;
      if (i.marker.dragging) {
        if (enable) {
          i.marker.dragging.enable();
        } else {
          i.marker.dragging.disable();
        }
      }
    });
  }

  function createButton(opt) {
    var link = L.DomUtil.create('a', opt.className, opt.container);
    var classDisabled = 'leaflet-disabled';
    link.innerHTML = opt.html;
    link.href = '#';
    link.title = opt.title;
    var stop = L.DomEvent.stopPropagation;

    L.DomEvent.on(link, 'click', stop)
      .on(link, 'mousedown', stop)
      .on(link, 'dblclick', stop)
      .on(link, 'click', L.DomEvent.preventDefault);

    link.disable = disable;
    link.enable = enable;

    function enable() {
      L.DomUtil.removeClass(link, classDisabled);
      L.DomEvent.on(link, 'click', opt.fn, opt.context);
    }

    function disable() {
      L.DomUtil.addClass(link, classDisabled);
      L.DomEvent.off(link, 'click', opt.fn, opt.context);
    }

    return link;
  }

  function toggleEdit() {
    amRelocateConfig.enabled = !amRelocateConfig.enabled;
    updateControlsState();
  }

  function findNext() {
    var visited = amRelocateConfig.visited;
    var item = amRelocateConfig.items.find(function(i) {
      return (
        !isValue(i.newValue) &&
        !isValue(i.value) &&
        visited.indexOf(i.id) === -1
      );
    });
    if (item) {
      visited.push(item.id);
      this.setView([item.lat, item.lng], 15);
    } else {
      amRelocateConfig.visited = [];
    }
  }
  function addRelocateItem(id, value, lat, lng, marker) {
    amRelocateConfig.items.push({
      id: id,
      value: value,
      lat: lat,
      lng: lng,
      marker: marker
    });
  }

  function getRelocateItem(id) {
    return amRelocateConfig.items.find(function(s) {
      return s.id === id;
    });
  }

  function updateRelocateItem(id, newValue, newLat, newLng, keepLog) {
    var item = getRelocateItem(id);
    keepLog = typeof keepLog === 'undefined' ? true : keepLog === true;
    if (item) {
      var log = {
        id: id,
        newValue: newValue,
        newLat: newLat,
        newLng: newLng,
        oldValue: item.value,
        oldLng: item.lng,
        oldLat: item.lat
      };

      item.value = newValue;
      item.lat = newLat;
      item.lng = newLng;

      item.marker.setIcon(getColorIconIfValue(item.value));
      item.marker.setLatLng({lng: item.lng, lat: item.lat});
      if (keepLog) {
        historyPush(log);
      }
    }

    //historyPlot();
    updateControlsState();
  }

  function historyPush(log) {
    amRelocateConfig.history.push(log);
    //historyPlot();
  }

  function historyPop() {
    var last = amRelocateConfig.history.pop();
    return last;
  }

/*  function historyPlot() {*/
    //console.table(amRelocateConfig.history);
  /*}*/
  function getChanges() {
    var history = amRelocateConfig.history;

    var out = history.reduce(function(a, h) {
      a[h.id] = {
        id: h.id,
        lng: h.newLng,
        lat: h.newLat
      };
      return a;
    }, {});

    var changes = Object.keys(out).map(function(id) {
      return out[id];
    });

    return changes;
  }

  /*  function sendChangesToShiny() {*/
  //var historyTrimmed = historyTrim();
  //Shiny.onInputChange(amRelocateConfig.id + '_relocate_data', {
  //history: historyTrimmed
  //});
  /*}*/

  function sendStateToShiny(state) {
    Shiny.onInputChange(amRelocateConfig.id + '_state', state);
  }

  function triggerUpdate(e) {
    var pos = e.target.getLatLng();
    Shiny.onInputChange(amRelocateConfig.id + '_marker_' + e.type, {
      lng: pos.lng,
      lat: pos.lat,
      id: e.target._id
    });
  }

  function undoLastRelocate() {
    var lastAction = historyPop();
    var la = lastAction;
    if (la) {
      updateRelocateItem(la.id, la.oldValue, la.oldLat, la.oldLng, false);
    }
    updateControlsState();
  }

  function initRelocateStatus(map, enable) {
    var c = amRelocateConfig;
    c.id = map.id;
    c.items = [];
    c.history = [];
    c.visited = [];
    if (enable) {
      c.controls = new RelocateControls();
      map.addControl(c.controls);
      updateControlsState();
    } else {
      if (c.controls) {
        map.removeControl(c.controls);
        c.controls = null;
      }
    }
  }

  function getColorIconIfValue(value) {
    return getColorIcon(isValue(value) ? 'green' : 'red');
  }

  function isValue(v) {
    // Numeric and positiv or other not null or undefined
    return (
      (typeof v === 'number' && v >= 0) ||
      (typeof v !== 'undefined' && v && v !== null && v !== '-')
    );
  }

  function getColorIcon(color) {
    var colors = [
      'blue',
      'red',
      'black',
      'green',
      'orange',
      'yellow',
      'violet',
      'gray'
    ];
    color = colors.indexOf(color) === -1 ? 'gray' : color;
    return new L.Icon({
      iconUrl: 'img/marker-icon-2x-' + color + '.png',
      shadowUrl: 'img/marker-shadow.png',
      iconSize: [25, 41],
      iconAnchor: [12, 41],
      popupAnchor: [1, -34],
      shadowSize: [41, 41]
    });
  }
})();
