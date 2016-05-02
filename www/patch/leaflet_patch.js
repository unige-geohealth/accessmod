

LeafletWidget.methods.addPng =  function(layerId,group,imgUrl,lat1, lng1, lat2, lng2, options) {

  (function() {
    this.layerManager.addLayer(
        L.imageOverlay(imgUrl,
          [
          [lat1, lng1],
          [lat2, lng2]
          ],
          options
          ),
        'image', layerId, group);
  }).call(this);


};
