#!/bin/sh

#
# Test sessionless
#

docker run \
  -e LOCATION=demo \
  -e MAPSET=demo \
  --rm \
  fredmoser/accessmod:5.7.17 sh << EOF 
echo "test"
GISRC=$(mktemp am_grass_rc.XXXXXX)
echo "GISDBASE:/data/dbgrass" > $GISRC
echo "LOCATION_NAME: demo" >> $GISRC
echo "MAPSET: demo" >> $GISRC
g.list rast
EOF

