#!/bin/sh

#
# Test sessionless
#

GISRC=$(mktemp am_grass_rc.XXXXXX)

echo "GISDBASE:/data/dbgrass" > $GISRC
echo "LOCATION_NAME: demo" >> $GISRC
echo "MAPSET: demo" >> $GISRC

g.list rast

rm $GISRC
