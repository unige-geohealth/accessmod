#!/usr/bin/env sh

#
# Pre build rc files for each project
#
if [ -d $GISDBASE ]; then
  for location in $GISDBASE/*; do
    # 
    # Each project has at least a location and 
    #  a mapset of the same name
    #
    if [ -d "$location" ] && [ -d "$AM5_GRASS_RC_FILES" ] ; then
      project=$(basename $location)
      rcFile="$AM5_GRASS_RC_FILES/$project"".rc"
      echo "Create file ${rcFile}"
      echo "GISDBASE: $GISDBASE" > $rcFile
      echo "LOCATION_NAME: $project" >> $rcFile
      echo "MAPSET: $project" >> $rcFile
    fi
  done
fi
