#!/bin/bash

# Exit the script on any command with non-0 exit code
set -e

# Set GRASS to overwrite existing layers by default
export GRASS_OVERWRITE=1

# Create random points
v.random output=vFacility__demo_50k n=50000

# Connect an attribute table to the vector
v.db.addtable map=vFacility__demo_50k

# Add columns for type, class, and ldc_value
v.db.addcolumn map=vFacility__demo_50k columns="type varchar(20), class varchar(20), ldc_value double precision"

# Populate the type column with "foo" 1/6 of the time and "bar" the rest
v.db.update map=vFacility__demo_50k column=type qcolumn="CASE WHEN (random() % 6) = 0 THEN 'foo' ELSE 'bar' END"

# Populate the class column with "sonub" 1/10 of the time and "sonuc" the rest
v.db.update map=vFacility__demo_50k column=class qcolumn="CASE WHEN (random() % 10) = 0 THEN 'sonub' ELSE 'sonuc' END"

# Attach raster values to each point using v.what.rast
v.what.rast map=vFacility__demo_50k raster=rLandCoverMerged__demo column=ldc_value

# Select only the points that have non-NULL values in the ldc_value column
v.extract input=vFacility__demo_50k output=vFacility__demo_50k_filtered where="ldc_value IS NOT NULL"

# Rename the filtered vector to the original name (optional)
g.rename vector=vFacility__demo_50k_filtered,vFacility__demo_50k
