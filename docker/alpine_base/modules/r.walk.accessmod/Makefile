MODULE_TOPDIR = ../..

PGM = r.walk.accessmod

LIBES = $(SEGMENTLIB) $(VECTORLIB) $(RASTERLIB) $(GISLIB) $(MATHLIB)
DEPENDENCIES = $(SEGMENTDEP) $(VECTORDEP) $(RASTERDEP) $(GISDEP)
EXTRA_INC = $(VECT_INC)
EXTRA_CFLAGS = $(VECT_CFLAGS)

CFLAGS="-O2 -Wall -march=native -mtune=native"
LDFLAGS="-s"

include $(MODULE_TOPDIR)/include/Make/Module.make

default: cmd
