#!bin/bash

mkdir -p icons



for s in 16x16 24x24 32x32
do
      convert logo.pdf -resize $s icons/logo$s.ico;
done

for s in 24x24 32x32 128x128 1024x1024
do
      convert logo.pdf -density 1200 -resize $s icons/logo$s.png;
done



