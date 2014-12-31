# AccessModShiny

This is the repository of the user interface part of the AccessMod 5.0 project, scripted as a [R-shiny](http://shiny.rstudio.com) web application. 

AccessModShiny work with a running [GRASS-GIS](http://grass.osgeo.org/grass7/) session in background, trough the package [R](http://cran.r-project.org/) [spgrass6](http://cran.r-project.org/web/packages/spgrass6/).

Because a large set of dependencies of GIS tools, it's not recommended to run this app directly from R as common Shiny App. 

Noneless, being an open-source project, everything is possible. For more information about the optimal environment to create for this app, please refer to the [provision file](https://raw.githubusercontent.com/fxi/accessmodServer/master/provision.sh) main project : [accessModServer](https://github.com/fxi/accessmodServer/)
