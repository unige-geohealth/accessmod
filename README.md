#![](https://raw.githubusercontent.com/fxi/accessModShiny/master/www/logo/icons/logo24x24.png) _accessmod 5_

### Summary 

This is the repository of the main package of the _accessmod 5_ project. It contains the user interface and general functionality. This project is in a early development stage and should not be used in a production context.

This repository is related to two other components:

* A [virtual machine](https://github.com/fxi/accessmodServer) to provide a working environment for _accessmod 5_
* A [modified version](https://github.com/fxi/rWalkAccessmod)  of the module r.walk from [GRASS GIS](grass.osgeo.org/grass70)


_accessmod 5_ is a tool to analyse geographical accessibility to or from given locations, taking in account anisotropic displacement and multimodal transport process (walk, bicycle, motorized, boat). This package may help to analyse catchments of peoples who can reach a central point in a given time and transport model or determine where new public services should be scaled up in prority, based on physical constraints. 

<figure>
<img src="https://raw.githubusercontent.com/wiki/fxi/accessModShiny/img/anisoCumulativeCostSample.jpg" alt="Accessmod 5, module 2 : cumulative cost">
</a>
<figcaption>
Example of cumulative cost map computed with multimodal transportation scheme, anisotropic displacement for a sample region.
</figcaption>
</figure>

This product is developed by the Department of Health Systems Financing (WHO/HSS/HSF) in collaboration with the WHO Vulnerability and Risk Analysis & Mapping programme (VRAM).

AccessModShiny work with a running [GRASS-GIS](http://grass.osgeo.org/grass7/) session in background, trough the package [R](http://cran.r-project.org/) [spgrass6](http://cran.r-project.org/web/packages/spgrass6/). The configuration is done with the config.R file in config folder.

Because a large set of dependencies of GIS tools, it's not recommended to run this app directly from R as common Shiny App. 

Noneless, being an open-source project, everything is possible. For more information about the optimal environment to create for this app, please refer to the [provision file](https://raw.githubusercontent.com/fxi/accessmodServer/master/provision.sh).
