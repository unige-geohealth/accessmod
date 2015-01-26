---
title: accesmod 5 User Manual
tags: [test, documentation, accessibility]
abstract: accessmod 5 is a tool to analyse geographical accessibility to or from given locations, taking in account anisotropic displacement and multimodal transport process (e.g. walk, bicycle, motorized vehicles). This package may help to analyse catchments of peoples who can reach a central point in a given time and transport model or determine where new public services should be scaled up in prority.
header: AccessMod 5
footer: AccessMod 5
...


# Introduction
*accessmod 5* is a standalone tool to analyse geographical accessibility to or from given locations, taking in account anisotropic displacement and multimodal transport process (e.g. walk, bicycle, motorized vehicles). This package may help to analyse catchments of peoples who can reach a central point in a given time and transport model or determine where new public services should be scaled up in prority.

![**Fig.1** accessmod 5: cumulated travel time in seconds, from health facilities](img/anisoCumulativeCostSample.jpg)


## Context
  [World Health Organisation (WHO)](http://www.who.int/en/) has been working together with a team of experts in geographic mapping to conduct analysis on geographic access to health services. The results are used to inform policy discussions in low-and middle-income countries regarding inequities across regions and barriers that may hinder the population accessing quality health care. 

  The analysis makes use of Accessmod, which is WHO Geographic Information System (GIS) software that works of the standard GIS ESRI ArcGIS platform but performs additional analytical functions, such as examining the capacity of each facility to deliver services, and to run scale up scenarios. These are important functions related to strategic planning and forecasting population health needs. 

  AccessMod allows for taking into account conjointly the location and the maximum coverage capacity of each health care provider, the geographic distribution of the population, the environment that the patient will have to cross to reach the care provider, as well as the transportation mode s/he will be using, and the capacity of each facility to provide services (i.e., up to a threshold or saturation point).

## History of AccessMod 
  Over the years, two versions of AccessMod have been freely shared with the public (Version 3.0, released in 2008, has been downloaded more than 1'000 times from ESRI’s ArcScript web site), and Version 4.0 for ArcGIS 9.3.1 has been downloaded 466 times since its release in June 2012, also accessible from ArcGIS online). AccessMod has been used to measure physical accessibility and geographic coverage in many low- and middle income countries including Rwanda, Namibia and Sao Tomé and Principe.

  Lessons learnt from country application include challenges related to using GIS software that is tied to a specific system, namely ArcGIS which is the GIS package most commonly used around the world. Countries wanting to run Accessmod analysis are dependent on having the right version of ArcGIS available, meanwhile new versions are frequently released with cost implications for countries. 

## New version
  There was a need for another strategy. We wanted to make AccessMod independent of ESRI products by using a suite of open-source products such that it can be run on any preferred GIS package (e.g., ArcGIS, QGIS, GRASS, etc.) thus benefitting the user community. Moreover this would facilitate development of future versions of AccessMod if additional functions were to be added. 

  The new *accessmod* is a standalone product in the form of a virtual machine with all libraries included (GRASS, R, Python). First, the GIS software extension will be open source and a public good to serve health planners and the GIS community. Secondly, functionalities will be improved in relation to the integration of a spatial optimization function when deciding on the location for potential new facilities when conducting a scaling up analysis. Such a function can be used to model scenarios for expanding population access to health services, but also have additional uses, such as modelling the supply chain in relation to transporting vaccines and medicines. In the latter case, the user would also enter coordinates for warehouses and run analysis on the travel time between facilities and warehouses. Third, the hierarchical mapping of facilities within the model will be facilitated, such that the user can run analysis of travel time between primary and secondary level facilities (i.e., in relation to the referral system).

  Added value for low-and middle income countries will include easy and free access to a version of AccessMod that they can apply within a national context, without the need for external GIS software support and/or purchasing new GIS software. Given that use of GIS is expected to increase in the context of working towards UHC and attaining the sustainable development goals (SDGs) post 2015, making an independent version of AccessMod available will provide a foundation on which country GIS units can develop capacity and apply GIS to inform national policy discussions. 

  Added value for WHO will include wider dissemination of a WHO-copyright GIS analytical tool that is already widely used in the scientific community, but for which the development will also mean increasing use in low and middle income countries. 


## Technical details

  _Accessmod 5_ uses a large range of open source libraries in background. This represents a complex environment that could be difficult to reproduce on a desktop computer. We choose to deliver this project in a virtual machine, so everyone can use it on a simple desktop computer without worrying about dependencies and configurations, licences, fees, expensive GIS software, etc.. The name of this virtual machine is [_accessmodServer_](https://github.com/fxi/accessmodServer). When installed, _accessmod 5_ is  available at this uri from a web browser : `http://localhost:8080`.

  From a user point of view, [_accessmodServer_](https://github.com/fxi/accessmodServer) is a self-contained software. When switched on, it provides a single web application : [_accessmod 5_](https://github.com/fxi/accessModShiny). 

  After installation, there is no need for an internet connection : the user can use this tool on the field. 

## Hardware and software prerequisite
  - Windows, Linux or Mac computer; 
  - Administrator rights (depending on your os / configuration);
  - A GIS software to prepare the data (spatial reference system, extent, resolution,etc..) and analyse the results. E.g. [Qgis](http://www.qgis.org/en/site/), [Grass](http://grass.osgeo.org/grass70), [Saga](http://www.saga-gis.org/), ArcGIS, etc.. Full list : [GIS software on wikipedia.org](http://en.wikipedia.org/wiki/List_of_geographic_information_systems_software#Desktop_GIS);
  - A modern web browser ([Firefox](https://www.mozilla.org/en-US/firefox/new/), [opera](http://www.opera.com/), safari, [chrome](https://www.google.com/chrome/browser/desktop/index.html) or internet explorer 10+)
  - Minimum 2GB of free RAM. Recommended: 3GB or more;
  - Minimum 4GB of free disk space. Recommended: 20GB or more, depending on your dataset.

----

# Quick start
## Install componants
  + [Download](https://www.virtualbox.org/wiki/Downloads) and [install](https://www.virtualbox.org/manual/ch02.html) [_VirtualBox_]('http://www.virtualbox.org') for your OS
  + Download accessmodServer.ova and double click on it.
  + In _VirtualBox_, Select accessmodServer and click "start"

  At this point, the server should be ready to provide _accessmod 5_ services at this URI : `http://localhost:8080`


## Create your first project
### Prerequisite 
  To create a new project, you will need at least a digital elevation model (DEM) projected in metric system, for the region to analyse. The map should be preferably projected with a coordinate reference system (CRS) of type "equal area". [More info on CRS](http://docs.qgis.org/2.0/en/docs/gentle_gis_introduction/coordinate_reference_systems.html).
  You can download this kind of data from multiple sources, e.g.[csi.org](http://www.cgiar-csi.org/data/srtm-90m-digital-elevation-database-v4-1). In _accessmod 5_ every calculations will be based on the resolution and extent of this DEM map. 

  Format supported (for now):

    * [Geotiff (.tiff)](http://en.wikipedia.org/wiki/GeoTIFF).
    * [Esri grid (.adf)](http://en.wikipedia.org/wiki/Esri_grid).

### Open the application

    Launch a browser (Firefox, opera, safari, chrome or internet explorer 10+) and go to `http://localhost:8080`
![](img/project_001.png)

  * 1. click on "New location"
  * 2. type a location name: e.g. "test_90_m"

![](img/project_002.png)

  * 3. choose a DEM dataset: here, a set of [ESRI Grid](http://en.wikipedia.org/wiki/Esri_grid) files (.adf)

![](img/project_003.png)

  * 4. After uploading process, you can see some information about the projection used and visualise the dataset extent on the right panel:

![](img/project_005.png)

  Done ! Your location is set and you can manage your data!

### Uninstall
  **Caution : this step will also delete unsaved changes inside _accessmodServer_ !**

  To remove completly accessmod5 from your computer :

  + In VirtualBox, righ-click on _accessmodServer_ > Close > PowerOFF
  + Then, right-click on _accessmodServer_ > remove > delete all files.
  + Your machine is clean.


----

# Advanced installation guide.
## Installation with Vagrant.
  [Vagrant](https://www.vagrantup.com) is a tool to set up virtual machine environment, from a bare OS. _accessmodServer_ uses this techlology to stack dependencies on a Linux image. With a simple script, the machine is set up automatically from a bare system to a working environment, including numerous packages installation  and compilation process. 

  In this section, you will find basic instructions to install _accessmodServer_ from source. This guide is intended for users who want to modify the base system or simply see how things are done. As this step requires a lot of external dependencies, there is no guarantee that the final product will be functionnal. In theory, there is no reason that could happen, but there is sometimes a gap with the practice. 

  To complete this guide, you will need to install 2 mains software :

  + [VirtuaBox](http://www.virtualbox.com): a virtual machine manager;
  + [Vagrant](https://www.vagrantup.com): a virtual environment management tool.
### Installation


#### First step
  To use the virtual server on mac, linux or windows:
  1. Git clone or download [_accessmodServer_](https://github.com/fxi/accessmodServer) to a folder on your computer. e.g: 
  - Linux or mac `$HOME/vm/accessModServer` ;  
  - Windows `c:\Users\<yourname>\vm\accessModServer`.
  2. Download and install [VirtualBox](https://www.virtualbox.org/wiki/Downloads) ;
  2. Download and install [Vagrant](https://www.vagrantup.com).

#### Linux and Mac
  1. Open a terminal and go to your accessmod folder. (e.g. cd `$HOME/vm/accessModServer`);
  2. Enter this command : `vagrant up` (this could take a while to complete);
  3. Open a web browser (preferably google chrome, firefox, opera or safari) and go to <http://localhost:8080/> or <http://192.168.38.38:3838/> .

#### Windows
  1. Navigate to the accessmod server folder (e.g. `c:\Users\<yourname>\vm\accessModServer`);
  2. Holding the shift key, right click on the folder, then chose "open command window here"
  3. type: `vagrant up` (this could take a while to complete);
  4. open a browser (chrome, safari, firefox, opera) and go to <http://localhost:8080/> or <http://192.168.38.38:3838/>.

#### Uninstall

  To start again on a *fresh* base machine :
  1. Inside the folder containing the Vagrantfile, open a command widow and type `vagrant destroy` 
  2. Reinstall whith `vagrant up` 


  To completly remove accessmod 5:

  1. Delete the folder you've created in installation step
  2. [Remove Vagrant](https://docs.vagrantup.com/v2/installation/uninstallation.html)
  3. [Remove Virtual box](https://www.virtualbox.org/manual/ch02.html)



----

# Modules

+ [Module 1](#module-1-merging-maps)


----


## Module 1 merging maps

+ [Description](#description)
+ [Parameters](#parameters)
+ [Return](#return)
+ [Detail](#detail)

## Description
  This module allow user to create a raster map that contains categories of land cover, categories of road and barriers. The module will merge the maps provided in selected order.


## Usage
1. In map manager, add maps: _land cover_ (raster), _road_ (vector), _barrier_ (vector) 
2. In module 1, for each maps, define the table of categories and add maps to _stack_
3. In _stack_ move maps in selected order (see [detail](#detail))
4. Add at least one tag and proceed to merging maps process.


## Parameters
  1. Landcover: a table of categories and corresponding labels. Edition can be done from the UI
  ![](img/mod1_lcv.png)
  2. Road : a table of distinct combinaison from a category column and a label column. Selection is done with a drop down menu.
  ![](img/mod1_stack.png)
  3. Barrier : selection on the type of geometry to import as a barrier: point; line; area. 
  ![](img/mod1_barrier.png)
  4. Merge
  ![](img/mod1_stack_merge.png)


## Return
  `merged__<tags>`

## Detail
  This main function of this module uses only raster maps with the same resolutions and coordinate reference system as the base DEM map, proceeded by the _add to stack_ process described below. This step will populate the *stack*, which is a set of raster map formatted to contains defined distinct categories. The logic of this module relies on the fact that the order of the merging procedure impacts directly the accessibility analysis. A raster cell can contain only one category, even if in reality multiple category are present in the surface covered by the cell. This module let you select in such case which category should be prioritised over another. In other word, depending of which scenario the user want to produce, the position of roads in the stack (e.g. path under highways; preference given on pedestrian path) or the position of barrier (e.g rivers under a bridge, flooded area over roads) will produce a potentially different reality of travelling time.

### Add to stack process
  - Land cover. After the map has been selected, the original category are extracted and displayed in a table. After editing this table, when _add to stack_ is called, the new category table is linked to the provided map and a copy is made to be displayed in stack list.
  - Road. After the map selection, a function search for an integer and a character column in the attribute table and display the results in menu. When the choice of category and value columns are set, the distinct combinaison are shown in a table. When the resulting table is correct, the _add to stack_ process will look on each category and extract a subset of geometry to be rasterized and added to the stack.
  - Barrier. After the map selection, a table will show a summary of geometry feature. The user can then select area, line or point geometry to be rasterised. After this choice, the _add to stack_ process will create, a raster layer. This stack layer will not contain any category and will be used as a inverse mask for the raster merging.

### Ordering and merging process
  Once selected maps are added to the stack, the layer ordering can be done. During the merging process, the non-null cells from first layer on top will be added to the resulting merged map. The fallowing layers will fill any NULL value given by the precedent layers. The barrier layers act as inverse masks for the next maps below them: value are then added only if they match an available cell and if they match a cell not covered by the barrier.

## Exemple

  * Illustration of merging logic

  top layer

  1 1 1 1   
  1 0 0 1   
  1 2 3 0   
  0 0 0 0   

  barrier layer

  1 0 0 0   
  0 1 0 0   
  0 0 1 0    
  0 0 1 0   

  bottom layer

  3 4 5 7   
  8 9 9 7   
  3 4 8 7   
  8 7 6 5   

  Result merged

  1 1 1 1   
  1 0 9 1   
  1 0 3 1   
  8 7 0 5   

  * Illustration of different stack order selection
  We want to merge three maps : Lake, Highways and Track. The selected stack order will define the final merged map. In the figure below, we can observe that 4 different stack order will create 4 distinct scenarios:

  * 1 Track > Highway > Lake : priority given on travel on track.
  * 2 Highway > Track > Lake : priority given on travel on highways.
  * 3 Track > Lake > Highway : Flood event. 
  * 4 Lake > Highways > Track : Flood event, with priority given on highways.

  ![](img/mod1_stack_order.png)

----

## Module 2 :

----

## Module 3 :

----

## Module 4 :

