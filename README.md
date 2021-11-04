![](https://raw.githubusercontent.com/fxi/accessModShiny/master/www/logo/icons/logo32x32.png) _AccessMod 5_

## Summary

This is the main repository of `AccessMod 5`.

`AccessMod 5` is a tool to analyze geographical accessibility to or from given locations, using anisotropic movements and multimodal transport processes (e.g. walk, bicycles, motorized vehicles). This package may help to analyze catchments of peoples who can reach a central point in a given time and transport model or determine where new public services should be scaled up in priority.

This product is developed by the Department of Health Systems Financing (WHO/HSS/HSF) in collaboration with the WHO Vulnerability and Risk Analysis & Mapping program (VRAM).

In Fig. 1, we can see an exemple of the cumulative time map produced with the module 2 of `AccessMod 5`, displayed here in a composition created with the map composer in QGIS.

<figure>
<img src="https://raw.githubusercontent.com/wiki/fxi/accessModShiny/img/anisoCumulativeCostSample.jpg" alt="Accessmod 5, module 2 : cumulative cost">
</a>
<figcaption>
Fig. 1<em> Example of cumulative time map computed with multimodal transportation scheme, anisotropic displacement for a sample region.</em>
<hr>
</figcaption>
</figure>


## User manual

Download the latest version of the user manual in the Download section of the Accessmod website : [accessmod.org](https://www.accessmod.org/).

Or Access the online version of the user manual : [accessmod online user manual](https://doc-accessmod.unepgrid.ch/display/EN/AccessMod+5+user+manual)


## Development

```sh
# Launch a development session
docker-compose up
$ docker exec am5_dev sh
$ cd /appdev
$ R
> source('app.r')

# Build base images
cd docker
./build.sh
```

## Electron notes 
- State is stored in application data 
- Browser, communication with electron : 
   - amcom.getState('data_location').then(console.log);
   - amcom.request('list_versions',{}).then(console.log);
   - amcom.getState('port_host').then(console.log);
   - ...

## Issues

The bugs and new functionality request should be reported here :
[`AccessMod 5` issues](https://github.com/fxi/accessModShiny/issues)
