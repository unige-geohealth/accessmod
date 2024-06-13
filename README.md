![](https://raw.githubusercontent.com/fxi/accessModShiny/master/www/logo/icons/logo32x32.png) _AccessMod 5_

## Summary

This is the main repository of `AccessMod 5`.

`AccessMod 5` is a tool to analyze geographical accessibility to or from given locations, using anisotropic movements and multimodal transport processes (e.g. walk, bicycles, motorized vehicles). This package may help to analyze catchments of peoples who can reach a central point in a given time and transport model or determine where new public services should be scaled up in priority.

This product is developed by the [GeoHealth group](https://unige.ch/geohealth) at the University of Geneva, in collaboration with the World Health Organization and [MORU/Health GeoLab Group](https://www.tropmedres.ac/units/moru-bangkok/epidemiology/our-team-1/health-geolab) (Manila, Philippines).

<figure>
<img src="www/img/am_screenshot_map.jpg" alt="AccessMod 5 : Interactive Map">
</a>
<figcaption>
</em>AccessMod user interface, interactive map. Visualization of generated travel time layer and a set of facilities.</em>
<hr>
</figcaption>
</figure>


## User manual

Download the latest version of the user manual in the Download section of the Accessmod website : [accessmod.org](https://www.accessmod.org/).

Or Access the online version of the user manual : [accessmod online user manual](https://doc-accessmod.unepgrid.ch/display/EN/AccessMod+5+user+manual)


## Development

```sh
# Launch AccessMod stack 
# with app files bind mounted -> /app in docker-compose.yml 
$ docker compose up

# Check if the app server is working:  http://localhost:3080/status
# Check if the agent server is working:  http://localhost:5080/status

# --------- app dev session 
$ docker compose exec am5_dev R
> source('run.r')

# --------- app dev non-interactive session  
$ docker compose exec am5_dev Rscript --vanilla run.r
# Debbuging http.r in a secondary session :
# 1) Comment the source(http.r) line in run.r 
# 2) docker compose exec am5_dev R 
# 3) source('http.r')


# ---------- Replay analysis (dev)

# Launch a development session for the app
$ docker compose exec am5_dev R
> source('global.R')
> amAnalysisReplayExec("<path to config>.json")
# exemple in a dev session
> amAnalysisReplayExec("/data/dbgrass/demo/demo/accessmodConfigs/lAnalysisParameters__425.json")



# ---------- BUILD IMAGES
# Build base images
cd docker
./build.sh

# ---------- Github actions testing (not fully implemented)
act --secret-file .secrets --remote-name github --container-architecture linux/amd64


# ---------- BASIC END-TO-END TESTS 
# Default script 
$ npm run test
# - or - direct command with docker compose 
$ docker compose exec am5_dev Rscript tests/start.R 
# - or - from an interactive session 
$ docker compose exec am5_dev R
> source('tests/start.R') 

```

## Branches
- `main`: Contains the production-ready code. This branch holds the latest stable code. Pushing to this branch does not trigger any automated processes.
- `staging`: Used for integrating new features and minor versions. Pushing to this branch triggers the build of a Docker image.
- `release`: Contains stable versions ready for release. Pushing to this branch triggers the build of a Docker image, a VirtualBox OVA, and all Electron builds.

## Versioning

To create a version, use `npm run version` from the `staging` or `release` branches. This command helps in selecting the version and commits it.

### Expected Workflow

1. **Integrate a feature in a dedicated branch.**
2. **Test the feature.**
3. **Merge into the `staging` branch.**
4. **Merge into the `release` branch** (optional).
5. **Create a version**: Select the correct semver value; it will push to the selected remote.
    - **If on the `release` branch**: All assets will be built.
    - **If on the `staging` branch**: Only the Docker image will be built.

### Hot Fixes

In case of a hot fix, pushing to the `staging` or `release` branches manually will trigger the corresponding actions and overwrite the built version.

## Electron

```sh
cd electron
yarn start 
# yarn start:debug for an interactive session, with external debugger.
```

- State is stored in application data 
- Browser, communication with electron : 
   - amcom.getState('data_location').then(console.log);
   - amcom.request('list_versions',{}).then(console.log);
   - amcom.getState('port_host').then(console.log);
   - ...

## Issues

The bugs and new functionality request should be reported here :
[`AccessMod 5` issues](https://github.com/fxi/accessModShiny/issues)
