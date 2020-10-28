# AccessMod5 Docker

##  Launch stack 

```sh
> docker-compose up
```

## Launch app in a interactive R session (to develop)

- Start a shell session inside am5 service:

```sh
$ docker-compose exec am5 sh
```

- Start R:

```sh
/app # R
```

- Source run.R shortcut:

```sh
> source('run.R')
```


## Build all images and tag them

```sh
$ ./build.sh
```

## Folders

  - `am5_common` : base layer. GDAL + deps
  - `am5_grass` : grass layer
  - `am5_r` : r layer
  - `am5_app` : app layer + import all layers

##  TODO 
### Quick
  - [x] Write docker files
  - [x] Solve dependecy hell 
  - [x] Modify app config files
  - [x] Convert map plot to Leaflet
  - [ ] Finalise am5 image with import app files in /app
  - [ ] Remove git references
  - [ ] Remove/convert update system
  - [ ] Solve reactive hell
  - [ ] Test

### Long term
  - [ ] Launch child docker from app
  - [ ] Wrap application in electron app


