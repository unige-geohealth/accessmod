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

  - `am5_grass` : grass layer
  - `am5_r` : r layer
  - `am5_app` : app layer + import all layers


