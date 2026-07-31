# Build AccessMod base image

The default build pins GRASS GIS 8.5.0. Override `GRASS_VERSION` only to test
another exact release; do not use a branch name or a moving tag.

Nothing is built or pushed unless the `-a` argument is specified.

Example : 

- stop at `test` stage (t):

```sh
./build_alpine_base.sh -t -a
```

- build local (l)  

```sh
GRASS_VERSION=8.5.0 BASE_IMAGE_TAG=5.9-f ./build_alpine_base.sh -l -a
```
- build and push prod (could be very time consuming) (p)  

```sh
GRASS_VERSION=8.5.0 BASE_IMAGE_TAG=5.9-f ./build_alpine_base.sh -p -a
```

`AM_VERSION_MINOR` remains available as a deprecated alias for existing local
commands. Application builds and Compose can override the selected base image
with `ACCESSMOD_BASE_IMAGE`.
