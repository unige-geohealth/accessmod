# Build AccessMod base image 

_nothing will be applied unless the a argument is specified_

Example : 

- stop at `test` stage (t):

```sh
./build_alpine_base.sh -t
```

- build local (l)  

```sh
AM_VERSION_MINOR=5.8-e ./build_alpine_base.sh -l
```
- build and push prod (could be very time consuming) (p)  

```sh
AM_VERSION_MINOR=5.8-e ./build_alpine_base.sh -p
```
