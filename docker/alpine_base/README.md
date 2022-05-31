# Build AccessMod base image 

_nothing will be applied unless the a argument is specified_

Example : 

- stop at `test` stage (t):

```sh
./build.sh -t
```

- build local (l)  

```sh
AM_VERSION_MINOR=5.7 ./build.sh -l
```

- build and push prod (could be very time consuming) (p)  

```sh
AM_VERSION_MINOR=5.7 ./build.sh -p
```
