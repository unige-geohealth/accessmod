# Build AccessMod base image 

Nothing is built or pushed unless the `-a` argument is specified.

Example : 

- stop at `test` stage (t):

```sh
./build_alpine_base.sh -t -a
```

- build local (l)  

```sh
AM_VERSION_MINOR=5.9-d ./build_alpine_base.sh -l -a
```
- build and push prod (could be very time consuming) (p)  

```sh
AM_VERSION_MINOR=5.9-d ./build_alpine_base.sh -p -a
```
