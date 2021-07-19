Build accessmod base images :

`accessmod_r` -> intermediate image for R
`accessmod_grass` -> intermediate image for grass 
`accessmod` -> actual image used in prod


*example*

```sh
./build -p # build and push all iamges
./build -d # dry run
```
