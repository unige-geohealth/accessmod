# Advanced Replay Example

This script will perform a replay analysis using a `bash` script.

```txt 
.
|-- readme.md    -> the file you are reading right now
|-- config.json  -> replay config file
|-- data.json    -> addtional data: multiple scenarios
|-- out          -> Output directory. Results will be writen in this directory 
|-- project.am5p -> AccessMod project archive to import 
|-- script.R     -> Main script that will be run inside the container
`-- script.sh    -> Launch script from as server with `docker` running

```

The entry point is `script.sh`. From this directory, on a `*nix` OS (linux, MacOS), simply run 

```sh 
$ ./script.sh 
```

What will happen : 

- Check for existing files and directory 
- Launch the `accessmod` container using the image set
- The main command will execute `Rscript` on file `script.R`
- From the R script, inside the Docker container, the environment will be loaded, variables set 
- The project will be imported. This step can be done with `amAnalysisReplayExec`, but in this case, we need a working project before launching the replay, to extract some statistics
- A nested loop on multiple scenarios, multiple travel times will be performed 
- Each pair "travel time + scenario" will create raster layers, vector layers, and tables, inside the `./out` directory, for further processing.
- A summary table will collect intermediate results
- At the end, the summary table is written as `JSON` file in `./out` directory.
