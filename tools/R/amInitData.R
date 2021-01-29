
#
# Copy Demo file if nothing exists in GRASS database
#
hasProjects <- length(list.files(config$pathGrassDataBase)) > 0
hasDemo <- dir.exists(config$pathGrassDemo) 

if(!hasProjects && hasDemo){
  cmd <- sprintf("cp -r %1$s %2$s/",
    config$pathGrassDemo,
    config$pathGrassDataBase
  )
  system(cmd)
}


