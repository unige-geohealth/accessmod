
#
# Copy Demo file if nothing exists in GRASS database
#
hasProjects <- length(list.files(config$pathGrassDataBase)) > 0
hasDemo <- dir.exists(config$pathGrassDemo) 

if(!hasProjects && hasDemo){
  args = c(
    "-r",
    config$pathGrassDemo,
    config$pathGrassDataBase
  )
  system2('cp',args)
}


