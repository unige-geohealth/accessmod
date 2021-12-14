
#
# Copy Demo file if nothing exists in GRASS database
#
if(!dir.exists(config$pathGrassDataBase)){
  dir.create(
    config$pathGrassDataBase,
    recursive = T,
    showWarnings = F
  )
}
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


