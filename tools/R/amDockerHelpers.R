#         ___                                  __  ___            __   ______
#        /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#       / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#      / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#     /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
#    AccessMod 5 Supporting Universal Health Coverage by modelling physical accessibility to health care
#    
#    Copyright (c) 2014-2020  WHO, Frederic Moser (GeoHealth group, University of Geneva)
#    
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#    
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#    
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <https://www.gnu.org/licenses/>.

library(stevedore)
library(jsonlite)
library(semver)
library(memoise)
library(cachem)

cm <- cache_mem(max_age = 5 * 60)
docker <- docker_client()
baseTag <- 'fredmoser/accessmod' 
versionCurrent <- readLines('version.txt')
versionMin <- '5.7.16-alpha'
urlHubFetch <- 'https://hub.docker.com/v2/repositories/%1$s/tags/?page_size=%2$s&page=%3$s'

amFilterMinVersions <-function(versions){
  versions <- versions[!versions %in% 'latest']
  sVersions <- parse_version(versions)
  versions <- versions[sVersions>=versionMin]
  versions <- unique(c(versions,versionCurrent))
  return(versions)
}

amDockerListVersionsLocal = function(){
  imgList <- docker$image$list()
  repoTagList <- unlist(imgList$repo_tags)
  repoList <- vapply(repoTagList,function(x){strsplit(x,':')[[1]][[1]]},character(1)) 
  repoTagListSelect <- repoTagList[repoList %in%  baseTag]
  versions <- vapply(repoTagListSelect,function(x)strsplit(x,':')[[1]][[2]],character(1))
  versions <- amFilterMinVersions(versions)
  return(versions)
}

amDockerListVersionsRemote <- function(pageSize=10,page=1){
  tags <- fromJSON(
    sprintf(
      urlHubFetch,
      baseTag,
      pageSize,
      page
    )
  )
  versions <- tags$results$name
  versions <- amFilterMinVersions(versions)
  return(versions);
}

amDockerVersionsSummary_net <- function(){
  remoteVersions <- amDockerListVersionsRemote()
  localVersions <- amDockerListVersionsLocal()
  sRemoteVersions <- parse_version(remoteVersions)
  sLocalVersions <- parse_version(localVersions)
  summary <- list(
    local = as.list(localVersions),
    remote = as.list(remoteVersions),
    current = as.character(versionCurrent),
    maxLocal = as.character(max(sLocalVersions)),
    maxRemote = as.character(max(sRemoteVersions)),
    minLocal = as.character(min(sLocalVersions)),
    minRemote = as.character(min(sRemoteVersions)),
    hasUpdate = max(sRemoteVersions) > max(sLocalVersions),
    date = Sys.time()
  )
  return(summary)
}



amDockerVersionsSummary <- memoise(amDockerVersionsSummary_net,cache=cm)

