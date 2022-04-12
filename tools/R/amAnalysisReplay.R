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

# Save and replay analysis

#
# Save 'replay' file
#
amAnalysisReplaySave <- function(
  analysis,
  mapset,
  location,
  args,
  name,
  data_output = c(),
  timestamp = Sys.time(),
  overwrite = TRUE
) {
  valid <- amGrassSessionIsValid()

  if (!valid) {
    stop("Require valid am grass session")
  }

  # NULL values are converted to {} :(
  # ex. toJSON(list(a=NULL,b=NA,c="",d=character(1)),auto_unbox=T)
  # result = {"a":{},"b":null,"c":"","d":""}
  args <- lapply(
    args,
    function(a) {
      if (isEmpty(a)) {
        return(NA)
      }
      return(a)
    }
  )

  #
  # JSON + dir / file creation
  #
  str <- toJSON(list(
    timestamp = timestamp,
    analysis = analysis,
    mapset = mapset,
    location = location,
    data_output = data_output,
    args = args
  ),
  auto_unbox = TRUE
  )
  aPath <- system2("echo", config$pathConfigs)
  fPath <- file.path(aPath, sprintf("%s.json", name))

  if (!dir.exists(aPath)) {
    dir.create(aPath)
  }
  if (file.exists(fPath)) {
    if (overwrite) {
      unlink(fPath)
    } else {
      stop("amAnalysisReplaySave: file already exists")
    }
  }

  write(str, fPath)
}




#
# Read 'replay' file and restart the analysis
#
amAnalysisReplayExec <- function(replayConf, exportDirectory = NULL) {
  conf <- amAnalysisReplayParseConf(replayConf)

  amGrassNS(
    location = conf$location,
    mapset = conf$mapset,
    {
      amReMemoizeCostlyFunctions()
      res <- do.call(conf$analysis, conf$args)

      if (!isEmpty(exportDirectory)) {
        amAnalysisReplayExport(conf, exportDirectory)
      }
      return(res)
    }
  )
}

#
# Export all output
#
amAnalysisReplayExport <- function(replayConf, exportDirectory) {
  conf <- amAnalysisReplayParseConf(replayConf)

  if (isEmpty(exportDirectory)) {
    exportDirectory <- file.path(tempDir(), amRandomName(""))
  }
  mkdirs(exportDirectory)

  for (dataName in conf$data_output) {
    amExportData(
      dataName = dataName,
      exportDir = exportDirectory
    )
  }

  return(exportDirectory)
}


#
# Parse replay file or config list
#
amAnalysisReplayParseConf <- function(replayConf) {
  if (typeof(replayConf) == "list") {
    return(replayConf)
  }

  if (file.exists(replayConf)) {
    return(fromJSON(replayConf))
  }

  stop("amAnalysisReplayParseFile : unexpected input")
}
