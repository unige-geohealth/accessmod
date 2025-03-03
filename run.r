#         ___                                  __  ___            __   ______
#        /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#       / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#      / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#     /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
#    AccessMod 5 Supporting Universal Health Coverage by modelling physical accessibility to health care
#
#    Copyright (c) 2014-present WHO, Frederic Moser (GeoHealth group, University of Geneva)
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

tryCatch(
  {
    host <- "0.0.0.0"
    args <- commandArgs(trailingOnly = TRUE)

    port <- as.numeric(Sys.getenv("AM5_PORT_APP"))

    # first arg = force app port
    if (length(args) == 1) {
      port <- as.numeric(args[1])
      Sys.setenv("AM5_PORT_APP" = port)
    }

    source("global.R")

    runApp(
      "app.R",
      host = host,
      launch.browser = FALSE,
      port = port
    )
    
  },
  error = function(e) {
    warning(e)
    return(e) ## to remove in prod
    quit(
      save = "no",
      status = 1
    )
  }
)
