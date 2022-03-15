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

tryCatch(
  {
    host <- "0.0.0.0"
    args <- commandArgs(trailingOnly = TRUE)

    port <- as.numeric(Sys.getenv("AM5_PORT_APP"))
    portHttp <- as.numeric(Sys.getenv("AM5_PORT_HTTP"))
    portHttpPublic <- as.numeric(Sys.getenv("AM5_PORT_HTTP_PUBLIC"))

    if (length(args) == 3) {
      port <- as.numeric(args[1])
      portHttp <- as.numeric(args[2])
      portHttpPublic <- as.numeric(args[3])
      Sys.setenv("AM5_PORT_APP" = port)
      Sys.setenv("AM5_PORT_HTTP" = portHttp)
      Sys.setenv("AM5_PORT_HTTP_PUBLIC" = portHttpPublic)
    }

    system("Rscript http.r", wait = F)

    source("global.R")

    runApp(
      ".",
      host = host,
      launch.browser = F,
      port = port
    )
  },
  error = function(e) {
    warning(e)
    quit(
      save = "no",
      status = 1
    )
  }
)
