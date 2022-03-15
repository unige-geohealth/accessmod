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


#' encode in base64
encodeB64 <- function(text) {
  print(text)
  if (length(text) != 1) {
    text <- "[NA]"
  }
  base64enc::base64encode(charToRaw(as.character(text)))
}

progressBarControl <- function(id = config$pBarId,
                               percent = 0,
                               title = "default",
                               text = "default",
                               tooltip = "",
                               visible = TRUE,
                               session = getDefaultReactiveDomain(),
                               timeOut = NULL,
                               timeout = NULL) {
  hasSession <- !is.null(session)

  if (percent == 0 || percent == 100) {
    amProgressStopClean()
  }

  quit <- amProgressStopExists()

  # default time out
  if (!amNoDataCheck(timeout)) {
    timeOut <- timeout
  }
  if (amNoDataCheck(timeOut) || !is.numeric(timeOut)) {
    timeOut <- config$pBarTimeOut
  }

  if (quit) {
    amProgressStopClean()
    if (hasSession) {
      session$sendCustomMessage(
        type = "progressUpdate",
        list(
          visible = visible,
          id = id,
          percent = 100,
          title = encodeB64(title),
          text = encodeB64("Interruption")
        )
      )
    }
    stop("pBarQuit")
  } else {
    if (hasSession) {
      session$sendCustomMessage(
        type = "progressUpdate",
        list(
          visible = visible,
          id = id,
          percent = percent,
          title = encodeB64(title),
          text = encodeB64(text)
        )
      )
    } else {
      cat(
        toJSON(
          list(
            visible = visible,
            id = id,
            percent = percent,
            title = title,
            text = text
          )
        )
      )
    }
  }
}
pbc <- progressBarControl



amProgressStopWrite <- function() {
  write("", "/tmp/am_progress_stop")
}
amProgressStopExists <- function() {
  file.exists("/tmp/am_progress_stop")
}
amProgressStopClean <- function() {
  unlink("/tmp/am_progress_stop")
}
