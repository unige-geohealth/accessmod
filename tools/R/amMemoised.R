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



#
# Centralised memoise functions : <functionName>_cached  = memoised
# NOTE: this should not be used for "side effects" functions ( that do not return something )
#
amReMemoizeCostlyFunctions <- function() {
  amMemoizeThat("amGetRasterCategory")
  amMemoizeThat("amGetRasterStat")
  amMemoizeThat("amGetFacilitiesTable")
  amMemoizeThat("amZonalAnalysis")
}

amMemoizeThat <- function(funName) {
  funNameCached <- paste0(funName, "_cached")
  if (exists(funNameCached) && is.memoised(get(funNameCached))) {
    forget(get(funNameCached))
  } else {
    assign(funNameCached, memoise(get(funName)), envir = environment(get(funName)))
  }
}
