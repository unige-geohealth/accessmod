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

amTryConditional <- function(expr,
                             test = function() {
                               TRUE
                             },
                             timeInterval = 1000,
                             maxTries = 10,
                             finally = function() {}) {
  nTries <- 0

  tryLater <- observe({
    nTries <<- nTries + 1
    overtried <- nTries >= maxTries
    success <- FALSE
    tryCatch(
      {
        success <- test()
      },
      error = function(cond) {
        warning(cond)
        success <<- FALSE
      }
    )
    amDebugMsg("success = ", success, ", ntries = ", nTries)
    if (overtried || success) {
      tryLater$destroy()
      if (success) {
        eval.parent(expr)
      }
    } else {
      invalidateLater(timeInterval)
    }
  })
}

#' Debounce reactive
#' taken from https://github.com/rstudio/shiny/blob/master/R/reactives.R
amReactiveDebounce <- function(r, millis, priority = 100, domain = getDefaultReactiveDomain()) {
  force(r)
  force(millis)

  if (!is.function(millis)) {
    origMillis <- millis
    millis <- function() origMillis
  }

  v <- reactiveValues(
    trigger = NULL,
    when = NULL # the deadline for the timer to fire; NULL if not scheduled
  )

  # Responsible for tracking when f() changes.
  firstRun <- TRUE
  observe(
    {
      r()

      if (firstRun) {
        # During the first run we don't want to set v$when, as this will kick off
        # the timer. We only want to do that when we see r() change.
        firstRun <<- FALSE
        return()
      }

      # The value (or possibly millis) changed. Start or reset the timer.
      v$when <- Sys.time() + millis() / 1000
    },
    label = "debounce tracker",
    domain = domain,
    priority = priority
  )

  # This observer is the timer. It rests until v$when elapses, then touches
  # v$trigger.
  observe(
    {
      if (is.null(v$when)) {
        return()
      }

      now <- Sys.time()
      if (now >= v$when) {
        # Mod by 999999999 to get predictable overflow behavior
        # v$trigger <- isolate(v$trigger %OR% 0) %% 999999999 + 1
        v$trigger <- runif(1)
        v$when <- NULL
      } else {
        invalidateLater((v$when - now) * 1000)
      }
    },
    label = "debounce timer",
    domain = domain,
    priority = priority
  )

  # This is the actual reactive that is returned to the user. It returns the
  # value of r(), but only invalidates/updates when v$trigger is touched.
  er <- eventReactive(v$trigger,
    {
      r()
    },
    label = "debounce result",
    ignoreNULL = FALSE,
    domain = domain
  )

  # Force the value of er to be immediately cached upon creation. It's very hard
  # to explain why this observer is needed, but if you want to understand, try
  # commenting it out and studying the unit test failure that results.
  primer <- observe(
    {
      primer$destroy()
      er()
    },
    label = "debounce primer",
    domain = domain,
    priority = priority
  )

  er
}
