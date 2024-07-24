
amErrHandler <- function(session = shiny:::getDefaultReactiveDomain(),
  errMsgTable,
  call,
  conditionMsg,
  title = NULL,
  type = "warning") {
  #
  # in all case, return message as log.
  #
  textDefault <- tagList(
    tags$p(conditionMsg),
    tags$p("Call"),
    tags$p(call)
  )
  amMsg(
    session,
    type = "log",
    text = textDefault,
    title = title
  )
  errMsg <- data.frame()

  # try to find a registered simplified message to display in UI
  tryCatch(
    {
      errMsg <- errMsgTable[
        sapply(errMsgTable$cond, grepl, as.character(conditionMsg)),
      ]
    },
    error = function(cond) {
      amMsg(
        session,
        type = "log",
        text = cond$message,
        title = "Error handling issue"
      )
    }
  )

  # replace original message
  if (nrow(errMsg) > 0) {
    for (i in 1:nrow(errMsg)) {
      if (errMsg[i, "type"] != "discarded") {
        amMsg(
          session,
          type = tolower(errMsg[i, "type"]),
          text = errMsg[i, "text"],
          title = title
        )
      }
    }
    # if no match found in msg table, return
    # original text and type found by amErrorAction
  } else {
    amMsg(
      session,
      type = type,
      text = textDefault,
      title = title
    )
  }
}

amErrorAction <- function(expr,
  errMsgTable = config$msgTableError,
  quotedActionError = NULL,
  quotedActionWarning = NULL,
  quotedActionMessage = NULL,
  quotedActionFinally = NULL,
  title,
  warningToLog = TRUE,
  messageToLog = TRUE,
  pBarFinalRm = TRUE,
  session = shiny:::getDefaultReactiveDomain()) {
  withCallingHandlers(
    {
      tryCatch(
        {
          expr
        },
        # error : stop process, eval error quoted function, return condition to amErrHandler
        error = function(cond) {
          msg <- cond$message
          call <- paste(deparse(cond$call), collapse = " ")
          if (!is.null(quotedActionError)) eval(quotedActionError)
          amErrHandler(session, errMsgTable,
            conditionMsg = msg,
            title = title,
            call = call,
            type = "error"
          )

          if (pBarFinalRm) {
            progressBarControl(percent = 100)
          }
          return()
        }
      )
    },
    # warning, don't stop process, but return condition to amErrHandler
    warning = function(cond) {
      msg <- amSubQuote(cond$message)
      call <- paste(deparse(cond$call), collapse = "")

      if (!is.null(quotedActionWarning)) eval(quotedActionWarning)
      if (!warningToLog) {
        amErrHandler(session, errMsgTable,
          conditionMsg = msg,
          title = title,
          call = call,
          type = "warning"
        )
      } else {
        amErrHandler(session, errMsgTable,
          conditionMsg = msg,
          title = title,
          call = call,
          type = "log"
        )
      }

      return()
    },
    # simple message : don't stop, write in log. usa amMsg(type=message for a real message.)
    message = function(cond) {
      msg <- amSubQuote(cond$message)
      if (is.null(quotedActionMessage)) eval(quotedActionMessage)


      if (!messageToLog) {
        amMsg(session,
          text = msg,
          title = title,
          type = "message"
        )
      } else {
        amMsg(session,
          text = msg,
          title = title,
          type = "log"
        )
      }

      return()
    },
    finally = {
      if (!is.null(quotedActionFinally)) {
        eval(quotedActionFinally)
      }
    }
  )
}
