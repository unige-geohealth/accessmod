



withCallingHandlers({
  tryCatch({
    print('before')
    message('continue on message')
    warning('continue on warning!')
    print('after message')
    stop('stop on error.')
    print('after error')
  },error=function(c)print(paste('the error is',conditionMessage(c)))
  )
},
message=function(m)print(paste('the message is',conditionMessage(m))),
warning=function(w)print(paste('the warning is',conditionMessage(w)))
)

message2error <- function(code) {
    withCallingHandlers(code, message = function(e) stop(e))
}

f <- function() g()
g <- function() message("Hi!")
g()
# Error in message("Hi!"): Hi!
message2error(g())





withCallingHandlers({
  warning("A")
  1+2
},warning = function(w) {print(w)}
)
