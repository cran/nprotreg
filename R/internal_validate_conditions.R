## Package independent support for validating conditions

condition <- function(subclass, message, call = sys.call(-1), ...) {
  structure(
    class = c(subclass, "condition"),
    list(message = message, call = call),
    ...
  )
}

is.condition <- function(x) inherits(x, "condition")

Validator <- setRefClass("Validator",
  methods = list(
    base_format_message = function(formatting, ...) {
     objects <- list(...)
      number_of_objects <- length(objects)
      for (i in 1:number_of_objects) {
        formatting <- sub(
          paste0("[{]{1}[", i, "]{1}[}]{1}"),
          objects[i],
          formatting,
          useBytes = FALSE)
      }
      formatting
    },
    base_stop = function(
      subclass,
      message,
      which = -1,
      ...) {
      cond <- condition(
        c(subclass, "error"),
        message,
        call = sys.call(which),
        ...
      )
      stop(cond)
    },
    base_validate = function(condition, message, which) {
      if (condition) {
        base_stop(
          "Error in ",
          message,
          which
        )
      }
    }
  )
)
