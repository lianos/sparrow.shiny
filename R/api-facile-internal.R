# This functions are defined in facilebio, but are redefined and kept internal
# here. We define them like so because we don't want to add a facilebio
# dependency.

#' Tests if a module is initialized
#'
#' @param x A thing used in a shiny environment that requires time to initialize
#'   before it can be used.
#' @param ... pass through
#' @return logical TRUE/FALSE
initialized <- function(x, ...) {
  UseMethod("initialized", x)
}

#' @noRd
initialized.ReactiveGeneSetDb <- function(x, ...) {
  is(x$gdb(), "GeneSetDb") && nrow(x$geneSets()) > 0
}

#' Checks if the return value from a selectInput looks like it's unselected
#'
#' Copied from FacileViz, but we don't want to depend on that.
#'
#' For some reason, sometimes a selectInput returns `NULL` and other times
#' it returns `""`, so I'm just making this utility function to deal with that
#'
#' NOTE: This is really a function that is used by shiny modules, but instead
#' of having every shiny function check if something is "unselected" and setting
#' it to NULL, we move that functionality in here so that internal vizualization
#' functions can do that jusst once
#'
#' @param value The (character) object returned from a `selectInput`
#' @param ignore strings that are included in a select box that define a
#'   "not selected / active" state
#' @return boolean flag used to indicate wether "this thing" has been set to
#'   a specific state by the user.
unselected <- function(value, ignore = c("---", "__initializing__", "")) {
  if (is.null(value)) return(TRUE)
  # Otherwise this is a character
  if (is(value, "data.frame") || is(value, "tbl")) {
    val <- nrow(value) == 0
  } else {
    val <- length(value) == 0L || nchar(value) == 0L || all(value %in% ignore)
  }
  val
}
