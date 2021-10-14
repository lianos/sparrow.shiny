#' Rounds every numeric column of a data.table to given precision
#' @param x a `data.table` like thing
#' @param digits the number of digits to round to
#' @return the rounded `x`
round.dt <- function(x, digits=3) {
  stopifnot(is(x, 'data.table'))
  for (cname in names(x)[sapply(x, is.numeric)]) {
    vals <- x[[cname]]
    if (!is.integer(vals)) {
      x[, (cname) := round(vals, digits=3)]
    }
  }
  x
}

#' Generates the CSS/UI setup to reduce code required in documentation examples
#'
#' Includes some boilerplate code required in the shiny app UI setup that we
#' don't want to repeat over and over.
#'
#' For internal function only, but exported so we can use it in examples.
#' @export
#' @return inlined html/css
exampleUISetup <- function() {
  shinyjs::useShinyjs()
  shiny::tags$head(
    shiny::includeCSS(
      system.file("shiny", "www", "dashboard.css", package = "sparrow.shiny")),
    shiny::includeCSS(
      system.file("shiny", "www", "miniUI.css", package = "sparrow.shiny")))
}
