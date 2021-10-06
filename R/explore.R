#' Interactively explore a SparrowResult via a shiny app
#'
#' This will launch a shiny application that enables exploratory analysis of
#' the results from the different GSEA methods run within a `SparrowResul`.
#'
#' Reference the "shiny-sparrow" vignette for more detailed documentation of
#' the functionality provided by this application.
#'
#' @export
#' @param x A `SparrowResult` object, or path to one as an *.rds. If
#'   missing, the shiny app will load without a `SparrowResult` object
#'   to explore, and the user can upload one into the app.
#' @examples
#' \donttest{
#' vm <- sparrow::exampleExpressionSet()
#' gdb <- sparrow::exampleGeneSetDb()
#' sr <- sparrow::seas(vm, gdb, vm$design, methods=c('camera', 'fry'))
#' explore(sr)
#' }
explore <- function(x) {
  if (!missing(x)) {
    if (is.character(x)) x <- readRDS(x)
    assert_class(x, 'SparrowResult')
    options(EXPLORE_SPARROW_RESULT = x)
    on.exit(options(EXPLORE_SPARROW_RESULT = NULL))
  }
  shiny::runApp(system.file("shiny", package = "sparrow.shiny"))
}
