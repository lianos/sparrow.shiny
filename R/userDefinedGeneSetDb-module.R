#' Creates a GeneSetDb from a user-specfied gene set definition table.
#'
#' This module provides an upload button that allows a user to upload a
#' table of gene set definitions in [sparrow::GeneSetDb()] format as a CSV file
#' or excel file (if the readxl package is installed). Minimal validation checks
#' are implemented.
#'
#' @export
#' @param input,output,session shiny module bits
#' @param ... pass through arguments
#' @return A list of reactive components. `$gdb()` will be a GeneSetDb when
#'   the user uploades a valid gene set definition file. Otherwise it will be
#'   `NULL`.
#' @examples
#' # You can upload the file available here:
#' (ex.fn <- system.file("testdata", "user-defined-genesets-example.xlsx",
#'                       package = "sparrow.shiny"))
#' app <- shiny::shinyApp(
#'   ui = shiny::shinyUI(shiny::fluidPage(
#'     exampleUISetup(),
#'     title = "Costum Gene Set Collection Upload",
#'     userDefinedGeneSetDbUI("mod"))),
#'   server = function(input, output, session) {
#'     shiny::callModule(userDefinedGeneSetDb, "mod", gdb)
#'   })
#' if (interactive()) {
#'   shiny::runApp(app)
#' }
userDefinedGeneSetDb <- function(input, output, session, ...) {
  xlsx.ok <- requireNamespace("readxl", quietly = TRUE)

  empty.def <- data.frame(
    collection = character(), name = character(), feature_id = character(),
    stringsAsFactors = FALSE)

  state <- shiny::reactiveValues(
    dat = empty.def)

  shiny::observeEvent(input$upload, {
    type <- input$upload$type
    path <- input$upload$datapath
    ext <- tolower(tools::file_ext(path))

    if (ext %in% c("xlsx", "xls")) {
      shiny::validate(
        shiny::need(xlsx.ok, "readxl package required to upload Excel files"))
    } else {
      shiny::validate(
        shiny::need(
          ext == "csv",
          "Only CSV of Excel files supported"))
    }

    if (ext == "csv") {
      dat <- try(utils::read.csv(path, stringsAsFactors = FALSE), silent = TRUE)
    } else {
      dat <- try(readxl::read_excel(path), silent = TRUE)
    }

    shiny::validate(
      shiny::need(is.data.frame(dat), "Error parsing geneset definition file")
    )

    req.cols <- c("collection", "name", "feature_id")
    missed <- setdiff(req.cols, colnames(dat))
    shiny::validate(
      shiny::need(
        length(missed) == 0L,
        sprintf("Missing columns: %s", paste(missed, collapse = ","))))
    dat[["name"]] <- as.character(dat[["name"]])
    dat[["colelction"]] <- as.character(dat[["collection"]])
    dat[["feature_id"]] <- as.character(dat[["feature_id"]])
    state$dat <- dat
  })

  gdb <- shiny::reactive({
    dat. <- state$dat
    if (is.null(dat.) || nrow(dat.) == 0) NULL else sparrow::GeneSetDb(dat.)
  })

  shiny::observe({
    shinyjs::toggleState("rm", condition = is(gdb(), "GeneSetDb"))
  })

  shiny::observeEvent(input$rm, {
    state$dat <- empty.def
  })

  vals <- list(
    gdb = gdb,
    .ns = session$ns)
  class(vals) <- "ReactiveGeneSetDb"
  vals
}

#' @describeIn userDefinedGeneSetDb ui for the module
#' @export
#' @param id the module namespace
#' @param ... pass through arguments
userDefinedGeneSetDbUI <- function(id, ...) {
  ns <- shiny::NS(id)
  inline.style <- "display: inline-block; vertical-align:top;"

  shiny::tagList(
    shiny::tags$div(
      style = inline.style,
      shiny::fileInput(
        ns("upload"), "Additional Gene Set Definition File",
        multiple = FALSE,
        accept = c(
          # --- CSV stuff ---
          "text/csv", ".csv",
          "text/comma-separated-values,text/plain",
          # --- xlsx stuff (not .xls) ---
          "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
          ".xlsx"))),
    shiny::tags$div(
      style = paste(inline.style, "margin-top: 1.8em"),
      shiny::actionButton(ns("rm"), "Reset")))
}
