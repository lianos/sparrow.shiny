#' A gene set selector that enables users to pre-filter universe to choose from
#'
#' This provides the user to pick a single gene set from a GeneSetDb collection.
#' To help the user find the gene set, the user can select which collections
#' and gene set sizes that serve as the universe of potential gene sets to
#' pick from in a GeneSetDb.
#'
#' @export
#' @param input,output,session shiny module bits
#' @param gdb A [reactiveGeneSetDb()] module, or `NULL`
#' @param ... pass through args to internal modules
#' @return A list of geneset info and membersihp
#' @examples
#' sres <- sparrow::exampleSparrowResult()
#' gdb <- sparrow::geneSetDb(sres)
#' app <- shiny::shinyApp(
#'   ui = shiny::shinyUI(shiny::fluidPage(
#'     exampleUISetup(),
#'     title = "Gene set selctor",
#'     reactiveGeneSetSelectUI("mod"))),
#'   server = function(input, output, session) {
#'     shiny::callModule(reactiveGeneSetSelect, "mod", gdb)
#'   })
#' if (interactive()) {
#'   shiny::runApp(app)
#' }
reactiveGeneSetSelect <- function(input, output, session, 
                                  gdb = shiny::reactive(NULL), ...) {
  assert_multi_class(gdb, c("reactive", "GeneSetDb"), null.ok = TRUE)

  state <- shiny::reactiveValues(
    collection = NULL,
    name = NULL)

  if (is.null(gdb)) {
    out <- list(
      collection = shiny::reactive(NULL),
      name = shiny::reactive(NULL),
      membership = shiny::reactive(data.frame(feature_id = character())))
    return(out)
  }

  rgdb <- shiny::callModule(reactiveGeneSetDb, "gdb", gdb, ...)

  shiny::observe({
    shiny::req(initialized(rgdb))
    gsets <- rgdb$geneSets()
    collections <- unique(gsets$collection)
    choices <- sapply(collections, function(coll) {
      gnames <- subset(gsets, collection == coll)$name
      key <- paste(coll, gnames, sep = ";;")
      names(key) <- gnames
      if (length(key) == 1L) list(key) else key
    }, simplify = FALSE)
    choices <- c(list("---"), choices)
    shiny::updateSelectizeInput(session, "geneset", choices = choices,
                                selected = NULL, server = TRUE)
  })

  shiny::observeEvent(input$geneset, {
    if (unselected(input$geneset)) {
      collection <- ""
      name <- ""
    } else {
      info <- strsplit(input$geneset, ";;")[[1]]
      collection <- info[1L]
      name <- info[2L]
    }
    if (!isTRUE(state$collection == collection)) {
      state$collection <- collection
    }
    if (!isTRUE(state$name == name)) {
      state$name <- name
    }
  })

  gscoll <- shiny::reactive(state$collection)
  gsname <- shiny::reactive(state$name)

  # membership <- shiny::reactive({
  #   shiny::req(initialized(rgdb))
  #   gdb. <- rgdb$gdb()
  #   coll <- gscoll()
  #   name <- gsname()
  #   if (!unselected(coll) && !unselected(name)) {
  #     out <- sparrow::geneSet(gdb., collection = coll, name = name)
  #     out <- out[, "feature_id", drop = FALSE]
  #   } else {
  #     out <- data.frame(feature_id = character())
  #   }
  #   out
  # })

  membership <- shiny::reactive({
    out <- data.frame(feature_id = character())
    if (initialized(rgdb)) {
      gdb. <- rgdb$gdb()
      coll <- gscoll()
      name <- gsname()
      if (!unselected(coll) && !unselected(name)) {
        out <- tryCatch({
          gs <- sparrow::geneSet(gdb., collection = coll, name = name)
          gs[, "feature_id", drop = FALSE]
        }, error = function(e) out)
      }
    }
    out
  })
  
  list(
    collection = gscoll,
    name = gsname,
    membership = membership)
}

#' @describeIn reactiveGeneSetSelect The UI function
#' @param id the id/namespace for the module
#' @param label passed to [shiny::selectizeInput()]
#' @param dropdown_width the width of the dropdown element in pixels
#' @param ... pass through (not used)
#' @export
reactiveGeneSetSelectUI <- function(id, label = NULL, dropdown_width = "350px",
                                    ...) {
  ns <- shiny::NS(id)
  shiny::fluidRow(
    shiny::column(
      9,
      shiny::selectizeInput(ns("geneset"), label = label, choices = NULL,
                            multiple = FALSE)),
    shiny::column(
      3,
      shinyWidgets::dropdown(
        inputId = ns("opts"),
        icon = shiny::icon("sliders"),
        status = "primary",
        with = dropdown_width,
        shiny::tags$div(
          id = ns("genesetdbconfig"),
          shiny::tags$h4("Gene Set Selection"),
          reactiveGeneSetDbUI(ns("gdb")))))
  )
}
