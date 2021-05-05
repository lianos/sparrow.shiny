#' A geneset selector for a ReactiveGeneSetDb
#'
#' This module is unique becauase if you pass `gdb = NULL` it returns a shim
#' of iteself that will never trigger reactivity.
#'
#' @export
#' @importFrom shiny observeEvent
#' @param gdb A [reactiveGeneSetDb()] module, or `NULL`
#' @return A list of geneset info and membersihp
reactiveGeneSetSelect <- function(input, output, session, gdb = NULL, ...) {
  assert_multi_class(gdb, c("reactive", "GeneSetDb"), null.ok = TRUE)

  state <- shiny::reactiveValues(
    collection = NULL,
    name = NULL)

  if (is.null(gdb)) {
    out <- list(
      collection = reactive(NULL),
      name = reactive(NULL),
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

  membership <- shiny::reactive({
    shiny::req(initialized(rgdb))
    gdb. <- rgdb$gdb()
    coll <- gscoll()
    name <- gsname()
    if (!unselected(coll) && !unselected(name)) {
      out <- sparrow::geneSet(gdb., collection = coll, name = name)
      out <- out[, "feature_id", drop = FALSE]
    } else {
      out <- data.frame(feature_id = character())
    }
    out
  })

  list(
    collection = gscoll,
    name = gsname,
    membership = membership)
}

#' @noRd
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
          reactiveGeneSetDbFilterUI(ns("gdb")))))
  )
}
