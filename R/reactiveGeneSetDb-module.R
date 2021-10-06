
#' A ReactiveGeneSetDb for use in the shiiny world.
#'
#' This can be instantiated from a "static" or "reactive" GeneSetDb object.
#' It allows users to customize which genesets are active by:
#'
#' 1. Filtering out entire collections; and
#' 2. Filtering genesets based on min and max (gene) size.
#'
#' @export
#' @param input,output,session shiny module bits
#' @param gdb A static or reactive GeneSetDb object
#' @param min.gs.size,max.gs.size the default minimum and maximum geneset size
#'   set in the UI when `gdb` is first loaded or changes (when reactive)
#' @param default_collections a character vector of collections that are by
#'   default selected for use
#' @return A list of reactive elements wired to the input `gdb`:
#' \describe{
#'   \item{gdb}{a `reactive(gdb)`}
#'   \item{geneSets}{a `reacvtive(geneSets(gdb))`}
#'   \item{min.gs.size,max.gs.size}{
#'     reactives that indicate current selection of minimum and maximum gene set
#'     sizes to be used in `sparrow::conform(gdb, ...)`
#'   }
#'   \item{.state}{a `shiny::reactiveList` that contains the state of this module}
#'   \item{.ns}{the shiny namespace for this module}
#' }
reactiveGeneSetDb <- function(input, output, session, gdb,
                              min.gs.size = 2L, max.gs.size = Inf,
                              default_collections = NULL, ...) {
  assert_character(default_collections, null.ok = TRUE)
  min.gs.size <- assert_number(round(min.gs.size), lower = 2L, null.ok = FALSE)
  max.gs.size <- assert_number(round(max.gs.size), lower = 2L, null.ok = TRUE)

  state <- shiny::reactiveValues(
    gdb = NULL,
    min.gs.size = min.gs.size,
    max.gs.size = max.gs.size)

  rgdb <- shiny::reactive({
    gdb. <- if (is(gdb, "reactive")) gdb() else gdb
    assert_class(gdb., "GeneSetDb")
    gdb.
  })

  shiny::observe({
    gs.range <- input$size
    if (state$min.gs.size != gs.range[1L]) state$min.gs.size <- gs.range[1L]
    if (state$min.gs.size != gs.range[2L]) state$max.gs.size <- gs.range[2L]
  })

  rmin.gs.size <- shiny::reactive(state$min.gs.size)
  rmax.gs.size <- shiny::reactive(state$max.gs.size)

  shiny::observeEvent(rgdb(), {
    gdb. <- shiny::req(rgdb())
    shiny::req(is(gdb., "GeneSetDb"))

    # ........................................................ collection picker
    gsets <- sparrow::geneSets(gdb.)
    min.n <- min(gsets$N)
    max.n <- max(gsets$N)
    colls.all <- unique(gsets$collection)
    colls.selected <- intersect(colls.all, default_collections)

    if (length(colls.selected) == 0L) {
      colls.selected <- colls.all
    }

    gs.count <- table(gsets$collection)
    names(colls.all) <- sprintf("%s [%d total gene sets]", colls.all,
                                gs.count[colls.all])
    shinyWidgets::updatePickerInput(
      session,
      "collections",
      choices = colls.all,
      selected = colls.selected)

    # .............................................................. size slider
    if (!is.null(min.gs.size)) {
      min.val <- max(min.gs.size, min.n)
    } else {
      min.val <- min.n
    }
    if (!is.null(max.gs.size)) {
      max.val <- min(max.gs.size, max.n)
    } else {
      max.val <- max.n
    }
    state$min.gs.size <- min.val
    state$max.gs.size <- max.val
    shiny::updateSliderInput(session, "size", min = min.n, max = max.n,
                      value = c(min.n, max.n))
  })

  genesets <- shiny::reactive({
    selected.colls <- input$collections
    gdb. <- shiny::req(rgdb())
    shiny::req(is(gdb., "GeneSetDb"))
    gsets <- sparrow::geneSets(gdb., as.dt = TRUE)
    # silence R CMD check NOTES for data.table NSE mojo
    collection <- N <- NULL
    gsets <- gsets[
      collection %in% selected.colls &
        N >= rmin.gs.size() &
        N <= rmax.gs.size()]
    setDF(gsets)
  })

  output$gscount <- shiny::renderUI({
    shiny::tags$span(nrow(genesets()))
  })

  vals <- list(
    gdb = rgdb,
    geneSets = genesets,
    min.gs.size = rmin.gs.size,
    max.gs.size = rmax.gs.size,
    .state = state,
    .ns = session$ns)
  class(vals) <- c("ReactiveGeneSetDb")
  vals
}

#' @describeIn reactiveGeneSetDb the UI for the module
#' @param id shiny module namespace
#' @param min,max ranges for the min/max geneset slider
#' @param ... pass through args
reactiveGeneSetDbFilterUI <- function(id, min = 2, max = 100L, ...) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::tags$p(
      shiny::tags$span("Gene Sets Selected:",
                       style = "font-weight: bold; color: #FF7F00"),
      shiny::uiOutput(ns("gscount"), inline = TRUE)),
    shiny::sliderInput(ns("size"), "Set Size", min = min, max = max,
                       value = c(min, max)),
    shinyWidgets::pickerInput(
      ns("collections"),
      "Collections",
      choices = NULL,
      multiple = TRUE,
      options = list(
        `selected-text-format`= "count",
        `count-selected-text` = "{0} collections chosen"
      )))
}

#' Returns the filtered unreactive GeneSetDb from the reactiveGeneSetDb module.
#'
#' The user can choose to include subsets of collections, as well as
#' genesets of a certain size.
#'
#' This can only be run within a reactive context.
#'
#' @export
#' @param x A ReactiveGeneSetDb
#' @param ... pass through (not used)
#' @return A standard / static GeneSetDb object
GeneSetDb.ReactiveGeneSetDb <- function(x, ...) {
  gdb <- x$gdb()
  gsets.all <- sparrow::geneSets(gdb)
  gsets.selected <- x$geneSets()

  keep <- is.element(
    paste(gsets.all$collection, gsets.all$name),
    paste(gsets.selected$collection, gsets.selected$name))

  if (!all(keep)) {
    gdb <- gdb[keep]
  }

  gdb
}
