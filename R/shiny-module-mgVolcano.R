#' Creates a volcano plot from a source input
#'
#' @export
#' @rdname mgVolcano
#' @param id the shiny id of the output widget
#' @param x the object to build a volcano plot from
#' @param stats the stats to pull from \code{x} (if necessary) to build the
#'   volcano for.
#' @param x the name of the column from the stats table to use on x axis
#' @param y the name of the column from the stats table to use on y axis
mgVolcanoUI <- function(id, x, stats='dge', xaxis='logFC', yaxis='padj',
                        idx='idx', hexbin=TRUE, default_xhex=1,
                        default_yhex=0.10) {
  ns <- shiny::NS(id)
  if (hexbin) {
    out <- shiny::tagList(
      shinyjs::useShinyjs(),
      shiny::tags$a(id=ns('settings'), shiny::icon("wrench")),
      plotly::plotlyOutput(ns("plot")),
      shinyjs::hidden(
        shiny::tags$div(
          id=ns('widgets'),
          shiny::sliderInput(
            ns("xhex"), 'x filter', min=0, max=5, step=0.25, value=default_xhex),
          shiny::sliderInput(
            ns("yhex"), 'y filter', min=0, max=1, step=0.025, value=default_yhex))))
  } else {
    out <- plotly::plotlyOutput(ns("plot"))
  }
  out
}

#' @section Module Return:
#' This module returns a data.frame with the information of the selected
#' features. If no genes are selected in a volcano, it will return \code{NULL}.
#'
#' @rdname mgVolcano
#' @export
#' @param highlight a reactive vector of featureIds that indicate which points
#'   to highlight on the volcano, irresespective of their "hexbin" status.
mgVolcano <- function(input, output, session,
                      x, stats='dge', xaxis='logFC', yaxis='pval', idx='idx',
                      tools=c('box_select', 'reset', 'save'),
                      width=NULL, height=NULL, highlight=reactive(NULL),
                      default_xhex=1, default_yhex=0.10, webgl = FALSE, ...) {
  shinyjs::onclick("settings", toggle(id = "widgets", anim = TRUE))
  if (missing(idx)) {
    if (stats == 'dge') idx <- 'feature_id'
  }

  # Extract the data used in the volcano to keep it handy
  dat <- shiny::reactive({
    shiny::req(x())
    sparrow::volcanoStatsTable(x(), stats, xaxis, yaxis, idx)
  })

  # If UI is showing the hexbin sliders, fix ranges and labels when dat()
  # is initialized
  shiny::observeEvent(dat(), {
    if (!is.null(input$xhex)) {
      shiny::updateSliderInput(session, 'yhex', sprintf('%s filter', yaxis),
                               min=0, max=1, step=0.025, value=default_yhex)
      # max.x <- ceiling(max(abs(dat()[['xaxis']]))) - 0.5
      max.x <- ceiling(max(abs(dat()[['.xvt']]))) - 0.5
      shiny::updateSliderInput(session, 'xhex', sprintf('%s filter', xaxis),
                               min=0, max=max.x, step=0.25, value=default_xhex)
    }
  })

  plt <- shiny::reactive({
    shiny::req(x())
    ns <- session$ns
    xhex <- input$xhex
    yhex <- input$yhex
    p <- sparrow::volcanoPlot(
      x(), stats, xaxis, yaxis, idx, xhex = xhex, yhex = yhex,
      highlight = highlight(),
      tools = tools, shiny_source = 'mgvolcano',
      width = width, height = height)
    if (webgl) {
      p <- plotly::toWebGL(p)
    }

    p
  })

  output$plot <- plotly::renderPlotly({
    shiny::req(plt())
  })

  # This module returns a data.frame containing info genes that are brushed
  # by the user
  vals <- shiny::reactive({
    dat <- shiny::req(plt())
    dat <- plotly::plotly_data(dat)
    event <- plotly::event_data('plotly_selected', source='mgvolcano')
    if (!is.null(event)) {
      out <- subset(dat, feature_id %in% event$key)
    } else {
      out <- NULL
    }
    out
  })

  return(vals)
}
