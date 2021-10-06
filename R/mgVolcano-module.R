#' A module to visualize a volcano plot.
#'
#' Users can specify logFC and FDR thresholds for which the individual dots
#' in the volcano are rather hexbins so that the browser doesn't have to handle
#' a single point for each gene/dot.
#'
#' @export
#' @param input,output,session shiny bits
#' @inheritParams sparrow::volcanoPlot
#' @param tools the plotly tools to include in the figures toolbar
#' @param highlight a reactive vector of featureIds that indicate which points
#'   to highlight on the volcano, irresespective of their "hexbin" status.
#' @param default_xhex,default_yhex default values to set the slider inputs
#'   to pass in for hex-ization
#' @param webgl use webgl to make the plotly plot
#' @return A reactive data.frame that includes the information of the genes
#'   that are brushed in the volcano plot. If no genes have been selected, then
#'   `NULL`.
mgVolcano <- function(input, output, session,
                      x, stats='dge', xaxis='logFC', yaxis='pval', idx='idx',
                      tools=c('box_select', 'reset', 'save'),
                      width=NULL, height=NULL, highlight=reactive(NULL),
                      default_xhex=1, default_yhex=0.10, webgl = FALSE, ...) {
  shinyjs::onclick("settings", shinyjs::toggle(id = "widgets", anim = TRUE))
  if (missing(idx)) {
    if (stats == 'dge') idx <- 'feature_id'
  }

  # Extract the data used in the volcano to keep it handy
  dat <- shiny::reactive({
    xx <- shiny::req(x())
    sparrow::volcanoStatsTable(xx, stats, xaxis, yaxis, idx)
  })

  # If UI is showing the hexbin sliders, fix ranges and labels when dat()
  # is initialized
  shiny::observeEvent(dat(), {
    if (!is.null(input$xhex)) {
      shiny::updateSliderInput(session, 'yhex', sprintf('%s filter', yaxis),
                               min=0, max=1, step=0.025, value=default_yhex)
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

  # This module returns a data.frame containing information of the genes in
  # the volcano plot that have been brushed by the user.
  vals <- shiny::reactive({
    dat <- shiny::req(plt())
    dat <- plotly::plotly_data(dat)
    event <- plotly::event_data('plotly_selected', source='mgvolcano')
    if (!is.null(event)) {
      out <- dat[dat$feature_id %in% event$key,,drop = FALSE]
    } else {
      out <- NULL
    }
    out
  })

  return(vals)
}

#' @export
#' @describeIn mgVolcano the UI for the module
#' @param id the shiny id of the output widget
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

