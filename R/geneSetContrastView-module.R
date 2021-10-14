#' A module to encapsulate browsing differential statistics of a geneset.
#'
#' The module is meant to be displayed in "a box" so that the user can examine
#' the coherent (or not) behavior of the geneset across a contrast with respect
#' to the background distribution of all genes in the contrast.
#'
#' Embedded within this module is the [geneSetSelect()] module, which
#' provides the list of genesets the user can examine, as well as the title of
#' the current geneset under scrutiny.
#'
#' Below the geneSet picker, we embed an [iplot()] view of the result so the
#' user can observe the behavior of the geneset across the contrast. The user
#' can pick the type of plot to show (density or boxplot) as well as which
#' statistics to use for plotting (logFC or t-statistics).
#'
#' An `updateActiveGeneSetInContrastView` function is provided to enable
#' interactions external to this module the ability to update the geneset
#' selected in the [geneSetSelect()] module.
#'
#' @export
#' @inheritParams geneSetSelect
#' @param feature.link.fn a function that takes a name of a column and returns
#'   a url linked version of the element that will provide more info for the
#'   particular feature
#' @param feature_table_filter filter param for [renderFeatureStatsDataTable()]
#' @param itools the plotly tools to show in the geneset contrast visual
#' @return returns a list of reactive elements:
#' \describe{
#'   \item{gs}{the [geneSetSelect()] module}
#'   \item{selected}{
#'     a character vector of feature_ids currently brushed in the contrast view
#'   }
#' }
#' @examples
#' sres <- sparrow::exampleSparrowResult()
#' app <- shiny::shinyApp(
#'   ui = shiny::shinyUI(shiny::fluidPage(
#'     exampleUISetup(),
#'     title = "Gene Set Contrast View",
#'     geneSetContrastViewUI("mod"))),
#'   server = function(input, output, session) {
#'     src <- shiny::reactive(SparrowResultContainer(sres))
#'     shiny::callModule(geneSetContrastView, "mod", src)
#'   })
#' if (interactive()) {
#'   shiny::runApp(app)
#' }
geneSetContrastView <- function(input, output, session, src,
                                server=TRUE, maxOptions=Inf, sep="_::_",
                                feature.link.fn=ncbi.entrez.link,
                                feature_table_filter = "none",
                                itools=c('wheel_zoom', 'box_select', 'reset', 'save')) {
  gs <- shiny::callModule(
    geneSetSelect, 'gs_select', src, server = server,
    maxOptions = maxOptions, sep = sep)

  plt <- shiny::reactive({
    coll <- shiny::req(gs()$collection)
    name <- shiny::req(gs()$name)
    ns <- session$ns
    shinyjs::js$reset_gs_viz_selected()
    sparrow::iplot(src()$mg, collection = coll, name = name,
                   value = input$gs_viz_stat,
                   type = input$gs_viz_type,
                   tools = itools,
                   main = NULL, with.legend = FALSE, with.data = TRUE,
                   shiny_source = 'gs_viz', width = 350, height = 350)

  })

  selected_features <- shiny::reactive({
    event <- plotly::event_data('plotly_selected', source='gs_viz')
    if (!is.null(event)) {
      out <- event$key
    } else {
      out <- character()
    }
    out
  })

  output$gs_viz <- plotly::renderPlotly({
    shiny::req(plt())
  })

  # shiny::outputOptions(output, "gs_viz", suspendWhenHidden=FALSE)

  output$gs_members <- DT::renderDataTable({
    shiny::req(gs())
    gs.stats <- shiny::req(gs()$stats)
    if (!is(gs.stats, 'data.table')) {
      shiny::req(NULL)
    }
    renderFeatureStatsDataTable(gs.stats, feature.link.fn=feature.link.fn,
                                filter = feature_table_filter)
  }, server = server)

  output$gs_gene_table <- shiny::downloadHandler(
    filename = function() {
      sprintf('sparrow-gene-statistics-%s_%s.csv', gs()$collection, gs()$name)
    },
    content = function(file) {
      utils::write.csv(gs()$stats, file, row.names = FALSE)
    }
  )

  shiny::outputOptions(output, "gs_gene_table", suspendWhenHidden=FALSE)

  out <- list(gs = gs, selected = selected_features)
  class(out) <- c("geneSetContrastView", class(out))
  out
}

#' @describeIn geneSetContrastView the UI for the module
#' @export
#' @param id the shiny id of the module
#' @param height,width the height and width of the module
#' @return A `tagList` of html stuff to dump into the UI.
geneSetContrastViewUI <- function(id, height="590px", width="400px") {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::tags$div(
      # class="gadget-container", style=paste("height:", height),
      class="gadget-container",
      style=sprintf("height: %s; width %s;", height, width),
      shiny::tags$div(
        style="padding: 0 5px 0 5px",
        geneSetSelectUI(ns("gs_select"), "Select Gene Set")),
     miniUI::miniTabstripPanel(
        miniUI::miniTabPanel(
          "Visualize",
          # shiny::icon("area-chart"), # deprecated in font awesome 5
          icon = shiny::icon("chart-area"),
          miniUI::miniContentPanel(
            plotly::plotlyOutput(ns("gs_viz"), height="350px"),
            # call with js$reset_gs_viz_selected()
            insertPlotlyReset('gs_viz', 'selected'),
            shiny::fluidRow(
              shiny::column(
                8,
                shiny::selectInput(ns("gs_viz_type"), NULL,
                                   c('boxplot', 'density'), 'density')),
              shiny::column(
                4,
                shiny::selectInput(ns("gs_viz_stat"), NULL,
                                   c('logFC'='logFC', 't-statistic'='t'),
                                   'logFC'))
            )
          )
        ), ## Viz miniTabPanel
        miniUI::miniTabPanel(
          "Genes", icon = shiny::icon("table"),
          miniUI::miniContentPanel(
            DT::dataTableOutput(ns("gs_members")),
            shiny::downloadButton(ns("gs_gene_table"), 'Download'))
        ) ## Members Table miniTabPanel
      ) ## miniTabstripPanel
    ) ## div.gadget-container
  ) ## tagList
}



#' @export
#' @describeIn geneSetContrastView allows you to update the active
#'   geneset in a contrast view module externally.
#' @param session the shiny session
#' @param viewer a `geneSetContrastViewModule`
#' @param geneset the key of a geneset to select
#' @param src a `SparrowResultContainer`
updateActiveGeneSetInContrastView <- function(session, viewer, geneset, src) {
  stopifnot(is(src, 'SparrowResultContainer'))
  stopifnot(is(viewer, "geneSetContrastView"))
  shiny::withReactiveDomain(session, {
    # id <- req(viewer()$gs()$select.id)
    # 2016-12-23
    # Hack to enable this to work within arbitray module nesting levels.
    # This might not be necessary, but it seems like it was because if this
    # module is called from another module, then the `session` object is somehow
    # dispatching its IDs with as many module prefixes as it is being called
    # down from the stack. Since we are calling the geneSet-select module using
    # its global ID, we need to strip out the prefixes that are already assumed
    # to be working here. (also, I doubt this paragraph will make sense when I
    # read it in a few months)
    modname <- sub('-test$', '', session$ns('test'))
    id <- shiny::req(viewer$gs()$select.id)
    id <- sub(paste0(modname, '-'), '', id)
    shiny::updateSelectizeInput(
      session, id, choices = src$choices, selected = geneset, server = TRUE)
  })
}
