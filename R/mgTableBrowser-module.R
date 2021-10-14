#' Displays a an interactive table of GSEA statistics from an analysis.
#'
#' This module lists all of the genesets from an analysis that are significant
#' given an `fdr` and `method` of analysis. Row selections trigger a shiny
#' event that broadcasts the key of the geneset that was selected. You would
#' then want to `observeEvent(this$selected)` in your `server.R` (or similar)
#' so you can react to the gene set selection the user triggers.
#'
#' @export
#' @param input,output,session shiny module bits
#' @param src the \code{SparrowResultContainer} object
#' @param method a reactive that determines which method to explore from this
#'   result
#' @param fdr a reactive that gives the fdr threshold to filter results in the
#'   table by.
#' @param server boolean passed to [DT::renderDataTable()] (default: `TRUE`).
#' @return a list with reactives:
#' \describe{
#'   \item{$stats}{
#'     The table of gene sets and their statistics that pass the prescribed
#'     \code{fdr} thershold
#'   }
#'   \item{$selected}{
#'     The geneset that is currently "active"/selected by the user. This
#'     is defined as \code{<collection>_::_<name>}
#'   }
#' }
#' @examples
#' sres <- sparrow::exampleSparrowResult()
#' app <- shiny::shinyApp(
#'   ui = shiny::shinyUI(shiny::fluidPage(
#'     exampleUISetup(),
#'     title = "GSEA Stats Table Browser Module",
#'     mgTableBrowserUI("mod"))),
#'   server = function(input, output, session) {
#'     src <- shiny::reactive(SparrowResultContainer(sres))
#'     method <- shiny::reactive("camera")
#'     fdr <- shiny::reactive(0.2)
#'     shiny::callModule(mgTableBrowser, "mod", src, method, fdr)
#'   })
#' if (interactive()) {
#'   shiny::runApp(app)
#' }
mgTableBrowser <- function(input, output, session, src, method, fdr,
                           server = TRUE) {

  ## under the FDR threshold
  gsea.result.table <- shiny::reactive({
    sr <- shiny::req(src()$sr)
    if (is.null(method()) || method() == "") {
      # msg("... gseaMethod not selected yet")
      return(NULL)
    }
    ## MultiGSEResult object, method, and FDR thersholds all set, now fetch
    ## the data that corresponds to this criteria
    constructGseaResultTable(sr, method(), fdr())
  })

  selected <- shiny::reactive({
    tbl <- shiny::req(gsea.result.table())
    idx <- input$gseaResultTable_row_last_clicked
    ## By defualt, if user doesn't click a row we will say that the first
    ## row is selected
    if (is.null(idx)) {
      idx <- 1L
    }
    xcol <- as.character(gsea.result.table()$collection[idx])
    xname <- as.character(gsea.result.table()$name[idx])
    paste(xcol, xname, sep='_::_')
  })

  output$resultTableMessage <- shiny::renderUI({
    gst <- shiny::req(gsea.result.table())
    if (!is(gst, 'data.frame')) {
      tmsg <- ''
    } else if (nrow(gst) == 0) {
      tmsg <- sprintf('No results at FDR cutoff of %.2f', fdr())
    } else {
      tmsg <- sprintf('Showing %d genesets at FDR cutoff of %.2f',
                      nrow(gst), fdr())
    }
    shiny::tags$h5(tmsg)
  })

  output$gseaResultTable <- DT::renderDataTable({
    shiny::req(gsea.result.table(), src())
    renderGseaResultTableDataTable(gsea.result.table(), method(),
                                   src()$sr)
  }, server = server)

  list(stats = gsea.result.table, selected = selected)
}

#' @describeIn mgTableBrowser The UI for the module.
#' @export
#' @param id the shiny id of the UI module
mgTableBrowserUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::uiOutput(ns("resultTableMessage")),
    DT::dataTableOutput(ns("gseaResultTable")))
}
