#' A module to explore the GSEA statistics generated from a sparrow::seas Run.
#'
#' @description
#' The UI is a DT::datatable of GSEA statistics for the selected method. The
#' module returns a list with the following reactive elements:
#'
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
#'
#' You probably want to `observeEvent(this$selected)` in your
#' `server.R` (or similar) so you can react to user clicks on different
#' gene sets
#'
#' @rdname mgTableBrowserModule
#' @export
#' @param id the shiny id of the UI module
mgTableBrowserUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::uiOutput(ns("resultTableMessage")),
    DT::dataTableOutput(ns("gseaResultTable")))
}

#' @rdname mgTableBrowserModule
#'
#' @export
#' @param input shiny server input object
#' @param output shiny server output object
#' @param session shiny server session object
#' @param mgc the \code{SparrowResultContainer} object
#' @param method a reactive that determines which method to explore from this
#'   result
#' @param fdr a reactive that gives the fdr threshold to filter results in the
#'   table by.
mgTableBrowser <- function(input, output, session, mgc, method, fdr,
                           server=TRUE) {

  ## under the FDR threshold
  gsea.result.table <- shiny::reactive({
    mg <- shiny::req(mgc()$mg)
    if (is.null(method()) || method() == "") {
      # msg("... gseaMethod not selected yet")
      return(NULL)
    }
    ## MultiGSEResult object, method, and FDR thersholds all set, now fetch
    ## the data that corresponds to this criteria
    constructGseaResultTable(mg, method(), fdr())
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
    shiny::req(gsea.result.table(), mgc())
    renderGseaResultTableDataTable(gsea.result.table(), method(),
                                   mgc()$mg)
  }, server = server)

  list(stats = gsea.result.table, selected = selected)
}
