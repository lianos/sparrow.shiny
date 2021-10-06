#' A module to show a tabular view of genesets that contain genes of interest.
#'
#' This provides a shiny module over the [sparrow::geneSetSummaryByGenes()]
#' functionality. It accepts a SparrowResult and a feature query and will
#' enumerate the other genesets in the SparrowResult that include those genes.
#'
#' This is useful when you are exploring GSEA hits and find esoteric results
#' there. You can select some of the "more extreme" genes in that geneset, for
#' instance to see if they belong to another one that makes more sense to you
#' given the biological context of your experiment.
#'
#' @export
#' @param input,output,session shiny bits
#' @param mgc A [SparrowResultContainer()]
#' @param features a character vector of feature id's to query
#' @param method,fdr the GSEA method and FDR threshold used to filter the
#'   returned gene sets against. Gene sets with features found in `features`
#'   who don't make the `fdr` cutoff under the specific GSEA `method` will
#'   not be returned
#' @return a reactive list that contains the following reactives
#' \describe{
#'   \item{$others}{
#'     the result from [sparrow::geneSetSummaryByGenes()] given the query
#'     params}
#'   \item{$selected}{the key of the user-selected geneset from the table}
#' }
mgGeneSetSummaryByGene <- function(input, output, session, mgc,
                                   features, method, fdr) {
  genesets <- shiny::reactive({
    fids <- shiny::req(features())
    mg <- shiny::req(mgc()$mg)

    if (input$genesets_sigonly) {
      method <- method()
      max.p <- fdr()
    } else {
      method <- NULL
      max.p <- NULL
    }

    if (is(fids, 'data.frame')) {
      fids <- fids$feature_id
    }

    mg.fids <- intersect(fids, sparrow::featureIds(mg))
    if (length(mg.fids)) {
      out <- sparrow::geneSetSummaryByGenes(
        mg, mg.fids, feature.rename = 'symbol',
        method = method, max.p = max.p, as.dt = TRUE)
      out <- out[order(n, decreasing=TRUE)]
    } else {
      out <- NULL
    }
    out
  })

  output$selected_message <- shiny::renderUI({
    fids <- shiny::req(features())
    if (is.null(fids)) {
      n <- 0L
      ngs <- 0L
    } else {
      n <- if (is.vector(fids)) length(fids) else nrow(fids)
      gs <- genesets()
      if (is.null(gs)) {
        ngs <- 0L
      } else {
        ngs <- nrow(genesets())
      }
    }
    shiny::tags$p(sprintf('%d features selected across %d genesets', n, ngs))
  })

  # silence R CMD check notes from data.table NSE mojo
  collection <- active <- name <- NULL
  output$other_genesets <- DT::renderDataTable({
    out <- copy(shiny::req(genesets()))
    mg <- shiny::req(mgc()$mg)
    out[, collection := factor(collection)]
    out[, active := NULL]
    out[, name := {
      url <- sparrow::geneSetURL(mg, as.character(collection), name)
      xname <- gsub('_', ' ', name)
      html <- '<a href="%s" target="_blank">%s</a>'
      ifelse(is.na(url), xname, sprintf(html, url, xname))
    }]

    out <- round.dt(out)
    DT::datatable(setDF(out), filter='top', escape=FALSE,
                  selection=list(mode='single', selected=NA, target='row'),
                  rownames=FALSE, colnames=c("FDR"="padj"))
  })

  # the selected geneset
  selected <- shiny::reactive({
    idx <- input$other_genesets_row_last_clicked
    if (!is.null(idx)) {
      others <- genesets()
      xcol <- as.character(others$collection[idx])
      xname <- as.character(others$name[idx])
      sel <- paste(xcol, xname, sep='_::_')
      sparrow::msg("Selected: ", selected)
    } else {
      sel <- NULL
    }
    sel
  })


  # Reactives to return
  list(others = genesets, selected = selected)
}


#' @describeIn mgGeneSetSummaryByGene the UI for the module
#' @export
#' @param id the namespace for the UI element
mgGeneSetSummaryByGeneUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::checkboxInput(ns('genesets_sigonly'),
                         'Show membership for significant gene sets only',
                         value=TRUE, width="100%"),
    shiny::uiOutput(ns('selected_message')),
    DT::dataTableOutput(ns("other_genesets")))
}


