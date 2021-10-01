#' A module to show a tabular view of genesets that contain genes of interest.
#'
#' Users can look up which gene sets include query genes of interest. This is
#' useful when you are exploring geneset hits and find esoteric results there.
#' You can select some of the "more extreme" genes in that geneset, for instance
#' to see if they belong to another one that makes more sense to you given the
#' biological context of your experiment.
#'
#' @export
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

  # Return the selected geneset
  shiny::reactive({
    idx <- input$other_genesets_row_last_clicked
    if (!is.null(idx)) {
      others <- genesets()
      xcol <- as.character(others$collection[idx])
      xname <- as.character(others$name[idx])
      selected <- paste(xcol, xname, sep='_::_')
      msg("Selected: ", selected)
    } else {
      selected <- NULL
    }
    list(others=genesets, selected=selected)
  })
}


#' Module that displays gene sets related to (by membership) a set of genes.
#' @export
#' @rdname mgGeneSetSummaryByGene
mgGeneSetSummaryByGeneUI <- function(id, mg=NULL) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::checkboxInput(ns('genesets_sigonly'),
                         'Show membership for significant gene sets only',
                         value=TRUE, width="100%"),
    shiny::uiOutput(ns('selected_message')),
    DT::dataTableOutput(ns("other_genesets")))
}


