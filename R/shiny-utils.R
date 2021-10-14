#' Adds a js$reset_<id>_<event>() to reset the selection on a plot
#'
#' selected elements stick on a plot even after it is redrawn, most of the
#' times you don't want to do that.
#'
#' See:
#' https://community.plot.ly/t/reseting-click-events/2718/2
#' https://stackoverflow.com/questions/44412382/
#'
#' @noRd
#' @param the name of the "shiny source" to add the reset for
#' @param event clear the selection on which event?
insertPlotlyReset <- function(source, event = c('hover', 'click', 'selected')) {
  assert_string(source)
  event <- match.arg(event)
  text <- sprintf(
    "shinyjs.reset_%s_%s = function() {
       var jsid = '.clientValue-plotly_%s-%s';
       // window.alert(jsid);
       Shiny.onInputChange(jsid, 'null'); }",
    source, event, event, source)
  shinyjs::extendShinyjs(
    text = text,
    functions = sprintf("reset_%s_%s", source, event))
}

# Gene Set Level Table Helpers =================================================

#' Builds the table of GSEA statistics to present to the user.
#'
#' Creates a table gene set statistics genereated from a GSEA `method` for all
#' gene sets with an FDR (by collection) less than given threshold.
#'
#' @export
#' @param mg `SparrowResult` object
#' @param method the method to show statistics for
#' @param fdr the FDR cut off to present statistics for
#' @param prioritize the preferred collections to put at the top of the
#'   list. The collection column of the table is turned into a factor for
#'   more useful display with datatable's filter. Specifcying collections
#'   here will put those collections at the front of the factor levels and
#'   therofre prioritize their display in the select dropdown for the filter
#' @return a data.table of the statistics that match the filtering criteria.
#'   A 0-row data.table is returned if nothing passes.
#' @examples
#' sres <- sparrow::exampleSparrowResult()
#' constructGseaResultTable(sres)
constructGseaResultTable <- function(mg, method = sparrow::resultNames(mg)[1L],
                                     fdr = 0.10, prioritize = NULL) {
  assert_character(prioritize, null.ok = TRUE)
  assert_number(fdr, lower = 0.01, upper = 1)

  # silence R CMD check notes from data.table NSE mojo
  padj.by.collection <- collection <- mean.logFC.trim <- NULL

  out <- sparrow::result(mg, method, as.dt=TRUE)
  out <- out[padj.by.collection <= fdr]
  if (nrow(out)) {
    colls <- sort(unique(out$collection))
    priority <- intersect(prioritize, colls)
    lvls <- c(priority, setdiff(colls, priority))
    out[, collection := factor(collection, lvls, ordered=TRUE)]
    out <- out[order(mean.logFC.trim, decreasing=TRUE)]
  }
  out
}

#' Creates a DT::datatable of geneset level GSEA results for use in shiny bits.
#'
#' This function is closely tied to the exact output of the GSEA methods from
#' the [sparrow::seas()].
#'
#' @export
#' @param x The set of GSEA statistics generated from from
#'   [constructGseaResultTable()]
#' @param method the GSEA method being used fo rdisplay
#' @param mg The `SparrowResult` object. This is used swap in the
#' URL links for genesets using [sparrow::geneSetURL()].
#' @param digits the number of digits to round numer columns to.
#' @return A [DT::datatable()] for display
#' @examples
#' sres <- sparrow::exampleSparrowResult()
#' method <- sparrow::resultNames(sres)[1L]
#' stable <- constructGseaResultTable(sres, method)
#' renderGseaResultTableDataTable(stable, method, sres, digits = 2)
renderGseaResultTableDataTable <- function(x, method, mg, digits = 3) {
  stopifnot(is(x, 'data.frame'))
  stopifnot(is.character(method) && length(method) == 1L)
  stopifnot(is(mg, 'SparrowResult'))

  x <- setDT(copy(x))

  rcols <- c('collection'='collection', 'name'='name', 'n'='n',
             ## 'padj'='padj', 'padj.by.collection'='padjByColl',
             'pval'='pval',
             'padj.by.collection'='FDR',
             'mean.logFC.trim'='logFC', 'n.sig.up'='nSigUp',
             'n.sig.down'='nSigDown', 'n.up'='nUp', 'n.down'='nDown')

  res <- x[, names(rcols), with=FALSE]
  setnames(res, names(rcols), rcols)

  # silence R CMD check notes from data.table NSE mojo
  name <- collection <- NULL
  res[, name := {
    url <- sparrow::geneSetURL(mg, as.character(collection), name)
    xname <- gsub('_', ' ', name)
    html <- '<a href="%s" target="_blank">%s</a>'
    ifelse(is.na(url), xname, sprintf(html, url, xname))
  }]

  dt.order <- list()
  lfc.col <- which(colnames(res) == 'logFC') - 1L
  dt.order[[length(dt.order) + 1L]] <- list(lfc.col, 'desc')

  length.opts <- c(10, 20, 50, 100, 250)
  length.opts <- length.opts[length.opts < nrow(res)]
  length.opts <- c(length.opts, nrow(res))

  dt.opts <- list(
    dom = 'ltpir',
    order = dt.order,
    scrollX = TRUE,
    pageLength = length.opts[1L],
    lengthMenu = length.opts)

  setDF(res)
  dtargs <- list(
    data = res, filter = 'top',
    selection = list(
      mode = 'single', selected = NA, target='row'),
    # extensions='Buttons',
    escape = FALSE, rownames = FALSE,
    options = dt.opts)
  out <- do.call(DT::datatable, dtargs)
  roundDT(out, digits = digits)
}

# Gene level Table Helpers =====================================================

#' Transform a column in feature table to web link for that feature.
#'
#' @description
#' When listing features in an interactive table, it's often useful to link
#' the feature to an external webpage that has more information about that
#' feature. We include a series of functions to link entrez or ensembl id's
#' to different web pages. The data.frame needs to have a `feature_id` column
#' which will be used to link out to the reference page.
#'
#' If `link.col` is not found in the data.frame `x` then the provided
#' functions are NO-OPS, ie. the same data.frame is simply returned.
#'
#' @details
#' These were implemented a while ago when our world focused on entrez id's,
#' we need to update.
#'
#' @rdname feature-link-functions
#' @examples
#' sr <- sparrow::exampleSparrowResult()
#' lfc <- sparrow::logFC(sr)
#' # the symbol column in `lfhc` will be a linked to the entrez web page for
#' # each gene
#' linked <- ncbi.entrez.link(lfc, link.col = "symbol")
#' head(linked[["symbol"]])
.empty_link_function <- function(x, link.col=NULL) x

#' @describeIn feature-link-functions links gene to ncbi web site by entrez id.
#' @export
#' @param x a data.frame from `SparrowResult`
#' @param link.col the column in `x` that should be transformed to a link
#' @return a modified `x` with an html link in `link.col`.
ncbi.entrez.link <- function(x, link.col='symbol') {
  assert_character(x[["feature_id"]])
  if (is.character(link.col) && is.character(x[[link.col]])) {
    url <- sprintf('https://www.ncbi.nlm.nih.gov/gene/?term=%s', x$feature_id)
    html <- sprintf('<a href="%s" target="_blank">%s</a>', url, x$symbol)
    x[[link.col]] <- html
  }
  x
}


#' @describeIn feature-link-functions links a gene row to the genecards website
#'   by entrez id
#' @export
genecards.entrez.link <- function(x, link.col='symbol') {
  assert_character(x[["feature_id"]])
  if (is.character(link.col) && is.character(x[[link.col]])) {
    url <- sprintf('http://www.genecards.org/cgi-bin/carddisp.pl?gene=%s',
                   x$feature_id)
    html <- sprintf('<a href="%s" target="_blank">%s</a>', url, x$symbol)
    x[[link.col]] <- html
  }
  x
}

#' Create a DT::datatable of feature level statistics for use in shiny bits
#'
#' We often want to display an interactive table of feature level statistics
#' for all features. This function is a convenience wrapper to do that.
#'
#' @export
#' @param x A `SparrowResult` or `data.frame` of feature level
#'   statistics. When `x` is a `SparrowResult`, the
#'   `logFC` feature level statistics will be extracted for
#'   display.
#' @param features A character vector that specifies the subset of
#'   `feature_id`'s to display from `x`. If `NULL` (default),
#'   all of `x` will be used.
#' @param digits number of digits to round the numeric columns to
#' @param columns the columns from `x` to use. If `missing`, then
#'   only `c('symbol', 'feature_id', 'logFC', 'pval', 'padj', order.by)`
#'   will be used. If explicitly set to `NULL` all columns will be used.
#' @param feature.link.fn A function that receives the data.frame of statistics
#'   to be rendered and transforms one of its columns into a hyperlink for
#'   further reference. Refer to [ncbi.entrez.link()] as an example.
#' @param order.by name of the column to order the geneset stats by
#' @param order.dir do we order the table by the values in `order.by` in
#'   `"desc"`-ending, or `"asc"`-ending order (default: `"desc"`)
#' @param filter passed to [DT::datatable()]
#' @param length.opts parameter passed as an option to [DT::datatable()] that
#'   specifies how long the table can be per page.
#' @return A [DT::datatable()] of feature statistics
#' @examples
#' sr <- sparrow::exampleSparrowResult()
#' lfc <- sparrow::logFC(sr)
#' if (interactive()) {
#'   renderFeatureStatsDataTable(sr, sample(lfc$feature_id, 200),
#'                               feature.link.fn = ncbi.entrez.link)
#' }
renderFeatureStatsDataTable <- function(x, features=NULL, digits=3,
                                        columns=NULL, feature.link.fn=NULL,
                                        order.by='logFC',
                                        order.dir=c('desc', 'asc'),
                                        filter='none',
                                        length.opts=c(10, 25, 50, 100, 250)) {
  if (is(x, 'SparrowResult')) {
    x <- copy(sparrow::logFC(x, as.dt = TRUE))
  }
  stopifnot(is(x, 'data.table'), !is.null(x$feature_id))
  order.dir <- match.arg(order.dir)
  if (is.character(features)) {
    x <- x[x$feature_id %in% features,,drop = FALSE]
  }

  ## Figure out what columns to keep in the outgoing datatable
  if (missing(columns)) {
    # at this point, the "name" column is the geneset name

    # support feature_id or feature_id columns # dataframe-refactor
    fid <- intersect(c("feature_id", "feature_id"), colnames(x))[1]
    if (is.na(fid)) fid <- character()
    columns <- c('symbol', fid, 'logFC', 'pval', 'padj', 'F', 't')
    columns <- intersect(columns, colnames(x))
  } else if (is.null(columns)) {
    columns <- colnames(x)
  }
  if (!is.null(order.by)) {
    stopifnot(is.character(order.by),
              length(order.by) == 1L,
              !is.null(x[[order.by]]))
    columns <- c(columns, order.by)
  }
  bad.cols <- setdiff(columns, colnames(x))
  if (length(bad.cols)) {
    warning("The following columns not found: ", paste(bad.cols, collapse=','))
  }
  columns <- intersect(columns, colnames(x))
  x <- x[, columns, with=FALSE]

  # remove all NA columns # dataframe-refactor
  for (cname in columns) {
    if (all(is.na(x[[cname]]))) x[, (cname) := NULL]
  }

  if (is.function(feature.link.fn)) {
    x <- feature.link.fn(x)
  }
  if (!is.null(order.by)) {
    x <- setorderv(x, order.by, order=if (order.dir == 'asc') 1L else -1L)
  }


  ## Tweak length.opts
  if (nrow(x) <= 10) {
    length.opts <- nrow(x)
  } else {
    length.opts <- length.opts[length.opts <= nrow(x)]
    if (utils::tail(length.opts, 1) > nrow(x)) {
      length.opts <- c(utils::head(length.opts, -1L), nrow(x))
    }
  }

  dt.opts <- list(
    pageLength = length.opts[1L],
    lengthMenu = length.opts,
    dom = 'ltipr')

  rename.cols <- c("FDR" = "padj")
  rename.cols <- rename.cols[rename.cols %in% colnames(x)]
  if (length(rename.cols) == 0) {
    rename.cols <- colnames(x)
  }
  out <- DT::datatable(
    setDF(x), selection = 'none', escape = FALSE, rownames = FALSE,
    options=dt.opts, filter = filter, colnames=rename.cols)
  roundDT(out)
}

#' Creates an HTML-ized version of \code{tabuleResults}
#'
#' The table produced here is broken into two sections (left and right). The
#' left provides meta information about the geneset collections tested, ie.
#' their names and number of genesets the contain. The right contains columns
#' of results
#'
#' @export
#' @inheritParams sparrow::resultNames
#' @return a \code{tagList} version of an HTML table for use in a shiny app
#' @examples
#' sr <- sparrow::exampleSparrowResult()
#' summaryHTMLTable.sparrow(sr, max.p = 0.10, p.col = "padj")
summaryHTMLTable.sparrow <- function(x, names = sparrow::resultNames(x),
                                     max.p, p.col) {
  stopifnot(is(x, 'SparrowResult'))
  s <- sparrow::tabulateResults(x, names, max.p, p.col)

  ## The header of this table is two rows
  thead <- shiny::tags$thead(
    ## Super headers
    shiny::tags$tr(class='super-header',
                   shiny::tags$th("Gene Sets", colspan="2"),
                   shiny::tags$th("Analysis Summary", colspan=length(names))),
    ## Sub headers
    do.call(
      shiny::tags$tr,
      c(list(class='sub-header',
             shiny::tags$th("Collection"), # class="sparrow-summary-table"),
             shiny::tags$th("Count")), # class="sparrow-summary-table")),
        lapply(names, function(name) {
          target.dom.id <- paste0('sparrow-result-', name)
          ## I wanted the headers that had the method names to link to the
          ## tab with the results, but that takes a bit more tweaking
          ## tags$th(tags$a(href=paste0('#', target.dom.id), name))
          shiny::tags$th(name)
        })
      )))

  # The body of the table one row per collection
  collections <- unique(s$collection)
  tbody.rows <- lapply(collections, function(col) {
    sc <- s[s$collection == col,,drop=FALSE]
    stopifnot(all(names %in% sc$method))
    stopifnot(length(unique(sc$geneset_count)) == 1L)

    mres <- lapply(names, function(name) {
      # with(sc[sc[['method']] == name,,drop=FALSE], {
      #   shiny::tags$td(
      #     sprintf("%d (%d up; %d down)", sig_count, sig_up, sig_down))
      # })
      xsc <- sc[sc[['method']] == name,,drop=FALSE]
      shiny::tags$td(
        sprintf("%d (%d up; %d down)", xsc$sig_count, xsc$sig_up, xsc$sig_down))
    })

    do.call(
      shiny::tags$tr,
      c(list(shiny::tags$td(sc$collection[1L]),
             shiny::tags$td(sc$geneset_count[1L])),
        mres))
  })

  tbody <- shiny::tags$tbody(tbody.rows)
  shiny::tags$div(
    class='sparrow-summary-table',
    shiny::tags$table(thead, tbody))
}

#' Rounds all of the numeric columns of a DT
#'
#' This is a convenience function around [DT::formatRound()] that identifies
#' all of the numeric columns and rounds them, as opposed to just rounding
#' prespecified columns.
#'
#' @export
#' @param x a DT::datatable
#' @param digits the number of digits to round. If \code{NA}, then no rounding
#'   is performed
#' @return a rounded DT::datatable
#' @examples
#' df <- data.frame(a=rnorm(10), b=sample(letters, 10), c=rnorm(10))
#' roundDT(DT::datatable(df),  digits=2)
roundDT <- function(x, digits=3) {
  stopifnot(is(x, "datatables"))
  if (is.na(digits)) {
    return(x)
  }
  round.me <- sapply(x$x$data, function(x) {
    # half baked test to see if the numeric is integerish
    is.numeric(x) && any(as.integer(x) != x, na.rm = TRUE)
  })
  if (any(round.me)) {
    x <- DT::formatRound(x, round.me, digits=digits)
  }
  x
}
