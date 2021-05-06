#' Interactively explore a SparrowResult via a shiny app
#'
#' @description
#' This will launch a shiny application that enables exploratory analysis of
#' the results from the different GSEA methods run within a `SparrowResul`.
#'
#' Reference the "shiny-sparrow" vignette for more detailed documentation of
#' the functionality provided by this application.
#'
#' @export
#' @param x A `SparrowResult` object, or path to one as an *.rds. If
#'   missing, the shiny app will load without a `SparrowResult` object
#'   to explore, and the user can upload one into the app.
#' @examples
#' \dontrun{
#' vm <- exampleExpressionSet()
#' gdb <- exampleGeneSetDb()
#' sr <- seas(gdb, vm, vm$design, methods=c('camera', 'fry'))
#' explore(sr)
#' }
explore <- function(x) {
  if (!missing(x)) {
    if (is.character(x)) x <- readRDS(x)
    stopifnot(is(x, 'SparrowResult'))
    options(EXPLORE_SPARROW_RESULT = x)
    on.exit(options(EXPLORE_SPARROW_RESULT = NULL))
  }
  shiny::runApp(system.file("shiny", package = "sparrow.shiny"))
}

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
insertPlotlyReset <- function(source, event=c('hover', 'click', 'selected')) {
  stopifnot(is.character(source), length(source) == 1L)
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

#' Builds the table of GSEA statistics to present to the user
#'
#' @description
#' The application will present the set of gene sets that pass a given
#' \code{fdr} for a given \code{method} as a central piece of the UI. This
#' function accepts those to arguments and prepares the statistics generated
#' from the analysis for display.
#'
#' @export
#' @param mg `SparrowResult` object
#' @param method the method to show statistics for
#' @param fdr the FDR cut off to present statistics for
#' @param prioritize the preffered collections to put at the top of the
#'   list. The collection column of the table is turned into a factor and for
#'   more usful display with datatable's filter. Specifcying collections
#'   here will put those collections at the front of the factor levels and
#'   therofre prioritize their display in the select dropdown for the filter
#' @return a data.table of the statistics that match the filtering criteria.
#'   A 0-row data.table is returned if nothing passes.
constructGseaResultTable <- function(mg, method, fdr, prioritize=c('h')) {
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

#' Creates a DT::datatable of geneset level GSEA results for use in shiny bits
#'
#' @export
#' @param x The set of GSEA statistics generated from from
#'   [constructGseaResultTable()]
#' @param method the GSEA method being used fo rdisplay
#' @param mg The `SparrowResult` object. This is used swap in the
#' URL links for genesets using [sparrow::geneSetURL()].
#' @return a DT::DataTable
renderGseaResultTableDataTable <- function(x, method, mg, digits=3) {
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

# Gene evel Table Helpers =====================================================

#' Transforms a column in feature table to an external link for that feature.
#'
#' @description
#' When listing features in an interactive table, it's often useful to link
#' the feature to an external webpage that has more information about that
#' feature. Functions to genes to their NCBI or GeneCards webpage via their
#' `feature_id` are provided via `ncbi.entrez.link` and
#' `genecards.entrez.link`. The column used to transform into a link
#' is specified by `link.col`.
#'
#' If `link.col` is not found in the data.frame `x` then the provided
#' functions are NO-OPS, ie. the same data.frame is simply returned.
#'
#' @rdname feature-link-functions
#' @export
#' @param x a data.frame from `SparrowResult`
#' @param link.col the column in `x` that should be transformed to a link
#' @return a modified `x` with an html link in `link.col`.
ncbi.entrez.link <- function(x, link.col='symbol') {
  if (is.character(link.col) && is.character(x[[link.col]])) {
    url <- sprintf('https://www.ncbi.nlm.nih.gov/gene/%s', x$feature_id)
    html <- sprintf('<a href="%s" target="_blank">%s</a>', url, x$symbol)
    x[[link.col]] <- html
  }
  x
}


#' @rdname feature-link-functions
#' @export
genecards.entrez.link <- function(x, link.col='symbol') {
  if (is.character(link.col) && is.character(x[[link.col]])) {
    url <- sprintf('http://www.genecards.org/cgi-bin/carddisp.pl?gene=%s',
                   x$feature_id)
    html <- sprintf('<a href="%s" target="_blank">%s</a>', url, x$symbol)
    x[[link.col]] <- html
  }
  x
}

#' Creates a DT::datatable of feature level statistics for use in shiny bits
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
#'
#' @param feature.link.fn A funcion that receives the data.frame of statistics
#'   to be rendered and transforms one of its columns into a hyperlink for
#'   further reference. Refer to the [ncbi.entrez.link()` function
#'   as an example
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
    x <- subset(x, feature_id %in% features)
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
    if (tail(length.opts, 1) > nrow(x)) {
      length.opts <- c(head(length.opts, -1L), nrow(x))
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
#' @return a \code{tagList} version of an HTML table for use in a shiny app
summaryHTMLTable.sparrow <- function(x, names = resultNames(x), max.p, p.col) {
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
      tags$tr,
      c(list(class='sub-header',
             tags$th("Collection"), # class="sparrow-summary-table"),
             tags$th("Count")), # class="sparrow-summary-table")),
        lapply(names, function(name) {
          target.dom.id <- paste0('sparrow-result-', name)
          ## I wanted the headers that had the method names to link to the
          ## tab with the results, but that takes a bit more tweaking
          ## tags$th(tags$a(href=paste0('#', target.dom.id), name))
          shiny::tags$th(name)
        })
      )))

  ## The body of the table one row per collection
  collections <- unique(s$collection)
  tbody.rows <- lapply(collections, function(col) {
    sc <- subset(s, collection == col)
    stopifnot(all(names %in% sc$method))
    stopifnot(length(unique(sc$geneset_count)) == 1L)

    mres <- lapply(names, function(name) {
      with(sc[sc[['method']] == name,,drop=FALSE], {
        shiny::tags$td(sprintf("%d (%d up; %d down)", sig_count, sig_up, sig_down))
      })
    })

    do.call(
      shiny::tags$tr,
      c(list(shiny::tags$td(sc$collection[1L]), # class='sparrow-summary-table'),
             shiny::tags$td(sc$geneset_count[1L])), # class='sparrow-summary-table')),
        mres))
  })

  tbody <- shiny::tags$tbody(tbody.rows)
  html <- shiny::tags$div(
    class='sparrow-summary-table',
    shiny::tags$table(thead, tbody))
}

#' Round the numeric columns of a DT
#'
#' @export
#' @param x a DT::datatable
#' @param digits the number of digits to round. If \code{NA}, then no rounding
#'   is performed
#' @return a rounded DT::datatable
#' @examples
#' \dontrun{
#' df <- data.frame(a=rnorm(10), b=sample(letters, 10), c=rnorm(10))
#' roundDT(datatable(df),  digits=2)
#' }
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
