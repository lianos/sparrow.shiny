#' A wrapper for a SparrowResult that makes some shiny bits explicit
#'
#' @export
#' @param x A `[sparrow::SparrowResult()]` object, or a path to
#'   an rds-serliazed one
#' @return a `SparrowResultContainer` object
SparrowResultContainer <- function(x) {
  ## TODO: S4ize SparrowResultContainer
  if (is.character(x)) {
    ## Assume this is a file
    if (!file.exists(x)) {
      stop("file does not exist: ", x)
    }
    sr <- readRDS(x)
  } else if (is(x, 'GeneSetDb')) {
    ## Hack to init some shinybits that are useful to have ontop of a GeneSetDb
    ## (I feel horrible for having this)
    fids <- sparrow::featureIds(x)
    fids <- setNames(rnorm(length(fids)), fids)
    sr <- sparrow::seas(x, fids, methods=NULL)
  } else if (is(x, 'SparrowResult')) {
    sr <- x
  } else {
    sr <- NULL
  }

  if (!is(sr, 'SparrowResult')) {
    stop("Don't know how to create container from: ", class(sr)[1L])
  }

  methods <- local({
    tmp <- sparrow::resultNames(sr)
    if (length(tmp) == 0L) {
      warning("No GSEA methods found in SparrowResult, ",
              "you can only make a geneSetSelectUI")
      out <- character()
    } else {
      ## I am biased and prefer to show these methods first, if available
      pref <- c('camera', 'cameraPR', 'fgsea',
                'goseq', 'goseq.up', 'goseq.down')
      pref <- pref[pref %in% tmp]
      out <- c(pref, setdiff(tmp, pref))
    }
    out
  })

  gs.choices <- gs.select.choices(sr)

  out <- list(mg = sr, methods = methods, choices = gs.choices)
  class(out) <- c('SparrowResultContainer', class(out))
  out
}
