#' A module that creates a dynamic selectizeInput for a SparrowResult object
#'
#' This code was inspired from the
#' \href{https://gist.github.com/MarkEdmondson1234/7e56ee7ac5caa74224327489b0849e61}{dynamicSelectShinyModule.R}
#' gist.
#'
#' @section Module Return:
#' Returns information about the `geneSetSelect` object
#'
#' @export
#' @aliases geneSetSelect
#'
#' @param input,output,session the shiny-required bits for the module
#' @param mgc A [SparrowResultContainer()] object
#' @param server boolean to indicate whether the genesets in the geneSetSelect
#'   widget should be rendered server side or not (Default: `TRUE`)
#' @param maxOptions a paremeter used to customize the
#'   `GeneSetSelect::selectizeInput` UI element to configure the maximum number
#'   of elements to include in the select dropdown, the remainder of the
#'   genesets will be loaded from the server side. Default: `Inf` for all.
#' @param sep the separater to put between the collection and name bits of a
#'   geneset. These are the values used in the gene set `selectizeInput` to
#'   create unique keys for each geneset.
#' @return a reactive list of information about the selected geneset.
#' * `collection`: the collection its from
#' * `name`: the name within the collection
#' * `stats`: a data.frame of "contrast statistics" for the features in the
#'   geneset
#' * `select.id`: the shiny id for this module
#' * `sep`: the separator used to key the collection,name string for this
#'   geneset
geneSetSelect <- function(input, output, session, mgc, server=TRUE,
                          maxOptions=Inf, sep='_::_') {
  # Programmatically create the UI from the SparrowResults
  output$geneset_picker <- shiny::renderUI({
    shiny::req(mgc())
    if (is.infinite(maxOptions)) {
      mo <- nrow(sparrow::geneSets(mgc()$mg))
    } else {
      mo <- maxOptions
    }
    gs.render.select.ui(session$ns, mgc()$choices, server = server,
                        maxOptions = mo)
  })
  shiny::outputOptions(output, "geneset_picker", suspendWhenHidden = FALSE)

  if (server) {
    shiny::observeEvent(mgc(), {
      shiny::updateSelectizeInput(session, "geneset", choices=mgc()$choices,
                                  server=TRUE, selected=NULL)
    }, priority = 5)
  }

  vals <- shiny::reactive({
    gs <- input$geneset
    if (is.null(gs) || length(gs) == 0 || nchar(gs) == 0) {
      # HACK, just put something here if it's not selectd
      # gs <- mgc()$choices$value[1L]
      coll <- name <- stats <- NULL
    } else {
      info <- unlist(strsplit(gs, sep, fixed = TRUE))
      info <- sapply(info, as.character)
      names(info) <- c("collection", "name")

      coll <- info[1L]
      name <- info[2L]

      # When this is used as a module in another application
      # (ie. FacileExplorer), it is possible that the GeneSetDb used
      # swaps from under our feet and the geneset (collection,name) you
      # had loaded in this UI element disappears. This should be caught
      # by some reactive expression upstream, but I can't get it to work,
      # (I thought the observeEvent(..., priority=5) would do the trick)
      # so I'm ensuring that the geneSet() call doesn't fail. If it does, it
      # means that geneset you are looking for disappeared, likely due to
      # the reason I stated above.
      stats <- sparrow::failWith(NULL, {
        sparrow::geneSet(mgc()$mg, info[1L], info[2L], as.dt = TRUE)
      })
      if (is.null(stats)) {
        coll <- name <- stats <- NULL
      } else {
        stats <- stats[order(logFC, decreasing=TRUE)]
      }
    }

    list(collection = coll, name = name, stats = stats,
         select.id = session$ns('geneset'), sep = sep)
  })
  return(vals)
}

#' @describeIn geneSetSelect the ui for the module
#' @export
#' @param id the shiny namespace for the module
#' @param label the label for the [shiny::selectizeInput()]
geneSetSelectUI <- function(id, label = "Select Gene Set") {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("geneset_picker"))
}

#' @export
#' @describeIn geneSetSelect update geneSetSelect externally.
#'   with new choices
#' @param id the 'naked' module id
#' @inheritParams shiny::updateSelectizeInput
updateGeneSetSelect <- function(session, id, label = NULL, choices = NULL,
                                selected = NULL, options = list(),
                                server = FALSE) {
  childScope <- session$makeScope(id)
  shiny::withReactiveDomain(childScope, {
    mod.id <- childScope$ns('geneset')
    shiny::updateSelectizeInput(session, mod.id, label = label,
                                choices = choices, selected = selected,
                                options = options,
                                server = server)
  })
}

## Utility Functions -----------------------------------------------------------

#' @describeIn geneSetSelect Internal function to build a `selectizeInput`
#'   widget that is specific to a SparrowResult.
#'
#' @param ns the namespace function for this module
#' @param choices the output of `gs.select.choices(SparrowResult)`
#' @param server logical indicating wether the options should be generated in
#'   the server module, default: `TRUE`
#' @return a properly wired `[shiny::selectizeInput()]` UI element.
gs.render.select.ui <- function(ns, choices, server = TRUE,
                                maxOptions = 1000, sep = '_::_') {
  # predefine all options groups
  optgroups = lapply(unique(choices$collection), function(col) {
    list(value=col, label=col)
  })

  # define options to customize the selectize object
  si.opts <- list(
    placeholder='Select Gene Set',
    optgroups=optgroups,
    optgroupField='collection',
    searchField = c('label'),
    maxOptions=maxOptions,
    render=I("{
             option: function(item, escape) {
             return '<div>' + escape(item.label) + '</div>';
             }}"))

  if (server) {
    ui <- shiny::selectizeInput(ns("geneset"), label=NULL, choices=NULL,
                                options=si.opts, width="100%")
  } else {
    choices <- sapply(unique(choices$collection), function(x) {
      out <- choices[choices[["collection"]] == x,]
      setNames(out$value, out$label)
    }, simplify=FALSE)
    ui <- shiny::selectizeInput(ns("geneset"), label = NULL, choices = choices,
                                width = "100%")
  }

  ui
}

#' @describeIn geneSetSelect Internal function to build a `data.frame`
#'   used to populate geneset choices for a select input.
#'
#' Note that when returning a data.frame for the choices from
#' `gs.select.choice()`, we need a column called `"value"` and a column called
#' `"label"`.
#'
#' * `value`: the value that is sent back when an item is selected
#' * `label`: athe text that appears in the selection after its triggered
#'
#' @param mg `SparrowResult` to build options for
#' @param sep the string used to concatenate geneset `collection` and `name`
#'   to generate a uniqe string for a geneset
#' @return `data.table` to populate `choices` of `selectizeInput`
gs.select.choices <- function(mg, sep = "_::_") {
  out <- sparrow::geneSets(mg, as.dt=TRUE)[, {
    list(collection, label=name, value=paste(collection, name, sep=sep))
  }]
  out
}
