#' A module that allows users to select which GSEA results to display.
#'
#' The app is setup to only allow users to explore GSEA results one method at a
#' time. Given a SparrowResultContainer `mgc`, this module presents the user
#' with a dropdown list of GSEA methods that were run that they want to explore
#' and an FDR cutoff that limits the gene sets returned for exploration.
#'
#' @export
#' @rdname mgResultFilter
#' @param input,output,session shiny bits
#' @param mgc the `SparrowResultContainer`
#' @return A list that includes the following reactives:
#' \describe{
#'   \item{$method}{The name of the GSEA method selected by the user}
#'   \item{$fdr}{The fdr threshold specified by the user}
#' }
mgResultFilter <- function(input, output, session, mgc) {

  # When the SparrowResult changes, we want to update different aspects of
  # the application
  shiny::observeEvent(mgc(), {
    shiny::req(mgc())
    shiny::updateSelectInput(
      session, "gseaMethod",
      choices = mgc()$methods,
      selected = mgc()$methods[1L])
  })

  output$gseaDownloadStats <- shiny::downloadHandler(
    filename = function() {
      sprintf('sparrow-gsea-statistics-%s.csv',
              shiny::isolate(input$gseaMethod))
    },
    content = function(file) {
      utils::write.csv(
        sparrow::result(mgc()$mg, shiny::isolate(input$gseaMethod)),
        file, row.names = FALSE)
    }
  )

  list(
    method = shiny::reactive(input$gseaMethod),
    fdr = shiny::reactive(input$gseaReportFDR))
}

#' Shiny module to pick GSEA method and fdr params used for display
#'
#' @export
#' @describeIn mgResultFilter the UI for the module: presents a selectInput
#'   for the user to pick which GSEA method to expl
#' @param id the shiny namespace for the module
mgResultFilterUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        5,
        shiny::selectInput(ns("gseaMethod"), "GSEA Method", "")),
      shiny::column(
        3,
        shiny::numericInput(ns("gseaReportFDR"), "FDR Cutoff", min = 0, max = 1,
                            value = 0.2, step = 0.05)),
      shiny::column(
        4,
        shiny::tags$div(
          style="padding-top: 1.7em;",
          shiny::downloadButton(ns("gseaDownloadStats"), 'Download'))))
  )
}


