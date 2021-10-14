#' A module that allows users to select which GSEA results to display.
#'
#' The app is setup to only allow users to explore GSEA results one method at a
#' time. Given a SparrowResultContainer `src`, this module presents the user
#' with a dropdown list of GSEA methods that were run that they want to explore,
#' an FDR cutoff used to limit the gene sets returned for exploration, and a
#' download button that will deliver the GSEA stats as a CSV to the user.
#'
#' @export
#' @rdname mgResultFilter
#' @param input,output,session shiny bits
#' @param src the `SparrowResultContainer`
#' @return A list that includes the following reactives:
#' \describe{
#'   \item{$method}{The name of the GSEA method selected by the user}
#'   \item{$fdr}{The fdr threshold specified by the user}
#' }
#' @examples
#' sres <- sparrow::exampleSparrowResult()
#' app <- shiny::shinyApp(
#'   ui = shiny::shinyUI(shiny::fluidPage(
#'     exampleUISetup(),
#'     title = "Sparrow method result filter",
#'     mgResultFilterUI("mod"))),
#'   server = function(input, output, session) {
#'     src <- shiny::reactive(SparrowResultContainer(sres))
#'     shiny::callModule(mgResultFilter, "mod", src)
#'   })
#' if (interactive()) {
#'   shiny::runApp(app)
#' }
mgResultFilter <- function(input, output, session, src) {
  # When the SparrowResult changes, we want to update different aspects of
  # the application
  shiny::observeEvent(src(), {
    shiny::req(src())
    shiny::updateSelectInput(
      session, "gseaMethod",
      choices = src()$methods,
      selected = src()$methods[1L])
  })

  output$gseaDownloadStats <- shiny::downloadHandler(
    filename = function() {
      sprintf('sparrow-gsea-statistics-%s.csv',
              shiny::isolate(input$gseaMethod))
    },
    content = function(file) {
      utils::write.csv(
        sparrow::result(src()$sr, shiny::isolate(input$gseaMethod)),
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


