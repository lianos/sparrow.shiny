#' Shiny module to pick GSEA method and fdr params used for display
#'
#' @export
#' @rdname mgResultFilter
mgResultFilterUI <- function(id, mg=NULL) {
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

#' @export
#' @rdname mgResultFilter
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

  shiny::reactive({
    list(
      method = shiny::reactive(input$gseaMethod),
      fdr = shiny::reactive(input$gseaReportFDR))
  })
}
