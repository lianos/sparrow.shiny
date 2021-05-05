shiny::shinyUI(fluidPage(
  title="Sparrow Explorer",
  shinyjs::useShinyjs(),
  shiny::tags$head(
    shiny::tags$link(rel="stylesheet", type="text/css", href="dashboard.css"),
    shiny::tags$link(rel="stylesheet", type="text/css", href="miniUI.css")
  ),


  shiny::fluidRow(
    shiny::column(
      3,
      shiny::wellPanel(shiny::fileInput("mgresult", 'Sparrow Result Upload'))),
    shiny::column(
      9,
      shiny::wellPanel(sparrow.shiny::mgResultFilterUI("mg_result_filter")))),

  shiny::tabsetPanel(
    shiny::tabPanel(
      "Overview",
      shiny::fluidRow(
        shiny::column(
          12,
          shiny::tags$div(
            style="margin-bottom: 10px; padding: 5px; background-color: white",
            title='GSEA Results',
            shiny::uiOutput("gseaMethodSummary"))))),

    shiny::tabPanel(
      "GSEA Results",
      shiny::fluidRow(
        shiny::column(
          5, style="padding: 0",
          shiny::wellPanel(
            sparrow.shiny::geneSetContrastViewUI("geneset_viewer"))),
        shiny::column(
          7,
          sparrow.shiny::mgTableBrowserUI("mg_table_browser"))),
      shiny::fluidRow(
        shiny::column(
          12,
          shiny::tags$h4("Other Gene Sets with Selected Genes"),
          sparrow.shiny::mgGeneSetSummaryByGeneUI('other_genesets_gsea')))
      ),

    shiny::tabPanel(
      "Differential Gene Expression",
      shiny::fluidRow(
        shiny::column(
          5,
          sparrow.shiny::mgVolcanoUI("dge_volcano")),
        shiny::column(
          7,
          shiny::tags$div(
            style="float:right",
            shiny::downloadButton('download_dge_stats', 'Download Statistics')),
          DT::dataTableOutput("dge_volcano_genestats"))),
      shiny::fluidRow(
        shiny::column(
          12,
          shiny::tags$h4("Other Gene Sets with Selected Genes"),
          sparrow.shiny::mgGeneSetSummaryByGeneUI('other_genesets_volcano'))))
  ) ## tabsetPanel
))
