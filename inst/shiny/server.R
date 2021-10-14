shiny::shinyServer(function(input, output, session) {
  ## If this application was invoked via explore(SparrowResult), then
  ## getOption(EXPLORE_SPARROW_RESULT='path/to/result.rds') was set that
  ## we can load, otherwise this will respond to a user upload.
  src <- shiny::reactive({
    ## Are we here because the user uploaded something, or did the user ask
    ## to `explore(SparrowResult)`? This implementation feels wrong, but ...
    if (is.null(input$mgresult)) {
      sr <- getOption('EXPLORE_SPARROW_RESULT', NULL)
      res <- sparrow::failWith(
        NULL,
        sparrow.shiny::SparrowResultContainer(sr), silent=TRUE)
      return(res)
    }
    ## User uploaded a file
    return(sparrow::failWith(
      NULL,
      sparrow.shiny::SparrowResultContainer(input$mgresult$datapath)))
  })

  lfc <- shiny::reactive({
    lfc <- shiny::req(src()$sr)
    lfc <- sparrow::logFC(lfc, as.dt=TRUE)
    lfc[order(logFC, decreasing=TRUE)]
  })

  gs_result_filter <- shiny::callModule(
    sparrow.shiny::mgResultFilter, 'mg_result_filter', src)

  ## Overview Tab ==============================================================
  output$gseaMethodSummary <- shiny::renderUI({
    obj <- sparrow::failWith(NULL, expr = src(), silent = TRUE)
    if (!is(obj, 'SparrowResultContainer')) {
      shiny::tags$p(
        style = "font-weight: bold; color: red",
        "Upload the SparrowResult object to initialize the application")
    } else {
      shiny::tagList(
        shiny::tags$h4("GSEA Analyses Overview"),
        sparrow.shiny::summaryHTMLTable.sparrow(
          obj$sr, obj$methods,
          gs_result_filter$fdr(),
          p.col = 'padj.by.collection')
      )
    }
  })

  ## GSEA Results Tab ==========================================================
  gs_viewer <- shiny::callModule(
    sparrow.shiny::geneSetContrastView,
    'geneset_viewer', src, maxOptions=500, server=TRUE)

  ## A table of GSEA statistics/results for the given method and fdr threshold
  ## The table is wired to the gs_viewer so that row clicks can signal updates
  ## to the contrast viewer
  gs_table_browser <- shiny::callModule(
    sparrow.shiny::mgTableBrowser,
    'mg_table_browser', src,
    method = gs_result_filter$method,
    fdr = gs_result_filter$fdr,
    server = TRUE)
  ## clicks on gsea result table update the contrast view
  shiny::observeEvent(gs_table_browser$selected(), {
    .src <- shiny::req(src())
    geneset <- shiny::req(gs_table_browser$selected())
    sparrow.shiny::updateActiveGeneSetInContrastView(
      session, gs_viewer, geneset, .src)
  })

  ## A table of other genesets that brushed genes in the contrast viewer
  ## belong to. This table is also wired to the contrast viewer, so that
  ## a click on a row of the table will update the contrast view, too.
  other_genesets_gsea <- shiny::callModule(
    sparrow.shiny::mgGeneSetSummaryByGene,
    'other_genesets_gsea',
    src, features = gs_viewer$selected,
    method = gs_result_filter$method,
    fdr = gs_result_filter$fdr)
  ## DEBUG: Can we add a DT row click listner to the `other_genesets_gsea` so
  ## that it updates the `gs_viewer`? My first shot at doing sends the
  ## application into a tailspin, my best guess is because the selection is
  ## still active in the interactive boxp/density plot.

  ## Differential Gene Expression Tab ==========================================
  gene.volcano <- shiny::callModule(
    sparrow.shiny::mgVolcano, 'dge_volcano', src,
    width=400, height=350)

  output$dge_volcano_genestats <- DT::renderDataTable({
    res.all <- shiny::req(lfc())
    res <- res.all[, list(symbol, feature_id, logFC, pval, padj)]

    selected <- gene.volcano()
    if (!is.null(selected)) {
      res <- subset(res, feature_id %in% selected$feature_id)
    }

    sparrow.shiny::renderFeatureStatsDataTable(
      res, filter = 'top',
      feature.link.fn = sparrow.shiny::ncbi.entrez.link)
  })

  ## Respond to user click to download differential expression statistics
  output$download_dge_stats <- shiny::downloadHandler(
    filename = function() "sparrow-feature-level-statistics.csv",
    content = function(file) write.csv(lfc(), file, row.names=FALSE))

  ## A table of other genesets that brushed genes in the contrast viewer
  ## belong to. This table is also wired to the contrast viewer, so that
  ## a click on a row of the table will update the contrast view, too.
  other_genesets_volcano <- shiny::callModule(
    sparrow.shiny::mgGeneSetSummaryByGene,
    'other_genesets_volcano',
    src, features=gene.volcano,
    method=gs_result_filter$method,
    fdr=gs_result_filter$fdr)
})
