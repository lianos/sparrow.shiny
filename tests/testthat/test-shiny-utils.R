test_that("constructGseaResultTable filters results appropriately", {
  sres <- sparrow::exampleSparrowResult()
  method <- sparrow::resultNames(sres)[1L]
  rstats <- sparrow::result(sres, method)

  fdrs <- c(0, 0.01, 0.05, 0.10, 0.25)
  breaks <- cut(rstats$padj.by.collection, fdrs, include.lowest = TRUE)
  per.break <- table(breaks)

  for (i in 2:length(fdrs)) {
    gstats <- constructGseaResultTable(sres, method, fdrs[i])
    expect_equal(nrow(gstats), sum(head(per.break, i - 1)))
  }
})

test_that("mgResultFilter download handler requires a non-empty gseaMethod", {
  # https://github.com/lianos/sparrow.shiny issue: downloadHandler returned
  # an HTML error page (named *.html) when input$gseaMethod was empty because
  # sparrow::result() errored out on an invalid name. req() should halt the
  # handler instead, so the click is a no-op rather than a broken download.
  sres <- sparrow::exampleSparrowResult()
  src_val <- SparrowResultContainer(sres)

  # mgResultFilter uses the legacy callModule signature, so wrap it in a
  # moduleServer adapter to make it testServer-compatible.
  module <- function(id, src) {
    shiny::moduleServer(id, function(input, output, session) {
      mgResultFilter(input, output, session, src)
    })
  }

  shiny::testServer(
    module,
    args = list(src = shiny::reactive(src_val)),
    {
      session$setInputs(gseaMethod = "")
      expect_error(output$gseaDownloadStats, class = "shiny.silent.error")

      session$setInputs(gseaMethod = src_val$methods[1L])
      expect_silent(output$gseaDownloadStats)
    }
  )
})

test_that("constructGseaResultTable puts priotiy collections up front", {
  sres <- sparrow::exampleSparrowResult()
  method <- sparrow::resultNames(sres)[1L]
  rstats <- sparrow::result(sres, method)
  collections <- unique(rstats$collection)

  unordered <- constructGseaResultTable(sres, method, 1.0)
  expect_equal(levels(unordered$collection), collections)

  set.seed(123)
  shuffled <- sample(collections)
  assert_true(!all(shuffled == collections)) # shuffling works
  ordered <- constructGseaResultTable(sres, method, 1.0, prioritize = shuffled)
  expect_equal(levels(ordered$collection), shuffled)
})
