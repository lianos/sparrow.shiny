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
