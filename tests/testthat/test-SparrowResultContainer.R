test_that("SparrowResultContainer gracefully handles GeneSetDb's of length 1", {
  # https://github.com/lianos/sparrow.shiny/issues/1
  db_single <- sparrow::GeneSetDb(
    tibble::tribble(
      ~collection, ~name,      ~feature_id, ~symbol,
      "some_coll", "some_name", "some_id",   "some_symbol"))
  expect_warning(src <- sparrow.shiny::SparrowResultContainer(db_single),
                 "No.*methods")
})
