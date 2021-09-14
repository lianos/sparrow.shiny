library("sparrow")
library("testthat")
library("data.table")
library("dplyr")

test_check("sparrow.shiny")

# Remove temporary files that were generated
test.dir <- system.file('tests', package = "sparrow.shiny")
pdfs <- dir(test.dir, "\\.pdf$", full.names=TRUE)
if (length(pdfs)) {
  unlink(pdfs)
}
