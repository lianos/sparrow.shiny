% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mgResultFilter-module.R
\name{mgResultFilter}
\alias{mgResultFilter}
\alias{mgResultFilterUI}
\title{A module that allows users to select which GSEA results to display.}
\usage{
mgResultFilter(input, output, session, src)

mgResultFilterUI(id)
}
\arguments{
\item{input, output, session}{shiny bits}

\item{src}{the `SparrowResultContainer`}

\item{id}{the shiny namespace for the module}
}
\value{
A list that includes the following reactives:
\describe{
  \item{$method}{The name of the GSEA method selected by the user}
  \item{$fdr}{The fdr threshold specified by the user}
}
}
\description{
The app is setup to only allow users to explore GSEA results one method at a
time. Given a SparrowResultContainer `src`, this module presents the user
with a dropdown list of GSEA methods that were run that they want to explore,
an FDR cutoff used to limit the gene sets returned for exploration, and a
download button that will deliver the GSEA stats as a CSV to the user.
}
\section{Functions}{
\itemize{
\item \code{mgResultFilterUI}: the UI for the module: presents a selectInput
for the user to pick which GSEA method to expl
}}

\examples{
sres <- sparrow::exampleSparrowResult()
app <- shiny::shinyApp(
  ui = shiny::shinyUI(shiny::fluidPage(
    exampleUISetup(),
    title = "Sparrow method result filter",
    mgResultFilterUI("mod"))),
  server = function(input, output, session) {
    src <- shiny::reactive(SparrowResultContainer(sres))
    shiny::callModule(mgResultFilter, "mod", src)
  })
if (interactive()) {
  shiny::runApp(app)
}
}
