
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Overview

The `sparrow.shiny` package provides an interactive shiny applications
that enables users to explore the results of a gene set enrichment
analysis performed using the
[sparrow](https://github.com/lianos/sparrow)

Although this is a standalone package, it’s really only used as an
“enhancement” to the sparrow package itself, as well as providing shiny
modules to the
[FacileAnalysis](https://github.com/facilebio/FacileAnalysis) package,
too.

All that of that is to say: there isn’t much end-user stuff here to play
with.

# Usage

More thorough documentation of the shiny application will be provided in
the near future in the form of a vignette, or more likely a screen cast.

In the meantime, this will just have to get you started:

``` r
vm <- sparrow::exampleExpressionSet(dataset = 'tumor-vs-normal', do.voom = TRUE)
gdb <- sparrow::exampleGeneSetDb()
sr <- sparrow::seas(gdb, vm, vm$design, "tumor", methods = c("camera", "fry"))
sparrow.shiny::explore(sr)
```

The `explore` function will launch the application and load it with the
`SparrowResult` object produced by the call to the `sparrow::seas()`
function. You can then explore the results of the “camera” or “fry”
analysis through there.

Users can serialize `SparrowResult` objects to `*.rds` files on their
filesystem, which can also be loaded individually once the application
is launched.

# Application Deployment

Analysts can simply launch the `sparrow.shiny::explore()` application
from their workstation, however these applications can also be deployed
to a shiny server.

## Docker

The [`inst/docker`](inst/docker) folder provides examples of how to
containerize and deploy this application in different contexts.

The [`Dockerfile-base`](inst/docker/Dockerfile-base) creates a docker
image that, when run, launches the shiny app on
`http://container.ip/sparrow` (ie. `http://localhost/sparrow`).

## ShinyProxy

The [`Dockerfile-shinyproxy`](inst/docker/Dockerfile-shinyproxy) creates
an image that can be deployed via a [ShinyProxy
server](https://www.shinyproxy.io/).

Notes on setting up a ShinyProxy server on AWS are provided in the
[`aws-ubuntu-deployment.md`](inst/docker/aws-ubuntu-deployment.md) file.

# Installation

The sparrow suite of package will soon be submitted to bioconductor and
installable via the recommended `BiocManager` mechanism. In the
meantime, these packages can be installed like so:

``` r
# install.packages("remotes")
remotes::install_github("lianos/sparrow.shiny")
```
