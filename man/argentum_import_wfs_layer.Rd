% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import_wfs.R
\name{argentum_import_wfs_layer}
\alias{argentum_import_wfs_layer}
\title{Import WFS Layer}
\usage{
argentum_import_wfs_layer(wfs_url, layer_name)
}
\arguments{
\item{wfs_url}{The URL of the WFS service}

\item{layer_name}{The name of the layer to import}
}
\value{
An sf object
}
\description{
Import WFS Layer
}
\examples{
\donttest{
tryCatch({
  sf_layer <- argentum_import_wfs_layer("http://example.com/wfs", "example_layer")
  print(sf_layer)
}, error = function(e) {
  message("Error occurred: ", e$message)
})
}
}
