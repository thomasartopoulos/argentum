## Test environments
* Windows 10, R 4.3.2
* Ubuntu 22.04, R 4.3.2
* MacOS 14, R 4.3.2

## R CMD check results
* 0 errors | 0 warnings | 2 notes
* NOTE 1: Unable to verify current time (expected on Windows)
* NOTE 2: Found 'data' directory (false positive; `dir.create()` used in function)

## Comments
* This is the second submission of `Argentum` to CRAN.
* The package provides tools to download and manage WFS layers from Argentinian geospatial data sources.
* New function to download files in different formats was added (download_layers.R and interactive_download.R)
