\name{PFTmasks}
\alias{PFTmasks}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plant Functional Type Mask Function
}
\description{
This function converts the climate threshold stacks (outputs of crossover()) to three (or two, if tempmask and precipmask are single raster layers as opposed to a raster stack) separate mask layers, where each layer equals the extent of a given grassland classification. Also generates a weighted ratio to determine the split of plant functional type dominance for pixels that have mixed C3 and C4 dominance. Output is a RasterLayer or the per-pixel plant functional type ratio, weighted with a biomass weight, such as NDVI, or SAVI.
}
\usage{
PFTmasks(tempmask, precipmask,
c4.threshold = NULL, biomass.weight = NULL,
filename ='', ...)
}
\arguments{
\item{tempmask}{
RasterLayer, RasterStack or RasterBrick of temperature threshold rasters, generated from crossover_fun
generated from \code{"crossover"}.
}
\item{precipmask}{
RasterLayer, RasterStack or RasterBrick of precipitation threshold rasters, generated from crossover_fun
generated from \code{"crossover"}.
}
\item{c4.threshold}{
numeric. percentage of time a pixel must meet the C4 climate threshold in order to be classified as C4 dominant. Default is if a pixel meets the c4 climate threshold greater than half the time is is classified as c4 dominant. Fractions are converted to integers rounding up.
}
\item{biomass.weight}{
RasterStack or RasterBrick. Optional. Stack vegetation or biomass weight layers corresponding to the time period and temporal grain being modeled. NDVI can be used generated with the NDVI function included in this package. If a biomass weight object is not provided, mixed grassland pixels are given a c3/c4 dominance ratio based on the proportion of times the pixel is either c3 or c4 dominant.
}
  \item{filename}{
Character. Output filename (optional)
}
  \item{...}{
additional arguments for writing files as in writeRaster
}}
\value{
RasterBrick
\item{layer 1}{C3 dominant layer}
\item{layer 2}{C4 dominant layer}
\item{layer 3}{mixed grassland layer. not returned if climate inputs are a single layer.}
\item{layer 3}{mixed grassland ratio. not returned if climate inputs are a single layer.}
}
\references{
Powell, R.L., Yoo, E-H., & Still, C.J. (2012) Vegetation and soil carbon-13 isoscapes for
South America: integrating remote sensing and ecosystem isotope measurements.
Ecosphere, 3, 109, doi: 10.1980/ES12-00162.1.
}
\author{
Sydney M Firmin
}
\examples{
##pftlayers <- PFTmasks(crossover_temp, crossover_precip, ndvi)
}
\keyword{ PFT }

