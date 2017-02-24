\name{crossover}
\alias{crossover}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plant Functional Type Crossover Temperature
}
\description{
This function takes a raster stack of monthly climate data and determines per-pixel plant functional type based on a given climate threshold (e.g., mean monthly temperature = 20 C)
}
\usage{
crossover(climate.stack, threshold, filename = '', ...)
}
\arguments{
  \item{climate.stack}{
RasterBrick or RasterStack. Layers are temperature or precipitation values for the grain, the stack is the extent of the period of interest.
}
  \item{threshold}{
numeric. Temperature or Precipitation crossover value for pixel to be considered C3 or C4/minimum precipitation value for growing season.
}
  \item{filename}{
Character. Output filename (optional)
}
  \item{...}{
additional arguments for writing files as in writeRaster
}
}
\details{
This function can be used for any period of time and for any grain of time. The individual layers of the RasterBrick object represent the temporal grain of the dataset, and the span of the layers the temporal extent. To account for climates where the threhsold temperature for C4 dominance is met during a period where there is not enough precipitation (mediterranean climate), this function should be run on a temperature metric and precipitation threshold, and combined with the gs.sum function.
}
\value{
RasterBrick
}
\references{
Collatz, G.J., Berry, J.A., & Clark, J.S. (1998) Effects of climate and atmospheric CO2 partial pressure on the global distribution of C4 grasses: present, past, and future. Oecologia.

Still, C.J., Berry, J.A., Collatz, G.J. & DeFries, R.S. (2003) Global distribution of C3 and C4 vegetation: carbon cycle implications. Global Biogeochemical Cycles, 17, 1006.

Powell, R.L., Yoo, E-H., & Still, C.J. (2012) Vegetation and soil carbon-13 isoscapes for South America: integrating remote sensing and ecosystem isotope measurements. Ecosphere, 3, 109.
}
\author{
Sydney M Firmin
}
\examples{
##data("CO_MeanTemp")
##crossover_temp <- crossover(CO_MeanTemp, 25)

##data("CO_MeanPrecip")
##crossover_precip <- crossover(CO_MeanPrecip, 25)
}
\keyword{ Collatz }
\keyword{ Crossover }
