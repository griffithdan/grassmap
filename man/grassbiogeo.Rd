\name{grassbiogeo}
\alias{grassbiogeo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Models C3 C4 Grass Biogeography based on input data and parameters (wrapper function)
}
\description{
Users provide climate and percent herbaceous cover data to create a model of C3 versus C4 presence. The temporal grain and extent of the precipitation and climate data must match one another. Users may specify crossover temperature and minimum precipitation thresholds, the defaults are a mean temperature 22 degrees celcius and minmum mean precipitation 25mm. Users have the option to have the function return percent herbaceous (C3 and C4 per-pixel percentage normalized by total herbaceous vegetation) by setting percent.herb = TRUE or percent cover (C3 and C4 per-pixel percent is not normalized, and represents the relative cover of C3 or C4 in the pixel) by setting percent.herb = FALSE. The default is to return percent cover.
}
\usage{
grassbiogeo(temperature, precip, herb, biomass.weight = NULL,
            c4.threshold = NULL, temp.threshold = 22,
            precip.threshold = 25, percent.herb = FALSE,
            filename = '', ...)
}
\arguments{
  \item{temperature}{
RasterBrick or RasterStack. Layers are temperature values for the grain, the stack is the extent of the period of interest.
}
  \item{precip}{
RasterBrick or RasterStack. Layers are precipitation values for the grain, the stack is the extent of the period of interest.
}
  \item{herb}{
RasterLayer. Per-pixel percentage of natural herbaceous vegetation.
}
\item{biomass.weight}{
RasterBrick or RasterStack. Biomass weights for mixed grass pixels (examples: NDVI, SAVI, EVI). nlayers is the same as the input climate stacks.
}
\item{c4.threshold}{
numeric. percentage of time a pixel must meet the C4 climate threshold in order to be classified as C4 dominant. Default is if a pixel meets the c4 climate threshold greater than half the time is is classified as c4 dominant.
}
\item{temp.threshold}{
Numeric. Crossover temperature threshold. Default is 22 degrees (celcius).
}
\item{precip.threshold}{
Numeric. Crossover precipitation threshold. Default is 25 mm.
}
\item{percent.herb}{
Logical. TRUE or FALSE. If True, function returns percent C3 and C4 cover normalized by total herbaceous vegetation. If False, function returns percent C3 and percent C4 cover.
}
  \item{filename}{
Character. Output filename (optional)
}
  \item{...}{
additional arguments for writing files as in writeRaster
}
}
\value{
RasterBrick. Layer[[1]] is percent C3 vegetation or cover, Layer[[2]] is percent C4 vegetation or cover.
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
##grassbiogeo(temp.stack, precip.stack, percent.herb,
##biomass.weight= NDVI, temp.threshold=22, precip.threshold=25, percent.herb=FALSE)
}
\keyword{ PFT }
\keyword{ Crossover }
\keyword{ Biogeography }
\keyword{ Grass }
