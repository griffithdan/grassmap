\name{isoscape}
\alias{isoscape}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate an Isoscape based on input data and parameters (Wrapper Function)
}
\description{
This is a wrapper function that can calculate an isoscape based on input data and parameters. At minimum a user must provide a temperature and precipitation layer or temporal series, where the temporal grain and window of the temperature and precipitation data match one another, and a percent herbaceous vegetation layer. These inputs would result in a natural herbaceous only isoscape. Users can also provide woody vegetation data, cultivated vegetation data (crops) and custom crossover thresholds and isotopic endmembers. If the users chooses to include crop data, the user must provide and specify a percent c3 crop layer and a percent c4 crop layer. Default values for crossover thresholds and isotopic endmembers are listed below.
}
\usage{
isoscape(temperature, precip, herb, woody=NULL, c3.crop=NULL,
        c4.crop=NULL, biomass.weight=NULL, temp.threshold=22,
        precip.threshold=25, c4.threshold = NULL,
        c3.endmember=-26.7, c4.endmember=-12.5,
        woody.endmember=-27.2, c3.sd=2.3, c4.sd=1.1, woody.sd=2.5,
        filename = '', ...)
}
%- maybe also 'usage' for other objects documented here.
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
  \item{woody}{
RasterLayer. Per-pixel percentage of natural woody vegetation.
}
  \item{c3.crop}{
RasterLayer. Per-pixel percentage of cultivated C3 crops that may not follow natural biogeoraphical distribution. If including C3 layer must include C4 layer
}
  \item{c4.crop}{
RasterLayer. Per-pixel percentage of cultivated C4 crops that may not follow natural biogeoraphical distribution.
}
\item{biomass.weight}{
RasterBrick or RasterStack. Biomass weights for mixed grass pixels (examples: NDVI, SAVI, EVI). nlayers is the same as the input climate stacks.
}
\item{c4.threshold}{
numeric. percentage of time a pixel must meet the C4 climate threshold in order to be classified as C4 dominant. Default is if a pixel meets the c4 climate threshold greater than half the time is is classified as c4 dominant.
}
\item{temp.threshold}{
Crossover temperature threshold. Default is 22 degrees (celcius).
}
\item{precip.threshold}{
Crossover precipitation threshold. Default is 25 mm.
}
\item{c3.endmember}{
Isotope endmember assigned to C3 grass. Default is -26.7
}
\item{c4.endmember}{
Isotope endmember assigned to C4 grass. Default is -12.5
}
\item{woody.endmember}{
Isotope endmember assigned to woody vegetation. Default is -27.2
}
\item{c3.sd}{
Standard deviation of isotope endmember for C3 grass. Default is 2.3
}
\item{c4.sd}{
Standard deviation of isotope endmember for C4 grass. Default is 1.1
}
\item{woody.sd}{
Standard deviation of isotope endmember for woody. Default is 2.5
}
  \item{filename}{
Character. Output filename (optional)
}
  \item{...}{
additional arguments for writing files as in writeRaster
}
}
\value{
RasterLayer. Final del13C herbaceous only isoscape.
}
\references{
Still, C.J., Berry, J.A., Collatz, G.J. & DeFries, R.S. (2003) Global distribution of C3 and C4 vegetation: carbon cycle implications. Global Biogeochemical Cycles, 17, 1006, doi: 10.1029/2001GB001807.

Powell, R.L., Yoo, E-H., & Still, C.J. (2012) Vegetation and soil carbon-13 isoscapes for South America: integrating remote sensing and ecosystem isotope measurements. Ecosphere, 3, 109, doi: 10.1980/ES12-00162.1.
}
\author{
Sydney M Firmin
}
\examples{
##herb.isoscape <- isoscape(temperature = CO_MeanTemp,
##precip = CO_MeanPrecip, herb = CO_NatHerb.resample)
##natural.isoscape <- isoscape(temperature = CO_MeanTemp,
##precip = CO_MeanPrecip, herb = CO_NatHerb.resample,
##woody = CO_woody.resample)
##maxtemp.isoscape<- isoscape(temperature = CO_MaxTemp,
##precip = CO_MeanPrecip, herb = CO_NatHerb.resample,
##woody = CO_woody.resample, temp.threshold = 27)
}
\keyword{ isoscape }
\keyword{ del13C }
