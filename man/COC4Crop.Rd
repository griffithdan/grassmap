\name{COC4Crop}
\alias{COC4Crop}
%- Also NEED an '\alias' for EACH other topic documented here.
\docType{data}
\title{
Per-Pixel Percentage of C4 Crop in Colorado, USA
}
\description{
RasterLayer. 5-min spatial resolution. Depicts the per-pixel percentage of C4 crop cropped to the spatial extent of Colorado, USA for the year 2000.This data layer was created from the  Agricultural Lands in the Year 2000 (M3-Cropland Data) data set (Ramankutty et al., 2008) and the Harvested Area and Yields of 175 Crops (M3-Crops Data) data set.
}
\usage{data("COC4Crop")}
\format{
Formal class 'RasterLayer' [package "raster"] with 12 slots
}
\details{
This data layer was created by first partitioning the VCF percent herbaceous layer into percent natural herbaceous layer and percent crop layer using the Agricultural Lands in the Year 2000 (M3-Cropland Data) data set (Ramankutty et al., 2008). The Harvested Area and Yields of 175 Crops (M3-Crops Data) data set delineating crop type (Monfreda et al., 2008) was then applied to the percent crop layer to separate the crop layer into either C3 or C4 vegetation (Powell et al., 2012).
}
\source{
http://www.earthstat.org/data-download/
}
\references{
Monfreda, C., Ramankutty, N. & Foley, J. A. (2008) Farming the planet: 2. Geographic
distribution of crop areas, yields, physiological types, and net primary production in the
year 2000. Global Biogeochemical Cycles, doi: 22:10.1029/2007GB002947.

Powell, R.L., Yoo, E-H., & Still, C.J. (2012) Vegetation and soil carbon-13 isoscapes for
South America: integrating remote sensing and ecosystem isotope measurements.
Ecosphere, 3, 109, doi: 10.1980/ES12-00162.1.

Ramankutty, N., Evan, A.T., Monfreda, C., & Foley, J.A. (2008) Farming the Planet: 1.
Geographic distribution of global agricultural lands in the year 2000. Global
Biogeochemical Cycles, doi: 22:10.1029/2007GB002952.
}
\keyword{datasets}
\keyword{crop}
\keyword{C4}
