\name{del13c}
\alias{del13c}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate the del13C for a given Plant Functional Type.
}
\description{
This function applies a carbon isotope endmember to a given plant plant functional type and calculates the per-pixel contribution of the plant functional type for the given extent, resulting in a layer of the relative contribution of the endmember, used an an input for calc_cisoscape function.
}
\usage{
del13c(total_veg, PFTlayer, endmember, filename = '', ...)
}
\arguments{
  \item{total_veg}{
RasterLayer. Per-pixel percentage of total vegetation cover present.
}
  \item{PFTlayer}{
RasterLayer. Per-pixel percentage of plant functional type. Output of partitionNatHerb function, or percent tree/woody cover.
}
  \item{endmember}{
numeric. Value to be used as a del13C endmember for the plant functional type.
}
  \item{filename}{
Character. Output filename (optional)
}
  \item{...}{
additional arguments for writing files as in writeRaster
}
}
\value{
RasterLayer
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
##C3endmember <- del13c(total_veg = total, c3.all, -26.7)
##C3endmember_sd <- del13c(total_veg = total, c3.all, 2.3)
}
\keyword{ del13c }
\keyword{ PFT }
