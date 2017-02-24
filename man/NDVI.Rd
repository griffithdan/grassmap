\name{NDVI}
\alias{NDVI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Normalized Difference Vegetation Index Function
}
\description{
This function calculates the NDVI for the given extent based on remotely sensed reflectance values for Red and Near Infared (NIR) bands. The function removes any NA and negative values, and multiplies NDVI values by 10000.
}
\usage{
NDVI(NIRstack, REDstack, NAValue, scalefactor, filename = '', ...)
}
\arguments{
  \item{NIRstack}{
RasterStack or RasterBrick with the remotely sensed Near Infared (NIR) band reflectance values. Must have corresponding layers with REDstack input.
}
  \item{REDstack}{
RasterStack or RasterBrick with the remotely sensed visible red band reflectance values. Must have corresponding layers with NIRstack input.
}
  \item{NAValue}{
value to be set as NA in input layers
}
  \item{scalefactor}{
factor to scale NDVI output by
}
  \item{filename}{
Character. Output filename (optional)
}
  \item{...}{
additional arguments for writing files as in writeRaster
}
}
\details{
It is important to perform any spatial resizing of reflectance data prior to running this function.
}
\value{
RasterBrick
}
\author{
Sydney M Firmin
}
\examples{
##load in NIR Reflectance Stack
##data("MODIS_NIR")
##load in Red Reflectance Stack
##data("MODIS_Red")

##aggregate reflectance layers and resample to match final output prior to calculating NDVI
##MODIS_NIR_ag <- aggregate(MODIS_NIR, fact=9, fun=mean, expand=TRUE)
##MODIS_NIR_rs <- resample(MODIS_NIR_ag, CO_C3Crop, method="bilinear")
##MODIS_Red_ag <- aggregate(MODIS_Red, fact=9, fun=mean, expand=TRUE)
##MODIS_Red_rs <- resample(MODIS_Red_ag, CO_C3Crop, method="bilinear")

##calculate NDVI removing NA and Negative values, multiplied by 10000
##NDVI <- NDVI(NIRstack=MODIS_NIR_rs, REDstack=MODIS_Red_rs,
##=-1000, scalefactor=10000, nlayers=12, filename="NDVI")
}
\keyword{ NDVI }

