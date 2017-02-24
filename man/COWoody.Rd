\name{COWoody}
\alias{COWoody}
%- Also NEED an '\alias' for EACH other topic documented here.
\docType{data}
\title{
Per-Pixel Percent Woody Vegetation (Tree and Shrub). Colorado, USA
}
\description{
RasterLayer. This data is the per-pixel percent natural herbaceous vegetation data for Colorado, USA. This is an adjusted data set created from the MODIS Vegetation Continuous Fields (VCF) Yearly L3 Global Collection 3 (MOD448) data set, modified to account for shrubs using the MODIS Land Cover Type Yearly LC Global Collection 5 (MOD12Q1).
}
\usage{data("COWoody")}
\format{
Formal class 'RasterLayer' [package "raster"] with 12 slots
}
\details{
The VCF Tree layer represents the percent of canopy cover per grid cell, which results in an overestimation of the herbaceous layer, as maximum crown cover is fixed at 80 percent (Hansen et al., 2003). To address this shortcoming, I converted the VCF percent tree-cover (i.e., canopy cover) layer to percent tree-crown (treecrown = treecover/0.8). I then accounted for woody (shrub) vegetation in the herbaceous layer by using the land-cover descriptions for the 17-class International Geosphere-Biosphere Programme (IGBP) land-cover classification,included in the MODIS 500-m Global Land Cover Type product for 2001 (Loveland & Belward 1997). The non-tree layer pixels that correspond to the land-cover classes described as predominantly shrub and/or tree by the IGBP classification system were assigned to the shrub layer, and for pixels that correspond to classes composed predominantly of herbaceous cover, the non-tree layer is assigned as herbaceous. For pixels that correspond to IGBP classes composed of a mixture of shrubs and grasses, non-tree layer was partitioned into shrub and herbaceous following the maximum herbaceous cover rules described by Powell et al. (2012). The treecrown and shrub layers were then combined into a final percent woody vegetation layer.
}
\references{
Powell, R.L., Yoo, E-H., & Still, C.J. (2012) Vegetation and soil carbon-13 isoscapes for
South America: integrating remote sensing and ecosystem isotope measurements.
Ecosphere, 3, 109.

Friedl, M.A., Sulla-Menashe, D., Tan, B., Schneider, A., Ramankutty, N., Sibley, A., & Huang, X. (2010). MODIS Collection 5 global land cover: Algorithm refinements and characterization of new datasets. Remote Sensing of Environment, 114:168-182.

Hansen, M.C., DeFries, R.S., Townshend, J.R.G., Carroll, M., Cimiceli, C., & Sohlberd,
R.A. (2003) Global percent tree cover at a spatial resolution of 500 meters: first results of
the MODIS vegetation continuous fields algorithm. Earth Interactions, 7, 10.
}
\keyword{datasets}
\keyword{woody}
