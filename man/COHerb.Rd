\name{COHerb}
\alias{COHerb}
%- Also NEED an '\alias' for EACH other topic documented here.
\docType{data}
\title{
Per-Pixel Percent Natural Herbaceous Vegetation. Colorado, USA
}
\description{
RasterLayer. This data is the per-pixel percent natural herbaceous vegetation data for Colorado, USA. This is an adjusted data set created from the MODIS Vegetation Continuous Fields (VCF) Yearly L3 Global Collection 3 (MOD448) data set, modified to account for woody vegetation (shrubs) and managed agro-ecosystems (crops) using the MODIS Land Cover Type Yearly LC Global Collection 5 (MOD12Q1) and the Agricultural Lands in the Year 2000 (M3-Cropland Data) data sets.
}
\usage{data("COHerb")}
\format{
Formal class 'RasterLayer' [package "raster"] with 12 slots
}
\details{
The MODIS Vegetation Continuous Fields (VCF) Yearly L3 Global Collection 3 for the year 2001 includes layers that represent perpixel percent tree cover, percent herbaceous cover, and percent bare surface at 500-m spatial resolution (Hansen et al., 2003). However, the VCF data product was designed to map percent tree cover, and a major limitation of the VCF herbaceous layer is that it does not discriminate between woody shrubs and grasses, which is important for discriminating between C3 and C4 vegetation (Hansen et al., 2003; Still et al., 2003; Powell et al., 2012). To address this shortcoming, I first created a percent non-tree layer by subtracting the adjusted percent tree-crown layer and the VCF percent bare layer from 100, assuming everything that is not tree crown or bare is non-tree (non-tree = 100 - treecrown + bare) (Powell et al. 2012). I then accounted for woody (shrub) vegetation in the herbaceous layer by using the land-cover descriptions for the 17-class International Geosphere-Biosphere Programme (IGBP) land-cover classification,included in the MODIS 500-m Global Land Cover Type product for 2001 (Loveland & Belward 1997). The non-tree layer pixels that correspond to the land-cover classes described as predominantly shrub and/or tree by the IGBP classification system were assigned to the shrub layer, and for pixels that correspond to classes composed predominantly of herbaceous cover, the non-tree layer is assigned as herbaceous. For pixels that correspond to IGBP classes composed of a mixture of shrubs and grasses (i.e., open shrub-land, woody savanna, and savanna) non-tree layer were partitioned into shrub and herbaceous following the maximum herbaceous cover rules described by Powell et al. (2012). Finally, I accounted for managed agro-ecosystems by subtracting per-pixel percent crop and pasture global data from the global crop and pasture lands in 2000 area data set (Ramankutty et al., 2008). The remaining percent herbaceous vegetation is the percent natural herbaceous vegetation layer.
}
\references{
Powell, R.L., Yoo, E-H., & Still, C.J. (2012) Vegetation and soil carbon-13 isoscapes for
South America: integrating remote sensing and ecosystem isotope measurements.
Ecosphere, 3, 109.

Friedl, M.A., Sulla-Menashe, D., Tan, B., Schneider, A., Ramankutty, N., Sibley, A., & Huang, X. (2010). MODIS Collection 5 global land cover: Algorithm refinements and characterization of new datasets. Remote Sensing of Environment, 114:168-182.

Hansen, M.C., DeFries, R.S., Townshend, J.R.G., Carroll, M., Cimiceli, C., & Sohlberd,
R.A. (2003) Global percent tree cover at a spatial resolution of 500 meters: first results of
the MODIS vegetation continuous fields algorithm. Earth Interactions, 7, 10.

Ramankutty, N., Evan, A.T., Monfreda, C., & Foley, J.A. (2008) Farming the Planet: 1.
Geographic distribution of global agricultural lands in the year 2000. Global
Biogeochemical Cycles.
}
\keyword{datasets}
\keyword{herbaceous}
