\name{partitionNatHerb}
\alias{partitionNatHerb}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Partition Natural Herbaceous Values by Plant Functional Type
}
\description{
This function divides the total natural herbaceous layer into percent C3 and percent C4 natural herbaceous layers based on crossover temperature values, while accounting for mixed dominance grasslands, by a weighted NDVI ratio.
}
\usage{
partitionNatHerb(nat.herb, c3.clim, c4.clim, mixed.clim, mixed.ratio, filename = '', ...)
}
\arguments{
  \item{nat.herb}{
RasterLayer. Per-pixel percentage of natural herbaceous vegetation.
}
  \item{c4.clim}{
RasterLayer. C4 dominance climate mask. Output of PFTmasks function. PFTmasks[2]
}
  \item{c3.clim}{
RasterLayer. C3 dominance climate mask. Output of PFTmasks function. PFTmasks[1]
}
  \item{mixed.clim}{
RasterLayer. Mixed plant functional type dominance climate mask. Output of PFTmasks function.
  PFTmasks[3]
}
  \item{mixed.ratio}{
RasterLayer. Plant functional type dominance ratio
}
  \item{filename}{
Character. Output filename (optional)
}
  \item{...}{
additional arguments for writing files as in writeRaster
}
}
\value{
RasterBrick
\item{Layer1 }{per-pixel percentage of C3 dominant natural herbaceous vegetation over period of interest}
\item{Layer2 }{per-pixel percentage of C4 dominant natural herbaceous vegetation over period of interest}
}
\author{
Sydney M Firmin
}
\examples{
##partitionNatHerb()
}
\keyword{ PFT }
\keyword{ Crossover }

