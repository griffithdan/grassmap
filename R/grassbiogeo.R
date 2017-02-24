grassbiogeo <- function(temperature, precip, herb, biomass.weight=NULL, c4.threshold = NULL,
                        temp.threshold=22, precip.threshold=25, percent.herb=FALSE, filename = '', ...){

  ##apply crossover function with standard thresholds to temperature and precipitation layer
  crossover_temp <- crossover(climate.stack = temperature, threshold= temp.threshold)
  crossover_precip <- crossover(climate.stack = precip, threshold= precip.threshold)

  if(nlayers(temperature) == 1) {

    ##sum crossover layers to determine how many times in given period a pixel meets C4 climate threhsold
    ##create plant functional type masks from growing season sum based on pre-defined count threshold
    pftlayers <- PFTmasks(tempmask = crossover_temp, precipmask = crossover_precip, c4.threshold = c4.threshold, biomass.weight = NULL)
    ##resample PFT layers (3) to match the spatial resolution of the herbaceous layer
    ##partition natural herbaceous layer into c3, c4 and mixed zones
    natherb <- partitionNatHerb(nat.herb = herb, c3.clim = pftlayers[[1]], c4.clim = pftlayers[[2]], mixed.clim = NULL, mixed.ratio = NULL)

    if(percent.herb){
      ##if percent vegetation is true, return C3/C4 normalized by the total herbaceous input
      output <- overlay(natherb, herb, fun=function(x, y){return(x/y)}, recycle=TRUE)
      names(output) <- c("Percent Vegetation C3", "Percent Vegetation C4")

    }else{

      ##if percent vegetation is false, return C3/C4 percent cover
      output <- natherb
      names(output) <- c("Percent Cover C3", "Percent Cover C4")

      }}else{

      ##create plant functional type masks from growing season sum based on pre-defined count threshold
      pftlayers <- PFTmasks(tempmask = crossover_temp, precipmask = crossover_precip, c4.threshold = c4.threshold, biomass.weight = biomass.weight)
      ##resample PFT layers (3) to match the spatial resolution of the herbaceous layer
      ##partition natural herbaceous layer into c3, c4 and mixed zones
      natherb <- partitionNatHerb(nat.herb = herb, c3.clim = pftlayers[[1]], c4.clim = pftlayers[[2]], mixed.clim = pftlayers[[3]], mixed.ratio = pftlayers[[4]])

      if(percent.herb){
        ##if percent vegetation is true, return C3/C4 normalized by the total herbaceous input
        output <- overlay(natherb, herb, fun=function(x, y){return(x/y)}, recycle=TRUE)
        names(output) <- c("Percent Vegetation C3", "Percent Vegetation C4")

        }else{

          ##if percent vegetation is false, return C3/C4 percent cover
          output <- natherb
          names(output) <- c("Percent Cover C3", "Percent Cover C4")}
      }
  if (filename != '') {
    writeRaster(output, filename, ...)
  }
  ##return isoscapes
  return(output)
}

