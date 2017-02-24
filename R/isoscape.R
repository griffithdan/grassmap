isoscape <- function(temperature, precip, herb, woody=NULL, c3.crop=NULL, c4.crop=NULL, biomass.weight=NULL,
                          temp.threshold=22, precip.threshold=25, c4.threshold = NULL, c3.endmember=-26.7, c4.endmember=-12.5,
                          woody.endmember=-27.2, c3.sd=2.3, c4.sd=1.1, woody.sd=2.5, filename = '', ...){

  ##apply crossover function with standard thresholds to temperature and precipitation layer
  crossover_temp <- crossover(climate.stack = temperature, threshold= temp.threshold)
  crossover_precip <- crossover(climate.stack = precip, threshold= precip.threshold)

  if(nlayers(temperature) == 1) {
    ##sum crossover layers to determine how many times in given period a pixel meets C4 climate threhsold
    pftlayers <- PFTmasks(tempmask = crossover_temp, precipmask = crossover_precip, c4.threshold = c4.threshold, biomass.weight = NULL)
    ##create plant functional type masks from growing season sum based on pre-defined count threshold
    ##resample PFT layers (3) to match the spatial resolution of the herbaceous layer
    ##partition natural herbaceous layer into c3, c4 and mixed zones
    natherb <- partitionNatHerb(nat.herb = herb, c3.clim = pftlayers[[1]], c4.clim = pftlayers[[2]], mixed.clim=NULL, mixed.ratio=NULL)
    ##reclassify total layer to identify land mask
    mask <- c(-Inf, Inf, 1)
    rcl_mask <- matrix(mask, ncol=3, byrow = TRUE)
    lndmsk <- reclassify(temperature[[1]], rcl_mask)

    if(is.null(woody)){

      if(is.null(c3.crop)){
        ##set total
        total <- herb
        ##calcualte herbaceous endmembers and create isoscape
        C3endmember <- del13c(total_veg = total, natherb[[1]], c3.endmember)
        C4endmember <- del13c(total_veg = total, natherb[[2]], c4.endmember)
        isoscape <- combine_endmembers(C3endmember, C4endmember, landmask =  lndmsk)
        ##repeat standard deviation layer
        C3endmember_sd <- del13c(total_veg = total, natherb[[1]], c3.sd)
        C4endmember_sd <- del13c(total_veg = total, natherb[[2]], c4.sd)
        sd_isoscape <- combine_endmembers(C3endmember_sd, C4endmember_sd, landmask =  lndmsk)

      }else{

        ##set total
        total <- herb + c3.crop + c4.crop
        ##sum c3 and c4 layers
        c3.all <- sum(natherb[[1]], c3.crop)
        c4.all <- sum(natherb[[2]], c4.crop)
        ##calculate herbaceous endmembers and create isoscape
        C3endmember <- del13c(total_veg = total, c3.all, c3.endmember)
        C4endmember <- del13c(total_veg = total, c4.all, c4.endmember)
        isoscape <- combine_endmembers(C3endmember, C4endmember, landmask =  lndmsk)
        ##repeat standard deviation layer
        C3endmember_sd <- del13c(total_veg = total, c3.all, c3.sd)
        C4endmember_sd <- del13c(total_veg = total, c4.all, c4.sd)
        sd_isoscape <- combine_endmembers(C3endmember_sd, C4endmember_sd, landmask =  lndmsk)}

    }else{

      if(is.null(c3.crop)){
        ##set total
        total <- herb + woody
        ##calcualte herbaceous endmembers and create isoscape
        C3endmember <- del13c(total_veg = total, natherb[[1]], c3.endmember)
        C4endmember <- del13c(total_veg = total, natherb[[2]], c4.endmember)
        Woodyendmember <- del13c(total_veg = total, woody, woody.endmember)
        ##calculate into final isoscape layer
        isoscape <- combine_endmembers(C3endmember, C4endmember, Woodyendmember, landmask =  lndmsk)
        ##repeat standard deviation layer
        C3endmember_sd <- del13c(total_veg = total, natherb[[1]], c3.sd)
        C4endmember_sd <- del13c(total_veg = total, natherb[[2]], c4.sd)
        woodyendmember_sd <- del13c(total_veg = total, woody, woody.sd)
        sd_isoscape <- combine_endmembers(C3endmember_sd, C4endmember_sd, woodyendmember_sd, landmask =  lndmsk)

      }else{

        ##set total
        total <- herb + woody + c3.crop + c4.crop
        ##sum c3 and c4 layers
        c3.all <- sum(natherb[[1]], c3.crop)
        c4.all <- sum(natherb[[2]], c4.crop)
        ##calculate herbaceous endmembers and create isoscape
        C3endmember <- del13c(total_veg = total, c3.all, c3.endmember)
        C4endmember <- del13c(total_veg = total, c4.all, c4.endmember)
        Woodyendmember <- del13c(total_veg = total, woody, woody.endmember)
        ##calculate into final isoscape layer
        isoscape <- combine_endmembers(C3endmember, C4endmember, Woodyendmember, landmask =  lndmsk)
        ##repeat standard deviation layer
        C3endmember_sd <- del13c(total_veg = total, c3.all, c3.sd)
        C4endmember_sd <- del13c(total_veg = total, c4.all, c4.sd)
        woodyendmember_sd <- del13c(total_veg = total, woody, woody.sd)
        sd_isoscape <- combine_endmembers(C3endmember_sd, C4endmember_sd, woodyendmember_sd, landmask =  lndmsk)}}

    }else{

      ##sum crossover layers to determine how many times in given period a pixel meets C4 climate threhsold
      pftlayers <- PFTmasks(tempmask = crossover_temp, precipmask = crossover_precip, c4.threshold = c4.threshold, biomass.weight = NULL)
      ##create plant functional type masks from growing season sum based on pre-defined count threshold
      ##resample PFT layers (3) to match the spatial resolution of the herbaceous layer
      ##partition natural herbaceous layer into c3, c4 and mixed zones
      natherb <- partitionNatHerb(nat.herb = herb, c3.clim = pftlayers[[1]], c4.clim = pftlayers[[2]], mixed.clim = pftlayers[[3]], mixed.ratio = pftlayers[[4]])
      ##reclassify total layer to identify land mask
      mask <- c(-Inf, Inf, 1)
      rcl_mask <- matrix(mask, ncol=3, byrow = TRUE)
      lndmsk <- reclassify(temperature[[1]], rcl_mask)

      if(is.null(woody)){

        if(is.null(c3.crop)){
          ##set total
          total <- herb
          ##calcualte herbaceous endmembers and create isoscape
          C3endmember <- del13c(total_veg = total, natherb[[1]], c3.endmember)
          C4endmember <- del13c(total_veg = total, natherb[[2]], c4.endmember)
          isoscape <- combine_endmembers(C3endmember, C4endmember, landmask =  lndmsk)
          ##repeat standard deviation layer
          C3endmember_sd <- del13c(total_veg = total, natherb[[1]], c3.sd)
          C4endmember_sd <- del13c(total_veg = total, natherb[[2]], c4.sd)
          sd_isoscape <- combine_endmembers(C3endmember_sd, C4endmember_sd, landmask =  lndmsk)

          }else{

            ##set total
            total <- herb + c3.crop + c4.crop
            ##sum c3 and c4 layers
            c3.all <- sum(natherb[[1]], c3.crop)
            c4.all <- sum(natherb[[2]], c4.crop)
            ##calculate herbaceous endmembers and create isoscape
            C3endmember <- del13c(total_veg = total, c3.all, c3.endmember)
            C4endmember <- del13c(total_veg = total, c4.all, c4.endmember)
            isoscape <- combine_endmembers(C3endmember, C4endmember, landmask =  lndmsk)
            ##repeat standard deviation layer
            C3endmember_sd <- del13c(total_veg = total, c3.all, c3.sd)
            C4endmember_sd <- del13c(total_veg = total, c4.all, c4.sd)
            sd_isoscape <- combine_endmembers(C3endmember_sd, C4endmember_sd, landmask =  lndmsk)}

        }else{

          if(is.null(c3.crop)){
            ##set total
            total <- herb + woody
            ##calcualte herbaceous endmembers and create isoscape
            C3endmember <- del13c(total_veg = total, natherb[[1]], c3.endmember)
            C4endmember <- del13c(total_veg = total, natherb[[2]], c4.endmember)
            Woodyendmember <- del13c(total_veg = total, woody, woody.endmember)
            ##calculate into final isoscape layer
            isoscape <- combine_endmembers(C3endmember, C4endmember, Woodyendmember, landmask =  lndmsk)
            ##repeat standard deviation layer
            C3endmember_sd <- del13c(total_veg = total, natherb[[1]], c3.sd)
            C4endmember_sd <- del13c(total_veg = total, natherb[[2]], c4.sd)
            woodyendmember_sd <- del13c(total_veg = total, woody, woody.sd)
            sd_isoscape <- combine_endmembers(C3endmember_sd, C4endmember_sd, woodyendmember_sd, landmask =  lndmsk)

            }else{

              ##set total
              total <- herb + woody + c3.crop + c4.crop
              ##sum c3 and c4 layers
              c3.all <- sum(natherb[[1]], c3.crop)
              c4.all <- sum(natherb[[2]], c4.crop)
              ##calculate herbaceous endmembers and create isoscape
              C3endmember <- del13c(total_veg = total, c3.all, c3.endmember)
              C4endmember <- del13c(total_veg = total, c4.all, c4.endmember)
              Woodyendmember <- del13c(total_veg = total, woody, woody.endmember)
              ##calculate into final isoscape layer
              isoscape <- combine_endmembers(C3endmember, C4endmember, Woodyendmember, landmask =  lndmsk)
              ##repeat standard deviation layer
              C3endmember_sd <- del13c(total_veg = total, c3.all, c3.sd)
              C4endmember_sd <- del13c(total_veg = total, c4.all, c4.sd)
              woodyendmember_sd <- del13c(total_veg = total, woody, woody.sd)
              sd_isoscape <- combine_endmembers(C3endmember_sd, C4endmember_sd, woodyendmember_sd, landmask =  lndmsk)}}}

  ##stack isoscape and standard deviation layer to an output
  output <- stack(isoscape, sd_isoscape)
  if (filename != '') {
    writeRaster(output, filename, ...)
  }
  ##return isoscapes
  return(output)
}

