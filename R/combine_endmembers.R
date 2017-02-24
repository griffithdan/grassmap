combine_endmembers <- function(..., landmask, filename = '') {
  ##takes in series of endmember layers, compiles into list of raster layers, and combines into final isoscape
  final <- do.call("sum", list(...))

  ##0s are set to NA, these are areas without isotopic signature
  final[final==0] <- NA
  ##apply landmask to remove any values outside region of interest
  final <- final*landmask

  if (filename != '') {
    writeRaster(final, filename, ...)
  }

  ##return final isoscape!
  return(final)

}

