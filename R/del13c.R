##this function applies an endmember value to a corresponding vegetation layer
del13c <- function(total_veg, PFTlayer, endmember, filename ='', ...) {
  out <- raster(total_veg)
  big <- ! canProcessInMemory(out, 2)
  filename <- trim(filename)
  if (big & filename == '') {
    filename <- rasterTmpFile()
    }
  if (filename != '') {
    out <- writeStart(out, filename, ...)
    todisk <- TRUE
  }else{
    vv <- matrix(ncol=nrow(out[[1]]), nrow=ncol(out[[1]]))
    todisk <- FALSE
  }
  if (todisk) {
    for (i in 1:nrow(out)) {
      t <- getValuesBlock(total_veg, r)
      v1 <- getValuesBlock(PFTlayer, r)
      v <- ifelse(t > 0, (endmember)*(v1/(t)), 0)
      out <- writeValues(out, v, r)}
    out <- writeStop(out)
    }else{
      for (r in 1:nrow(out)) {
        t <- getValues(total_veg, r)
        v1 <- getValuesBlock(PFTlayer, r)
        v <- ifelse(t > 0, (endmember)*(v1/(t)), 0)
        vv[,r] <- v}
      out <- setValues(out, as.vector(vv))}
    return(out)
}
