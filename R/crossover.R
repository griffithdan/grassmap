##This function generates a stack of per-pixel plant functional type dominance or growing season classifications
##based on a crossover threshold
crossover <- function(climate.stack, threshold, filename = '', ...) {

  if(nlayers(climate.stack) == 1) {

    makemat <- TRUE
    out <- raster(climate.stack)

  }else{

    makemat <- FALSE
    out <- brick(climate.stack, nl=nlayers(climate.stack), values=FALSE)}

  big <- ! canProcessInMemory(out, 3)
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

    for (r in 1:nrow(out)) {
      v <- getValues(climate.stack, r)
      ##if pixel value is greater than or equal to threshold, pixel = 1, if less the pixel is 0
      v <- ifelse(v>=threshold, 1, 0)
      out <- writeValues(out, v, r)}
    out <- writeStop(out)


  }else{

    if(makemat){

      for (r in 1:nrow(out)) {
        v <- getValues(climate.stack, r)
        ##if pixel value is greater than or equal to threshold, pixel = 1, if less the pixel is 0
        v <- ifelse(v>=threshold, 1, 0)
        vv[,r] <- v}

      out <- setValues(out, as.vector(vv))

    }else{

      for (q in 1:nlayers(out)){
        for (r in 1:nrow(out)) {
          v <- getValues(climate.stack[[q]], r)
          ##if pixel value is greater than or equal to threshold, pixel = 1, if less the pixel is 0
          v <- ifelse(v>=threshold, 1, 0)
          vv[,r] <- v}

        out <- setValues(out, as.vector(vv), layer = q)}
    }}
  ##output is a rasterStack equal to the number of layers of the input rasterStack
  return(out)
}
