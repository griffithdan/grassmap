NDVI <- function(NIRstack, REDstack, NAValue, scalefactor, filename = '', ...) {##input is monthly NIR and RED bands
  NAvalue(NIRstack) <- NAValue
  NAvalue(REDstack) <- NAValue

  if(nlayers(NIRstack) == 1) {

    makemat <- TRUE
    out <- raster(NIRstack)
  }else{

    makemat <- FALSE
    out <- brick(NIRstack, nl=nlayers(NIRstack), values=FALSE)}

  big <- ! canProcessInMemory(out, 3)
  filename <- trim(filename)
  if (big & filename == '') {

    filename <- rasterTmpFile()
  }
  if (filename != '') {

    out <- writeStart(out, filename, ...)
    todisk <- TRUE

  }else{

    vv <- matrix(ncol=nrow(out), nrow=ncol(out))
    todisk <- FALSE

  }

  if (todisk) {

    for (r in 1:nrow(out)) {
      x <- getValues(NIRstack, r)
      y <- getValues(REDstack, r)
      ##if pixel value is greater than or equal to threshold, pixel = 1, if less the pixel is 0
      v <- ((x-y)/(x+y))*scalefactor
      out <- writeValues(out, v, r)}
    out <- writeStop(out)

  }else{

    if(makemat){

      for (r in 1:nrow(out)) {
        x <- getValues(NIRstack, r)
        y <- getValues(REDstack, r)
        ##if pixel value is greater than or equal to threshold, pixel = 1, if less the pixel is 0
        v <- ((x-y)/(x+y))*scalefactor
        vv[,r] <- v}

      out <- setValues(out, as.vector(vv))

    }else{

      for (q in 1:nlayers(out)){
        for (r in 1:nrow(out)) {
          x <- getValues(NIRstack[[q]], r)
          y <- getValues(REDstack[[q]], r)
          ##if pixel value is greater than or equal to threshold, pixel = 1, if less the pixel is 0
          v <- ((x-y)/(x+y))*scalefactor
          vv[,r] <- v}

        out <- setValues(out, as.vector(vv), layer = q)

      }}
  }
  ##output is a rasterStack equal to the number of layers of the input rasterStack
  return(out)
}



