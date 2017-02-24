partitionNatHerb <- function(nat.herb, c3.clim, c4.clim, mixed.clim=NULL, mixed.ratio=NULL, filename ='', ...) {

  out <- brick(nat.herb, nl=2, values=FALSE)

  big <- ! canProcessInMemory(out, 6)
  filename <- trim(filename)

  if (big & filename == '') {
    filename <- rasterTmpFile()
  }

  if (filename != '') {

    out <- writeStart(out, filename, ...)
    todisk <- TRUE

  }else{

    vv <- matrix(ncol=nrow(out), nrow=ncol(out))
    vv2 <- matrix(ncol=nrow(out), nrow=ncol(out))
    todisk <- FALSE

    }

  if (todisk) {

    if(is.null(mixed.clim)){

      for (r in 1:nrow(out)) {
        x <- getValues(c3.clim, r)
        y <- getValues(c4.clim, r)
        z <- getValues(nat.herb, r)

        ##if pixel value is greater than or equal to threshold, pixel = 1, if less the pixel is 0
        v <- x*z
        v2 <- y*z

        v.f <- matrix(c(v, v2), nrow = length(v), ncol =2)

        out <- writeValues(out, v.f, r)}

      out <- writeStop(out)

    }else{

      for (r in 1:nrow(out)) {
        t <- getValues(mixed.clim, r)
        w <- getValues(mixed.ratio, r)
        x <- getValues(c3.clim, r)
        y <- getValues(c4.clim, r)
        z <- getValues(nat.herb, r)


        v <- (y*z) + (t*z*w)
        v2 <- (x*z) + ((t*z) - (t*z*w))

        v.f <- matrix(c(v2, v), nrow = length(v), ncol =2)

        out <- writeValues(out, v.f, r)}

      out <- writeStop(out)

  }}else{

    if(is.null(mixed.clim)){

      for (r in 1:nrow(out)) {
        x <- getValues(c3.clim, r)
        y <- getValues(c4.clim, r)
        z <- getValues(nat.herb, r)

        ##if pixel value is greater than or equal to threshold, pixel = 1, if less the pixel is 0

        v <- x*z
        v2 <- y*z
        vv[,r] <- v
        vv2[,r] <- v2

        out <- setValues(out, as.vector(vv), layer = 1)
        out <- setValues(out, as.vector(vv2), layer = 2)}

  }else{

    for (r in 1:nrow(out)) {
        t <- getValues(mixed.clim, r)
        w <- getValues(mixed.ratio, r)
        x <- getValues(c3.clim, r)
        y <- getValues(c4.clim, r)
        z <- getValues(nat.herb, r)


        v <- (y*z) + (t*z*w)
        v2 <- (x*z) + ((t*z) - (t*z*w))

        vv[,r] <- v
        vv2[,r] <- v2}

        out <- setValues(out, as.vector(vv), layer = 2)
        out <- setValues(out, as.vector(vv2), layer = 1)
  }}
  ##output is a rasterStack equal to the number of layers of the input rasterStack
  names(out) <- c("C3", "C4")
  return(out)
}

