# convert spatstat objects to sp classes

owin2Polygons <- function(x, id="1") {
  stopifnot(is.owin(x))
  x <- as.polygonal(x)
  closering <- function(df) { df[c(seq(nrow(df)), 1), ] }
  pieces <- lapply(x$bdry,
                   function(p) {
                     Polygon(coords=closering(cbind(p$x,p$y)),
                             hole=is.hole.xypolygon(p))  })
  z <- Polygons(pieces, id)
  return(z)
}

tess2SP <- function(x) {
  stopifnot(is.tess(x))
  y <- tiles(x)
  nam <- names(y)
  z <- list()
  for(i in seq(y))
    z[[i]] <- owin2Polygons(y[[i]], nam[i])
  return(SpatialPolygons(z))
}

owin2SP <- function(x) {
  stopifnot(is.owin(x))
  y <- owin2Polygons(x)
  z <- SpatialPolygons(list(y))
  return(z)
}

data(lansing)
write.csv(as.data.frame(lansing)[-600,], "lansing.csv", row.names=FALSE)

lansing.sp = owin2SP(lansing$window)
centroids <- coordinates(lansing.sp)
x <- centroids[,1]
y <- centroids[,2]

SpatialPolygonsDataFrame(lansing.sp,data=data.frame(x=x, y=y, row.names=row.names(lansing.sp))) -> lansing.spdf

writeSpatialShape(lansing.spdf, "lansing")

