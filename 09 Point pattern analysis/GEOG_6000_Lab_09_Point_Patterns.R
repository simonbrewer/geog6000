## ----echo=FALSE-----------------------------------------------------------------------------
options(width=50)
set.seed(1234)


## ----message=FALSE--------------------------------------------------------------------------
require(spatstat)


## -------------------------------------------------------------------------------------------
bei <- read.csv("../datafiles/bei.csv")
summary(bei)
bei.owin <- owin(xrange = c(0,1000), yrange = c(0,500))
bei.ppp <- ppp(bei$x, bei$y, window = bei.owin)


## -------------------------------------------------------------------------------------------
plot(bei.ppp, pch = 16, cols = 'gray')
summary(bei.ppp)


## ----fig.keep='high'------------------------------------------------------------------------
bei.qc <- quadratcount(bei.ppp, nx = 6, ny = 3)
plot(bei.qc)


## ----fig.keep='high', results='hide'--------------------------------------------------------
bei.qt <- quadrat.test(bei.ppp, nx = 6, ny = 3)
bei.qt
plot(bei.qt, cex=0.8)


## ----fig.keep='high'------------------------------------------------------------------------
bei.slope <- read.csv("../datafiles/beislope.csv", header = FALSE)
bei.slope.owin <- owin(xrange = c(-2.5, 1002.5), yrange = c(-2.5, 502.5))
bei.slope <- as.im(as.matrix(bei.slope), W = bei.slope.owin)
plot(bei.slope)
plot(bei.ppp, pch = 16, cex = 0.7, add = TRUE)


## ----fig.keep='high'------------------------------------------------------------------------
b <- quantile(bei.slope, probs = seq(0,1,by=0.25))
bei.slope.cut <- cut(bei.slope, breaks = b, labels = 1:4)
bei.slope.tess <- tess(image = bei.slope.cut)
plot(bei.slope.tess, valuesAreColours=FALSE)
plot(bei.ppp, add = TRUE, pch = "+")


## ----fig.keep='high'------------------------------------------------------------------------
qb <- quadratcount(bei.ppp, tess = bei.slope.tess)
plot(qb, valuesAreColours=FALSE)


## ----fig.keep='high', results='hide'--------------------------------------------------------
bei.qt <- quadrat.test(bei.ppp, tess = bei.slope.tess)
bei.qt
plot(bei.qt, cex  =0.8, valuesAreColours = FALSE)


## -------------------------------------------------------------------------------------------
plot(density(bei.ppp, sigma = 60))


## -------------------------------------------------------------------------------------------
bei.bw <- bw.diggle(bei.ppp)
plot(density(bei.ppp, sigma = bei.bw))


## ----echo=FALSE, message=FALSE--------------------------------------------------------------
require(maptools)
require(rgdal)
library(sf)

## ----fig.keep='high', message=FALSE, results='hide'-----------------------------------------

## Option 1
redwood.sf <- st_read("../datafiles/redwood/redwood.shp")
redwood.sp <- as(redwood.sf, "Spatial")
redwood.sp <- as(redwood.sp, "SpatialPoints")
redwood <- as(redwood.sp, "ppp")

## Option 2
redwood.sf <- st_read("../datafiles/redwood/redwood.shp")
redwood.sp <- as(redwood.sf, "Spatial")
redwood <- as(redwood.sp, "ppp")
marks(redwood) <- NULL

## Option 3
redwood.sf <- st_read("../datafiles/redwood/redwood.shp")
redwood <- ppp(redwood.sf$x, redwood.sf$y)

y <- as(redwood.sp, "SpatialPoints")
redwood <- as(y, "ppp")
redwood.owin <- owin(x = c(0, 1), y = c(0, 1))
redwood <- redwood[redwood.owin]
plot(redwood)


## ----fig.keep='high'------------------------------------------------------------------------
redwood.kest <- Kest(redwood)
plot(redwood.kest)


## -------------------------------------------------------------------------------------------
redwood.kest.mc <- envelope(redwood, fun = 'Kest', 
                            nsim = 99, verbose = FALSE)
plot(redwood.kest.mc, shade = c("hi", "lo"))


## ----fig.keep='high'------------------------------------------------------------------------
redwood.kest.mc <- envelope(redwood, fun = 'Kest', 
                            nsim = 99, verbose = FALSE, global = TRUE)
plot(redwood.kest.mc, shade = c("hi", "lo"))


## ----fig.keep='high'------------------------------------------------------------------------
redwood.lest.mc <- envelope(redwood, fun = 'Lest', 
                            nsim = 99, verbose = FALSE, global = TRUE)
plot(redwood.lest.mc, shade = c("hi", "lo"))


## ----fig.keep='high'------------------------------------------------------------------------
redwood.pcf.mc <- envelope(redwood, fun = 'pcf', 
                            nsim = 99, verbose = FALSE)
plot(redwood.pcf.mc, shade=c("hi", "lo"), ylim = c(0,5))


## -------------------------------------------------------------------------------------------
lansing <- read.csv("../datafiles/lansing/lansing.csv")
str(lansing)


## ----fig.keep='high', message=FALSE, results='hide'-----------------------------------------
lansing$species <- as.factor(lansing$species)
lansing.win <- readOGR("../datafiles/lansing/lansing.shp")
lansing.ppp <- ppp(lansing$x, lansing$y, win = lansing.win, 
                   marks = lansing$species)
plot(lansing.ppp)


## ----fig.keep='high'------------------------------------------------------------------------
plot(split(lansing.ppp))
plot(split(lansing.ppp)$maple)


## ----fig.keep='high'------------------------------------------------------------------------
plot(density(split(lansing.ppp)))


## ----cache=FALSE----------------------------------------------------------------------------
lansing.kc <- envelope(lansing.ppp, Kcross, 
                      i = "blackoak", j = "hickory",
                      nsim = 99, verbose = FALSE)


## ----fig.keep='high'------------------------------------------------------------------------
plot(lansing.kc)


## -------------------------------------------------------------------------------------------
fit0 <- ppm(bei.ppp, ~ 1)
fit0


## -------------------------------------------------------------------------------------------
exp(coef(fit0))


## -------------------------------------------------------------------------------------------
summary(bei.ppp)


## ----results='hide'-------------------------------------------------------------------------
fit1 <- ppm(bei.ppp, ~ polynom(x, y, 2))
fit1


## ----fig.keep='high'------------------------------------------------------------------------
plot(fit1, how = 'image', se = FALSE, pause = FALSE)


## ----fig.keep='high'------------------------------------------------------------------------
fit2 <- ppm(bei.ppp, ~ bei.slope)
fit2


## ----fig.keep='high'------------------------------------------------------------------------
plot(fit2, how = 'image', se = FALSE, pause = FALSE)


## ----eval=FALSE-----------------------------------------------------------------------------
## # Tree locations
## urkiola.sp <- readOGR("./urkiola/urkiola.shp")
## # Park boundary
## urkiola.win <- readOGR("./urkiola/urkiolaWindow.shp")
## # First convert boundary to owin object
## y <- as(urkiola.win, "SpatialPolygons")
## # Then convert SpatialPolygons to owin class
## urkiola.owin <- as(y, "owin")
## # Now get tree coordinates
## urkiola.x <- coordinates(urkiola.sp)[,1]
## urkiola.y <- coordinates(urkiola.sp)[,2]
## # Finally make up ppp object using coordinates, tree names and owin
## urkiola.ppp <- ppp(urkiola.x, urkiola.y,
##                    window = urkiola.owin,
##                    marks = urkiola.sp$tree)

