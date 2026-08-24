library(ncdf4)

ncid <- nc_open("./air.mon.mean.nc")
ncid

## Find variable names
names(ncid$var)

## Variable dimensions
ncid$var$air$size

## Get individual dimensions
## Longitude
ncid$var$air$dim[[1]]
ncid$var$air$dim[[1]]$vals
ncid$var$air$dim[[1]]$units

## Latitude
ncid$var$air$dim[[2]]
ncid$var$air$dim[[2]]$vals
ncid$var$air$dim[[2]]$units

## Time
ncid$var$air$dim[[3]]
ncid$var$air$dim[[3]]$vals
ncid$var$air$dim[[3]]$units

## Extract values for a region (note that lons are 0-360)
lonmn = 360 - 111.0
lonmx = 360 - 100.5
latmn = 30.75
latmx = 38.5

yrmn = 1901
yrmx = 2010

## Get attributes
mylon_att = ncatt_get(ncid, "lon")
mylat_att = ncatt_get(ncid, "lat")
mytime_att = ncatt_get(ncid, "time")
myvar_att = ncatt_get(ncid, "air")

## Longitude
mylon = ncvar_get(ncid, "lon")
gxmn = which(mylon >= lonmn)[1]
gxmx = rev(which(mylon <= lonmx))[1]
gxres = diff(mylon[gxmn:gxmx])[1]
glon = seq(mylon[gxmn], mylon[gxmx], by = gxres)
nx = length(glon)

## Latitude
mylat = ncvar_get(ncid, "lat")
gymn = rev(which(mylat >= latmn))[1]
gymx = which(mylat <= latmx)[1]
gyres = diff(mylat[gymn:gymx])[1]
glat = seq(mylat[gymn], mylat[gymx], by = gyres)
ny = length(glat)

## Get time indicies
mytime = ncvar_get(ncid, "time")
nt = length(mytime)
ncdates = as.POSIXct(mytime * 3600, 
                     origin = '1800-01-01 00:00',
                     tz = "GMT")
ncmnth = as.numeric(format(ncdates, "%m"))
ncyear = as.numeric(format(ncdates, "%Y"))
yearID = which(ncyear >= yrmn & ncyear <= yrmx)

## Finally extract array
myvar = ncvar_get(ncid,"air", 
                  start=c(gxmn,gymn,min(yearID)), 
                  count=c(nx,ny,max(yearID)))

## myvar is a 3D array
dim(myvar)

## Close file when done
nc_close(ncid)
