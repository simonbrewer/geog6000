## Fix Oregon data

library(sf)
library(stars)

orotl <- st_read("../datafiles/oregon/orotl.shp")
st_crs(orotl) <- 4326
ortann <- st_read("../datafiles/oregon/oregontann.shp")
st_crs(ortann) <- 4326
# Oregon DEM file
orgrid <- st_read("../datafiles/oregon/orgrid.shp") 
st_crs(orgrid) <- 4326
orgrid.dem <- st_rasterize(orgrid, dx = 0.1667, dy = 0.1667)
plot(orgrid.dem, reset = FALSE)
plot(ortann["tann"], add = TRUE, pch = 16, cex = 1.5)

ortann.var = variogram(tann ~ 1, ortann, cutoff = 300)
plot(ortann.var)

ortann.sph.vgm = vgm(psill = 4, model = "Sph", range = 175, nugget = 0.1)
plot(ortann.var, ortann.sph.vgm)

ortann.sph.vgm = fit.variogram(ortann.var, ortann.sph.vgm)
plot(ortann.var, ortann.sph.vgm)

ortann.ok = krige(tann ~ 1, ortann, orgrid.dem, ortann.sph.vgm)

plot(ortann.ok)
