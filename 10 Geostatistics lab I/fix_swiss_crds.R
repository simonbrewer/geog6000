## Update swiss data with offset

swiss <- read.csv("../datafiles/swiss/swiss_ppt_old.csv")
swiss$x = swiss$x - 17791.29 + 2672591
swiss$y = swiss$y - 13224.66 + 1200225

write.csv(swiss, "../datafiles/swiss/swiss_ppt.csv", row.names = FALSE)

head(swiss)
swiss.sf <- st_as_sf(swiss, 
                     coords = c("x", "y"),
                     crs = 2056)

cnty <- st_read("../datafiles/ne_50m_admin_0_countries/ne_50m_admin_0_countries.shp")
swiss.bord <- subset(cnty, NAME == "Switzerland")
swiss.bord <- st_transform(swiss.bord, crs = 2056)
plot(swiss.bord)

tmap_mode("view")
tm_shape(swiss.bord) + tm_borders() +
  tm_shape(swiss.sf) + tm_symbols("ppt")

ggplot() + 
  geom_sf(data = swiss.bord) +
  geom_sf(data = swiss.sf, aes(col = ppt), size = 2.5) +
  theme_bw()





tm_shape(swiss.sf) + tm_symbols("ppt")

cnty <- st_read("../datafiles/ne_50m_admin_0_countries/ne_50m_admin_0_countries.shp")

stop()
swiss.bord <- st_read("../datafiles/borders/borders.shp")


x <- as.data.frame(st_coordinates(swiss.bord))
x$X <- x$X - 17791.29 + 2672591
x$Y <- x$Y - 13224.66 + 1200225
x$L2 <- 1
library(sfheaders)

sf <- sfheaders::sf_multipolygon(
  obj = x
  , x = "X"
  , y = "Y"
  , polygon_id = "L2"
)

st_crs(sf) <- 2056
plot(sf)
tm_shape(sf) + tm_borders()


library(sp)
library(rgdal)
swiss.sp <- readOGR("../datafiles/borders/borders.shp")
proj4string(swiss.sp) <- CRS("+init=epsg:2056")

newx = 2600000 - 17791.29 + 2672591
newx
newy = 1200000 - 13224.66 + 1200225
newy
proj4string(swiss.sp) <- CRS("+proj=somerc +lat_0=46.9524055555556 +lon_0=7.43958333333333 +k_0=1 +x_0=2600000 +y_0=1200000 +ellps=bessel +towgs84=674.374,15.056,405.346,0,0,0,0 +units=m +no_defs")

tm_shape(swiss.sp) + tm_lines()

ggplot() + 
  # geom_sf(data = swiss.bord) +
  geom_sf(data = sf, fill = "blue") +
  theme_bw()
