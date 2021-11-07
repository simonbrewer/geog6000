library(tidyverse)
library(sf)
library(INLA)
library(spdep)
library(tmap)
library(ggpubr)

ufos <- st_read("../data/ufo_sf_annual.shp")

## 2014
ufos_2014 <- ufos %>%
  filter(year == 2014)

tm_shape(ufos_2014) + tm_fill("count", style = "jenks") +
  tm_borders() +
  tm_layout(main.title = "UFO sightings 2014")

tm_shape(ufos_2014) + tm_fill("population", style = "jenks") +
  tm_borders() +
  tm_layout(main.title = "State population 2014")

## Basic linear regression
ggscatter(ufos_2014, x = "population", y = "count",
          main = "UFO sightings vs. population",
          add = "reg.line")
res_lm <- lm(count ~ population, ufos_2014)
summary(res_lm)

mod1 <- inla(count ~ population,
             data = ufos_2014,
             control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
             control.predictor = list(compute = TRUE))
summary(mod1)

tmp = inla.tmarginal(function(x) x, mod1$marginals.fixed$`(Intercept)`)
p1 = ggline(as.data.frame(tmp), x = "x", y = "y", 
            numeric.x.axis = TRUE, plot_type = "l", main = "Post. estimates of intercept") +
  geom_vline(xintercept = 0)
tmp = inla.tmarginal(function(x) x, mod1$marginals.fixed$population)
p2 = ggline(as.data.frame(tmp), x = "x", y = "y", 
            numeric.x.axis = TRUE, plot_type = "l", main = "Post. estimates of popn") +
  geom_vline(xintercept = 0)
ggarrange(p1, p2)

tmp = inla.tmarginal(function(x) 1/x, mod1$marginals.hyperpar$`Precision for the Gaussian observations`)
ggline(as.data.frame(tmp), x = "x", y = "y", 
       numeric.x.axis = TRUE, plot_type = "l", main = "Posterior estimates of variance")

mod2 <- inla(count ~ population,
             data = ufos_2014,
             control.fixed = list(mean = 0, prec = 0.2, 
                                  mean.intercept = 0, prec.intercept = 0.001),
             control.family=list(hyper=list(prec=list(prior='loggamma', 
                                                      param=c(0.1,0.1)))),
             control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
             control.predictor = list(compute = TRUE))
summary(mod2)

#ufos_2014$idarea <- 1:nrow(ufos_2014)
ufos_nb <- poly2nb(ufos_2014)
ufos_W <- nb2mat(ufos_nb)
plot(st_geometry(ufos_2014), reset = FALSE)
plot(ufos_nb, st_coordinates(st_centroid(ufos_2014)), add = TRUE, col = 2, lwd = 2)

mod3 <- inla(count ~ population + f(struct, model = "bym", graph = ufos_W), 
             data = ufos_2014,
             control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
             control.predictor = list(compute = TRUE)
)
summary(mod3)
ufos_2014$u <- mod3$summary.random$struct$mean[1:49]
tm_shape(ufos_2014) + tm_fill("u") + 
  tm_borders() + tm_layout(main.title = "UFO SIR 2014")

## Epidemiology

## Expected numbers
ufos_2014$Ei <- (sum(ufos_2014$count) / sum(ufos_2014$population)) * ufos_2014$population

ufos_2014$SIR <- ufos_2014$count / ufos_2014$Ei
tm_shape(ufos_2014) + tm_fill("SIR") + 
  tm_borders() + tm_layout(main.title = "UFO SIR 2014")

prior <- list(
  prec = list(
    prior = "pc.prec",
    param = c(0.5 / 0.31, 0.01)),
  phi = list(
    prior = "pc",
    param = c(0.5, 2 / 3))
)

mod4 <- inla(count ~ 1 + f(idarea, model = "bym2", graph = ufos_W), 
             data = ufos_2014, family = "poisson", E = Ei, 
             control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
             control.predictor = list(compute = TRUE)
)
summary(mod4)

ufos_2014$RR <- mod4$summary.fitted.values[, "mean"]

tm_shape(ufos_2014) + tm_fill("RR", palette = "-inferno") +
  tm_borders() +
  tm_layout(main.title = "RR")

ufos_2014$exc <- sapply(mod4$marginals.fitted.values,
                       function(marg){1 - inla.pmarginal(q = 2, marginal = marg)})

tm_shape(ufos_2014) + tm_fill("exc", palette = "Blues") +
  tm_borders() +
  tm_layout(main.title = "Excedence probability (RR > 2)")

airports <- read.csv("airports.csv")
ufos_2014 <- full_join( ufos_2014, airports, by = c("postal" = "state"))

ggscatter(ufos_2014, x = "airports", y = "count",
          main = "UFO sightings vs. airports",
          add = "reg.line") #+ xscale("log10", .format = TRUE)  + yscale("log10", .format = TRUE)

mod5 <- inla(count ~ airports + f(struct, model = "bym2", graph = ufos_W), 
             data = ufos_2014, family = "poisson", E = Ei, 
             control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
             control.predictor = list(compute = TRUE)
)
summary(mod5)
mod5$summary.fixed

tmp = inla.tmarginal(function(x) x, mod5$marginals.fixed$`(Intercept)`)
p1 = ggline(as.data.frame(tmp), x = "x", y = "y", 
            numeric.x.axis = TRUE, plot_type = "l", main = "Post. estimates of intercept") +
  geom_vline(xintercept = 0)
tmp = inla.tmarginal(function(x) x, mod5$marginals.fixed$airports)
p2 = ggline(as.data.frame(tmp), x = "x", y = "y", 
            numeric.x.axis = TRUE, plot_type = "l", main = "Post. estimates of airports") +
  geom_vline(xintercept = 0)
ggarrange(p1, p2)

ufos_2014$RR <- mod5$summary.fitted.values[, "mean"]

tm_shape(ufos_2014) + tm_fill("RR") +
  tm_borders() +
  tm_layout(main.title = "RR")


## ST model
ggscatter(ufos, x = "population", y = "count", col = "postal", 
          legend = "none", add = "reg.line",
          main = "UFO Sightings 1990-2014")  + xscale("log10")

sum(ufos$count) / sum(ufos$population) 
ufos$Ei <- (sum(ufos$count) / sum(ufos$population)) * ufos$population

### Priors
prior <- list(
  prec = list(
    prior = "pc.prec",
    param = c(0.5 / 0.31, 0.01)),
  phi = list(
    prior = "pc",
    param = c(0.5, 2 / 3))
)
u = 1
## From INLA Germany example (time should be a RW)
u = 0.2/0.31
alpha = 0.01
rw1_prior <- list(theta = list(prior="pc.prec", param=c(u, alpha)))
rw1_prior <- list(prec = list(param = c(0.001, 0.001)))

mod6 <- inla(count ~ 1 + f(struct, model = "bym2", graph = ufos_W) +
               f(year, model = "rw1", hyper = rw1_prior), #temporal random effect
             data = ufos, family = "poisson", E = Ei, 
             control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
             control.predictor = list(compute = TRUE)
)
summary(mod6)
mod6$summary.fixed

tmp <- mod6$summary.random$year
ggline(tmp, x = "ID", y = "mean",
       numeric.x.axis = TRUE, xlab= "year")

ggplot(tmp) + geom_line(aes(x = ID, y = mean)) +
  geom_ribbon(aes(ymin = `0.025quant`, ymax = `0.975quant`))

h <- ggplot(tmp, aes(ID))
h + geom_ribbon(aes(ymin = `0.025quant`, ymax = `0.975quant`), fill = "grey70") +
  geom_line(aes(y = mean)) + theme_bw() +
  scale_x_continuous("Year") +
  ggtitle("Relative risk trend")


ufos$RR <- mod6$summary.fitted.values[, "mean"]

myyear = 2000
tmp <- ufos %>%
  filter(year == myyear)

p1 <- tm_shape(tmp) + tm_fill("RR", palette = "-magma",
                        title = paste("RR:",myyear))

myyear = 2010
tmp <- ufos %>%
  filter(year == myyear)

p2 <- tm_shape(tmp) + tm_fill("RR", palette = "-magma",
                        title = paste("RR:",myyear))

# p1 <- tm_shape(tmp) + tm_fill("RR", title = myyears[i])
tmap_arrange(p1, p2)

