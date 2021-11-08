library(tidyverse)
library(sf)
library(INLA)
library(spdep)
library(tmap)
library(ggpubr)

## ----echo=FALSE, cache=FALSE, warning=FALSE, results='hide'--------------------------------------
ufos <- st_read("../datafiles/nuforc/ufo_sf_annual.shp")

## 2014
ufos_2014 <- ufos %>%
  filter(year == 2014)

p1 <- tm_shape(ufos_2014) + tm_fill("count", style = "jenks") +
  tm_borders() +
  tm_layout(main.title = "UFO sightings 2014")

p2 <- tm_shape(ufos_2014) + tm_fill("population", style = "jenks") +
  tm_borders() +
  tm_layout(main.title = "State population 2014")

tmap_arrange(p1, p2)


## ----message=FALSE,echo=FALSE--------------------------------------------------------------------
ufo_lm <- lm(count ~ population, ufos_2014)
summary(ufo_lm)


## ----message=FALSE-------------------------------------------------------------------------------
ufo_inla_lm <- inla(count ~ population,
                    data = ufos_2014,
                    control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                    control.predictor = list(compute = TRUE))


## ----message=FALSE,echo=FALSE--------------------------------------------------------------------
summary(ufo_inla_lm)
ufo_inla_lm$summary.fixed

## Posterior distribution
ufo_inla_lm$marginals.fixed
ufo_inla_lm$marginals.hyperpar

## ----message=FALSE,echo=FALSE, fig.height=3.5----------------------------------------------------
## Posterior on population
plot_df <- as.data.frame(inla.tmarginal(function(x) x, 
                                        ufo_inla_lm$marginals.fixed$`(Intercept)`))
ggplot(plot_df, aes(x = x, y = y)) +
  geom_line(size = 2) +
  scale_x_continuous("Intercept") +
  scale_y_continuous("P") +
  theme_bw()

## Posterior on population
plot_df <- as.data.frame(inla.tmarginal(function(x) x, 
                                        ufo_inla_lm$marginals.fixed$population))
ggplot(plot_df, aes(x = x, y = y)) +
  geom_line(size = 2) +
  scale_x_continuous("Population") +
  scale_y_continuous("P") +
  theme_bw()

## Sigma
plot_df <- as.data.frame(inla.tmarginal(function(x) 1/x, 
                                        ufo_inla_lm$marginals.hyperpar$`Precision for the Gaussian observations`))
ggplot(plot_df, aes(x = x, y = y)) +
  geom_line(size = 2) +
  scale_x_continuous("Residual variance") +
  scale_y_continuous("P") +
  theme_bw()

## ----message=FALSE-------------------------------------------------------------------------------
ufo_inla_lm2 <- inla(count ~ population,
                     data = ufos_2014,
                     control.fixed = list(mean = 0, prec = 0.2, 
                                          mean.intercept = 0, prec.intercept = 0.2),
                     control.family=list(hyper=list(prec=list(prior='loggamma', 
                                                              param=c(0.1,0.1)))),
                     control.compute = list(dic = TRUE, waic = TRUE),
                     control.predictor = list(compute = TRUE))


## ----message=FALSE,echo=FALSE--------------------------------------------------------------------
summary(ufo_inla_lm2)
plot(ufo_inla_lm2, plot.fixed.effects = TRUE,
     plot.random.effects = FALSE,
     plot.hyperparameters = TRUE,
     plot.prior = TRUE,
     plot.predictor = FALSE)


## ----fig.keep='none', cache=FALSE----------------------------------------------------------------
ufos_2014$idarea <- 1:nrow(ufos_2014)
ufos_nb <- poly2nb(ufos_2014)
ufos_W <- nb2mat(ufos_nb)


## ----echo=FALSE, cache=FALSE---------------------------------------------------------------------
plot(st_geometry(ufos_2014), reset = FALSE)
plot(ufos_nb, st_coordinates(st_centroid(ufos_2014)), add = TRUE, col = 2, lwd = 2)


## ----message=FALSE,echo=TRUE---------------------------------------------------------------------
ufo_inla_bym <- inla(count ~ population + f(idarea, model = "bym2", graph = ufos_W), 
                     data = ufos_2014,
                     control.compute = list(dic = TRUE, waic = TRUE),
                     control.predictor = list(compute = TRUE)
)


## ----message=FALSE,echo=FALSE--------------------------------------------------------------------
summary(ufo_inla_bym)
plot(ufo_inla_bym, plot.fixed.effects = TRUE,
     plot.random.effects = FALSE,
     plot.hyperparameters = TRUE,
     plot.predictor = FALSE)

## ------------------------------------------------------------------------------------------------
prior_bym2 <- list(
  prec = list(
    prior = "pc.prec",
    param = c(1, 0.01)),
  phi = list(
    prior = "pc",
    param = c(0.5, 2 / 3))
)

## ----message=FALSE,echo=TRUE---------------------------------------------------------------------
ufo_inla_bym2 <- inla(count ~ population + f(idarea, model = "bym2", 
                                            graph = ufos_W,
                                            hyper = prior_bym2), 
                     data = ufos_2014,
                     control.compute = list(dic = TRUE, waic = TRUE),
                     control.predictor = list(compute = TRUE))

plot(ufo_inla_bym2, plot.fixed.effects = FALSE,
     plot.random.effects = FALSE,
     plot.hyperparameters = TRUE,
     plot.predictor = FALSE)

## Plot spatial effect
ufos_2014$yhat <- ufo_inla_bym2$summary.linear.predictor$`0.025quant`
tm_shape(ufos_2014) + tm_fill("yhat", style = "jenks") +
  tm_borders() +
  tm_layout(main.title = "State population 2014")

## Plot Bayesian estimates
ufos_2014$u <- ufo_inla_bym2$summary.random$idarea$mean[1:49]
tm_shape(ufos_2014) + tm_fill("u", style = "jenks") +
  tm_borders() +
  tm_layout(main.title = "State population 2014")

## Show effect of 'shrinkage'
## Find California
stateID <- which(ufos_2014$name == "California")

ufo_inla_bym2$marginals.linear.predictor[[stateID]]

lm_marg <- ufo_inla_lm$marginals.linear.predictor[[stateID]]
bym_marg <- ufo_inla_bym2$marginals.linear.predictor[[stateID]]
plot_df <- data.frame(rbind(lm_marg, bym_marg),
                      model = c(rep("LM", nrow(lm_marg)),
                                rep("BYM", nrow(bym_marg))))
ggplot(plot_df, aes(x = x, y = y, col = model)) +
  geom_line(size = 2) +
  geom_vline(xintercept = ufos_2014$count[stateID], linetype = 2, size = 1.5) +
  scale_x_continuous("Count") +
  scale_y_continuous("P") +
  ggtitle("California UFO sightings") +
  theme_bw()

## Lowest count
stateID <- which(ufos_2014$name == "Vermont")

lm_marg <- ufo_inla_lm$marginals.linear.predictor[[stateID]]
bym_marg <- ufo_inla_bym2$marginals.linear.predictor[[stateID]]
plot_df <- data.frame(rbind(lm_marg, bym_marg),
                      model = c(rep("LM", nrow(lm_marg)),
                                rep("BYM", nrow(bym_marg))))
ggplot(plot_df, aes(x = x, y = y, col = model)) +
  geom_line(size = 2) +
  geom_vline(xintercept = ufos_2014$count[stateID], linetype = 2, size = 1.5) +
  scale_x_continuous("Count") +
  scale_y_continuous("P") +
  ggtitle("California UFO sightings") +
  theme_bw()


## ----echo=FALSE----------------------------------------------------------------------------------
## Rate model
ufos_2014$Ei <- (sum(ufos_2014$count) / sum(ufos_2014$population)) * ufos_2014$population
ufos_2014$SIR <- ufos_2014$count / ufos_2014$Ei
tm_shape(ufos_2014) + tm_fill("SIR") + 
  tm_borders() + tm_layout(main.title = "UFO SIR 2014")

## ------------------------------------------------------------------------------------------------
ufo_inla_rr <- inla(count ~ 1 + f(idarea, model = "bym2", 
                           graph = ufos_W,
                           hyper = prior_bym2), 
             data = ufos_2014, family = "poisson", E = Ei, 
             control.compute = list(dic = TRUE, waic = TRUE),
             control.predictor = list(compute = TRUE),
             control.inla = list(strategy = "adaptive",int.strategy = "eb")
)


## ------------------------------------------------------------------------------------------------
summary(ufo_inla_rr)


## ----echo=FALSE, fig.height=3.5------------------------------------------------------------------
ufos_2014$RR <- ufo_inla_rr$summary.fitted.values[, "mean"]

tm_shape(ufos_2014) + tm_fill("RR", palette = "-inferno") +
  tm_borders() +
  tm_layout(main.title = "RR")


## ----echo=FALSE, fig.height=3.5------------------------------------------------------------------
ufos_2014$exc <- sapply(ufo_inla_rr$marginals.fitted.values,
                        function(marg){1 - inla.pmarginal(q = 2, marginal = marg)})

tm_shape(ufos_2014) + tm_fill("exc", palette = "Blues") +
  tm_borders() +
  tm_layout(main.title = "Excedence probability (RR > 2)")


## ----echo=FALSE----------------------------------------------------------------------------------
airports <- read.csv("../datafiles/nuforc/airports.csv")
airports$lairports <- log(airports$airports)
ufos_2014 <- merge( ufos_2014, airports, by.x = "postal", by.y = "state")
ggscatter(ufos_2014, x = "lairports", y = "count",
          main = "UFO sightings vs. airports",
          add = "reg.line") #+ xscale("log10", .format = TRUE)  + yscale("log10", .format = TRUE)
ggplot(ufos_2014, aes(x = airports, y = count)) +
  scale_x_log10() + scale_y_log10() +
  geom_point()

## ----echo=TRUE, cache=FALSE----------------------------------------------------------------------
prior_bym2 <- list(
  prec = list(
    prior = "pc.prec",
    param = c(0.6 / 0.31, 0.01)),
  phi = list(
    prior = "pc",
    param = c(0.005, 1 / 10))
)

ufo_inla_rr2 <- inla(count ~ lairports + 
               f(idarea, model = "bym2", 
                 graph = ufos_W,
                 hyper = prior_bym2), 
             data = ufos_2014, family = "poisson", E = Ei, 
             control.compute = list(dic = TRUE, waic = TRUE),
             control.predictor = list(compute = TRUE),
             control.inla = list(strategy = "adaptive",int.strategy = "eb")
)

## ------------------------------------------------------------------------------------------------
summary(ufo_inla_rr2)

plot(ufo_inla_rr2, plot.fixed.effects = FALSE,
     plot.random.effects = FALSE,
     plot.hyperparameters = TRUE,
     plot.predictor = FALSE)

save(ufo_lm, ufo_inla_lm, ufo_inla_lm2, 
     ufo_inla_bym, ufo_inla_bym2,
     ufo_inla_rr, ufo_inla_rr2,
     file = "ufo_models.RData")
