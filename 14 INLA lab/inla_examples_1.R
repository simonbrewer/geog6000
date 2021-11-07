## Bayesian exmaples
## Linear regresison

library(brms)
library(INLA)

gap <- read.csv("../datafiles/gapminderData5.csv")
gap$year2 <- gap$year - 1952
gap$gdpPercap2 <- log(gap$gdpPercap)

fit_lm <- lm(lifeExp ~ gdpPercap2, gap)
summary(fit_lm)

## Basic INLA model
inla_lm <- inla(lifeExp ~ gdpPercap2, 
                data = gap)

inla_lm <- inla(lifeExp ~ gdpPercap2, 
                data = gap,
                control.compute = list(dic = TRUE, waic = TRUE),
                control.predictor = list(compute = TRUE))

summary(inla_lm)
png("inla.plots/lm-1.png")
plot(inla_lm, plot.fixed.effects = TRUE,
     plot.random.effects = FALSE,
     plot.hyperparameters = FALSE,
     plot.predictor = FALSE, cex = 1.25)
dev.off()
png("inla.plots/lm-2.png")
plot(inla_lm, plot.fixed.effects = FALSE,
     plot.random.effects = FALSE,
     plot.hyperparameters = TRUE,
     plot.predictor = FALSE, cex = 1.25)
dev.off()

plot(gap$lifeExp, inla_lm$summary.linear.predictor$mean)
abline(0,1)

## Binomial regression
irished <- read.csv("../datafiles/irished.csv")
irished$DVRT.cen <- irished$DVRT - mean(irished$DVRT)
fit_glm <- glm(lvcert ~ DVRT.cen, 
               data = irished, 
               family = binomial(link = 'logit'))

summary(fit_glm)

## Basic INLA model
inla_glm <- inla(lvcert ~ DVRT.cen, 
                 data = irished, 
                 family = "binomial",
                 control.compute = list(dic = TRUE, waic = TRUE),
                 control.predictor = list(compute = TRUE))
summary(inla_glm)

png("inla.plots/glm-1.png")
plot(inla_glm, plot.fixed.effects = TRUE,
     plot.random.effects = FALSE,
     plot.hyperparameters = FALSE,
     plot.predictor = FALSE, cex = 1.25)
dev.off()

## Mixed effects
library(lme4)
fit_lmer <- lmer(lifeExp ~ year2 + (1 | country), gap)
summary(fit_lmer)

inla_lmer <- inla(lifeExp ~ year2 + f(country, model = "iid"), 
                  data = gap)
summary(inla_lmer)

png("inla.plots/lmer-1.png")
plot(inla_lmer, plot.fixed.effects = TRUE,
     plot.random.effects = FALSE,
     plot.hyperparameters = FALSE,
     plot.predictor = FALSE)
dev.off()
png("inla.plots/lmer-2.png")
plot(inla_lmer, plot.fixed.effects = FALSE,
     plot.random.effects = TRUE,
     plot.hyperparameters = FALSE,
     plot.predictor = FALSE, cex = 1)
dev.off()
png("inla.plots/lmer-3.png")
plot(inla_lmer, plot.fixed.effects = FALSE,
     plot.random.effects = FALSE,
     plot.hyperparameters = TRUE,
     plot.predictor = FALSE, 
     plot.prior = TRUE)
dev.off()

save(fit_lm, fit_glm, fit_lmer, inla_lm, inla_glm, inla_lmer, 
     file = "inla_models.RData")

plot(inla_lm, pdf = TRUE, prefix = "inla.plots/lm-", cex = 1.25)
plot(inla_glm, pdf = TRUE, prefix = "inla.plots/glm-", cex = 1.25)
plot(inla_lmer, pdf = TRUE, prefix = "inla.plots/lmer-", cex = 1.25)
