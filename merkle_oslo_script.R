##################################################
## blavaan examples for Oslo UseR group, 09Mar2022
##################################################
library("blavaan")
library("future")
plan("multicore")
library("bayesplot")


###########################
## 1. Regression & workflow
###########################
data(cars)

plot(cars)

## simple fit with defaults
reg <- ' dist ~ speed '
fit <- bsem(reg, data = cars)

summary(fit)


## now add custom prior distributions and examine implications
reg <- ' dist ~ prior("normal(0, 30)") * 1 + prior("normal(5, 10)") * speed
         dist ~~ prior("gamma(3, .1)[sd]") * dist '

prior_fit <- bsem(reg, data = cars, prisamp = TRUE)

## extract samples from the prior
samps <- blavInspect(prior_fit, "mcmc") ## mcmc list object
samps <- do.call("rbind", samps) ## convert to matrix

## graph regression lines from the prior
plot(c(0, 25), c(0, 120), type = "n")
apply(samps, 1, function(x) abline(x[1], x[2]))

## revised priors
reg <- ' dist ~ prior("normal(0, 5)") * 1 + prior("normal(8, 5)") * speed
         dist ~~ prior("gamma(3, .1)[sd]") * dist '

## draw samples from these priors and create a similar graph
prior_fit <- bsem(reg, data = cars, prisamp = TRUE)
samps <- blavInspect(prior_fit, "mcmc") ## mcmc list object
samps <- do.call("rbind", samps) ## convert to matrix

plot(c(0, 25), c(0, 120), type = "n")
apply(samps, 1, function(x) abline(x[1], x[2]))


## now fit the model using these priors
fit <- bsem(reg, data = cars)

## convergence diagnostics
blavInspect(fit, "rhat")  ## want close to 1.000
blavInspect(fit, "neff")  ## want large values
plot(fit)  ## want a hairy caterpillar

summary(fit)


###########################
## 2. Structural equations
###########################

## a famous example from Bollen (1989)
model <- ' 
       # latent variable definitions
         ind60 =~ x1 + x2 + x3
         dem60 =~ y1 + a*y2 + b*y3 + c*y4
         dem65 =~ y5 + a*y6 + b*y7 + c*y8
     
       # regressions
         dem60 ~ ind60
         dem65 ~ ind60 + dem60
     
       # residual correlations
         y1 ~~ y5
         y2 ~~ y4 + y6
         y3 ~~ y7
         y4 ~~ y8
         y6 ~~ y8
     '

fit <- bsem(model, data = PoliticalDemocracy,
            dp = dpriors(lambda = "normal(1,1)"), ## all loadings get this prior
            save.lvs = TRUE, ## sample latent variables
            bcontrol = list(cores = 3)) ## parallelize each chain

blavInspect(fit, "rhat")
blavInspect(fit, "neff")
plot(fit, 1:4)  ## the numbers specify which parameters to plot
cbind(1:length(coef(fit)), coef(fit))
plot(fit, 18:28)

summary(fit)

## Bayesian model fit and comparison metrics
fitMeasures(fit)

## latent variable samples
lvs <- blavInspect(fit, "lvs")
lvs <- do.call("rbind", lvs) ## convert to matrix

## posterior distributions of 1960 industrialization
mcmc_intervals(lvs, pars = paste0("eta[", 1:75, ",1]"))

## modify last graph to get rid of y labels
library(ggplot2)
p <- mcmc_intervals(lvs, pars = paste0("eta[", 1:75, ",1]"))
p + theme(axis.text.y = element_blank())


###########################
## 3. Binary/ordinal models
###########################
data(data.read, package="sirt")

model <- "F =~ A1 + A2 + A3 + A4 + B1 + B2 + B3 + B4 + C1 + C2 + C3 + C4"
## or:
model <- as.character(sapply(LETTERS[1:3], paste0, 1:4))
model <- paste(model, collapse = " + ")
model <- paste("F =~", model)

fit <- bcfa(model, data = data.read, ordered = TRUE,
             std.lv = TRUE,
             dp = dpriors(lambda = "normal(0,1)"),
             burnin = 150, sample = 150, ## short run for the presentation
             bcontrol = list(cores = 3),
             mcmcextra = list(data = list(llnsamp = 20))) ## controls speed-accuracy tradeoff
                                                          ## in post-processing

blavInspect(fit, "rhat")
blavInspect(fit, "neff")
plot(fit)
plot(fit, 1:12, plot.type = "intervals")

summary(fit, rsquare=TRUE)
fitMeasures(fit)
