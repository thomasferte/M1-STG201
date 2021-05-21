library(dplyr)

source("ci_lmer/lm_predict.R")

# load lmer object
lmer_b_npi <- readRDS(file = "ci_lmer/lmer_b_npi.rds")

# extract info from lmObject
fixedEffects <- lme4::fixef(lmer_b_npi) # fixed effects
vcovMatrix <- vcov(lmer_b_npi) # variance covariance of fixed effects
dof <- df.residual(lmer_b_npi) # degree of freedom

# generate a random new matrix
n <- 100
newdata <- data.frame(Intercept = 1,
                      lockdown1delay7 = rbinom(n = n, size = 1, prob = 0.2),
                      endlock1phase1 = rbinom(n = n, size = 1, prob = 0.2),
                      endlock1phase2 = rbinom(n = n, size = 1, prob = 0.2),
                      lockdown2delay7 = rbinom(n = n, size = 1, prob = 0.2),
                      lock2reduced = rbinom(n = n, size = 1, prob = 0.2),
                      closedSchool = rbinom(n = n, size = 1, prob = 0.2),
                      closedbarresto = rbinom(n = n, size = 1, prob = 0.2),
                      barriergestures = rbinom(n = n, size = 1, prob = 0.2),
                      curfew6pm = rbinom(n = n, size = 1, prob = 0.2),
                      curfew8pm = rbinom(n = n, size = 1, prob = 0.2),
                      variants = rnorm(n = n),
                      smoothWeather = rnorm(n = n)) %>%
  mutate(closedbarresto_int_smoothWeather = closedbarresto*smoothWeather)

# prediction
lspredictions <- lm_predict(fixedEffects = fixedEffects, vcovMatrix = vcovMatrix, dof = dof, newdataMatrix = as.matrix(newdata))
# predictions are in lspredictions$dfPred with the central estimate (fit), the lower (lwr) and upper (upr) bonds.

# test if central estimates are the same
meanpredictions <- predict(lmer_b_npi, newdata, re.form = NA)
bool <- any(lspredictions$dfPred$fit != meanpredictions)
# ok

