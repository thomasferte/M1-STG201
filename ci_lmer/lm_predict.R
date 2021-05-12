#' lm_predict
#' 
#' @description A function to predict with confidence intervals from a linear model fixed effects, variance covariance and degree of freedom.
#' @source https://stackoverflow.com/questions/38109501/how-does-predict-lm-compute-confidence-interval-and-prediction-interval
#' 
#' @param fixedEffects a numeric vector of fixed effects
#' @param vcovMatrix the variance covariance of fixed effects
#' @param dof the degree of freedom of the model
#' @param newdataMatrix the new data matrix. Should be of the same length as fixed effects. If fixed effects contains an intercept, first column should be 1.
#' @param alpha the alpha level (default is 0.95).
#' 
#' @return A list of 3: the prediction with confidence intervals (dfPred), the degree of freedom (dof), the variance of fitted values (var.fit).
lm_predict <- function (fixedEffects,
                        vcovMatrix,
                        dof,
                        newdataMatrix,
                        alpha = 0.95) {
  
  # extract info from lmObject
  # fixedEffects <- coef(lmObject) # fixed effects
  # vcovMatrix <- vcov(lmObject) # variance covariance of fixed effects
  # dof <- df.residual(lmObject) # degree of freedom
  # newdataMatrix <- model.matrix(delete.response(terms(lmObject)), newdata) # newdata matrix
  
  # mean estimate
  yh <- c(newdataMatrix %*% fixedEffects)
  
  # variance of yh
  mat.vcov.fit <- as.matrix(newdataMatrix %*% vcovMatrix %*% t(newdataMatrix))
  var.fit <- diag(mat.vcov.fit)
  
  # Confidence interval
  Qt <- c(-1, 1) * qt((1 - alpha) / 2, dof, lower.tail = FALSE)
  CI <- yh + outer(sqrt(var.fit), Qt)
  
  # shape predictions
  dfPred <- data.frame(fit = yh,
                       lwr = CI[,1],
                       upr = CI[,2])
  
  # return results
  lsresult <- list(dfPred = dfPred,
                   dof = dof,
                   var.fit = var.fit)
  
  return(lsresult)
}

# ### example 1 : linear model
# set.seed(1)
# toyData <- data.frame(X1 = rnorm(n = 50),
#                       X2 = rbinom(n = 50, prob = 0.5, size = 1))
# toyData$Y <- 2 + toyData$X1*2 + toyData$X2*(-1) + rnorm(50)
# 
# # train lm
# m1 <- lm(Y~X1+X2, data = toyData[1:40,])
# # usual predictions
# predUsual <- predict(object = m1, newdata = toyData[41:50,], interval = "confidence")
# # custom predictions
# newmatrix <- as.matrix(data.frame(Intercept = 1,
#                                   X1 = toyData$X1[41:50],
#                                   X2 = toyData$X2[41:50]))
# predCustom <- lm_predict(fixedEffects = coef(m1),
#                          vcovMatrix = vcov(m1),
#                          dof = df.residual(m1),
#                          newdataMatrix = newmatrix)
# 
# # both predictions are almost equals (difference is below e-15)
# predUsual - predCustom$dfPred
# 
# ### example 2 : linear mixed model
# set.seed(1)
# # random effect
# vecIdEffect <- rnorm(10)
# toyData2 <- data.frame(id = rep(c(1:10), 5),
#                       idEffect = rep(vecIdEffect, 5),
#                       X1 = rnorm(n = 50),
#                       X2 = rbinom(n = 50, prob = 0.5, size = 1))
# 
# toyData2$Y <- 2 + toyData2$X1*2 + toyData2$X2*(-1) + toyData2$idEffect + rnorm(50)
# 
# # train lme4
# m2 <- lme4::lmer(Y~X1+X2+(1|id), data = toyData2[1:40,])
# # custom predictions
# newmatrix2 <- as.matrix(data.frame(Intercept = 1,
#                                    X1 = toyData2$X1[41:50],
#                                    X2 = toyData2$X2[41:50]))
# 
# predCustom2 <- lm_predict(fixedEffects = lme4::fixef(m2),
#                           vcovMatrix = vcov(m2),
#                           dof = df.residual(m2),
#                           newdataMatrix = newmatrix2)
# 
# # mean predictions are equal
# predCustom2$dfPred$fit - predict(object = m2, toyData2[41:50,], re.form = NA)
