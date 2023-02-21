rm(list=ls())
library("dplyr")

################################################################################
n <- 100000

pred <- function(m,d){
  predict(m,newdata=d,type="response")+rnorm(n,sd=sd(residuals(m)))
}

M1 <- rnorm(n,0,0.1)
X <- 0.5*M1 + rnorm(n,0,0.1)
Z <- 0.5*M1 + rnorm(n,0,0.1) #Same distribution as X given M1
Y <- 0.6*M1 + rnorm(n,0,0.1)

d <- data.frame(X,Z,M1,Y)

m_meas <- lm(Z~X,d)
omega_sq_true <- sd(residuals(m_meas))^2

m_X <- lm(X~M1,d)
m_M <- lm(M1~X,d)

d$M_pred <- pred(m_M,d)
d$X_pred <- pred(m_X,d %>% mutate(M=M_pred))
m_meas_synthetic <- lm(X_pred~X,d)
omega_sq <- sd(residuals(m_meas_synthetic))^2

sig_sq <- sd(d$X)^2

m_Y <- lm(Y~Z,d)
beta_raw <- m_Y$coefficients[["Z"]]
c <- sig_sq/(sig_sq+omega_sq)
beta  <- beta_raw/c
truth <- lm(Y~X,d)$coefficients[["X"]]

c
beta
truth

#I think I've convinced myself that applying the correction
# factor in our case leads to an overinflation of the estimate
# rather than a correction
#This is because Z is not an error in measurement of X, but rather
# a separate manifestation of X given M1 when M1 are very well matched.
#Even using the true omega-squared leads to this inflation
#
#Is this different in the probabilistic matching situation? This
# adds noise in M_young-M_old, leading a decrease in match quality
#But this should be handled by downweighting poor matches
# But the weighting scenarios are worse
#
#