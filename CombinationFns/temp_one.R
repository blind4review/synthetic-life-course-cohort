rm(list=ls())
################################################################################
n <- 1000
X <- rnorm(n,0,0.1)
M1 <- 0.5*X + rnorm(n,0,0.1)
d <- data.frame(X,M1)
m_Z <-  lm(X~M1,d)
sd_z <- sd(residuals(m_Z))
Z <- X + rnorm(n,0,sd_z)
Y <- 0.6*M1 + rnorm(n,0,0.1)
d <- data.frame(X,Z,M1,Y)
plot(Z,X)
sd(Z)
sd(X)

#This would be the truth, but we don't know it bc Z and X never co-observed
m_z <- lm(Z~X,d)
## All of these should be very similar:
omega_sq_true <- sd(residuals(m_z))^2
sd_z^2 #Infinite-population truth

#
m_X <-  lm(X~M1,d)
sig_sq <- sd(d$X)^2
omega_sq <- sd(residuals(m_X))^2

c <- sig_sq/(sig_sq+omega_sq)
m_y <- lm(Y~Z,d)
betaDash <- m_y$coefficients[["Z"]]
beta <- betaDash/ c
truth <- lm(Y~X,d)$coefficients[["X"]]
beta
truth

