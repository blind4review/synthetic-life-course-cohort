################################################################################
# Test using measurement DAG
################################################################################
n <- 2500
X <- rnorm(n,0,0.5)
Z <- X + rnorm(n,0,0.2)
Y <- 0.3*X + rnorm(n,0,0.1)
d <- data.frame(X,Z,Y)

sig_sq <- sd(X)^2
omega_sq <- sd(residuals(lm(Z~X,d)))^2
c <- sig_sq/(sig_sq+omega_sq)
m_y <- lm(Y~Z,d)
betaDash <- m_y$coefficients[["Z"]]
beta <- betaDash/c
truth <- lm(Y~X,d)$coefficients[["X"]]
beta
truth

################################################################################
# Test using our data
################################################################################
d <- read.csv("temp_data.csv") %>%
  #Limit to one match per person
  group_by(older) %>%
  mutate(within_id=row_number()) %>%
  filter(within_id==1) #matched data

#Limit to higher quality matches: Doesn't help
# d <- d %>% mutate(M_dif = abs(M_younger-M_older),
#                   M2_dif = abs(M2_younger-M2_older)) %>%
#   filter(M_dif < 0.1055794,
#          M2_dif < 0.1128271)

d_older <- read.csv("temp_older.csv")
d_youger <- read.csv("temp_younger.csv")

#https://www.researchgate.net/profile/Daniel-Stram/publication/14874907_Exposure_Measurement_Error_Influence_on_Exposure-Disease_Relationships_and_Methods_of_Correction/links/0deec51db5dcfaf5fb000000/Exposure-Measurement-Error-Influence-on-Exposure-Disease-Relationships-and-Methods-of-Correction.pdf

# Step 1) Calculate the population standard deviation of c
# x and z should have the same marginal distribution, so we can use V in the younger cohort
# to estimate this
sig_sq <- sd(d_younger$V)^2 #x is distributed with mean mu and sd sigma^2
#True sigma squared in the DAG is 0.1^2

#Step 2) In matching, we use M to select the older participant's corresponding younger values of V
# If we assume that the matches are high quality, these matches will have a distribution centered around the
# mean of the distribution from which the older person's true V was drawn.
# Analogously, the distribution of the matched younger Vs should have sd (se?) of the residuals
# from a model of V given M in the younger group. These sds should be the same.
m_V <- lm(V_younger~M_younger+M2_younger-1, d_younger)
d_younger$V_pred <- predict(m_V) + rnorm(nrow(d_younger),
                                         mean=0,
                                         #What's the justification for this division? Why do we want se?
                                         sd=sd(residuals(m_V))/sqrt(nrow(d_younger)))

m_meas <- lm(V_pred~V-1, d_younger)
omega_sq <- sd(residuals(m_meas))^2 #z|x is distributed with mean x and sd omega^2
omega_sq
lm(V_younger~V_older-1,d)

#sd(d$V) #similar
# Step 3) Calculate the correction factor
c <- sig_sq/(sig_sq+omega_sq)
c

# Step 4) Estimate betaHat, the coefficient on V in the matched data
m_y_on_z <- lm(Y~V, d)
betaDash <- m_y_on_z$coefficients["V"]

# Step 5) Correct for measurement error
beta <- betaDash/c
truth <- lm(Y_older~V_older,d)$coefficients["V_older"]
betaDash
c
beta
truth

# Step 6) Apply the delta method to get the variance
#Var(betaHat) = Var(betaHatDash)*c^2 + ((betaHatDash/c^2)^2)*Var(c)
V_betaDash <- summary(m_y_on_z)$coefficients["V","Std. Error"]^2
# How to get variance of c?
#V_beta <- V_betaDash*c^2 + ((betaDash/c^2)^2)*Var_c

#Other ways to calculate c that would have SE?
#lm(V~V_pred,d_younger)
#lm(V_younger~V_older,d)


