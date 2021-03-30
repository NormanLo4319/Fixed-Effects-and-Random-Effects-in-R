getwd()
setwd("")

library(foreign)
panel <- read.dta("http://dss.princeton.edu/training/Panel101.dta")
coplot(y ~ year|country, type = "`", data = panel)
coplot(y ~ year|country, type = "b", data = panel)

library(car)
scatterplot(y ~ year|country, boxplots = FALSE, smooth = TRUE,
            reg.line = FALSE, data = panel)

library(gplots)
plotmeans(y ~ country, main = "Heterogeineity Across Countries", data = panel)
plotmeans(y ~ year, main = "Heterogeineity Across Years", data =panel)

detach("package:gplots")

# OLS Regression Model
ols <- lm(y ~ x1, data = panel)
summary(ols)

yhat <- ols$fitted

plot(panel$x1, panel$y, pch = 19, xlab ="x1", ylab = "y")
abline(lm(panel$y ~ panel$x1, lwd = 3, col = "blue"))

# Fixed Effects Model

# Fixed Effects Using LS Dummy Variable
fixed_dum <- lm(y ~ x1 + factor(country) - 1, data = panel)
summary(fixed_dum)

yhat <- fixed.dum$fitted

scatterplot(yhat ~ panel$x1|panel$country, boxplot = FALSE,
            xlab = "x1", ylab = "yhat", smooth = FLASE)
abline(lm(panel$y ~ panel$x1), lwd = 3, col = "blue")

library(apsrtable)
apsrtable(ols, fixed_dum, model.names = c("OLS", "OLS_DUM"))

# Fixed Effects: n entity-specific intercepts
library(plm)
fixed <- plm(y ~ x1, data = panel, index = c("country", "year"), model = "within")
summary(fixed)

fixef(fixed)

pFtest(fixed, ols)


# Random Effects Model
random <- plm(y ~ x1, data = panel, index=c("country", "year"), model="random")
summary(random)

# Alternative way to run the random effects model
panel_set <- plm.data(panel, index = c("country", "year"))
random_set <- plm(y ~ x1, data = panel_set, model = "random")
summary(random_set)


# Hausman Test for Fixed and Random Effects Models
phtest(fixed, random)

# Testing for time-fixed effects
fixed_time <- plm(y ~ x1 + factor(year), data = panel, index = c("country", "year"), model = "within")
summary(fixed_time)

pFtest(fixed_time, fixed)

# Testing for Random Effect: Breusch-Pagan Lagrange Multiplier (LM)
pool <- plm(y ~ x1, data = panel, index = c("country", "year"), model = "pooling")
summary(pool)

plmtest(pool, type = c("bp"))

# Testing for cross-sectional dependence / contemporaneous correlation
# Breusch-Pagan LM test of independence and Pasaran CD test
pcdtest(fixed, test = c("lm"))
pcdtest(fixed, test = c("cd"))

# Testing for Serial Correlation
pbgtest(fixed)

# Testing for Unit Roots / Stationary
library(tseries)
adf.test(panel_set$y, k = 2)

# Testing for heteroskedasticity
library(lmtest)
bptest(y ~ x1 + factor(country), data = panel, studentize = F)

# Controlling for heteroskedasticity: Random Effects
# Original coefficients
coeftest(random)

# Heteroskedasticity consistent coefficients
coeftest(random, vcovHC)

# Heteroskedasticity consistent coefficients, type 3
coeftest(random, vcovHC(random, type = "HC3"))

# Shows the HC standard errors of the coefficients
t(sapply(c("HCO", "HC1", "HC2", "HC3", "HC4"), function(x) sqrt(diag(vcovHC(random, type = x)))))

# Controlling for heteroskedasticity: Fixed Effects
# Original coefficients
coeftest(fixed)

# Heteroskedasticity consistent coefficients
coeftest(fixed, vcovHC)

# Heteroskedasticity consistent coefficients (Arellano)
coeftest(fixed, vcovHC(fixed, method = "arellano"))

# Heteroskedasticity consistent coefficients type 3
coeftest(fixed, type = "HC3")

# Shows the HC standard errors of the coefficients
t(sapply(c("HC0", "HC1", "HC2", "HC3", "HC4"), function(x) sqrt(diag(vcovHC(fixed, type = x)))))

