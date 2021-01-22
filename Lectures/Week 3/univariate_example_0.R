library(MARSS)
library(colorspace)
hcl_palette_52 <- colorspace::sequential_hcl(52)
hcl_palette_100 <- colorspace::sequential_hcl(100)
library(forecast)
library(ggplot2)

# Model Nile data with
# x_t = x_{t-1} + u + w_t, w_t ~ N(0, q)
# y_t = x_t + v_t, v_t ~ N(0, r)

mod.list <- list(
  U = matrix("u"),
  x0 = matrix("x0"),
  B = matrix(1),
  Q = matrix("q"),
  Z = matrix(1),
  A = matrix(0),
  R = matrix("r"),
  tinitx = 0
)
fit <- MARSS(Nile, model=mod.list)
plot(Nile)
lines(1871:1970, fit$states[1,], col="red")

# Run this to generate a forecast
autoplot(forecast(fit, h=10))

# Question one
# Change the x (state) model to x_t = x_{t-1} + u
# Don't change the y (observation) model
# What happens to the fit and forecast? Why?
mod.list <- list(
  U = matrix("u"),
  x0 = matrix("x0"),
  B = matrix(1),
  Q = matrix(0),
  Z = matrix(1),
  A = matrix(0),
  R = matrix("r"),
  tinitx = 0
)
fit <- MARSS(Nile, model=mod.list)
autoplot(forecast(fit, h=10))


# Question two
# Change the x (state) model to x_t = x_{t-1} + w_t
# Don't change the y (observation) model
# What happens to the fit and forecast? Why?
mod.list <- list(
  U = matrix(0),
  x0 = matrix("x0"),
  B = matrix(1),
  Q = matrix("q"),
  Z = matrix(1),
  A = matrix(0),
  R = matrix("r"),
  tinitx = 0
)
fit <- MARSS(Nile, model=mod.list)
autoplot(forecast(fit, h=10))

# Question three
# Change the x (state) model to x_t = u + w_t
# Don't change the y (observation) model
# Also SET x0 to 0 for this question; x0 = matrix(0). Otherwise the problem is underconstrained.
# What happens to the fit and forecast? Why?
mod.list <- list(
  U = matrix("u"),
  x0 = matrix(0),
  B = matrix(0),
  Q = matrix("q"),
  Z = matrix(1),
  A = matrix(0),
  R = matrix("r"),
  tinitx = 0
)
fit <- MARSS(Nile, model=mod.list)
autoplot(forecast(fit, h=10))
