#################################################################
##  Univariate state-space models
##  Example 2.  The hidden x is a random walk but we fit a linear regression
#################################################################

library(MARSS)

# Simulate some data
# x is the "hidden" trend we want to find
# x(t)=x(t-1)+u+w(t), w(t)~N(0,q)
# x(0)=x0
x0 <- 1
u <- 0.5
q <- .1
r <- .1
n <- 50
t <- 1:n
x <- x0 + u + rnorm(1, 0, sqrt(q))
for (i in 2:n) x[i] <- x[i - 1] + u + rnorm(1, 0, sqrt(q))
plot(x, xlim = c(1, n), type = "l")

# y is our observation of x with error
y <- x + rnorm(n, 0, sqrt(r))
points(y)

# How do we deal with x being a random walk??
# Let's just fit a straight line by setting the process variance to 0

# Let's write x as a AR-1 model and y as an observation of that
# x(t) = x(t-1) + u + w(t), w(t)~N(0,0)  so w(t)=0
# x(0) = x0
# y(t) = x(t) + v(t), v(t)~N(0,r)
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

fit2 <- MARSS(y, model = mod.list)
lines(fit2$states[1, ], col = "blue", lwd = 2)
title("fit is blue; true x is black")
est.u <- coef(fit2)$U[, 1]
est.x0 <- coef(fit2)$x0[, 1]
est.r <- coef(fit2)$R[, 1]

readline("Continue?")

# Let's forecast our OBSERVATIONS forward 10 time steps
# x(t+1)=x(t)+u
# y(t+1)=x(t+1)+v(t+1), v(t)~N(0,r)
# First let's set up our estimated x
# The last x at t=max(t)
x.est.end <- fit2$states[1, n]
t.forward <- 50

# Let's first add the the real x and observations
# this bit is to set up the x and y lims
x.est.end <- fit2$states[1, n]
x.mean.forecast <- x.est.end + est.u * (1:t.forward)
ylims <- c(x[n - 9] - (r + q * t.forward), x.mean.forecast[t.forward] + (r + q * t.forward))
xlims <- c(n - 9, n + t.forward)

plot((n - 9):n, x[(n - 9):n], xlim = xlims, ylim = ylims, type = "l", ylab = "y", xlab = "t")
points(y)
title(paste("forecast with", n, "data points for estimation\nblue is estimate; red is true"))

# Now let's forecast 1000 times using our estimates
x.forecast <- x.est.end + est.u * (1:t.forward)
for (i in 1:1000) {
  y.forecast <- x.forecast + rnorm(t.forward, 0, sqrt(est.r))
  jit <- rnorm(1, 0, .1) - .25
  points(n + 1:t.forward + jit, y.forecast, pch = ".", col = "blue")
}

# Now let's forecast 1000 times the real x model
x.end <- x[n]

for (i in 1:1000) {
  x.true.forecast <- x.end + u + rnorm(1, 0, sqrt(q))
  for (i in 2:t.forward) x.true.forecast[i] <- x.true.forecast[i - 1] + u + rnorm(1, 0, sqrt(q))
  y.true.forecast <- x.true.forecast + rnorm(t.forward, 0, sqrt(r))
  jit <- rnorm(1, 0, .1) + .25
  points(n + 1:t.forward + jit, y.true.forecast, pch = ".", col = "red")
}
text(n, y[n - 10], "Notice that the estimated forecast is too narrow.", adj = 0)
