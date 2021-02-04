## ----dlm-nile-plot, fig.align="center"-----------------------------------------------------------
## load MARSS package
library(MARSS)

## load Nile flow data
data(Nile, package = "datasets")

## plot the flow
plot.ts(Nile, las = 1, lwd = 2,
        xlab = "Year", ylab = "Flow of the River Nile")


## ----dlm-nile-fit, echo = TRUE, fig.align="center"-----------------------------------------------
## define model list
mod_list <- list(B = "identity", U = "zero", Q = matrix("q"),
                 Z = "identity", A = matrix("a"), R = matrix("r"))

## fit the model with MARSS
fit <- MARSS(matrix(Nile, nrow = 1), mod_list)

## plot the flow and fit
plot.ts(Nile, las = 1, lwd = 2,
        xlab = "Year", ylab = "Flow of the River Nile")
lines(seq(start(Nile)[1], end(Nile)[1]),
       lwd = 2, t(fit$states), col = "blue")
lines(seq(start(Nile)[1], end(Nile)[1]), t(fit$states + 2*fit$states.se),
       lwd = 2, lty = "dashed", col = "blue")
lines(seq(start(Nile)[1], end(Nile)[1]), t(fit$states - 2*fit$states.se),
       lwd = 2, lty = "dashed", col = "blue")


## ----dlm-quadratic-plot, fig.align="center"------------------------------------------------------
## some example data
exp_dat <- c(11.68,  7.68,  7.56,  7.73,  7.34,  8.75, 12.09, 12.77,
              6.57, 16.77, 15.45, 15.06, 11.23, 10.58, 14.02, 17.81,
             15.8,  15.31, 15.17, 19.87, 20.03, 21.23, 21.41, 21.12,
             21.91, 21.33, 28.72, 27.77, 25.68, 28.82, 32.51, 32.14,
             35.21, 35.35, 36.19, 37.28, 38.59, 38.58, 43.84, 45.59)

## plot them
plot.ts(exp_dat, las = 1, lwd = 2,
        ylab = expression(italic(y[t])))


## ----dlm-quadratic-fit---------------------------------------------------------------------------
## B matrix
BB <- matrix(c(1, 0, 1, 1), 2, 2)

## Z matrix (vector)
ZZ <- matrix(c(1, 0), 1, 2)

## define model list
mod_list <- list(B = BB, U = "zero", Q = "diagonal and equal",
                 Z = ZZ, A = matrix(0), R = matrix("r"))

## initial starting values for parameters
inits_list <- list(x0 = matrix(c(0, 0), 2, 1))

## fit the model with MARSS
fit_2 <- MARSS(matrix(exp_dat, nrow = 1),
               model = mod_list,
               inits = inits_list)


## ----dlm-quadratic-plots, fig.align="center"-----------------------------------------------------
## get estimates of alpha
alpha_hat <- c(fit_2$states[1,1],
               fit_2$states[1,-1] - fit_2$states[2,length(exp_dat)])

## get estimates of eta
eta_hat <- fit_2$states[2,]

## plot the estimated level and drift
par(mfrow = c(2,1), mai = c(0.8, 0.8, 0.2, 0.2), omi = c(0, 0, 0, 0))
## plot alpha
plot.ts(alpha_hat, las = 1, lwd = 2, col = "blue",
        ylab = expression(alpha[t]))
## plot eta
plot.ts(eta_hat, las = 1, lwd = 2, col = "blue",
        ylab = expression(eta[t]))


## ----dlm-quad-plot-fits, fig.align="center"------------------------------------------------------
## plot the data and fit
plot.ts(exp_dat, las = 1, lwd = 2,
        ylab = expression(italic(y[t])))
lines(seq(40), fit_2$states[1,],
       lwd = 2, col = "blue")
lines(seq(40), fit_2$states[1,] + 2*fit_2$states.se[1,],
       lwd = 2, lty = "dashed", col = "blue")
lines(seq(40), fit_2$states[1,] - 2*fit_2$states.se[1,],
       lwd = 2, lty = "dashed", col = "blue")


## ----read.in.data, eval=TRUE---------------------------------------------------------------------
## load the data
data(SalmonSurvCUI, package = "MARSS")
## get time indices
years <- SalmonSurvCUI[, 1]
## number of years of data
TT <- length(years)
## get response variable: logit(survival)
dat <- matrix(SalmonSurvCUI[,2], nrow = 1)


## ----z.score, eval=TRUE--------------------------------------------------------------------------
## get predictor variable
CUI <- SalmonSurvCUI[,3]
## z-score the CUI
CUI_z <- matrix((CUI - mean(CUI)) / sqrt(var(CUI)), nrow = 1)
## number of regr params (slope + intercept)
m <- dim(CUI_z)[1] + 1


## ----dlm-plotdata, eval=TRUE, echo=FALSE, fig=TRUE, fig.height=6, fig.width=6, fig.align="center"----
par(mfrow = c(2, 1), mai = c(0.8, 0.8, 0.2, 0.2), omi = c(0, 0, 0, 0))
plot(years, dat, xlab = "", ylab = "Logit(s)", las = 1,
     bty = "n", xaxt = "n", pch = 16, col = "darkgreen", type = "b")
plot(years, CUI_z, xlab = "", ylab = "CUI", las = 1,
     bty = "n", xaxt = "n", pch = 16, col = "blue", type = "b")
axis(1, at = seq(1965, 2005, 5))
mtext("Year of ocean entry", 1, line = 3)


## ----univ.DLM.proc, eval=TRUE--------------------------------------------------------------------
## for process eqn
B <- diag(m)                        ## 2x2; Identity
U <- matrix(0, nrow = m, ncol = 1)  ## 2x1; both elements = 0
Q <- matrix(list(0), m, m)          ## 2x2; all 0 for now
diag(Q) <- c("q.alpha", "q.beta")   ## 2x2; diag = (q1,q2)


## ----univ.DLM.obs, eval=TRUE---------------------------------------------------------------------
## for observation eqn
Z <- array(NA, c(1, m, TT))  ## NxMxT; empty for now
Z[1,1,] <- rep(1, TT)        ## Nx1; 1's for intercept
Z[1,2,] <- CUI_z             ## Nx1; predictor variable
A <- matrix(0)               ## 1x1; scalar = 0
R <- matrix("r")             ## 1x1; scalar = r


## ----univ.DLM.list, eval=TRUE--------------------------------------------------------------------
## only need starting values for regr parameters
inits_list <- list(x0 = matrix(c(0, 0), nrow = m))

## list of model matrices & vectors
mod_list <- list(B = B, U = U, Q = Q, Z = Z, A = A, R = R)


## ----univ.DLM.fit, eval=TRUE---------------------------------------------------------------------
## fit univariate DLM
dlm_s <- MARSS(dat, model = mod_list, inits = inits_list)


## ----dlm-plotdlm_s, eval=TRUE, echo=FALSE, fig=TRUE, fig.height=4, fig.width=6, fig.align="center"----
ylabs <- c(expression(alpha[t]), expression(beta[t]))
colr <- c("darkgreen","blue")
par(mfrow = c(m, 1), mar = c(4, 4, 0.1, 0), oma = c(0, 0, 2, 0.5))
for(i in 1:m) {
  mn <- dlm_s$states[i,]
  se <- dlm_s$states.se[i,]
  plot(years, mn, xlab = "", ylab = ylabs[i], bty = "n", xaxt = "n", type = "n",
       ylim = c(min(mn-2*se), max(mn+2*se)))
  lines(years, rep(0,TT), lty = "dashed")
  lines(years, mn, col = colr[i], lwd = 3)
  lines(years, mn+2*se, col = colr[i])
  lines(years, mn-2*se, col = colr[i])
}
axis(1, at = seq(1965, 2005, 5))
mtext("Year of ocean entry", 1, line = 3)


## ----univ.DLM.fore_mean, eval=TRUE---------------------------------------------------------------
## get list of Kalman filter output
kf_out <- MARSSkfss(dlm_s)

## forecasts of regr parameters; 2xT matrix
eta <- kf_out$xtt1

## ts of E(forecasts)
fore_mean <- vector()
for(t in 1:TT) {
  fore_mean[t] <- Z[,,t] %*% eta[, t, drop = FALSE]
}


## ----univ.DLM.fore_var, eval=TRUE----------------------------------------------------------------
## variance of regr parameters; 1x2xT array
Phi <- kf_out$Vtt1

## obs variance; 1x1 matrix
R_est <- coef(dlm_s, type="matrix")$R

## ts of Var(forecasts)
fore_var <- vector()
for(t in 1:TT) {
  tZ <- matrix(Z[, , t], m, 1) ## transpose of Z
  fore_var[t] <- Z[, , t] %*% Phi[, , t] %*% tZ + R_est
}


## ----dlm-plotdlmForeLogit, eval=TRUE, echo=FALSE, fig=TRUE, fig.height=3, fig.width=6, fig.align="center"----
par(mar = c(4, 4, 0.1, 0), oma = c(0, 0, 2, 0.5))
ylims=c(min(fore_mean - 2*sqrt(fore_var)), max(fore_mean+2*sqrt(fore_var)))
plot(years, t(dat), type = "p", pch = 16, ylim = ylims,
     col = "blue", xlab = "", ylab = "Logit(s)", xaxt = "n")
lines(years, fore_mean, type = "l", xaxt = "n", ylab = "", lwd = 3)
lines(years, fore_mean+2*sqrt(fore_var))
lines(years, fore_mean-2*sqrt(fore_var))
axis(1, at = seq(1965, 2005, 5))
mtext("Year of ocean entry", 1, line = 3)


## ----dlm-plotdlmForeRaw, eval=TRUE, echo=FALSE, fig=TRUE, fig.height=3, fig.width=6, fig.align="center"----
invLogit <- function(x) {
  1 / ( 1 + exp(-x))
  }
ff <- invLogit(fore_mean)
fup <- invLogit(fore_mean+2*sqrt(fore_var))
flo <- invLogit(fore_mean-2*sqrt(fore_var))
par(mar = c(4, 4, 0.1, 0), oma = c(0, 0, 2, 0.5))
ylims <- c(min(flo), max(fup))
plot(years, invLogit(t(dat)), type = "p", pch = 16, ylim = ylims,
     col = "blue", xlab = "", ylab = "Survival", xaxt = "n")
lines(years, ff, type = "l", xaxt = "n", ylab = "", lwd = 3)
lines(years, fup)
lines(years, flo)
axis(1, at = seq(1965, 2005, 5))
mtext("Year of ocean entry", 1, line = 3)


## ----dlmInnov, eval=TRUE, echo=TRUE--------------------------------------------------------------
## forecast errors
innov <- kf_out$Innov


## ----dlmQQplot, eval=FALSE, echo=TRUE------------------------------------------------------------
## ## Q-Q plot of innovations
## qqnorm(t(innov), main = "", pch = 16, col = "blue")
## ## add y=x line for easier interpretation
## qqline(t(innov))


## ----dlm-plotdlmQQ, eval=TRUE, echo=FALSE, fig=TRUE, fig.height=4, fig.width=4, fig.align="center"----
par(mai = c(0.8, 0.8, 0.2, 0.2), omi = c(0, 0, 0, 0))

## Q-Q plot of innovations
qqnorm(t(innov), main = "", pch = 16, col = "blue")
qqline(t(innov))


## ----dlmInnovTtest, eval=TRUE, echo=TRUE---------------------------------------------------------
## p-value for t-test of H0: E(innov) = 0
t.test(t(innov), mu = 0)$p.value


## ----dlmACFplot, eval=FALSE, echo=TRUE-----------------------------------------------------------
## ## plot ACF of innovations
## acf(t(innov), lag.max = 10)


## ----dlm-plotdlmACF, eval=TRUE, echo=FALSE, fig=TRUE, fig.height=4, fig.width=4, fig.align="center"----
par(mai = c(0.8, 0.8, 0.2, 0.2), omi = c(0, 0, 0, 0))

## ACF of innovations
acf(t(innov), lwd = 2, lag.max = 10)

