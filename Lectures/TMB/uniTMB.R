#' Fit a univariate MARSS model with TMB.
#' 
#' @param y the data. Can have NAs.
#' @param estimate_drift estimate the u parameter
#' @param estimate_rho estimate b parameter
#'
#' @return A data frame with estimates and se's
#' @example inst/examples/uni_example.R
#' @author Eric Ward and edited by Eli Holmes.
#' @export
uniTMB <- function(y, estimate_drift = TRUE, estimate_rho = FALSE){
                   
parameters <- list(
  log_obs_sd = 0,
  log_pro_sd = 0,
  x = rep(0, length(y)),
  u = 0,
  #x0 = 0,
  logit_rho = 0
)

# Map off parameters not being estimated
tmb_map <- list(x = as.factor(c(NA,1:(length(y)-1))))
if(estimate_drift == FALSE) tmb_map <- c(tmb_map, list(u = factor(NA)))
if(estimate_rho == FALSE) tmb_map <- c(tmb_map, list(logit_rho = factor(NA)))

# Create TMB data
data_list <- list(Y = y, n = length(y),
                  est_drift = as.numeric(estimate_drift),
                  est_rho = as.numeric(estimate_rho),
                  keep = ifelse(!is.na(y),1,0),
                  par = list(matrix(1,2,2), matrix(2,2,2)),
                  model = "uni")

# Create object for fitting
obj <- TMB::MakeADFun(
  data = data_list,
  map = tmb_map,
  random = "x",
  parameters = parameters,
  DLL = "marssTMB_TMBExports",
  silent = TRUE
)

# Do the fitting with stats::nlminb, sometimes need to change default control args if not converging
pars <- stats::nlminb(
  start = obj$par, objective = obj$fn,
  gradient = obj$gr
)

par_summary <- summary(TMB::sdreport(obj))

indx <- grep("pred", rownames(par_summary))
df <- data.frame(
  pred = as.numeric(par_summary[indx,"Estimate"]),
  se = as.numeric(par_summary[indx,"Std. Error"]),
  y = y,
  t = 1:length(y)
)
vals <- c("u", "obs_sigma", "pro_sigma")
pardf <- data.frame(
  name = vals,
  estimate = as.numeric(par_summary[vals, "Estimate"]),
  se = as.numeric(par_summary[vals,"Std. Error"])
)

return(list(df = df, coef = pardf, all = par_summary))
}