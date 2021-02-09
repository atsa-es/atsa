






## Fitting a DLM with time varying intercept
```{r echo=TRUE, fig.height=2}
df = data.frame("year"=1:42,
"pred"=apply(extract(mod, "intercept", permuted=FALSE), 3, mean),
"low"=apply(extract(mod, "intercept", permuted=FALSE), 3, quantile,0.025),
"hi"=apply(extract(mod, "intercept", permuted=FALSE), 3, quantile,0.975))
ggplot(df, aes(year,pred)) + 
  geom_ribbon(aes(ymin=low,ymax=hi),fill="grey30",alpha=0.5) + 
  geom_line() + ylab("Intercept") + xlab("Time") + 
  theme_bw()
```


## Fitting a DLM with single intercept and time-varying slope

```{r cache=TRUE, echo=TRUE, eval=FALSE}
mod_slope = fit_stan(y = SalmonSurvCUI$logit.s, 
x = SalmonSurvCUI$CUI.apr, model_name="dlm-slope")
```

```{r, fig.height=2, eval=FALSE}
df = data.frame("year"=1:42,
"pred"=apply(extract(mod, "beta", permuted=FALSE), 3, mean),
"low"=apply(extract(mod, "beta", permuted=FALSE), 3, quantile,0.025),
"hi"=apply(extract(mod, "beta", permuted=FALSE), 3, quantile,0.975))
ggplot(df, aes(year,pred)) + 
  geom_ribbon(aes(ymin=low,ymax=hi),fill="grey30",alpha=0.5) + 
  geom_line() + ylab("Beta[1]") + xlab("Time") + 
  theme_bw()
```


## Fitting a DLM time-varying intercept and time-varying slope

* Use model.matrix() to specify x
```{r echo=TRUE, eval=FALSE}
lmmod = lm(SalmonSurvCUI$logit.s ~ SalmonSurvCUI$CUI.apr)
x = model.matrix(lmmod)
```

```{r echo=TRUE, eval=FALSE}
lmmod = lm(SalmonSurvCUI$logit.s ~ SalmonSurvCUI$CUI.apr)
mod = fit_stan(y = SalmonSurvCUI$logit.s, 
               x = model.matrix(lmmod), 
               model_name="dlm")
```

## More time series models: univariate state space models

Estimates from the AR(1) state space model
```{r echo=FALSE}
ss_ar = fit_stan(y = SalmonSurvCUI$logit.s, 
                 est_drift=FALSE, 
                 model_name = "ss_ar")
ss_rw = fit_stan(y = SalmonSurvCUI$logit.s, 
                 est_drift=FALSE, 
                 model_name = "ss_rw")

pars_ar = broom.mixed::tidy(ss_ar)
pars_ar = pars_ar[grep("pred",pars_ar$term),]
pars_ar$low = pars_ar$estimate - 1.96*pars_ar$std.error
pars_ar$hi = pars_ar$estimate + 1.96*pars_ar$std.error
pars_ar$x = seq(1,nrow(pars_ar))

df = data.frame("x"=pars_ar$x, "y" = SalmonSurvCUI$logit.s, stringsAsFactors=FALSE)
ggplot(pars_ar, aes(x, estimate)) + 
geom_ribbon(aes(ymin=low,ymax=hi),fill="grey70") + 
xlab("Time") + ylab("Estimate (AR)") + 
geom_point(data=df, aes(x,y), col="blue", alpha=0.6, size=2)  + 
  theme_bw()
```

## More time series models: univariate state space models

Estimates from the RW state space model
```{r}
pars_rw = broom.mixed::tidy(ss_rw)
pars_rw = pars_rw[grep("pred",pars_rw$term),]
pars_rw$low = pars_rw$estimate - 1.96*pars_rw$std.error
pars_rw$hi = pars_rw$estimate + 1.96*pars_rw$std.error
pars_rw$x = seq(1,nrow(pars_rw))

df = data.frame("x"=pars_rw$x, "y" = SalmonSurvCUI$logit.s, stringsAsFactors=FALSE)
ggplot(pars_rw, aes(x, estimate)) + 
geom_ribbon(aes(ymin=low,ymax=hi),fill="grey70") + 
xlab("Time") + ylab("Estimate (AR)") + 
geom_point(data=df, aes(x,y), col="blue", alpha=0.6, size=2) + 
  theme_bw()
```

## More time series models: univariate state space models

Estimates from both models (note the difference in credible interval widths)
```{r}
pars_rw$model = "RW"
pars_ar$model = "AR"
pars = rbind(pars_rw,pars_ar)
df = data.frame("x"=pars_rw$x, "y" = SalmonSurvCUI$logit.s, stringsAsFactors=FALSE)
ggplot(pars, aes(x, estimate, fill=model, group=model)) + 
geom_ribbon(aes(ymin=low,ymax=hi,fill=model, alpha=0.3)) + 
xlab("Time") + ylab("Estimate") + 
geom_point(data=df, aes(x,y,group=NA,fill=NA), col="blue", alpha=0.6, size=2) + 
  theme_bw()
```

## More time series models: univariate state space models

We might be also interested in looking at posterior estimates for these models.

* What is the posterior distribution of $\phi$?

```{r echo=TRUE, fig.width=3, fig.height=1.5}
rstan::stan_dens(ss_ar, c("phi"))
```

* This shows with a value near 1, the behavior of the model should be very similar to the random walk (from the predictions, it is!). The multi-modal distribution probably is an indicator of convergence issues

## More time series models: univariate state space models

We might also be interested in derived quantities. 

* For example, what's the probability of temperature exceeding 90 degrees? 
  ```{r, echo=TRUE, results='markup'}
pars = extract(ss_ar)
p = length(which(pars$pred > 90)) / length(pars$pred)
print(round(p,3))
```








## Tidy summaries from Stan output
These tidy summaries can then be fed into ggplot for example
```{r, echo=TRUE, fig.height=3, fig.width=3, eval=FALSE}
coef = broom.mixed::tidy(lm_intercept)
ggplot(coef[grep("pred",coef$term),], aes(x = 1:100,y=estimate)) + 
  geom_point() + ylab("Estimate +/- SE")+ xlab("")+
  geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error)) + 
  theme_bw()
```


