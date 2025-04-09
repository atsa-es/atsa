
library(ggplot2)
library(viridis)
df = data.frame("id"=1:100000, "p" = rbeta(100000,1,1), "model"="prior")
df2 = data.frame("id"=1:100000, "p" = rbeta(100000,1+6,1+2), "model"="posterior")

df = rbind(df,df2)

jpeg("prior_posterior.jpg")
ggplot(df, aes(p,group=model,fill=model, col=model)) + 
  geom_density(alpha=0.6) + 
  xlab("Probability of heads") + 
  scale_color_viridis(discrete=TRUE,end=0.8) + 
  scale_fill_viridis(discrete=TRUE,end=0.8) + 
  theme_bw()
dev.off()
  

# bayesian regression
library(palmerpenguins)
library(tidybayes)
penguins = penguins %>% dplyr::filter(species=="Chinstrap")

fit = stan_glm(body_mass_g ~ flipper_length_mm, data=penguins)

alpha = fit$stanfit@sim$samples[[1]][[1]]
beta = fit$stanfit@sim$samples[[1]][[2]]
sigma= fit$stanfit@sim$samples[[1]][[3]]


