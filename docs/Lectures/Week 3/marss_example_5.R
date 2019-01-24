#################################################################
##  Example 5.  TWO hidden random walks but with the same parameters u and q.
##  We observe #1 2x and #2 once
#################################################################

library(MARSS)
library(MASS) #has mvrnorm
#x is the "hidden" trend we want to find
#x(t)=x(t-1)+slope+w(t), w(t)~N(0,q)
#x(0)=intercept
x0=1
u=0.05
q=.1
n=20
t=1:n
x=matrix(NA,2,n)
x[,1]=x0+u+rnorm(2,0,sqrt(q))
for(i in 2:n) x[,i]=x[,i-1]+u+rnorm(2,0,sqrt(q))

#y is our observation of the x's with error
r=0.1
nsites = 3
R=diag(r,nsites)
A=matrix(c(1, -2, -3),nsites,1)
Z=matrix(c(1,1,0,0,0,1),nsites,2)
y = matrix(NA,nsites,n)
for(i in 1:n) y[,i]=Z%*%x[,i]+mvrnorm(1,rep(0,nsites),R) + A
matplot(t(y), pch=as.character(1:nsites), ylab="Count", xlab="Time")
lines(x[1,], lwd=2)
lines(x[2,], lty=2, lwd=2)

mod.list=list(
  B="identity",
  U="equal",
  Q="diagonal and equal",
  x0="unequal",
  Z=factor(c(1,1,2)),
  A="scaling",
  R="diagonal and equal",
  tinitx=0)

fit=MARSS(y,model=mod.list)
lines(fit$states[1,], col="blue", lwd=2)
lines(fit$states[2,], col="blue", lwd=2, lty=2)
legend("bottomright", c("true pop size", "estimated pop size"), col=c("black","blue"), lty=1, lwd=2)

# Have MARSS tell you what model you just fit
summary(fit$model)
