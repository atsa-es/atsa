#################################################################
##  Example 3.  The hidden x is a random walk.
##  We observe it multiple times but some observations from a similar
##  type of survey and others from another
#################################################################

library(MARSS)
library(MASS) #has mvrnorm
set.seed(123)
#x is the "hidden" trend we want to find
#x(t)=x(t-1)+slope+w(t), w(t)~N(0,q)
#x(0)=intercept
x0=1
u=0.05
q=.1
n=20
t=1:n
x=x0+u+rnorm(1,0,sqrt(q))
for(i in 2:n) x[i]=x[i-1]+u+rnorm(1,0,sqrt(q))

#y is our observation of x with error
#first 2 surveys have the same variance and 3rd is different
r=c(.1,.1,1) #Two are the same
nsites = 3
R=diag(r,nsites)
A=matrix(c(1, -2, -3),nsites,1)
y = matrix(NA,nsites,n)
for(i in 1:n) y[,i]=x[i]+mvrnorm(1,rep(0,nsites),R) + A
matplot(t(y), pch=as.character(1:nsites), ylab="Count", xlab="Time")
lines(x)

#Let's write x as a AR-1 model and y as an observation of that
R.model = matrix(list(0),nsites,nsites)
diag(R.model)=c("ship","ship","land")
R.model

mod.list=list(
  B=matrix(1),
  U=matrix("u"),
  Q=matrix("q"),
  x0=matrix("mu"),
  Z=matrix(1,nsites,1),
  A="scaling",
  R=R.model,
  tinitx=0)

fit3=MARSS(y,model=mod.list)
lines(fit3$states[1,], col="blue", lwd=2)
legend("bottomright", c("true pop size", "estimated pop size"), col=c("black","blue"), lty=1, lwd=2)
