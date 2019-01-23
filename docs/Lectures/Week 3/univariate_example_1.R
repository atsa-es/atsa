#################################################################
##  Univariate state-space models
##  Example 1.  Fit a model with a hidden x is a random walk.
#################################################################

#We will fit this model
#x(t)=x(t-1)+slope+w(t), w(t)~N(0,q)
#x(0) = intercept
#y(t) = x(t) + v(t), v(t)~N(0,r)

# Create some data that are observations of a hidden random walk
# Notice that the code is the same as the x equation
intercept=1
slope=0.5
q=.1
r=.2
n=50
t=1:n
x=intercept+slope+rnorm(1,0,sqrt(q))
for(i in 2:n) x[i]=x[i-1]+slope+rnorm(1,0,sqrt(q))
plot(x,xlim=c(1,n),type="l")

#y is our observation of x with error
y=x+rnorm(n,0,sqrt(r))
points(y)

# To fit with MARSS, we write the model as a list.  Each parameter
# in the list has the same structure as the equation
mod.list=list(
  U=matrix("slope"),
  x0=matrix("intercept"),
  B=matrix(1),
  Q=matrix("q"),
  Z=matrix(1),
  A=matrix(0),
  R=matrix("r"),
  tinitx=0)

fit2=MARSS(y,model=mod.list)
lines(fit2$states[1,], col="blue", lwd=2)
est.slope=coef(fit2)$U[,1]
est.intercept=coef(fit2)$x0[,1]
est.r=coef(fit2)$R[,1]
est.q=coef(fit2)$Q[,1]
