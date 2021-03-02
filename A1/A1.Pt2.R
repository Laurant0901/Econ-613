# Ex5
set.seed(10000)
x1 = runif(10000,1,3)
x2 = rgamma(10000,3,scale=2)
x3 = rbinom(10000,1,0.3)
noise = rnorm(10000,2,1)
y = 0.5 + 1.2*x1-0.9+0.1*x3 + noise
ydum = c()
ybar = mean(y)
i = 1
for(val in y) {
  if (val> ybar) {
    ydum[i] =1 
  }
  else {
    ydum[i] = 0
  }
  i = i+1;
}

# Ex6
x_all = cbind(rep(10000,1),x1,x2,x3)
## coefficients
beta_hat = solve(t(x_all) %*% x_all) %*% t(x_all) %*% y
print(beta_hat)
## standard error
s2 = sum((y-x_all%*%beta_hat)^2)/(10000-4)
var = s2*solve(t(x_all)%*%x_all)
se = diag(sqrt(var))
print(se)

# Ex7
## logit
logit = glm(ydum~x1+x2+x3,family=binomial(link = "logit"))
summary(logit)
## probit
probit = glm(ydum~x1+x2+x3,family=binomial(link = "probit"))
summary(probit)
## linear
linear = lm(ydum~x1+x2+x3)
summary(linear)

# Ex8
## logit
library("margins")
x = glm(ydum~x1+x2+x3,family=binomial(link = "logit"))
m = margins(x)
summary(m)
## probit
library("margins")
x = glm(ydum~x1+x2+x3,family=binomial(link = "probit"))
m = margins(x)
summary(m)

