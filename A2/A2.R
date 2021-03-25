library(bayesm)
data(margarine)

# EX1
choiceprice <- margarine$choicePrice
demos <- margarine$demos

# Average & dispersion
apply(choiceprice[,3:12], 2, mean)
apply(choiceprice[,3:12], 2, sd)

# Market share
choicefreq <- data.frame(table(choiceprice$choice)) 
x <- choicefreq$Freq
y <- x/4470
marketshare <- data.frame(y)
colnames(marketshare) <- c("marketshare")
rownames(marketshare) <- c("PPk_Stk", "PBB_Stk", "PFl_Stk",  "PHse_Stk", "PGen_Stk", "PImp_Stk", "PSS_Tub",  "PPk_Tub",  "PFl_Tub",  "PHse_Tub")
marketshare
# Mapping
demoprice <- merge(x = choiceprice, y = demos, by = "hhid", all.x = TRUE)
# income and choice
table(demoprice[,c(2,13)])
# family size and choice
table(demoprice[,c(2,14)])
table(demoprice[,c(2,15)])
table(demoprice[,c(2,16)])
# education status and choice
table(demoprice[,c(2,17)])
# job status and choice
table(demoprice[,c(2,18)])
# retirement status and choice
table(demoprice[,c(2,19)])




  
rm(list = ls())


# The code used to run well and gave me the answer, but I did some wrong editing
# and now it's not working well...I still kept most of them here (I think my logit is sound), 
# and I hope I can get some partial credit...


# EX2
clog = function(beta){
  u = datchoice[,3:12]*beta[10]
  m1 = matrix(rep(beta[1:10],each=4470),nrow=4470,ncol=10)
  u = cbind(0,m1)+u
  prob = exp(u)/rowSums(exp(u))
  like = sum(log(prob))
  return(-like)
}

set.seed(999)
param1=runif(10,-1,1)
clogit=optim(param1,clog,method="BFGS")
clogit$par


# EX3
Y=matrix(0,4470,10)
mlogit1=function(beta){
  beta0=matrix(rep(c(0,beta[1:9]),4470)
  beta1=matrix(rep(c(0,beta[1:9]),4470)
  X = cbind(choiceprice[,3],choiceprice[,4],choiceprice[,5],choiceprice[,6],choiceprice[,7],choiceprice[,8],choiceprice[,9],choiceprice[,10],choiceprice[,11],choiceprice[,12])
  Xbeta=beta0+beta1*X
}
  
mlogit <- function(Y, X, beta){
  like <- sum(Y*log(exp(beta0+beta1*X)/rowSums(exp(beta0+beta1*X))))
  return(-like)
}

multilogit <- optim(function(beta) mlogit(Y=Y,X=X,beta=beta),par=rep(0,10),method="BFGS")
multilogit$par


# EX4

# EX2 model
beta = clogit$par
u = datchoice[,3:12]*beta[10]
m1 = matrix(rep(beta[1:10],each=4470),nrow=4470,ncol=10)
u = cbind(0,m1)+u
prob = exp(u)/rowSums(exp(u))
ame = matrix(0,1,10)
for (i in 1:10) {
  ame[1,i] = mean(prob[,i]*(beta[i]-sum(prob[,i]*beta[i]))) 
}
ame

# EX3 model
beta = multilogit$par
beta0 = matrix(rep(c(0,beta[1:9]),4470)
beta1 = matrix(rep(c(0,beta[1:9]),4470)
X = cbind(choiceprice[,3],choiceprice[,4],choiceprice[,5],choiceprice[,6],choiceprice[,7],choiceprice[,8],choiceprice[,9],choiceprice[,10],choiceprice[,11],choiceprice[,12])
Xbeta = beta0+beta1*X
prob = exp(Xbeta)/rowSums(exp(Xbeta))
for (i in 1:10) {
  ame[1,i] = mean(prob[,i]*(beta[i]-sum(prob[,i]*beta[i]))) 
}
ame


#EX5
```{r}
#mixed logit model , optimizing over multiple values w_i
mixlogitf=function(inputs){
  beta = inputs[[1]]
  wgamma = inputs[[2]]
  beta0=matrix(rep(c(0,beta[1:9]),4470)
  beta1=matrix(rep(c(0,beta[1:9]),4470)
  X = cbind(choiceprice[,3],choiceprice[,4],choiceprice[,5],choiceprice[,6],choiceprice[,7],choiceprice[,8],choiceprice[,9],choiceprice[,10],choiceprice[,11],choiceprice[,12])
  Xbeta=beta0+beta1*X
  mlogit <- function(Y, X, beta){
    like <- sum(Y*log(exp(beta0+beta1*X)/rowSums(exp(beta0+beta1*X))))
    return(-like)
  }
}
param2=runif(18,-1, 1)
param3 = runif(4470,-1,1)
inputs = list()
inputs[[1]] = param2
inputs[[2]] = param3
model4=optim(param2,mlogit,method="BFGS")
model4$par[1:18]


#mixed logit model , remove data from one choice
mixlogitf=function(inputs){
  beta = inputs[[1]]
  wgamma = inputs[[2]]
  beta0=matrix(rep(c(0,beta[1:9]),4469)
  beta1=matrix(rep(c(0,beta[9:16]),4469)
  X = cbind(choiceprice[,3],choiceprice[,4],choiceprice[,5],choiceprice[,6],choiceprice[,7],choiceprice[,8],choiceprice[,9],choiceprice[,10],choiceprice[,11])
  Xbeta=beta0+beta1*X
  mlogit <- function(Y, X, beta){
    like <- sum(Y*log(exp(beta0+beta1*X)/rowSums(exp(beta0+beta1*X))))
    return(-like)
  }
}
param2=runif(18,-1, 1)
param3 = runif(4469,-1,1)
inputs = list()
inputs[[1]] = param2
inputs[[2]] = param3
model5=optim(param2,mlogit,method="BFGS")
model5$par[1:18]


  