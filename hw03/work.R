setwd("~/Desktop/531w18/hw03")

#dat <- read.table(file="http://ionides.github.io/531w18/01/ann_arbor_weather.csv",header=TRUE)
dat = read.csv('mich_jan_dat.csv',header = T)

#time plot
plot(Low~Year,data=dat,type="l")

#autocorrelation function plot
acf(dat$Low,na.action = na.pass)

#Fit many ARMA models, using AIC as model selection criteria
aic_table <- function(data,P,Q){
  table <- matrix(NA,(P+1),(Q+1))
  for(p in 0:P) {
    for(q in 0:Q) {
      table[p+1,q+1] <- arima(data,order=c(p,0,q))$aic
    }
  }
  dimnames(table) <- list(paste("<b> AR",0:P, "</b>", sep=""),paste("MA",0:Q,sep=""))
  table
}

dat_aic_table <- aic_table(dat$Low,5,5)
#require(knitr)
#kable(huron_aic_table,digits=2)

#Lowest AIC suggests ARMA(1,1), ARMA(2,1), ARMA(1,2), ARMA(2,5)
arma11 = arima(dat$Low,order = c(1,0,1))
arma11

#Get roots of the AR polynomial
AR_roots = polyroot(c(1,-coef(arma11)[c("ar1")]))
abs(AR_roots) #outside unit circle -- suggest stationary causal ARMA model


#Get roots of the MA polynomial
MA_roots = polyroot(c(1,-coef(arma11)[c("ma1")]))
abs(MA_roots) #outside unit circle -- implying model is invertible

#MA1 Analysis
#
K <- 500
ma1 <- seq(from=-1.1,to=1.1,length=K)
profile_loglik <- rep(NA,K)
for(k in 1:K){
  profile_loglik[k] <- logLik(arima(dat$Low,order=c(1,0,1),
  fixed=c(NA,ma1[k],NA)))
}
#Profile likelihood plot
plot(profile_loglik~ma1,ty="l")

ma1_max = ma1[which.max(profile_loglik)]
ma1_prof_conf = ma1[which(profile_loglik[which.max(profile_loglik)] - profile_loglik < 1.92)]

# 95% Profile likelihood interval
#round(c(min(ma1_prof_conf), max(ma1_prof_conf)),4)

# 95% wald confidence interval
round(arma11$coef['ma1'] + c(-1,1) * 1.96*sqrt(arma11$var.coef[2,2]),4)

#Simulation study for MA1 & AR1 coefficient
set.seed(57892330)
J <- 1000
params <- coef(arma11)
ar <- params[grep("^ar",names(params))]
ma <- params[grep("^ma",names(params))]
intercept <- params["intercept"]
sigma <- sqrt(arma11$sigma2)
theta <- matrix(NA,nrow=J,ncol=length(params),dimnames=list(NULL,names(params)))
for(j in 1:J){
  Yj <- arima.sim(
    list(ar=ar,ma=ma),
    n=length(dat$Low),
    sd=sigma
    )+intercept
  theta[j,] <- coef(arima(Yj,order=c(1,0,1),method='ML'))
}
hist(theta[,"ma1"],freq=FALSE)
hist(theta[,"ar1"],freq=FALSE)

plot(density(theta[,"ma2"],bw=0.05,na.rm = T))
plot(density(theta[,"ar1"],bw=0.05,na.rm = T))
#par(mfrow=c(1,2))
