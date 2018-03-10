library(boot)
library(car) #for qqplot
library(forecast) #to get fitted arima values via fitted()
setwd("~/Desktop/531w18/midterm_project")
sql<-function(str){ #Function for fast querying with SQL
  require(sqldf)
  sqldf()
  ret<-sqldf(str,drv="SQLite")
  sqldf()
  return(ret)
}

genbal = read.csv('data//generic_polllist.csv')
genbal_sub = subset(genbal, pollster == 'Ipsos')
genbal_sub = genbal_sub[,colnames(genbal_sub)%in%c('startdate','enddate','pollster','samplesize','population','dem','rep')]
genbal_sub$enddate = as.Date(genbal_sub$enddate,'%m/%d/%Y')
genbal_sub$dem_margin = genbal_sub$dem - genbal_sub$rep

#There are three diff polls for each day (see 1,269,537 rows of genbal_sub) with varying sampe sizes)
#    startdate    enddate pollster samplesize population  dem  rep
#36  5/19/2017 2017-05-23    Ipsos        405         rv 43.0 36.1
#452 5/19/2017 2017-05-23    Ipsos        499          a 37.7 32.5
#767 5/19/2017 2017-05-23    Ipsos        405         rv 43.0 36.1

#Here, we just weight the three polls per day by the sample sizes (dem, rep, and dem_margin)
genbal_sub = sql("
select
  startdate
  ,enddate
  ,pollster
  ,sum(samplesize) as samplesize
  ,sum(samplesize*dem)/sum(samplesize) as dem
  ,sum(samplesize*rep)/sum(samplesize) as rep
  ,sum(samplesize*dem_margin)/sum(samplesize) as dem_margin
from genbal_sub gs
group by gs.enddate
")

#Plot untransformed democratic margin (diff in percentage points)
plot(genbal_sub$dem_margin,type='l')

#Plot untransformed time series of the differenced data
plot(diff(genbal_sub$dem_margin),type='l')

#AutoCorrelation plot
acf(genbal_sub$dem_margin)


layout(matrix(c(1,2,3,3), 2, 2, byrow = TRUE))
plot(genbal_sub$dem_margin ~ genbal_sub$enddate,type='l', 
     main='Time Series Plot -\nUnaltered Data',
     ylab = 'Democratic Margin (%-Points)',xlab='Poll End Date')
plot(diff(genbal_sub$dem_margin)~genbal_sub$enddate[2:nrow(genbal_sub)],type='l',
     main='Time Series Plot -\nDifferenced Data',
     ylab = 'Democratic Margin (%-Points)',xlab='Poll End Date')
acf(genbal_sub$dem_margin,main = 'Sample AutoCorrelation Plot')

plist = qlist = aiclist = c()
for(p in 0:6){
  for(q in 0:6){
    fit = arima(genbal_sub$dem_margin,order=c(p,1,q))
    plist = append(plist,p)
    qlist = append(qlist,q)
    aiclist = append(aiclist,fit$aic)
  }
}

aicdf = data.frame(p = plist, q = qlist, aic = aiclist)
#ARIMA(1,1,5) performs well by AIC criteria
aicdf

fit = arima(genbal_sub$dem_margin,order=c(1,1,5))

fit

print(paste('Roots of the AR Poly:',round(polyroot(c(1,-coef(fit)['ar1'])),3)))
print(paste('Modulus of the AR Poly:',round(abs(polyroot(c(1,-coef(fit)['ar1']))),3)))

paste('Roots of the MA Poly:',round(polyroot(c(1,-coef(fit)[c('ma1','ma2','ma3','ma4','ma5')])),3))
paste('Modulus of the MA Poly:',round(abs(polyroot(c(1,-coef(fit)[c('ma1','ma2','ma3','ma4','ma5')]))),3))


genbal_sub$fitted = fitted(fit)

layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
acf(fit$residuals,main='ARIMA(1,1,5) Residual AucoCorrelation Plot')
qqPlot(fit$residuals,main='ARIMA(1,1,5) Residual QQ-Plot',
       ylab = 'Residual Sample Quantiles',
       xlab = 'Standard Normal Quantiles')
plot(genbal_sub$fitted,fit$residuals,pch=16,
     main='ARIMA(1,1,5) Residual v. Fitted Plot',
     xlab = 'Fitted Values',
     ylab = 'Residuals')


#Use Loess smoothing to estimate trend nonparametrically:
dem_margin_loess = loess(genbal_sub$dem_margin ~ row.names(genbal_sub),span=0.5)

plot(dem_margin ~ enddate,genbal_sub,type='l',col='blue',
     main='Generic Ballot Ipsos Polling\n ARIMA(1,1,5) and Loess Smoothing',
     xlab = 'Poll End Date',
     ylab = 'Democratic Margin (%-Points)',bty="n")
lines(fitted ~ enddate,genbal_sub,type='l',lwd=2)
lines(genbal_sub$enddate,dem_margin_loess$fitted,type='l',col='red',lwd=2)
legend('topleft',
       c('ARIMA(1,1,5) Fit','Loess Smoothing','Observed'), 
       col = c('black','red','blue'), lty = 1,cex=0.8) 

#Forecast 7 days out
future = forecast(fit, h = 7)
plot(future)

############################################################################
# Investigate association b/t Trump approval and generic ballot results
############################################################################
approve = read.csv('data//approval_polllist.csv')
approve_sub = subset(approve, pollster == 'Ipsos')
approve_sub = approve_sub[,colnames(approve_sub)%in%c('startdate','enddate','pollster','samplesize','population','disapprove','approve')]
approve_sub$enddate = as.Date(approve_sub$enddate,'%m/%d/%Y')
approve_sub$dis_margin = approve_sub$disapprove - approve_sub$approve

approve_sub = sql("
select
  startdate
  ,enddate
  ,pollster
  ,sum(samplesize) as samplesize
  ,sum(samplesize*disapprove)/sum(samplesize) as disapprove
  ,sum(samplesize*approve)/sum(samplesize) as approve
  ,sum(samplesize*dis_margin)/sum(samplesize) as dis_margin
from approve_sub asub
group by asub.enddate
")

#R's default origin is 1970-01-01
approve_sub$enddate = as.Date(approve_sub$enddate,origin = "1970-01-01")

#Take only the approval rating polls with dates that match up with the 
#generic ballot polling

approve_sub = subset(approve_sub,enddate >= min(genbal_sub$enddate))

#Plot untransformed disapproval margin (diff in percentage points)
plot(approve_sub$dis_margin,type='l')

#Plot untransformed time series of the differenced data
plot(diff(approve_sub$dis_margin),type='l')

#AutoCorrelation plot
acf(approve_sub$dis_margin)

plot(genbal_sub$dem_margin,approve_sub$dis_margin,pch=19,
     main = 'Trump Disapproval Margin v. Democratic Midterm Support Margin',
     xlab = 'Democratic Midterm Support',
     ylab = 'Trump Disapproval',
     bty='n')

#Use Loess smoothing to estimate trend nonparametrically:
dis_margin_loess = loess(approve_sub$dis_margin ~ row.names(approve_sub),span=0.5)

plot(dem_margin ~ enddate,genbal_sub,type='l',col='blue',
     main='Trump Disapproval Margin and Dem. Midterm Support Margin',
     xlab = 'Poll End Date',
     ylab = 'Democratic Margin (%-Points)',ylim=c(2,27),bty="n")
lines(dis_margin ~ enddate,approve_sub,type='l',lwd=1,col='darkorange',lty='longdash')
lines(genbal_sub$enddate,dem_margin_loess$fitted,type='l',col='blue',lwd=2)
lines(genbal_sub$enddate,dis_margin_loess$fitted,type='l',col='darkorange',lwd=2,lty='longdash')
legend('topleft',
       c('Trump Disapproval Margin','Democratic Generic Ballot Margin'), 
       col = c('darkorange','blue'), lty = c(5,1),cex=0.8) 

#Run ordinary least-squares regression (acting as if errors are uncorrelated)
fitlinreg = lm(genbal_sub$dem_margin~genbal_sub$enddate+approve_sub$dis_margin)
res = fitlinreg$residuals
acf(res,main='AutoCorrelation Plot of Residuals')
qqPlot(res)

#Identify arma model for errors (using residuals as observed errors)
plist = qlist = aiclist = c()
for(p in 0:6){
  for(q in 0:6){
    fit = arima(res,order=c(p,0,q))
    plist = append(plist,p)
    qlist = append(qlist,q)
    aiclist = append(aiclist,fit$aic)
  }
}

aicdf = data.frame(p = plist, q = qlist, aic = aiclist)
#MA(4) performs well by AIC criteria for these errors
aicdf

#Regress democratic margin on trump disapproval with MA(4) errors
fitreg = arima(genbal_sub$dem_margin,order=c(0,0,4),
            xreg = cbind(TRUMPDISAPPROVAL = approve_sub$dis_margin))

acf(residuals(fitreg))
qqPlot(residuals(fitreg))
plot(fitted(fitreg),residuals(fitreg))

fitreg_detrended = arima(genbal_sub$dem_margin,order=c(0,0,4))

chi_sq_stat = 2*(fitreg$loglik - fitreg_detrended$loglik)
chi_sq_stat
qchisq(.95,1,lower.tail=T)

plot(dem_margin ~ enddate,genbal_sub,type='l',col='blue',
     main='Regressing Dem. Midterm Support on Trump Disapproval with MA(4) Errors',
     xlab = 'Poll End Date',
     ylab = 'Democratic Margin (%-Points)',bty="n")
lines(genbal_sub$enddate,dem_margin_loess$fitted,type='l',col='blue',lwd=2)
lines(fitted(fitreg) ~ genbal_sub$enddate,type='l',col='red',lwd=2,lty='dotted')
legend('topleft',
       c('Dem. Generic Ballot Margin','Lin. Regress. w/ MA(4) Errors Fit'), 
       col = c('blue','red'), lty = c(1,3),lwd = c(1,2),cex=0.8) 

##################################################################################

par(mfrow=c(2,1))
#unsmoothed periodogram
spectrum(genbal_sub$dem_margin,main='Unsmoothed Periodogram')
#smoothed periodogram
spec = spectrum(genbal_sub$dem_margin,spans=c(3,5,3),main='Smoothed Periodogram')

#Dominant frequency is
spec$freq[which.max(spec$spec)] #cycles per day
#Dominant cycle is
1/spec$freq[which.max(spec$spec)] #days per cycle

#second most dominant frequency is
spec$freq[which(spec$freq>0.09 & spec$freq < .15)[which.max(spec$spec[which(spec$freq>0.09 & spec$freq < .15)])]]
1/spec$freq[which(spec$freq>0.09 & spec$freq < .15)[which.max(spec$spec[which(spec$freq>0.09 & spec$freq < .15)])]]

