library(astsa)
library(tseries)
library(forecast)

################ ------------------tcehy-------------------

tcehy=read.table("C:/tcehy.txt",sep=",",head=TRUE)
tcehy5=tcehy[,5]
head(tcehy5,n=5)
tail(tcehy)

plot(tcehy5[2:51328],tcehy5[1:51327])
tcehydata=ts(tcehy5,frequency=6.5*60)

#,frequency=365*24*60
#tcehydata=tcehydata[,5]
head(tcehydata)
ts.plot(tcehydata)
#ts.plot(diff(tcehydata))
tail(tcehydata)

l.tcehydata=log(tcehydata)
plot(l.tcehydata)
acf(l.tcehydata)
pacf(l.tcehydata)
tseries::adf.test(l.tcehydata)   #0.3298

#### Differencing log data
#tcehydata=tcehydata-mean(tcehydata)
d.l.tcehydata=diff(l.tcehydata)
ts.plot(d.l.tcehydata)
acf(d.l.tcehydata)
pacf(d.l.tcehydata)
tseries::adf.test(d.l.tcehydata) 

#### Fit arima model
fit1=auto.arima(l.tcehydata,allowdrift=FALSE)
summary(fit1)   ##Arima(2,1,0)

#fit1=auto.arima(l.tcehydata)
#summary(fit1)   ##Arima(2,1,0)

### Examine the residuals
res1=residuals(fit1)
head(res1)
res1=res1[1:length(res1)]
plot(res1)
qqnorm(res1)
qqline(res1,col="red")
Box.test(res1,type="Ljung-Box") ##0.975 
#residuals are random and that the model provides an adequate fit to the data.

#arch test
ArchTest(res1)   ##0.9999
#Null hypothesis: no ARCH effects

#fitted
tcehy.fitted=fitted(fit1)
plot(tcehydata)
lines(exp(tcehy.fitted),col="red")
legend("topleft",c("Original Tcehy","Fitted Tcehy"),col=c("black","red"),
       lty=1)

#### RMSE for fit1 (also can be obtained by summary(fit1))
RMSE1=sqrt(mean(res1^2))
RMSE1

#### Forecast
fore1=forecast(fit1,h=10)
fore1$mean=exp(fore1$mean)
fore1$x=exp(fore1$x)
fore1$lower=exp(fore1$lower)
fore1$upper=exp(fore1$upper)
fore1
plot(fore1$mean)


############################ train and test
length(tcehydata)
100*6.5*60
m=100
daym=51320
head(tcehy5)

train.prev=ts(start=daym+1,end=51328)  ##51321~51328
train.se=ts(start=daym+1,end=51328)
tcehy5=tcehy[,5]
tcehy5.0=rep(1,51328)
tcehy5.0[1:daym]=tcehy5[1:daym]  ##1~51320
head(tcehy5.0)
tail(tcehy5.0)
length(tcehy5.0)
tcehy5.0.temp=c()

for (i in daym:51327){
  tcehy5.0.temp=log(ts(tcehy5.0))
  train.temp=Arima(window(tcehy5.0.temp,start=1,end=i),model=fit1)
  train.prev[i-daym+1]=exp(predict(train.temp,n.ahead=1)$pred)
  #train.se[i-daym+1]=exp(predict(train.temp,n.ahead=1)$se)
  tcehy5.0[i+1]=exp(predict(train.temp,n.ahead=1)$pred)
}

#for (i in 51320:51327){
  tcehy5.0.temp=window(ts(tcehy5.0),start=1,end=i)   #end=51321...51328
  train.temp=Arima(tcehy5.0.temp,model=fit1)
  train.prev[i-51320+1]=forecast(train.temp,h=1)$mean   #i=1...8
  #train.se[i-daym+1]=exp(predict(train.temp,n.ahead=1)$se)
  tcehy5.0[i+1]=train.prev[i-51320+1]       #tcehy5.0[51321...51328] train.prev[1..8]
}

train.prev
tcehy5[51320+1]
tcehy5.0[51320+1]
head(tcehy5)
length(tcehy5.0)


plot(window(tcehy5,start=1,end=809),col="red")
lines(data.prev.dyn,col="blue")
lines(data.prev.dyn+1.00*data.se.dyn,col="green")
lines(data.prev.dyn-1.00*data.se.dyn,col="green")






############## -------------Spy data-----------------
spy=read.table("C:/spy.txt",sep=",",head=TRUE)
spy5=spy[,5]
head(spy5)
length(spy5)
plot(spy5[2:53539],spy5[1:53539])

spydata=ts(spy5,frequency=6.5*60)
head(spydata)
ts.plot(spydata)

l.spydata=log(spydata)
#l.spydata0=l.spydata-mean(l.spydata)
ts.plot(l.spydata)
acf(l.spydata)
pacf(l.spydata)
adf.test(l.spydata)    ##0.1371

#### Differencing log data
d.l.spydata=diff(l.spydata)
ts.plot(d.l.spydata)
acf(d.l.spydata)
pacf(d.l.spydata)
adf.test(d.l.spydata)

#### Fit arima model
fit2=auto.arima(l.spydata,allowdrift=FALSE)
summary(fit2)   ###ARIMA(3,1,0)


#### Examine residuals
res2=residuals(fit2)
head(res2)
res2=res2[1:length(res2)]
ts.plot(res2)
qqnorm(res2)
qqline(res2)
tsdisplay(res2)
Box.test(res2,type="Ljung-Box") ##0.9909
jarque.bera.test(res2)  ##< 2.2e-16


spy.fitted=fitted(fit2)
plot(spydata)
lines(exp(spy.fitted),col="red")


##RMSE for fit2 (also can be obtained by summary(fit2))
RMSE.spy=sqrt(mean((res2)^2))
RMSE.spy   ##0.0002098099

##Forecast
fore2=forecast(fit2,h=5)
fore2$mean=exp(fore2$mean)
fore2$x=exp(fore2$x)
fore2$lower=exp(fore2$lower)
fore2$upper=exp(fore2$upper)
fore2
plot(fore2$mean)
plot(fore2)
#,xlim=c(132.59,132.63),ylim=c(54.5,55.5)


######
hold <- window(ts(spy5), start=53530)
fit_no_holdout = arima(ts(spy5[-c(53530:53539)]), order=c(3,1,0))
fcast_no_holdout <- forecast(fit_no_holdout,h=9)
plot(fcast_no_holdout, main=" ",xlim=c(53455,53540),ylim=c(257,259))
lines(ts(spy5))
