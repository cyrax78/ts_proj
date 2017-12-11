library(astsa)
library(tseries)
library(forecast)
library(FinTS)
library(RCurl)
library(robustHD)
library(ggplot2)



#------------------tcehy-------------------

###### Load data from GitHub

x=getURL("https://raw.githubusercontent.com/cyrax78/ts_proj/master/data/tcehy.csv")
tcehy=read.csv(text=x,header = TRUE)


###### Original data

row.names(tcehy)=tcehy$X.U.FEFF.Date
plot(tcehy$HIGH)
tcehy$X.U.FEFF.Date =as.POSIXct(tcehy$X.U.FEFF.Date,format="%m/%d/%Y %H:%M")
ggplot(tcehy,aes(X.U.FEFF.Date,LAST_PRICE))+geom_line()+
  scale_x_datetime(name="DateTime",breaks=waiver(),
  date_breaks=waiver(),labels=waiver(),date_labels=waiver(),
  minor_breaks=waiver(),date_minor_breaks=waiver(),timezone=NULL,
  limits=NULL,expand=c(0,1),position="bottom")+
  xlab("")+ylab("Minute Observation of Last Price")+
  ggtitle("Tencent Last Prices from May 2017 to November 2017")+
  theme(plot.title=element_text(hjust=0.5))


###### Normalize the original data -- Obviously non-stationary

tcehy5=tcehy[,5]
tcehydata=ts(tcehy5,frequency=6.5*60)
normalize=function(x){(x-mean(x))/(sqrt(var(x)))}
n.tcehydata=normalize(tcehydata)

ggplot(n.tcehydata,aes(tcehy$X.U.FEFF.Date,n.tcehydata))+geom_line()+
  scale_x_datetime(name="DateTime",breaks=waiver(),
  date_breaks=waiver(),labels=waiver(),date_labels=waiver(),
  minor_breaks=waiver(),date_minor_breaks=waiver(),timezone=NULL,
  limits=NULL,expand=c(0,1),position="bottom")+
  xlab("")+ylab("Normalized Minute Observation of Last Price")+
  ggtitle("Normalized Tencent Last Prices from May 2017 to November 2017")+
  theme(plot.title=element_text(hjust=0.5))

acf(n.tcehydata,lag=39,main="ACF of Normalized Tencent Stock",xaxt="n") 
axis(1,c(0,seq(0.1/39*5,0.1/39*35,0.1/39*5)),c(0,seq(5,35,5)))
adf.test(n.techydata)

###### Examine seasonality

### Decompose data
decompose(tcehydata)
plot(decompose(tcehydata))  ##Seasonal effects are very small

### Periodogram
I=abs(fft(tcehy5))^2/51328  ##the periodogram 
P=(4/51328)*I[1:(51328/2)]  ##the scaled periodogram
f=0:(51328/2-1)/51328  ##frequencies
plot(f, P, type="l", xlab="Frequency", ylab="Scaled Periodogram",main="The Scaled Periodogram")
plot(f,P,type="l",main="The Scaled Periodogram",xlab="Frequency",ylab="Scaled Periodogram",ylim=c(0,1),xlim=c(0,0.01))


###### Difference the normalized data -- Stationary-- Arima(p,1,q)

d.n.tcehydata=diff(n.tcehydata)
### ACF
acf(d.n.tcehydata,5,xaxt="n") 
axis(1,c(0,seq(0.1/39*1,0.1/39*5,0.1/39*1)),c(0,seq(1,5,1)))
### PACF
pacf(d.n.tcehydata,5,xaxt="n") 
axis(1,c(0,seq(0.1/39*1,0.1/39*5,0.1/39*1)),c(0,seq(1,5,1)))
### ADF Test
adf.test(d.n.tcehydata)  ##pass Augmented Dickey-Fuller Test


###### Fit ARIMA model (p=0,1,2,3 q=0,1,2,3)
fit.1=arima(n.tcehydata,order=c(0,1,1))$aic
fit.2=arima(n.tcehydata,order=c(0,1,2))$aic
fit.3=arima(n.tcehydata,order=c(0,1,3))$aic
fit.4=arima(n.tcehydata,order=c(1,1,0))$aic
fit.5=arima(n.tcehydata,order=c(1,1,1))$aic
fit.6=arima(n.tcehydata,order=c(1,1,2))$aic
fit.7=arima(n.tcehydata,order=c(1,1,3))$aic
fit.8=arima(n.tcehydata,order=c(2,1,0))$aic
fit.9=arima(n.tcehydata,order=c(2,1,1))$aic
fit.10=arima(n.tcehydata,order=c(2,1,2))$aic
fit.11=arima(n.tcehydata,order=c(2,1,3))$aic
fit.12=arima(n.tcehydata,order=c(3,1,0))$aic
fit.13=arima(n.tcehydata,order=c(3,1,1))$aic
fit.14=arima(n.tcehydata,order=c(3,1,2))$aic
fit.15=arima(n.tcehydata,order=c(3,1,3))$aic

### Compare AIC value of all models
aicvalue=c(fit.1,fit.2,fit.3,fit.4,fit.5,fit.6,fit.7,fit.8,fit.9,fit.10,
           fit.11,fit.12,fit.13,fit.14,fit.15)
aicvalue[which(aicvalue==min(aicvalue))]
which(aicvalue==min(aicvalue))

### The best model is ARIMA(0,1,2)
fit.best=arima(n.tcehydata,order=c(0,1,2))
summary(fit.best)


###### Examine the residuals (fit.2)

### Plot residuals
res.2=residuals(fit.best)
ggplot(tcehy,aes(X.U.FEFF.Date,res.2))+geom_line()+
  scale_x_datetime(name="Time",breaks=waiver(),
  date_breaks=waiver(),labels=waiver(),date_labels=waiver(),
  minor_breaks=waiver(),date_minor_breaks=waiver(),timezone=NULL,
  limits=NULL,expand=c(0,1),position="bottom")+
  xlab("")+ylab("Residuals")+ggtitle("Residuals of ARIMA(0,1,2)")+
  theme(plot.title=element_text(hjust=0.5))


### Test for correlation
acf2(res.2)
Box.test(res.2,lag=40,type="Ljung-Box")  ##Uncorrelated
adf.test(res.2)  ##Stationary

### Test for normolity
qqnorm(res.2)
qqline(res.2,col="red")
shapiro.test(res.2)  ##sample size must be between 3 and 5000
jarque.bera.test(res.2)  ##2.2e-16

### Test for arch effect
ArchTest(res.2)  ##0.9998 no ARCH effect (homoscedastic)









