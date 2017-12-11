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








#------------------New Technique-------------------


install.packages("RCurl")
library(RCurl)
install.packages("robustHD")
library(robustHD)
## load data
x=getURL("https://raw.githubusercontent.com/cyrax78/ts_proj/master/data/MP-13%20Raw%20Data.csv")
MP13=read.csv(text=x,header = TRUE,sep = "\t")
x=getURL("https://raw.githubusercontent.com/cyrax78/ts_proj/master/data/tcehy.csv")
tcehy=read.csv(text=x,header = TRUE)
## plot MP and Original Data in the same graph
par(mar = c(5,5,2,5))
with(MP13, plot(MP13$Order, MP13$MP, type="l", col="red3", 
                ylab="MP",
                ylim=c(0,5)))

par(new = T)
with(tcehy, plot(MP13$Order, tcehy$LAST_PRICE[1:(length(tcehy$LAST_PRICE)-13+1)], type="l",pch=16, axes=F, xlab=NA, ylab=NA, cex=1.2))
axis(side = 4)
mtext(side = 4, line = 3, 'Last Price')
legend("top",
       legend=c("MP", "Last Price"),
       lty=c(1,1), col=c("red3", "black"))

## locate top discord and top motif
location_discord=which.max(MP13$MP)
location_motif=which.min(MP13$MP)
location_motif_pair=MP13$Index[location_motif]

## plot discord vs last price
title("Top discord from 6/2/2017 14:34 to 6/2/2017 14:47")
par(mar = c(5,5,2,5))
with(MP13, plot(MP13$Order[location_discord:(location_discord+13)], MP13$MP[location_discord:(location_discord+13)], type="l", col="red3", 
                ylab="MP",
                ylim=c(0,5)))

par(new = T)
with(tcehy, plot(MP13$Order[location_discord:(location_discord+13)], tcehy$LAST_PRICE[location_discord:(location_discord+13)], type="l",pch=16, axes=F, xlab=NA, ylab=NA, cex=1.2,ylim=c(34.9,35)))
axis(side = 4)
mtext(side = 4, line = 3, 'Last Price')
legend("top",
       legend=c("MP", "Last Price"),
       lty=c(1,1), col=c("red3", "black"))

# tcehy$NUMBER_TICKS[location_discord:(location_discord+13)]
# [1]  3  3  5  4  2  2  6  3  2  2  5  5 12 
# number of ticks is low

## plot top motif
title("Top motif on 6/2/2017 vs 8/18/2017")
par(mar = c(5,5,2,5))
with(tcehy, plot(tcehy$LAST_PRICE[location_motif:(location_motif+13)], type="l", col="red3", 
                 ylab="Last Price from 6/2/2017 11:58 to 6/2/2017 12:11",
                 ylim=c(34.859,34.871)))
par(new = T)
with(tcehy, plot(tcehy$LAST_PRICE[location_motif_pair:(location_motif_pair+13)], type="l",pch=16, axes=F, xlab=NA, ylab=NA, cex=1.2,ylim=c(41.969,41.99)))
axis(side = 4)
mtext(side = 4, line = 3, 'Last Price from 8/18/2017 14:50 to 8/18/2017 15:03')
legend("bottomright",
       legend=c("Last Price on 6/2/2017", "Last Price on 8/18/2017"),
       lty=c(1,1), col=c("red3", "black"))








