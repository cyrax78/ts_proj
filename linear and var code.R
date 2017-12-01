tcehy=read.table("C:/tcehy.txt",sep=",",head=TRUE)
head(tcehy)
t.data=tcehy[,2:7]
head(t.data)

########### Linear
t.data$OPEN=(t.data$OPEN-mean(t.data$OPEN))/sqrt(var(t.data$OPEN))
t.data$HIGH=(t.data$HIGH-mean(t.data$HIGH))/sqrt(var(t.data$HIGH))
t.data$LOW=(t.data$LOW-mean(t.data$LOW))/sqrt(var(t.data$LOW))
t.data$LAST_PRICE=(t.data$LAST_PRICE-mean(t.data$LAST_PRICE))/sqrt(var(t.data$LAST_PRICE))
t.data$NUMBER_TICKS=(t.data$NUMBER_TICKS-mean(t.data$NUMBER_TICKS))/sqrt(var(t.data$NUMBER_TICKS))
t.data$VOLUME=(t.data$VOLUME-mean(t.data$VOLUME))/sqrt(var(t.data$VOLUME))
head(t.data)

##### Linear model
n=nrow(t.data)
fit0=lm(LAST_PRICE[2:n]~OPEN[1:n-1]+HIGH[1:n-1]+LOW[1:n-1]+NUMBER_TICKS[1:n-1]+VOLUME[1:n-1],data=t.data)
summary(fit0)

fit00=lm(LAST_PRICE~0+.,data=t.data)
summary(fit00)



################ Multivariate

############# whole (logged data)

library(MTS)
head(tcehy)

tcehy0=tcehy
tcehy0$LAST_PRICE=log(tcehy0$LAST_PRICE)
tcehy0$NUMBER_TICKS=log(tcehy0$NUMBER_TICKS)
tcehy0$VOLUME=log(tcehy0$VOLUME)
head(tcehy0)
tcehy00=tcehy0[,c(5:7)]
head(tcehy00)

plot(range(time(tcehy00$LAST_PRICE)), range(tcehy00$LAST_PRICE,tcehy00$NUMBER_TICKS,tcehy00$VOLUME), 
     type="n", 
     xlab="time", 
     ylab="",data=tcehy00)
lines(tcehy00$LAST_PRICE)
lines(tcehy00$NUMBER_TICKS,col="red")
lines(tcehy00$VOLUME,col="blue")





############# 1:100  (logged data)
tcehy0=tcehy
tcehy0$LAST_PRICE=log(tcehy0$LAST_PRICE)
tcehy0$NUMBER_TICKS=log(tcehy0$NUMBER_TICKS)
tcehy0$VOLUME=log(tcehy0$VOLUME)
head(tcehy0)
tcehy00=tcehy0[c(1:100),c(5:6)]
head(tcehy00)
tail(tcehy00)

plot(range(time(tcehy00$LAST_PRICE)), range(tcehy00$LAST_PRICE,tcehy00$NUMBER_TICKS,tcehy00$VOLUME), 
     type="n", 
     xlab="time", 
     ylab="",data=tcehy00)
lines(tcehy00$LAST_PRICE)
lines(tcehy00$NUMBER_TICKS,col="red")
lines(tcehy00$VOLUME,col="blue")


?VARMA
fit000=VARMA(da=tcehy00)
summary(fit000)





#################### 1:200  logged data     VAR
tcehy=read.table("C:/tcehy.txt",sep=",",head=TRUE)
head(tcehy)
t.data=tcehy[1:200,2:7]
head(t.data)
t.data$OPEN=log(t.data$OPEN)
t.data$HIGH=log(t.data$HIGH)
t.data$LOW=log(t.data$LOW)
t.data$LAST_PRICE=log(t.data$LAST_PRICE)
t.data$NUMBER_TICKS=log(t.data$NUMBER_TICKS)
t.data$VOLUME=log(t.data$VOLUME)
head(t.data)


VARselect(t.data,lag.max=10,type="const")
fit.m=VAR(t.data,p=1,type="both")
summary(fit.m)

t.data.v=t.data[,-6]
head(t.data.v)

VARselect(t.data.v,lag.max=10,type="both")
fit.m.v=VAR(t.data.v,p=1,type="both")
summary(fit.m.v)

?VARMA
VARMA(t.data,p=1,q=0)

