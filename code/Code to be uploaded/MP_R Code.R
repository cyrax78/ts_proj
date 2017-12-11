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

