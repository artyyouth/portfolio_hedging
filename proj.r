library(zoo)
library(quantmod)
# List the company ticker symbols
nam <- c("HD","WMT","XOM","WY","ABT","UPS","DD","VZ","ETR")
# Extract the data from Yahoo: "f" is daily price array and "d" is the 
# dividend array
f <- getYahooData(nam[1],20090101,20131108,adjust=F)
d <- getYahooData(nam[1],20090101,20131108,type='split',adjust=F)
p <- f[,4]
names(p) <- nam[1]
d <- merge(p,d[,1])
d$HD <- NULL
names(d) <- nam[1]
for(i in 2:9) {
  f <- getYahooData(nam[i],20090101,20131108,adjust=F)
  v <- getYahooData(nam[i],20090101,20131108,type='split',adjust=F)
  f <- f[,4]
  v <- v[,1]
  names(f)  <- names(v) <- nam[i]
  p <- merge(p,f)
  d <- merge(d,v)
}
# The merge results in NAs being generated when there is a missing observation
# so we convert all instances to zero
d[is.na(d)] <- 0

# Read the market capitalization file for the valuation date
dt <- '2011-05-02'
m <- read.zoo('mktcap.csv',format='%m/%d/%Y',sep=',',header=T)
shr <- as.numeric(as.numeric(m)/sum(as.numeric(m))*100000000)
p0 <- as.numeric(p[dt,])
# Initial number of shares
shr0 <- shr/p0
p$id <- 1:nrow(p)
# Set index for today: lower values are history and higher values are future
d0 <- as.numeric(p$id[dt])
p$id <- NULL
port <- s <- p
# zero out values
for(i in 1:9) s[,i]  <- port[,i] <- 0
# initalize 5/2/11 shares
for(i in 1:9) s[d0,i] <- shr0[i]
# make historical adjustments to number of shares
for(i in (d0-1):1) {
  for(j in 1:9)
    s[i,j] <- as.numeric(s[i+1,j])/(1 + as.numeric(d[i,j])/as.numeric(p[i,j]))
}
# make future adjustment to number of shares in portfolio
for(i in d0:nrow(p)) {
  for(j in 1:9)
    s[i,j] <- as.numeric(s[i-1,j])*(1 + as.numeric(d[i-1,j])/as.numeric(p[i-1,j]))
}
# Compute daily value of positions
port <- s*p
# add a variable with the total portfolio value
port$val <- rowSums(port)

# plot the portfolio value over time
plot.xts(port$val/1e6,minor.ticks=F,main='Portfolio Value',ylab="$MM")

# What day did the portfolio reach is MAX value?
port[port$val==max(port$val),]/1e6
# What day did the portfolio reach is MIN value?
port[port$val==min(port$val),]/1e6

# Now compute the daily and weekly returns: note that the first obs is dropped
d.rtn <- dailyReturn(port$val)[-1]
w.rtn <- weeklyReturn(port$val)[-1]
