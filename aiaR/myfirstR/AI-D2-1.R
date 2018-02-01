# 5/15

# install.packages("TTR")
library(TTR)
data(ttrc)
dim(ttrc)
head(ttrc)

t <- 1:100
sma.20 <- SMA(ttrc[t, "Close"], 20)
ema.20 <- EMA(ttrc[t, "Close"], 20)
wma.20 <- WMA(ttrc[t, "Close"], 20)

plot(ttrc[t,"Close"], type="l", main="ttrc")
lines(sma.20, col="red", lwd=2)
lines(ema.20, col="blue", lwd=2)
lines(wma.20, col="green", lwd=2)
legend("topright", legend=c("sma.20", "ema.20", "wma.20"), col=c("red", "blue", "green"), lty=1, lwd=2)


# 7/15
data(cars)
dim(cars)
head(cars)

par(mfrow=c(1, 3))
for(i in c(0.1, 0.3, 0.5)){
  plot(cars$dist ~ cars$speed, main=paste0("lowess (f=", i,")"))
  lines(lowess(cars$dist ~ cars$speed, f = i), col="red", lwd=2)
}


# 11/15
plot(density(iris$Sepal.Length))
hist(iris$Sepal.Length, prob=T)
lines(density(iris$Sepal.Length), col="red")

#13/15
#install.packages("jpeg")
library(jpeg)
ruddyduck.img <- readJPEG("ruddyduck.jpg")
plot(0, xlim=c(0, 14), ylim=c(-6, 4), type='n', xlab="", ylab="", main="Spline approximate to the top profile of the ruddy duck")
rasterImage(ruddyduck.img, 0.6, -6, 13.8, 3.3)
abline(v=1:14, h=-6:4, col="grey")
 
# 14/15
ruddyduck.dat <- read.table("ruddyduck.txt", header=T, sep="\t")
head(ruddyduck.dat)
points(ruddyduck.dat, col="blue", pch=16)

duck.spl <- smooth.spline(ruddyduck.dat$fx ~ ruddyduck.dat$x)
lines(duck.spl, col = "red", lwd=2)












