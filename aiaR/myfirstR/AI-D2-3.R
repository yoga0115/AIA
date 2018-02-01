# 9/53
par(mfrow=c(1,4))
raw.data <- 0:100
pa.data <- ifelse(raw.data >= 60, 1, 0)
id <- which(pa.data==1)
plot(raw.data[id], pa.data[id], main="present-absent", 
     type="l", lwd=2, col="blue", ylim=c(-1, 2), xlim=c(0, 100))
points(raw.data[-id], pa.data[-id], type="l", lwd=2, col="blue")
log.data <- log(raw.data)
plot(raw.data, log.data, main="log", type="l", lwd=2, col="blue")
sqrt10.data <- sqrt(raw.data)*10
plot(raw.data, sqrt10.data, main="sqrt*10", type="l", lwd=2, col="blue", asp=1)
abline(a=0, b=1)
trun.data <- ifelse(raw.data >= 80, 80, ifelse(raw.data < 20, 20, raw.data))
plot(raw.data, trun.data, main="truncation", type="l", lwd=2, col="blue")


# 10/53
par(mfrow=c(1,4))
raw.data <- 0:100
trans.data <- raw.data/max(raw.data)
plot(raw.data, trans.data, main="/max", type="l", lwd=2, col="blue")
trans.data <- raw.data/sum(raw.data)  #Species profile transformation
plot(raw.data, trans.data, main="/sum", type="l", lwd=2, col="blue")
trans.data <- raw.data/sqrt(sum(raw.data^2)) #length of 1, Chord transformation
plot(raw.data, trans.data, main="norm (Chord)", type="l", lwd=2, col="blue")
trans.data <- sqrt(raw.data/sum(raw.data)) #Hellinger transformation
plot(raw.data, trans.data, main="Hellinger", type="l", lwd=2, col="blue")


# 11/53
par(mfrow=c(1,3)); set.seed(12345)
raw.data <- c(sample(0:60, 100, replace=T), sample(90:100, 5, replace=T))
rank.data <- rank(raw.data)
hist(raw.data, main="raw")
hist(rank.data, main="rank")
plot(raw.data, rank.data, main="rank", lwd=2, col="blue")
raw.data <- c(rnorm(100), rnorm(5)+ 2*sqrt(qchisq(0.975, 5)))
...


# 12/53
x <- rnorm(50)
mycolor <- rainbow(150)[1:100]
z <- (x-min(x))/(max(x)-min(x))
plot(x, rep(1, length(x)), main="range (0, 1)", type="n", ylab="", ylim=c(0.3,1))
points(c(seq(min(x), max(x), length.out=100)), rep(1, 100), col=mycolor, cex=2, pch=15)
text(0, 0.95, "color spectrum")
points(x, rep(0.8, length(x)), col=mycolor, cex=2, pch=15)
text(0, 0.75, "x, col=mycolor")
points(sort(x), rep(0.6, length(x)), col=mycolor, cex=2, pch=15)
text(0, 0.55, "sort(x), col=mycolor")
points(x, rep(0.4, length(x)), col=mycolor[floor(z*99)+1], cex=2, pch=15)
text(0, 0.35, "x, col=mycolor[floor(z*99)+1]")


# 14/53
x <- sample(0:40, 50, replace=T)
y <- sample(40:100, 10)
z <- c(x,y)
par(mfrow=c(1,2))
hist(z)
hist(sqrt(z))


# 15/53
library('R.matlab')
data <- readMat("software.mat")
print(data)
str(data)


# 16/53
par(mfrow=c(1,2))
xylim <- range(data$prepsloc, data$defsloc)
plot(data$prepsloc, data$defsloc, xlab="PrepTime(min)/SLOC", ylab="Defects/SLOC", main="Software Data", xlim=xylim, ylim=xylim)
logxylim <- range(log(data$prepsloc), log(data$defsloc))
plot(log(data$prepsloc), log(data$defsloc), xlab="Log PrepTime/SLOC", 
ylab="Log Defects/SLOC", main="Software Data", xlim=logxylim, ylim=logxylim)


# 22/53
x <- seq(0.5, 2, length.out=100)
bc <- function(y, lambda){
    (y^lambda -1)/lambda
} 
lambda <- seq(-2, 3, 0.5)
plot(0, 0, type="n", xlim=c(0.5, 2), 
     ylim=c(-2, 2.5), main="Box-Cox transformation")
for(i in 1:length(lambda)){
   points(x, bc(x, lambda[i]), type="l", col=i)
   points(2, bc(2, lambda[i]), col=i, pch=i)
}
legend(0.7, 2.5, legend=as.character(rev(lambda)), 
       lty=1, pch=length(lambda):1, 
       col=length(lambda):1)


# 23/53
x <- rexp(1000)
bc <- function(y, lambda){
    (y^lambda -1)/lambda
} 
bc1.x <- bc(x, 0.1)
bc2.x <- bc(x, 0.268)
bc3.x <- bc(x, 0.5)
par(mfrow=c(2, 2))
qqnorm(x); qqline(x, col="red")
qqnorm(bc1.x, main="lambda=0.1")
qqline(bc1.x, col="red")
qqnorm(bc2.x, main="lambda=0.268") 
qqline(bc2.x, col="red")
qqnorm(bc3.x, main="lambda=0.5")
qqline(bc3.x, col="red")


# 31/53
x <- rpois(500, lambda=1)
hist(x, main="rpois(500, lambda=1)"); z <- scale(x); hist(z, main="")


# 33/53
head(USArrests)
par(mfrow=c(4,1))
r <- range(USArrests)
hist(USArrests$Murder, xlim = r)
hist(USArrests$Assault, xlim = r)
hist(USArrests$UrbanPop, xlim = r)
hist(USArrests$Rape, xlim = r)
USArrests.std <- as.data.frame(apply(USArrests, 2, scale))
r.std <- c(-3, 3)
hist(USArrests.std$Murder, xlim = r.std)
hist(USArrests.std$Assault, xlim = r.std)
hist(USArrests.std$UrbanPop, xlim = r.std)
hist(USArrests.std$Rape, xlim = r.std)


# 34/53
head(airquality )
r <- range(airquality[,1:4], na.rm = T)
hist(airquality$Ozone , xlim = r)
hist(airquality$Solar.R, xlim = r)
hist(airquality$Wind, xlim = r)
hist(airquality$Temp, xlim = r)
airquality.std <- as.data.frame(apply(airquality, 2, scale))
r.std <- c(-3, 3)
hist(airquality.std$Ozone, xlim = r.std)
hist(airquality.std$Solar.R, xlim = r.std)
hist(airquality.std$Wind, xlim = r.std)
hist(airquality.std$Temp, xlim = r.std)


# 36/53
cell.raw <- read.table("trad_alpha103.txt", row.names=1, header=T)
cell.xdata <- t(scale(t(cell.raw[,2:19]), center=T, scale=T))    
y.C <-  as.integer(cell.raw[,1])
table(y.C)
no.cluster <- length(unique(y.C))   
cellcycle.color <- c("darkgreen", "blue", "red", "gray50", "orange")
p <- ncol(cell.raw) -1           
ycolors <- cellcycle.color[y.C+1]
my.pch <- c(1:no.cluster)[y.C+1]    
phase <- c("G1", "S", "S/G2", "G2/M", "M/G1")
matplot(t(cell.xdata), pch = 1:p, lty=1, type = "l", ylab="gene expression", 
        col=ycolors, xlab="time", main="Time series", xaxt="n")
time.label <- parse(text=paste("t[",0:p,"]",sep=""))        
axis(1, 1:(p+1), time.label)
legend("bottom", legend=phase, col=cellcycle.color, lty=1, horiz = T, lwd=2)


# 37/53
library(MASS)
data(crabs)


# 38/53
boxplot(crabs$FL~crabs$sp, main="FL", horizontal=T)


# 39/53
pairs(crabs[,4:8], pch=as.integer(crabs$sex)+1, 
      col=c("blue","orange")[as.integer(crabs$sp)])


# 40/53
par(mfrow=c(1,2))
mp <- as.integer(crabs$sex)+1
mc <- c("blue","orange")[as.integer(crabs$sp)]
isometric.size <- apply(crabs[,4:8], 1, mean)
plot(isometric.size,  log(crabs$BD/crabs$RW), pch=mp, col=mc)
plot(isometric.size, log(crabs$CL/crabs$CW), pch=mp, col=mc)


# 44/53
n <- 100
mu <- c(-2, 2)
sigma <- matrix(c(1, 0.5, 0.5, 1), ncol=2)
library(MASS)
x <- mvrnorm(n, mu, sigma)
plot(x[,1], x[,2], main="Simulated Data")

x.bar <- colMeans(x)
ei <- eigen(cov(x))
D <- diag(ei$values)
V <- ei$vectors
xc <- x - matrix(rep(1, n), ncol=1)%*%x.bar
z <- xc%*%V%*%diag((ei$values)^{-1/2})
plot(z[,1], z[,2], main="After Sphering")




