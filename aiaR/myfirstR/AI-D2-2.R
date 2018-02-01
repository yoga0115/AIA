# 13/46
myvector <- c(10, 20, NA, 30, 40)
myvector
mycountry <- c("Austria", "Australia", NA, NA, "Germany", "NA")
mycountry
is.na(myvector)
which(is.na(myvector))
x <- c(1, 4, 7, 10)
x[4] <- NA # sets the 4th element to NA
x
is.na(x) <- 1 # sets the first element to NA
x

mydata$v1[mydata$v1==99] <- NA 

set.seed(12345)
mydata <- matrix(round(rnorm(20), 2), ncol=5)
mydata[sample(1:20, 3)] <- NA
mydata 
which(colSums(is.na(mydata))>0)


# 14/46
x <- c(1, 4, NA, 10)
summary(x)
mean(x)
sd(x)
mean(x, na.rm=TRUE)
sd(x, na.rm=TRUE)
x[!is.na(x)]


# 16/46
mydata <- as.data.frame(matrix(sample(1:20, 8), ncol = 2))
mydata[4, 2] <- NA
names(mydata) <- c("y", "x")
mydata
lm(y~x, data = mydata)
lm(y~x, data = mydata, na.action = na.omit)
lm(y~x, data = mydata, na.action = na.fail)


# 17/46
x <- c(1, 0, 10)
x/x
is.nan(x/x)

1/x
is.finite(1/x)
-10/x
is.infinite(-10/x)

exp(-Inf)
0/Inf
Inf - Inf
Inf/Inf


# 20/46
head(airquality)
dim(airquality)
mydata[4:10,3] <- rep(NA,7)
mydata[1:5,4] <- NA
summary(mydata)


# 21/46
library(mice)
md.pattern(mydata)

library(VIM)
mydata.aggrplot <- aggr(mydata, col=c('lightblue','red'), numbers=TRUE, prop = TRUE, sortVars=TRUE, labels=names(mydata), cex.axis=.7, gap=3)


# 22/46
matrixplot(mydata)


# 23/46
md.pairs(mydata)


# 24/46
marginplot(mydata[,c("Ozone", "Solar.R")], col = c("blue", "red"))


# 26/46
mdata <- matrix(rnorm(15), nrow=5)
mdata[sample(1:15, 4)] <- NA 
mdata <- as.data.frame(mdata)
mdata
(x1 <- na.omit(mdata))
(x2 <- mdata[complete.cases(mdata),])
mdata[!complete.cases(mdata),]


#27/46
cov(mdata)
cov(mdata, use = "all.obs")
cov(mdata, use = "complete.obs")

cov(mdata, use = "na.or.complete")
cov(mdata, use = "pairwise")


#28/46
mean.subst <- function(x) {
   x[is.na(x)] <- mean(x, na.rm = TRUE)
   x
}

mdata
mdata.mip <- apply(mdata, 2, mean.subst)
mdata.mip


# 31/46
names(airquality)
airquality.imp.median <- kNN(airquality[1:4], k=5)
head(airquality.imp.median)


# 32/46
matrixplot(airquality[1:4], interactive = F, main="airquality")
matrixplot(airquality.imp.median[1:4], interactive = F, main="imputed by median")

trim_mean <- function(x){
   mean(x, trim = 0.1)
}

airquality.imp.mean <- kNN(airquality[1:4], 
k=5, metric=dist, numFun=mean)
airquality.imp.tmean <- kNN(airquality[1:4], 
k=5, numFun=trim_mean)


# 34/46
airquality.imp.lm <- regressionImp(Ozone ~ Wind + Temp, data=airquality)
data(sleep)
summary(sleep)


# 35/46
sleep.imp.lm <- regressionImp(Dream + NonD ~ BodyWgt + BrainWgt, data=sleep)
summary(sleep.imp.lm)


# 39/46
methods(mice)
? mice


# 40/46
mydata.ip <- mice(mydata, m=5, maxit=50, meth='pmm', seed=500)
summary(mydata.ip)

mydata.ip$imp$Ozone

mydata.completed <- complete(mydata.ip, 1)


# 41/46
library(lattice)
xyplot(mydata.ip, Ozone ~ Wind + Temp + Solar.R, pch=16, cex=0.5)


# 42/46
densityplot(mydata.ip)


# 43/46
stripplot(mydata.ip, pch = 16, cex = 0.6)


# 44/46
modelFit1 <- with(mydata.ip, lm(Temp~ Ozone + Solar.R+Wind))
summary(pool(modelFit1))

mydata.ip2 <- mice(mydata, m=50, seed=245435)
modelFit2 <- with(mydata.ip2,lm(Temp ~ Ozone + Solar.R + Wind))
summary(pool(modelFit2))


# 45/46
iris.mis <- prodNA(iris, noNA = 0.1)  
summary(iris.mis)
iris.mis <- subset(iris.mis, select = -c(Species))
summary(iris.mis)
library(mice)
md.pattern(iris.mis)
library(VIM)
mice_plot <- aggr(iris.mis, col=c('navyblue','yellow'), numbers=TRUE, sortVars=TRUE,
                    labels=names(iris.mis), cex.axis=.7,
                    gap=3, ylab=c("Missing data","Pattern"))
imputed.Data <- mice(iris.mis, m=5, maxit = 50, method = 'pmm', seed = 500)
summary(imputed.Data)
imputed.Data$imp$Sepal.Width
completeData <- complete(imputed.Data,2)
fit <- with(data = imputed.Data, exp = lm(Sepal.Width ~ Sepal.Length + Petal.Width)) 
combine <- pool(fit)
summary(combine
