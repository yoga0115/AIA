# 3/32

prediction.accuracy.rate <- function(no.classifier=1, accuracy.rate=0.5){
   c(no.classifiers=no.classifier, 
     at.least.one.accuracy=1-(1-accuracy.rate)^no.classifier)
}
prediction.accuracy.rate()
t(sapply(1:10, prediction.accuracy.rate))


# 7/32
id <- sample(2, nrow(iris), replace = TRUE, prob = c(0.9, 0.1))
id
train.data <- iris[id == 1, ]
dim(train.data)
test.data <- iris[id == 2, ]
dim(test.data)

id <- sample(nrow(iris), floor(nrow(iris) * 0.9))
id
train.data <- iris[id, ]
dim(train.data)
test.data <- iris[-id, ]
dim(test.data)


# 8/32
splitdf <- function(df, train.ratio, seed=NULL) {
	if (!is.null(seed)) set.seed(seed)
	index <- 1:nrow(df)
	id <- sample(index, trunc(length(index)*train.ratio))
	train <- df[id, ]
	test <- df[-id, ]
	list(trainset=train,testset=test)
}

splits <- splitdf(iris, 0.9, 12345)
lapply(splits, dim)
iris.training <- splits$trainset
iris.testing <- splits$testset

library(dplyr)
iris.train <- sample_frac(iris, 0.9)
id <- as.numeric(rownames(iris.train)) 
iris.test <- iris[-id, ]


# 9/32
library(caTools)
Y <- iris[,5] # extract labels from the data
msk <- sample.split(Y, SplitRatio=4/5)
msk
table(Y, msk)
iris.train <- iris[msk, ] 
iris.test <- iris[!msk, ]  
dim(iris.train) 
dim(iris.test)

require(caTools)
set.seed(12345) 
id <- sample.split(1:nrow(iris), SplitRatio = 0.90)
iris.train <- subset(iris, id == TRUE)
iris.test <- subset(iris, id == FALSE)

library(caret)
id <- createDataPartition(y=iris$Species, p=0.9, list=FALSE)
iris.train <- iris[id, ]
iris.test <- iris[-id, ]

library(caret)
createFolds(iris$Species, k=3)


# 10/32
# install.packages("bootstrap")
library(bootstrap)
jackknife


# 11/32
set.seed(12345)
x <- runif(30) 
n <- length(x)
theta <- CV(x)
CV <- function(x) sqrt(var(x))/mean(x)
theta.i <- sapply(1:n, function(i) CV(x[-i]))
theta.i
theta.jack <- n*theta - (n-1)*mean(theta.i)
theta.jack
plot(theta.i)


# 12/32
library(bootstrap)
set.seed(12345)
n <- 50; p <- 5
mydata <- as.data.frame(matrix(rnorm(n*p), ncol=p))
head(mydata, 3)
model.lm <- formula(V1 ~ V2 + V3 + V4)             
theta <- function(x, xdata, coefficient){
+    coef(lm(model.lm, data=xdata[x, ]))[coefficient] 
+ }
results <- jackknife(1:n, theta, xdata=mydata, coefficient="(Intercept)")
results


# 15/32
set.seed(12345)
x <- runif(30) 
CV <- function(x) sqrt(var(x))/mean(x)
CV(x)
CV(sample(x, replace=T)) 
boot <- replicate(n=100, expr=CV(sample(x, replace=T)))
boot
mean(boot)
var(boot)
hist(boot)


# 16/32
library(bootstrap)
set.seed(12345)
x <- rnorm(20)                
mean(x)
x.bootstrap.mean <- bootstrap(x, 100, theta=mean)
x.bootstrap.mean
mean(x.bootstrap.mean$thetastar)

mu.hat <- mean(x)
n <- length(x) 
ja <- jackknife(x, mean)
mu.hat.jack <- n*mu.hat - (n-1)*mean(ja$jack.values)
mu.hat.jack <- mu.hat - ja$jack.bias


# 19/32
library(rpart); library(mlbench); library(adabag)
data(Vehicle)
dim(Vehicle)
head(Vehicle)
table(Vehicle$Class)

n <- nrow(Vehicle)
sub <- sample(1:n, 2*n/3)
Vehicle.train <- Vehicle[sub, ]
Vehicle.test <- Vehicle[-sub, ]

mfinal <- 10 #Defaults to mfinal=100 iterations
maxdepth <- 5
# apply rpart
Vehicle.rpart <- rpart(Class ~ ., data = Vehicle.train, maxdepth = maxdepth)
Vehicle.rpart.pred <- predict(Vehicle.rpart, newdata = Vehicle.test, type = "class")
(tb <- table(Vehicle.rpart.pred, Observed.Class=Vehicle.test$Class))
(error.rpart <- 1 - (sum(diag(tb)) / sum(tb)))


# 20/32
library(adabag)
Vehicle.adaboost <- boosting(Class ~., data = Vehicle.train, mfinal = mfinal, 
+                              control = rpart.control(maxdepth=maxdepth))
Vehicle.adaboost.pred <- predict.boosting(Vehicle.adaboost, newdata = Vehicle.test)
Vehicle.adaboost.pred$confusion
Vehicle.adaboost.pred$error
importanceplot(Vehicle.adaboost)
evol.train <- errorevol(Vehicle.adaboost, newdata = Vehicle.train)
evol.test <- errorevol(Vehicle.adaboost, newdata = Vehicle.test)
plot.errorevol(evol.test, evol.train)

sort(Vehicle.adaboost$importance, dec=T)[1:5]


# 21/32
Vehicle.boost.cv <- boosting.cv(Class ~., data = Vehicle, v = 10, mfinal = 5, 
                                control = rpart.control(maxdepth=maxdepth))
Vehicle.boost.cv$confusion
Vehicle.boost.cv$error


Vehicle.bag.cv <- bagging.cv(Class ~., data = Vehicle, v = 10, mfinal = 5, 
                                control = rpart.control(maxdepth=maxdepth))
Vehicle.bag.cv$confusion
Vehicle.bag.cv$error


# 27/32
library(unbalanced)
p <- ncol(ubIonosphere)
y <- ubIonosphere$Class
x <- ubIonosphere[ ,-p]
data <- ubBalance(X=x, Y=y, type="ubOver", k=0)
overData <- data.frame(data$X, Class=data$Y)
table(overData$Class)
data <- ubBalance(X=x, Y=y, type="ubUnder", perc=50, method="percPos")
underData <- data.frame(data$X, Class=data$Y)
table(underData$Class)
bdata <- ubBalance(X= x, Y=y, type="ubSMOTE", percOver=300, percUnder=150, verbose=TRUE)
str(bdata)
table(bdata$Y)

data(ubIonosphere)
dim(ubIonosphere)
head(ubIonosphere)
table(ubIonosphere$Class)


# 28/32
set.seed(12345)
n <- nrow(ubIonosphere) # 351
no.train <- floor(0.5*n) # 175
id <- sample(1:n, no.train)
x.train  <- x[id, ]  # 175 x 32
y.train <- y[id]
x.test <- x[-id, ] # 176  32
y.test <- y[-id]

library(e1071)
model1 <- svm(x.train, y.train) 
y.pred1 <- predict(model1, x.test)
table(y.pred1, y.test)

balancedData <- ubBalance(X=x.train, Y=y.train, type="ubSMOTE", percOver=200, percUnder=150)
table(balancedData$Y)

model2 <- svm(balancedData$X, balancedData$Y)
y.pred2 <- predict(model2, x.test)
table(y.pred2, y.test)


# 29/32
set.seed(1234)
load("creditcard.Rdata")
str(creditcard)
table(creditcard$Class)

ubConf <- list(percOver=200, percUnder=200, k=2, perc=50, method="percPos", w=NULL)
results <- ubRacing(Class ~., creditcard, "randomForest", 
                    positive=1, metric="auc", ubConf=ubConf, ntree=5)


# 31/32
results 

results <- ubRacing(Class ~., creditcard, "randomForest",                       positive=1, metric="auc", ubConf=ubConf, ncore=4)
library(e1071)
results <- ubRacing(Class ~., creditcard, "svm",                       positive=1, ubConf=ubConf)
library(rpart)
results <- ubRacing(Class ~., creditcard, "rpart",                       positive=1, ubConf=ubConf)
