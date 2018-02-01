# 13/41
curve(pnorm(x), -3, 3)
arrows(-1, 0, -1, pnorm(-1), col="red")
arrows(-1, pnorm(-1), -3, pnorm(-1), col="green")
pnorm(-1)


# 14/41
qnorm(0.025)
qnorm(0.5)
qnorm(0.975)


# 15/41
par(mfrow=c(2,2))
hist.sym <- hist(rnorm(10000),nclas=100,freq=FALSE,  
                 main="Symmetric Distribution", xlab="rnorm")
hist.flat <- hist(runif(10000),nclas=100,freq=FALSE, 
                  main="Symmetric Flat Distribution", xlab="runif")
hist.skr <- hist(rgamma(10000,shape=2,scale=1),freq=FALSE, nclas=100, 
                 main="Skewed to Right", xlab="rgamma")
hist.skl <- hist(rbeta(10000,8,2),nclas=100,freq=FALSE, 
                 main="Skewed to Left", xlab="rbeta")


# 17/41
x <- 1:5
sample(x) 
sample(2, size=20, replace=TRUE)
sample(20, size=10, replace=FALSE)


# 20/41
n <- 10
p <- 0.8
m <- 100
xbin <- rbinom(m, n, p)
table(xbin)
mu <- n*p; mu
sigma2 <- n*p*(1-p); sigma2
mean(xbin) 
var(xbin)


# 22/41
dnorm(0)
pnorm(-1)
qnorm(0.975)

dnorm(10, 10, 2) 
pnorm(1.96, 10, 2)
qnorm(0.975, 10, 2)
rnorm(5, 10, 2)
pnorm(15, 10, 2) - pnorm(8, 10, 2)  # P(8<=X<=15)

par(mfrow=c(1,4))
curve(dnorm, -3, 3, xlab="z", ylab="Probability density", main="Density")
curve(pnorm, -3, 3, xlab="z", ylab="Probability", main="Probability")
curve(qnorm, 0, 1, xlab="p", ylab="Quantile(z)", main="Quantiles")
hist(rnorm(1000), xlab="z", ylab="frequency", main="Random numbers")


# 23/41
norm.sample <- rnorm(250)
summary(norm.sample)
hist(norm.sample, xlim=c(-5, 5), ylab="probability", 
     main="Histogram of N(0, 1)", prob=T)
x <- seq(from=-5, to=5, length=300)
lines(x, dnorm(x))

x <- seq(from=-5, to=5, length=300)
plot(x, dnorm(x), type="l")
points(0, dnorm(0))
height <- round(dnorm(0), 4); height
text(1.5, height, paste("(0,", height, ")"))


# 24/41
par(mfrow = c(1, 2))
n <- 20 # 4
p <- 0.4 # 0.04
mu <- n * p
sigma <- sqrt(n * p * (1 - p))
x <- 0:n
plot(x, dbinom(x, n, p), type = 'h', lwd = 2, 
     xlab = "x", ylab = "P(X=x)",
     main = "B(20, 0.4)")
z <- seq(0, n, 0.1)
lines(z, dnorm(z, mu, sigma), col = "red", lwd = 2)
abline(h = 0, lwd = 2, col = "grey")


# 27/41
sample.size <- seq(from=1, to=800, by=5)
m <- length(sample.size)
xbar <- numeric(m)
for(i in 1:m){
  xbar[i] <- mean(rbinom(sample.size[i], 1, 0.5))
}
plot(sample.size, xbar, xlab="Number of observations, n", ylab="sample mean", main="Law of Large Numbers", type="l", col="red", lwd=1.5)
abline(h=0.5, col="blue")


# 30/41
z <- (126/210 -0.7)/sqrt(0.001) # 通過人數>126的機率
z
1 - pnorm(z)

pass.prob <- function(x, n, mu, sigma2, digit=m){
    xbar <- x/n
    z <- (xbar-mu)/sqrt(sigma2)
    zvalue <- round(z, digit)
    right.prob <- round(1-pnorm(z), digit)
    list(zvalue=zvalue, prob=right.prob)
  }

pass.prob(126, 210, 0.7, 0.001, 4)


# 32/41
umin <- 5
umax <- 80
n.sample <- 20
n.repeated <- 500

RandomSample <- matrix(0, n.sample, n.repeated)
for(i in 1:n.repeated){
   rnumber <- runif(n.sample, umin, umax)
   RandomSample[,i] <- as.matrix(rnumber) 
}
dim(RandomSample)


# 33/41
par(mfrow=c(2,2))
for(i in 1:4){
  title <- paste(i,"-th sampling", sep="")
  hist(RandomSample[,i], ylab="f(x)", xlab="random uniform", pro=T, main=title)
}


# 34/41
SampleMean <- apply(RandomSample, 2, mean)
hist(SampleMean, ylab="f(x)", xlab="sample mean", pro=T, main="n=20")

qqnorm(SampleMean)
qqline(SampleMean)


# 35/41
CLT.unif <- function(umin, umax, n.sample, n.repeated){ 
  RandomSample <- matrix(0, n.sample, n.repeated)
  for(i in 1:n.repeated){
    rnumber <- runif(n.sample, umin, umax)
    RandomSample[,i] <- as.matrix(rnumber) 
  }
  SampleMean <- apply(RandomSample, 2, mean)
  par(mfrow=c(1,2))
  title <- paste("n=",n.sample, sep="")
  hist(SampleMean, breaks=30, ylab="f(x)", xlab="sample mean", pro=T, main=title)
  qqnorm(SampleMean)
  qqline(SampleMean)
}


# 37/41
sample(1:49, 6, replace = FALSE)
sample(1:49, 6, replace = FALSE)
sample(1:49, 6, replace = FALSE)

set.seed(12345)
sample(1:49, 6, replace = FALSE)
sample(1:49, 6, replace = FALSE)
set.seed(12345)
sample(1:49, 6, replace = FALSE)
sample(1:49, 6, replace = FALSE)


# 39/41
1 / choose(49, 6)
choose(6, 5) / choose(49, 6)
(choose(6, 5)*choose(49-6-1, 1)) / choose(49, 6) 


# 41/41
girl.born <- function(n, show.id = F){
  
  girl.count <- 0
  for (i in 1:n) {
    if (show.id) cat(i,": ")
    child.count <- 0
    repeat {
      rn <- sample(0:99, 1) # random number
      if (show.id) cat(paste0("(", rn, ")"))
      is.girl <- ifelse(rn <= 48, TRUE, FALSE)
      child.count <- child.count + 1
      if (is.girl){
        girl.count <- girl.count + 1
        if (show.id) cat("女+")
        break
      } else if (child.count == 3) {
        if (show.id) cat("男")        
        break
      } else{
        if (show.id) cat("男")        
      }
    }
    if (show.id) cat("\n")
  }
  p <- girl.count / n
  p
  
}

girl.p <- 0.49 + 0.51*0.49 + 0.51^2*0.49
girl.p
girl.born(n=10, show.id = T)
girl.born(n=10000)


