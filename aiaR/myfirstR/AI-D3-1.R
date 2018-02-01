# 13/46
library(vegan)
source("panelutils.R")
spe <- read.csv("DoubsSpe.csv", row.names=1)
env <- read.csv("DoubsEnv.csv", row.names=1)
spa <- read.csv("DoubsSpa.csv", row.names=1)

library(ade4)
data(doubs)
?doubs


# 14/46
spe
spe[1:5,1:10]
head(spe)
nrow(spe)	
ncol(spe)		
dim(spe)		
colnames(spe)		
rownames(spe)	
summary(spe)	


# 15/46
range(spe)
(ab <- table(unlist(spe)))
windows(title="Distribution of abundance classes")
barplot(ab, las=1, xlab="Abundance class", ylab="Frequency", col=gray(5:0/5))
sum(spe==0)
sum(spe==0)/(nrow(spe)*ncol(spe))


# 16/46
windows(title="Site Locations")
plot(spa, asp=1, type="n", main="Site Locations", + xlab="x coordinate (km)", ylab="y coordinate (km)")
lines(spa, col="light blue")
text(spa, row.names(spa), cex=0.8, col="red")
text(50, 10, "Upstream", cex=1.2, col="red")
text(30, 120, "Downstream", cex=1.2, col="red")


# 18/46
windows(title="Species Locations", 9, 9)
par(mfrow=c(1,4))
xl <- "x coordinate (km)", 
yl <- "y coordinate (km)"
plot(spa, asp=1, col="brown", cex=spe$TRU, main="Brown trout", xlab=xl, ylab=yl) 
lines(spa, col="light blue", lwd=2)
plot(spa, asp=1, col="brown", cex=spe$OMB, main="Grayling", xlab=xl, ylab=yl)  
lines(spa, col="light blue", lwd=2)
plot(spa, asp=1, col="brown", cex=spe$BAR, main="Barbel", xlab=xl, ylab=yl)  
lines(spa, col="light blue", lwd=2)
plot(spa, asp=1, col="brown", cex=spe$BCO, main="Common bream", xlab=xl, ylab=yl) 
lines(spa, col="light blue", lwd=2)


# 19/46
spe.pres <- apply(spe > 0, 2, sum)
sort(spe.pres)
spe.relf <- 100*spe.pres/nrow(spe)
round(sort(spe.relf), 1)


# 20/46
windows(title="Frequency Histograms",8,5)
par(mfrow=c(1,2))
hist(spe.pres, main="Species Occurrences", right=FALSE, las=1, 
+ xlab="Number of occurrences", ylab="Number of species", 
+ breaks=seq(0,30,by=5), col="bisque")
hist(spe.relf, main="Species Relative Frequencies", right=FALSE, + las=1, xlab="Frequency of occurrences (%)", ylab="Number of species",
+ breaks=seq(0, 100, by=10), col="bisque")


# 21/46
sit.pres <- apply(spe > 0, 1, sum)
sort(sit.pres)


# 22/46
windows(title="Species Richness", 10, 5)
par(mfrow=c(1,2))
plot(sit.pres,type="s", las=1, col="gray",
+ main="Species Richness vs. \n Upstream-Downstream Gradient",
+ xlab="Positions of sites along the river", ylab="Species richness")
text(sit.pres, row.names(spe), cex=.8, col="red")
plot(spa, asp=1, main="Map of Species Richness", pch=21, col="white", 
+ bg="brown", cex=5*sit.pres/max(sit.pres), xlab="x coordinate (km)", 
+ ylab="y coordinate (km)")
lines(spa, col="light blue")


# 23/46
?diversity
N0 <- rowSums(spe > 0)         
H <- diversity(spe)           
N1 <- exp(H)                   
N2 <- diversity(spe, "inv")  
J <- H/log(N0)                
E10 <- N1/N0                 
E20 <- N2/N0                 
(div <- data.frame(N0, H, N1, N2, E10, E20, J))


# 24/46
?decostand
spe[1:5, 2:4]
spe.pa <- decostand(spe, method="pa")
spe.pa[1:5, 2:4]


# 25/46
spe.scal <- decostand(spe, "max")
spe.scal[1:5,2:4]
apply(spe.scal, 2, max)
apply(spe.relsp, 2, sum)


# 26/46
spe.rel <- decostand(spe, "total")
spe.rel[1:5,2:4]
apply(spe.rel, 1, sum)
spe.norm <- decostand(spe, "normalize")
spe.norm[1:5,2:4]
norm <- function(x) sqrt(x%*%x)
apply(spe.norm, 1, norm)


# 27/46
spe.hel <- decostand(spe, "hellinger")
spe.hel[1:5,2:4]
apply(spe.hel, 1, norm)


# 28/46
spe.chi <- decostand(spe, "chi.square")
spe.chi[1:5,2:4]
spe.chi[7:9,]
spe.wis <- wisconsin(spe)
spe.wis[1:5,2:4]


# 29/46
windows(title="Loach")
par(mfrow=c(1,4))
boxplot(spe$LOC, sqrt(spe$LOC), log1p(spe$LOC), las=1, main="Simple transformation", + names=c("raw data", "sqrt", "log"), col="bisque")
boxplot(spe.scal$LOC, spe.relsp$LOC, las=1, main="Standardization by species",
+ names=c("max", "total"), col="lightgreen")
boxplot(spe.hel$LOC, spe.rel$LOC, spe.norm$LOC, las=1, main="Standardization by sites",
+ names=c("Hellinger", "total", "norm"), col="lightblue")
boxplot(spe.chi$LOC, spe.wis$LOC, las=1, main="Double standardization",
+ names=c("Chi-square", "Wisconsin"), col="orange")


# 31/46
windows(title="Species profiles", 9, 9)
plot(env$das, spe$TRU, type="l", col=4, main="Raw data",
+ xlab="Distance from the source [km]", ylab="Raw abundance code")
lines(env$das, spe$OMB, col=3); lines(env$das, spe$BAR, col="orange")
lines(env$das, spe$BCO, col=2); lines(env$das, spe$LOC, col=1, lty="dotted")

plot(env$das, spe.scal$TRU, type="l", col=4, main="Species profiles (max)",
+ xlab="Distance from the source [km]", ylab="Standardized abundance")
lines(env$das, spe.scal$OMB, col=3); lines(env$das, spe.scal$BAR, col="orange")
lines(env$das, spe.scal$BCO, col=2); lines(env$das, spe.scal$LOC, col=1, lty="dotted")

plot(env$das, spe.hel$TRU, type="l", col=4, main="Site profiles (Hellinger)",
+ xlab="Distance from the source [km]", ylab="Standardized abundance")
lines(env$das, spe.hel$OMB, col=3); lines(env$das, spe.hel$BAR, col="orange")
lines(env$das, spe.hel$BCO, col=2); lines(env$das, spe.hel$LOC, col=1, lty="dotted")
 
plot(env$das, spe.chi$TRU, type="l", col=4, main="Double profiles (Chi-square)",
+ xlab="Distance from the source [km]", ylab="Standardized abundance")
lines(env$das, spe.chi$OMB, col=3); lines(env$das, spe.chi$BAR, col="orange")
lines(env$das, spe.chi$BCO, col=2); lines(env$das, spe.chi$LOC, col=1, lty="dotted")
legend("topright", c("Brown trout", "Grayling", "Barbel", "Common bream", "Stone loach"), + col=c(4,3,"orange",2,1), lty=c(rep(1,4),3))


# 32/46
windows(title="Bubble maps", 9, 9)
par(mfrow=c(1,4))
plot(spa, asp=1, main="Altitude", pch=21, col="white", + bg="red", cex=5*env$alt/max(env$alt), xlab="x", ylab="y")
lines(spa, col="light blue", lwd=2)
plot(spa, asp=1, main="Discharge", pch=21, col="white", 
+ bg="blue", cex=5*env$deb/max(env$deb), xlab="x", ylab="y")
lines(spa, col="light blue", lwd=2)
plot(spa, asp=1, main="Oxygen", pch=21, col="white", 
+ bg="green3", cex=5*env$oxy/max(env$oxy), xlab="x", ylab="y")
lines(spa, col="light blue", lwd=2)
plot(spa, asp=1, main="Nitrate", pch=21, col="white", 
+ bg="brown", cex=5*env$nit/max(env$nit), xlab="x", ylab="y")
lines(spa, col="light blue", lwd=2)


# 33/46
windows(title="Descriptor line plots")
par(mfrow=c(1,4))
plot(env$das, env$alt, type="l", xlab="Distance from the source (km)", 
+ ylab="Altitude (m)", col="red", main="Altitude")
plot(env$das, env$deb, type="l", xlab="Distance from the source (km)", 
+ ylab="Discharge (m3/s)", col="blue", main="Discharge")
plot(env$das, env$oxy, type="l", xlab="Distance from the source (km)", 
+ ylab="Oxygen (mg/L)", col="green3", main="Oxygen")
plot(env$das, env$nit, type="l", xlab="Distance from the source (km)", 
+ ylab="Nitrate (mg/L)", col="brown", main="Nitrate")


# 34/46
windows(title="Bivariate descriptor plots")
source("panelutils.R")
op <- par(mfrow=c(1,1), pty="s")
pairs(env, panel=panel.smooth, diag.panel=panel.hist, main="Bivariate Plots with Histograms and Smooth Curves")
par(op)


# 35/46
range(env$pen)
windows(title="Transformation and standardization of variable slope")
par(mfrow=c(1,4))
hist(env$pen, col="bisque", right=FALSE)
hist(log(env$pen), col="light green", right=F, main="Histogram of ln(env$pen)")
boxplot(env$pen, col="bisque", main="Boxplot of env$pen", ylab="env$pen")
boxplot(log(env$pen), col="light green", main="Boxplot of ln(env$pen)",
+ ylab="log(env$pen)")


# 36/46
env.z <- decostand(env, "standardize")
apply(env.z, 2, mean)
apply(env.z, 2, sd)
env.z <- as.data.frame(scale(env))
env.z

