# 11/135
head(anscombe, 3)
apply(anscombe, 2, mean)
apply(anscombe, 2, sd)
mapply(cor, anscombe[,1:4], anscombe[,5:8])
mapply(function(x, y) lm(y~x)$coefficients, anscombe[, 1:4], anscombe[, 5:8])

par(mfrow=c(2, 2))
regplot <- function(x, y){ 
  plot(y~x) 
  abline(lm(y~x), col="red")
}
mapply(regplot, anscombe[, 1:4], anscombe[, 5:8])


# 16/135
library(ggplot2)
qplot(Sepal.Length, Petal.Length, geom="point", data=iris, colour = Species, main="scatterplot")
qplot(Species, Sepal.Length, geom="boxplot", fill=Species, data=iris)


# 17/135
data <- iris[,1]
data[33] <- data[33]*10
plot(data)
ind <- which(data>15)
data[ind]
data[ind] <- 5.2
windows()
plot(data)


# 21/135
attach(OrchardSprays)
names(OrchardSprays)
OrchardSprays[1:5,]
stripchart(decrease~treatment, xlab="decrease", ylab="treatment")


# 24/135
plot(density(iris$Sepal.Length))


# 27/135
par(mfrow = c(1, 2))
set.seed(12345); 
n <- 100; mu <- 0.5; sigma <- 0.15
x <- rnorm(n, mu, sigma)
hist(x, freq=FALSE, ylim=c(0, 3), main="")
y <- seq(0, 1, length = n)
lines(y, dnorm(y, mu, sigma), type = 'l')
qqnorm(x, main = "rnorm(mu=0.5, sigma=0.15)"); 
qqline(x)

qqplot(x, rnorm(300)) 
qqline(x, col = 2)
qqplot(scale(x), rnorm(300)) 
qqline(scale(x), col = 2)


# 28/135
data(UKLungDeaths)
ts.plot(ldeaths, mdeaths, fdeaths, xlab="year", ylab="deaths", lty=c(1:3))
data(sunspots)
plot(sunspots) # sunspots is ts class
class(sunspots)
is.ts(sunspots)


# 29/135
cell.raw <- read.table("trad_alpha103.txt", row.names=1, header=T)
head(cell.raw)
cell.xdata <- t(scale(t(cell.raw[,2:19]), center=T, scale=T))    
y.C <-  as.integer(cell.raw[,1])
table(y.C)
no.cluster <- length(unique(y.C))            
p <- ncol(cell.raw) -1
cellcycle.color <- c("darkgreen", "blue", "red", "gray50", "orange")
ycolors <- cellcycle.color[y.C+1]
my.pch <- c(1:no.cluster)[y.C+1]    
phase <- c("G1", "S", "S/G2", "G2/M", "M/G1")
matplot(t(cell.xdata), lty=1, type = "l", ylab="gene expression", 
            col=ycolors, xlab="time", main="Time series", xaxt="n")
time.label <- parse(text=paste("t[",0:p,"]",sep=""))        
axis(1, 1:(p+1), time.label)
legend("bottom", legend=phase, col=cellcycle.color, lty=1, horiz = T, lwd=2)


# 30/135
pie.sales <- c(0.12, 0.3, 0.26, 0.16, 0.04, 0.12)
sum(pie.sales)
names(pie.sales) <- c("Blueberry", "Cherry", "Apple", "Boston Cream", "Other", "Vanilla Cream")
pie(pie.sales) # default colours

pie(pie.sales, col = c("purple", "violetred1", "green3", "cornsilk", "cyan", "white"))
pie(pie.sales, col = gray(seq(0.4,1.0,length=6)))
pie(pie.sales, clockwise=TRUE, main="pie with clockwise=TRUE")
pie(rep(1,200), labels="", col=rainbow(200), border=NA, main = "Rainbow Pie")


# 31/135
library(plotrix)
library(survival)
head(veteran)
slices <- summary(veteran$celltype)
p <- floor(100*slices/sum(slices))
pie3D(slices, labels=paste0(names(slices), " (",p, "%)"), explode=0.1)


# 32/135
xlab <- names(iris)[1]
ylab <- names(iris)[2]
title <- paste(ylab, "against", xlab, " of Iris Data")
x <- iris[,1]
y <- iris[,2]
plot(x, y, col="red", xlab=xlab, ylab=ylab, main=title)

range(x)
range(y)
plot(y~x, xlab=xlab, ylab=ylab, xlim=c(1.5,9), 
ylim=c(1.5,9), type="n")
points(x[1:50], y[1:50], col="red")
points(x[51:100], y[51:100], col="blue")
points(x[101:150], y[101:150], col="green")
abline(lm(y~x))


# 36/135
head(ChickWeight, 3)
coplot(weight ~ Time | Chick * Diet, type = "l", data = ChickWeight)

table(ChickWeight$Chick, ChickWeight$Diet)


# 37/135
curve(x^3-3*x, -2, 2)
curve(sin, -2*pi, 2*pi)

x <- seq(-2, 2, 0.01)
y <- x^3-3*x
plot(x, y, type="l")

weibull <- function(alpha, beta, x){
+   alpha * beta * (x^(alpha-1))
+ }
b <- c(1, 2, 4, 8)
for(i in 1:length(b)) {
+   curve(weibull(0.5, b[i], x), from=0, to=2, 
+         add=(i!=1), 
+         col=i, ylim=c(0, 50), main="alpha=.5")
+ }
legend(1.5, 40, legend=b, col=1:length(b), lty=1)


# 40/135
par(mfrow=c(1,4))
names(iris)
names(iris) <- c("SL", "SW", "PL", "PW", "Species")
boxplot(Sepal.Length, xlab="Sepal.Length")
boxplot(Sepal.Length~Species, ylab="Sepal.Length")
boxplot(iris[,which(sapply(iris, is.numeric))])
boxplot(iris[,which(sapply(iris, is.numeric))], horizontal=T, col=2:8)


# 42/135
pairs(iris[,1:4], col=as.integer(iris[,5])+1)
pairs(iris[,1:4], col=as.integer(iris[,5])+1, panel=panel.smooth)


# 48/135
library(MASS)
parcoord(iris[,1:4], col=as.integer(iris[,5])+1, var.label = T)
library(GGally) # Extension to 'ggplot2'
ggparcoord(data = iris, columns = 1:4, groupColumn = 5)
ggparcoord(data = iris, columns = 1:4, groupColumn = 5, boxplot = T)
ggparcoord(data = iris, columns = 1:4, groupColumn = 5, order = "anyClass",
+ showPoints = TRUE)

ggparcoord(data, columns = 1:ncol(data), 
  groupColumn = NULL,
  scale = "std", scaleSummary = "mean", centerObsID = 1,
  missing = "exclude", order = columns, showPoints = FALSE,
  splineFactor = FALSE, alphaLines = 1, boxplot = FALSE,
  shadeBox = NULL, mapping = NULL, title = "")


# 49/135
ploy <- function(x, y){x^2-x*y+y^2}
x.grid <- seq(-3, 3, length=50)
y.grid <- seq(-3, 3, length=50)
z.grid <- outer(x.grid, y.grid, FUN=ploy)
ploy.title <- paste("三維空間散圖\n", "f(x, y) =x^2-xy+y^2")
persp(x.grid, y.grid, z.grid, main= ploy.title)


# 50/135
par(mfrow = c(1,3), mai = c(0.3, 0.3, 0.2, 0.3)) 
scatter3D(x, y, z, bty = "f", colkey = FALSE, ticktype = "detailed",
          theta=0, phi=60)
scatter3D(x, y, z, bty = "g", pch = 18, 
          col.var = as.integer(s), 
          col = c("red", "blue", "green"),
          pch = 18, ticktype = "detailed",
          colkey = list(at = c(2, 3, 4), side = 1, 
                        addlines = TRUE, length = 0.5, width = 0.5,
                        labels = c("setosa", "versicolor", "virginica")))
text3D(x, y, z, labels = as.integer(s), colvar = w, bty = "b2")


# 51/135
library("rgl")
open3d()
plot3d(iris[,1:3], col=as.integer(iris[,5])+1, type ="p", size=10)

plot3d(iris[,1:3], col=as.integer(iris[,5])+1, type ="s", +        radius=0.15)
bbox3d(color=c("red", "black"), emission="gray", +        specular="yellow", shininess=5, alpha=0.8, nticks = 3) 
aspect3d(1,1,1)
 
lines3d(iris[c(1, 150), 1:3], col="purple", lwd=2)

shapes <- list(cube3d(), tetrahedron3d(), octahedron3d(), 
+           icosahedron3d(), dodecahedron3d(), cuboctahedron3d())
shapelist3d(shapes, x=1, y=1:6, z=1, size=0.3, col=1:6)
aspect3d(1,1,1)
 
texts3d(x=2, y=6, z=6, texts="rgl Example", font=2, 
+         color="blue", cex=2, family="serif")
> 
fit <- lm(iris[,3] ~ iris[,1] + iris[,2])
coefs <- coef(fit)
planes3d(a=coefs[2], b=coefs[3], c=-1, d= coefs["(Intercept)"], 
+          alpha = 0.5) 
play3d(spin3d(axis = c(0, 0, 1), rpm = 20), duration = 4)


# 52/135
terrain <- as.matrix(read.table("terrain_data.txt", header=F))
dim(terrain)
animal <- read.table("animal_data.txt", header=T)
dim(animal)
attach(animal)
head(animal, 3)
terrain.scale <- floor((terrain - min(terrain))/(max(terrain) - min(terrain))*99)+1
terrain.color <- terrain.colors(100)[terrain.scale] 
terrain.color[terrain==0] <- rgb(0, 0, 1) 
open3d()
clear3d("all") 
bg3d(col="gray") 
light3d() 
surface3d(1:100, seq(1,60,length=100), terrain, col=terrain.color, back="lines")
sex.col <- ifelse(sex==0, rgb(0, 0, 1), rgb(1, 0, 0)) 
z <- terrain[cbind(ceiling(loc.x), ceiling(loc.y*10/6))]
alpha.index <- (index-min(index))/(max(index)-min(index))
spheres3d(loc.x, loc.y, z + 0.5,
+           radius=0.3*number, col=sex.col, 
+           alpha=alpha.index)
detach(animal) 
play3d(spin3d(), duration=10)


# 53/135
open3d()
comet <- readOBJ("ESA_Rosetta_OSIRIS_67P_SHAP2P.obj")
class(comet)
str(comet)
shade3d(comet, col="gray")


# 54/135
x  <- as.matrix(mtcars)
rc <- rainbow(nrow(x), start = 0, end = .3)
cc <- rainbow(ncol(x), start = 0, end = .3)
hv <- heatmap(x, col = cm.colors(256), scale = "column",
              RowSideColors = rc, ColSideColors = cc, margins = c(5,10),
              xlab = "specification variables", ylab =  "Car Models",
              main = "heatmap(<Mtcars data>, ..., scale = \"column\")")


# 56/135
library(fields)
gbr <- two.colors(start="green", middle="black", end="red")
cell.raw <- read.table("trad_alpha103.txt", row.names=1, header=T)
cell.data <- t(scale(t(cell.raw[,2:19]), center=T, scale=T))    
n <- nrow(cell.data)
p <- ncol(cell.data)
gene.phase <- cell.raw[,1]
range(cell.data)
cell.data[cell.data > 2.802712] <- 2.802712 
cellcycle.color <- c("darkgreen", "blue", "red", "gray50", "orange")
rc <- cellcycle.color[gene.phase+1]
cc <- rainbow(ncol(cell.data))

hv1 <- heatmap(cell.data[n:1,], col = gbr, Colv=NA, Rowv=NA,
               RowSideColors = rc, 
               ColSideColors = cc, margins = c(5,10),
               xlab = "Times", ylab =  "Genes",main = "Heatmap of Microarray Data")

hv2 <- heatmap(cell.data, col = gbr, Colv=NA, Rowv=NULL,
               RowSideColors = rc, 
               ColSideColors = cc, margins = c(5,10),
               xlab = "Times", ylab =  "Genes",main = "Heatmap of Microarray Data")

dd <- as.dendrogram(hclust(as.dist(1-cor(t(cell.data)))))
hv3 <- heatmap(cell.data, col = gbr, Colv=NA, Rowv=dd,
              RowSideColors = rc, 
              ColSideColors = cc, margins = c(5,10),
              scale = "row",
              xlab = "Times", ylab =  "Genes",main = "Heatmap of Microarray Data")


# 58/135
x <- -6:16; length(x)
my.data <- outer(x, x);dim(my.data)
my.data
contour(my.data, method = "edge")

image(x, x, z)
contour(x, x, z, col = "blue", add = TRUE,           method = "edge", lwd=1.5, lty=3)


# 59/135
filled.contour(volcano, color.palette = terrain.colors, asp = 1)
title(main = "volcano data: filled contour map")
filled.contour(volcano, color.palette = colorRampPalette(c("red", "white", "blue")), asp = 1)


# 60/135
install.packages(c("tiff", "jpeg", "png", "fftwtools"), repos="http://cran.csie.ntu.edu.tw")
library(EBImage) # (Repositories: BioC Software)
Transformers <- readImage("Transformers07.jpg")
(dims <- dim(Transformers))
Transformers
plot(c(0, dims[1]), c(0, dims[2]), type='n', xlab="", ylab="")
rasterImage(Transformers, 0, 0, dims[1], dims[2])

source("https://bioconductor.org/biocLite.R")
biocLite("EBImage")

library(jpeg)
Transformers <- readJPEG("Transformers07.jpg")


# 61/135
Transformers.f <- Image(flip(Transformers))
rgb.weight <- c(0.2989, 0.587, 0.114)
Transformers.gray <- rgb.weight[1] * imageData(Transformers.f)[,,1] + 
rgb.weight[2] * imageData(Transformers.f)[,,2] + 
rgb.weight[3] * imageData(Transformers.f)[,,3]
dim(Transformers.gray)
Transformers.gray[1:5, 1:5]
par(mfrow=c(1,2), mai=c(0.1, 0.1, 0.1, 0.1))
image(Transformers.gray, col = grey(seq(0, 1, length = 256)), xaxt="n", yaxt="n")
image(Transformers.gray, col = rainbow(256), xaxt="n", yaxt="n")


# 64/135
GetMap(center = c(lat = 42, lon = -76), size = c(640, 640), destfile, 
    zoom = 12, markers, path = "", span, frame, hl, sensor = "true", 
    maptype = c("roadmap", "mobile", "satellite", "terrain", 
        "hybrid", "mapmaker-roadmap", "mapmaker-hybrid")[2], 
    format = c("gif", "jpg", "jpg-baseline", "png8", "png32")[5], 
    RETURNIMAGE = TRUE, GRAYSCALE = FALSE, NEWMAP = TRUE, SCALE = 1, 
    verbose = 0)

library(RgoogleMaps)
WorldMap <- GetMap(center=c(0,0), zoom =1, 
                   destfile = "World1.png")


# 65/135
TaiwanMap <- GetMap(center=c(lat = 23.58, lon =120.58), zoom =7, destfile = "Taiwan1.png")
TaiwanMap <- GetMap(center=c(lat = 23.58, lon =120.58), zoom = 10, destfile = "Taiwan2.png", maptype = "terrain")


# 66/135
my.lat <- c(25.175339, 25.082288, 25.042185, 25.046254)
my.lon <- c(121.450003, 121.565481, 121.614548, 121.517532)
bb = qbbox(my.lat, my.lon)
print(bb)
MyMap <- GetMap.bbox(bb$lonR, bb$latR, destfile = "my.png", maptype = "roadmap")

My.markers <- cbind.data.frame(lat = my.lat, lon = my.lon)
tmp <-  PlotOnStaticMap(MyMap, lat = My.markers[,"lat"], lon = My.markers[,"lon"], destfile = "my.png", cex=2.5, pch=20, col=1:4, add=F)


# 67/135
library(maps); library(maptools); library(mapdata); library(mapproj)
layout(matrix(c(1,1,1,0,2,0), ncol=2), widths=c(10, 1), heights=c(1, 10, 1))
map("world2Hires", xlim=c(118, 123), ylim=c(21, 26))
data <- read.table("20140714-weather.txt", sep="\t", header=TRUE, row.names=1)
x <- data$TEMP
tm <- floor((100-1)/(max(x)-min(x))*(x-min(x)) + 1)
used.col <- heat.colors(100)[tm]
points(data$lon, data$lat, pch=15, col=used.col)
text(data$lon, data$lat, labels=row.names(data))
title("20140714, 晚上8時各地溫度")
par(mar=c(1,1,1,1))
image(t(matrix(c(1:100), ncol=1)), 
      col=heat.colors(100), xaxt="n", yaxt="n")
axis(LEFT <- 2, at=tm/100, 
     labels=as.character(x), cex.axis=1)


# 68/135
library(ggplot2)
library(maps)
library(ggmap)
library(mapproj)
states.map <- map_data("state")
head(states.map, 3)

tail(states.map, 3)

ggplot(states.map, aes(x=long, y=lat, group=group)) + 
geom_polygon(fill="white", colour="black")

ggplot(states.map, aes(x=long, y=lat, group=group)) +
geom_path() + coord_map("mercator")


# 69/135
 head(USArrests, 3)
crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)
head(crimes, 3)
library(maps); library(ggmap)
states.map <- map_data("state")
head(states.map, 3)
head(crime.map, 3)


# 70/135
library(plyr)
crime.map <- arrange(crime.map, group, order)
head(crime.map, 3)
ggplot(crime.map, aes(x=long, y=lat, group=group, fill=Assault)) +
         geom_polygon(colour="black") +
         coord_map("polyconic")


# 71/135
library(googleVis)
demo(googleVis)


# 77/135
n <- 1e+02

<- 1e+08

n <- 1e+02
y <- as.factor(sample(LETTERS[1:4], n, replace=T, prob=c(0.1, 0.1, 0.5, 0.3)))
x1  <- rnorm(n)
x2  <- rbeta(n, 0.5, 0.5)
xydata <- data.frame(y, x1, x2)
par(mfrow=c(1,4))
boxplot(x1~y, data=xydata, ylab="x1", main="boxplot")
hist(x2, xlab="x2", main="hist")
barplot(table(y), xlab="y", col = 2:5, main="barplot")
plot(x1, x2, main="plot", col=as.integer(y)+1)


# 82/135
n <- 1e+04
x1 <- rnorm(n, mean = -1, sd = 1)
y1 <- rnorm(n, mean = -1, sd = 1)
x2 <- rnorm(n, mean = 2, sd = 1) 
y2 <- rnorm(n, mean = 2, sd = 1)
par(mfrow=c(1, 2))
plot(x1, y1, main="black")
smoothScatter(x1, y1, col="black", colramp=colorRampPalette(c("white", "black")), main='colorRampPalette(c("white", "black"))')


# 85/135
x <- rnorm(mean=1.5, 10000)
y <- rnorm(mean=1.6, 10000)
my.data <- data.frame(x, y)
pk <- c("RColorBrewer", "hexbin", "gplots")
install.packages(pk, repos="http://cran.csie.ntu.edu.tw")
library(RColorBrewer)
col_rb <- colorRampPalette(rev(brewer.pal(11, 'Spectral')))
plot(my.data, pch=16, col='black', cex=0.5)
library(hexbin)
h <- hexbin(my.data) # create a hexbin object
h
plot(h) # in grey level
plot(h, colramp=col_rb) # rainbow color


# 86/135
iris.pca <- princomp(iris[,-5])
plot(iris.pca$scores[,1:2], pch=15, col=as.integer(iris[,5])+1)
iris.hex <- hexbin(iris.pca$scores[,1:2])
plot(iris.hex, colramp=col_rb)


# 87/135
zz <- unz(description="household_power_consumption.zip", filename="household_power_consumption.txt")
colC <- c(rep("NULL", 2), "numeric", "NULL", "numeric", rep("NULL", 4))
power <- read.table(zz, header=T, sep=";", colClasses = colC, na.strings = "?")
summary(power)

plot(power)
power.hex <- hexbin(power)
plot(power.hex, colramp=col_rb)


# 89/135
library(bigvis)
library(ggplot2)
head(airquality)
par(mfrow=c(1, 2))
hist(airquality$Ozone)
hist(airquality$Temp)
binData <- with(airquality, condense(bin(Ozone, 20), bin(Temp, 5)))
binData
ggplot(data=binData, aes(Temp, Ozone, fill=.count))+ 
+   geom_tile() + 
+   geom_point(data=airquality, aes(fill=NULL), colour="orange")


# 90/135
data(movies)
dim(movies)
head(movies)

install.packages("gridExtra")
library(gridExtra)
g1 <- ggplot(data=movies, aes(length)) + geom_histogram()
g2 <- ggplot(data=movies, aes(rating)) + geom_histogram() + ylab("votes")
grid.arrange(g1, g2, nrow=1, ncol=2)


# 91/135
arrange(movies, desc(length))[1:10,] %>% select(title, year, length, rating, votes)


# 92/135
nobin <- 1e4
binData <- with(movies, condense(bin(length, find_width(length, nobin)),
+                                  bin(rating, find_width(rating, nobin))))
Summarising with count
ggplot(data=binData, aes(length, rating, fill = .count)) + geom_tile()
last_plot() %+% peel(binData) 
smoothBinData <- smooth(peel(binData), h=c(20, 1))
autoplot(smoothBinData)


# 93/135
source("https://bioconductor.org/biocLite.R")
biocLite("ALL")
library(ALL)
data(ALL)
ALL
str(ALL)
dim(exprs(ALL))
exprs(ALL)[1:3, 1:5]
table(ALL$mol.biol)
eset <- ALL[, ALL$mol.biol %in% 
                c("BCR/ABL", "ALL1/AF4")]
dim(exprs(eset))
f <- factor(as.character(eset$mol.biol))
eset.p <- apply(exprs(eset), 1, function(x) t.test(x ~ f)$p.value)
selected.eset <- eset[eset.p < 0.00001, ]
dim(selected.eset)
ma.col <- colorRampPalette(c("green", "black", "red"))(200)
var.col <- ifelse(f=="ALL1/AF4", "blue", "red")
heatmap(exprs(selected.eset), col=ma.col, ColSideColors=var.col,  
          scale="row")


# 94/135
source("https://bioconductor.org/biocLite.R")
biocLite("ComplexHeatmap")
library(ComplexHeatmap)
Heatmap(exprs(selected.eset))


# 96/135
install.packages("tabplot")
library(tabplot)
tableplot(iris, nBins=150, sortCol=5)


# 97/135
tableplot(iris, nBins=50, sortCol=4)


# 98/135
require(ggplot2)
data(diamonds)
dim(diamonds)
head(diamonds)
tableplot(diamonds)


