# 4/29
exp.m <- apply(df[, index.exp], 1, mean)
ctl.m <- apply(df[, index.ctl], 1, mean)
plot(exp.m, ctl.m)
abline(a=0, b=1)
fc <- exp.m/crl.m
no.genes <- 50
sort(fc, decreasing = TRUE)[1:no.genes]


# 18/29
x <- iris$Sepal.Length
y <- iris$Petal.Length
alpha <- 0.05
(vt <- (var.test(x, y)$p.value <= alpha))
t.test(x, y, var.equal = !vt ) 


# 19/29
myData <- data.frame(value = iris$Sepal.Width[-(1:50)], group <- iris[-(1:50), 5])
alpha <- 0.05
(bt <- bartlett.test(value ~ group, data=myData)$p.value <= alpha)
t.test(value ~ group, data=myData, var.equal=!bt)


# 27/29
source("https://bioconductor.org/biocLite.R")
biocLite("made4")
library(made4)
data(khan)
Anova.pvalues <- function(x){
   x <- unlist(x)
   SRBCT.aov.obj <- aov(x ~ khan$train.classes)
   SRBCT.aov.info <- unlist(summary(SRBCT.aov.obj))
   SRBCT.aov.info["Pr(>F)1"]
 }
SRBCT.aov.p <- apply(khan$train, 1, Anova.pvalues)


# 28/29
order.p <- order(SRBCT.aov.p)
ranked.genes <- data.frame(pvalues=SRBCT.aov.p[order.p], ann=khan$annotation[order.p, ])
top5.gene.row.loc <- rownames(ranked.genes[1:5,  ])
summary(t(khan$train[top5.gene.row.loc, ]))

par(mfrow=c(1, 5), mai=c(0.3, 0.4, 0.3, 0.3))
usr <- par("usr")
myplot <- function(gene){ 
boxplot(unlist(khan$train[gene, ]) ~ khan$train.classes, ylim=c(0, 6), main=ranked.genes[gene, 4])
   text(2, usr[4]-1, labels=paste("p=", ranked.genes[gene, 1], sep=""), col="blue")
   ranked.genes[gene,]
 }


# 29/29
#print the top5 DE genes info
do.call(rbind, lapply(top5.gene.row.loc, myplot))
# lappay returns "list" and use rbind to convert it to "data.frame"
# Try sapply?
