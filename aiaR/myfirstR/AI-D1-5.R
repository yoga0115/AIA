# 13/22
pain <- c(4, 5, 4, 3, 2, 4, 3, 4, 4, 6, 8, 4, 5, +  4, 6, 5, 8, 6, 6, 7, 6, 6, 7, 5, 6, 5, 5)
drug <- c(rep("A", 9), rep("B", 9), rep("C", 9))
migraine <- data.frame(pain, drug)
plot(pain ~ drug, data=migraine)
migraine.aov <- aov(pain ~ drug, data=migraine)
summary(migraine.aov)

kruskal.test(pain ~ drug, data=migraine)


# 14/22
pairwise.t.test(pain, drug, p.adjust="bonferroni")
TukeyHSD(migraine.aov)


# 15/22
par(mfrow=c(1, 2))
hist(iris$Sepal.Width)
qqnorm(iris$Sepal.Width)
qqline(iris$Sepal.Width, col="red")


# 16/22
x <- iris$Sepal.Width
ks.test(x, 'pnorm', mean(x), sd(x))

library(nortest)
ad.test(iris$Sepal.Width)

shapiro.test(iris$Sepal.Width)


# 22/22
M <- as.table(rbind(c(762, 327, 468), 
                      c(484, 239, 477)))
dimnames(M) <- list(gender = c("F", "M"),
+                     party = c("Democrat",
                                "Independent", 
                                "Republican"))
M
(res <- chisq.test(M))

