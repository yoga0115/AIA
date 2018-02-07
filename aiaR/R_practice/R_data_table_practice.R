#session 1
#Q1
shop_detials <- fread("~/Documents/GitHub/aiaR/R_practice/data/shop_details.csv")
shop_infos <- fread("~/Documents/GitHub/aiaR/R_practice/data/shop_infos.csv")
shop_reviews <- fread("~/Documents/GitHub/aiaR/R_practice/data/shop_reviews.csv")
sapply(shop_infos, typeof)
sapply(shop_detials, typeof)

#Q2
View(merge(shop_detials,shop_infos, by = "id"))

#session 2
#Q1
shop_lolipop <- shop_reviews[, .(avg.n.lolipop = mean(n.lolipop)), by = "id"]
View(merge(shop_infos,shop_lolipop, by = "id"))

#session 3
#Q1
View(shop_detials)
shop_infos$city <- 0
for (i in seq(length(shop_infos$addr))){
  shop_infos$city[i] <- substring(shop_infos$addr[i],1,3)
}
Q1_DT <- melt(shop_infos, id.vars = 2, measure.vars = 8, variable.name = "area")
Q1_DT2 <- dcast(Q1_DT, id ~ value)
Q1_DT2

#Q2
data.all <- merge(shop_infos,shop_detials)
data.before.melt <- data.table(data.all[,1], data.all[,5], data.all[,10])
View(data.before.melt)
# scoring.long <- melt(data.all, id.vars = 1, measure.vars = 10, variable.name = "scoring.mix", vlaue.name = "value")
# cate.minor.long <- melt(data.all, id.vars = 1, measure.vars = 5, vlaue.name = "cate.minor")
# data.mid <- merge(cate.minor.long, scoring.long, by = "id")

