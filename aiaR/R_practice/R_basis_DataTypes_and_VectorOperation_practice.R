#Q1
p1 <- c(1,4,2,NA,7,20,NA,15,10,5)
is.na(p1)
p1[is.na(p1)]
p1[is.na(p1)] <- median(p1, na.rm = TRUE)
p1

#Q2
movies = read.csv('https://goo.gl/ByTnpD',sep = '|', stringsAsFactors = FALSE)
sapply(movies, typeof)

#Q3
#轉金額
for (i in seq(length(movies$box))){
  movies$box[i] <- substring(movies$box[i],2)
}
movies$box <- as.numeric(movies$box)
movies$box
#前10
sum(head(sort(movies$box, decreasing = TRUE),10))
#後10
sum(tail(sort(movies$box, decreasing = TRUE),10))

#Q4
movies$year = 0
for (i in seq(length(movies$date))){
  movies$year[i] <- substring(movies$date[i],nchar(movies$date[i])-3,nchar(movies$date[i]))
}
movies$year <- as.numeric(movies$year)
movies$year.pass = 2018 - movies$year
movies$avg.box = movies$box / movies$year.pass
idx = head(sort(movies$avg.box, decreasing = TRUE),5)
movies$name[movies$avg.box == idx[1]]
idx
for (i in idx){
  print(i)
  print(movies$name[movies$avg.box == i])
}
  
  
  