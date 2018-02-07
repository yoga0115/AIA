'
R 語言探索性資料分析與資料視覺化
流程控制 (Flow control)
'

setwd("D:/Dropbox/Sinica_Project/Theta/AIA/lectures")

### Loop ====

# 創造一個從 1 到 10000，中間跨 2 的數值變數
# create a numeric variable from 1 to 100 with interval equal to 2
a <- seq(1,10000,2) # seq(start, stop, interval)
print(a[1:20]) # print first 20 elements in the variable "a"

# 每個數值都平方，使用 "for loop"
# square to each element in the variable with "for loop"
start.time <- proc.time()
for (index in a) {
  print(index^2)
}
end.time <- proc.time()
time.loop.for <- end.time - start.time
print(time.loop.for)

# 每個數值都平方，使用 "while"
# square to each element in the variable with "while"
start.time <- proc.time()
index <- 1
while(index <= length(a) ){
  print(a[index]^2)
  index <- index + 1
}
end.time <- proc.time()
time.loop.while <- end.time - start.time
print(time.loop.while)

# 每個數值都平方，使用 vector
# square to each element in the variable directly
start.time <- proc.time()
print(a^2)
end.time <- proc.time()
time.loop.noloop <- end.time - start.time
print(time.loop.noloop)

print(time.loop.for)
print(time.loop.while)
print(time.loop.noloop)
### Condition control ====

# 條件式
# 基本用法: 用於數字向量 (>, == , < , or: |, and: &)
# basic usage: with numeric vector (>, == , < , or: |, and: &)
this.is.a.numeric_vec <- c(1,2,3,4,5)
x <- this.is.a.numeric_vec > 3
print(x)
this.is.a.numeric_vec[this.is.a.numeric_vec > 3]
this.is.a.numeric_vec[this.is.a.numeric_vec < 3 | this.is.a.numeric_vec > 3]
this.is.a.numeric_vec[this.is.a.numeric_vec > 2 & this.is.a.numeric_vec < 4]

# 條件式
# 基本用法: 用於文字向量
# basic usage: with character vector
this.is.a.char_vec <- c("a","b","c","d","a","c")
x <- this.is.a.char_vec != "c"
print(x)
this.is.a.char_vec[this.is.a.char_vec != "c"]

# 用於 data.frame
# conditional statement in data frame
head(mtcars)
mtcars["mpg" > mean(mtcars$mpg), ]

x <- which(mtcars$mpg > mean(mtcars$mpg) & mtcars$carb == 1)
print(x)
mtcars[x,]

# 條件判斷式
# condition statement
a <- sample(1:100, 10, replace = F)
print(a)
for (i in a) {
  if (i <= 10 ) {
    print( paste(i, " is less than or equal to 10", sep = "") )
  } else if (i > 10 & i < 50) {
    print( paste(i, " is larger than 10 and smaller than 50"))
  } else {
    print( paste(i, " is larger than 50"))
  }
}

### Customized function ====

# 自訂義一個平方後加 1 的函式
# Define a function that squre(x) and add 1
squre_add_1 <- function(x) {
  # You can do anything here
  tmp <- x^2 + 1
  
  # You have to return something to get output
  return(tmp)
}
squre_add_1(10)

# 將前面判斷大小的範例轉成以向量進行, 結合 sapply 與 customized function
# Combine sapply and customized function to check element status in the variable list
check.status <- function(x) {
  if (x <= 10 ) {
    out <- paste(x, " is less than or equal to 10", sep = "")
  } else if (x > 10 & x < 50) {
    out <- paste(x, " is larger than 10 and smaller than 50")
  } else {
    out <- paste(x, " is larger than 50")
  }
  return(out)
}
sapply(a, check.status)

### END  ==== 
# This is the end of this section
rm(list = ls()) # clear all variables

