'
R 語言探索性資料分析與資料視覺化
資料操作: data.table

'

# 載入 data.table package
library(data.table)

### 基本操作 - 創建 data.table ====

# 創建一個 data.table
# create a data.table
DT <- data.table(V1=c(1L,2L), V2=LETTERS[1:3], V3=round(rnorm(4),4), V4=1:12)
DT
# 將其他物件轉為 data.table
# convert other object into data.table
# DT <- as.data.table(iris)

### 基本操作 - 讀寫資料 ====

# 將 data.table 寫入 csv
# write data.table to csv
fwrite(DT, file = "data.csv")
file.exists("data.csv")

# 讀取資料
# read data from file, type ?fread for more description
DT <- fread('data.csv')

### 基本操作 - 觀察 ====

# 檢查類型
# check the type
class(DT)

# 欄位名稱
# column name
names(DT)

# 資料的 row 數
# the number of rows of the data
nrow(DT)

# 資料的 column 數
# the number of columns of the data
ncol(DT)

# print 出第 2 row 的資料
# print the second row of the data
DT[2]
DT[2, ]

# print 出第 3 個 column 的資料
# print the third column as a data.table
DT[, 3]

# print 出第 3 個 column 的值
# print the third column as a vector
DT[[3]]

n <- 5
# 取出前 n row 的資料
# print the first n rows of the data
head(DT, n)

# 取出後 n row 的資料
# print the last n rows of the data
tail(DT, n)

# 在 R-Studio 腳本區觀察資料
# View the data at script area
View(DT)
