'
R 語言探索性資料分析與資料視覺化
資料操作: data.table

'

# 載入 data.table package
library(data.table)

DT <- data.table(iris)

### 如何重鑄資料 ====

# melt: 每一個測量變數獨占一個 row，新增 columns 來表示對應的是哪個測量變數
# 
long_DT <- melt(DT, id.vars = 5, measure.vars = 1:4, 
                variable.name = "key", value.name = "value")

# dcast: formula 左邊變量的 unique element 都會作為結果的一 row,
#        右邊變量的每個 unique element 都會在結果產生一個 column

DT2 <- copy(DT)
DT2[, id := 1:.N]
long_DT <- melt(DT2, id.vars = 5:6, measure.vars = 1:4, 
                variable.name = "key", value.name = "value")
wide_DT <- dcast(long_DT, id + Species ~ key, value.var = "value", fun=NULL)

# example for 'fun' parameter
dcast(long_DT, Species ~ key, value.var = "value", fun=length)

dcast(long_DT, Species ~ key, value.var = "value", fun=mean)
