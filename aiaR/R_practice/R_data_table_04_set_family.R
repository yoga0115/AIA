'
R 語言探索性資料分析與資料視覺化
資料操作: data.table

'

# 載入 data.table package
library(data.table)

DT <- data.table(iris)

### set 家族 ====

# 資料排序
# order rows
setorder(DT, Sepal.Length, -Petal.Length)

# 更改欄位名稱
# change column names
setnames(DT, "Species", "species")
setnames(DT, names(DT), tolower(names(DT)))
names(DT)

# 更改欄位順序
# change column ordering
setcolorder(DT, c(1, 3, 2, 4, 5))
