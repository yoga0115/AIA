'
R 語言探索性資料分析與資料視覺化
資料操作: data.table

'

# 載入 data.table package
library(data.table)

DT <- data.table(iris)

### 基本操作 - i ====

# 篩選第 3 ~ 5 行
# select 3rd to 5th row
DT[3:5, ]
DT[3:5]

# 篩選 Species 欄位中所有值是 'setosa' 的行
# select all rows that have value 'setosa' in column Sepcies
DT[Species == "setosa" & Sepal.Length > 5]

# 篩選所有 Sepal.Length 數值介於 5 ~ 5.2 的行
# select all rows that the Sepal.Length column have value between 5 and 5.2
DT[Sepal.Length %between% c(5, 5.2)]
DT[Sepal.Length >= 5 & Sepal.Length <= 5.2]

# 搭配 eval 及 parse 以字串控制篩選條件
# work with eval and parse to control subset condition with text
# Our goal: Sepal.Length < median(Sepal.Length) | Sepal.Width < median(Sepal.Width)
c.names <- paste0("Sepal.", c("Length", "Width")) # [1] "Sepal.Length" "Sepal.Width" 
condition_list <- paste0(c.names, " < median(", c.names, ")") # [1] "Sepal.Length < median(Sepal.Length)" "Sepal.Width < median(Sepal.Width)"  
or.condition <- paste0(condition_list, collapse = "|") # [1] "Sepal.Length < median(Sepal.Length)|Sepal.Width < median(Sepal.Width)"
DT[eval(parse(text = or.condition))]

# 搭配 order() 用於排序
# work with order() for sorting
DT[order(Sepal.Length, -Sepal.Width)]

## 基本操作 - j ====

# 篩選欄位 & with = FALSE
# subset columns and with = FALSE
DT[, 1] # same as DT[, .(Sepal.Length)]
DT[[1]] # same as DT[, Sepal.Length]
DT[, c(1, 3)] # same as DT[, .(Sepal.Length, Petal.Length)]
DT[, grep("Length", names(DT))] # not what we desired
DT[, grep("Length", names(DT)), with=F] # return data.table that columns names contain "Length"
DT[, -grep("Length", names(DT)), with=F] # return data.table that columns names do not contain "Length"

# 增加/更新欄位
# adding/updating columns
DT[, prod.SL.SW := Sepal.Length * Sepal.Width] # add a column "prod.SL.SW" if it doesn't exist before, 
                                               # else update it with Sepal.Length * Sepal.Width

DT[, c("prod.SL.SW", "prod.PL.PW") := list(Sepal.Length * Sepal.Width, Petal.Length * Petal.Width)] # update multi columns simultaneously
# alternatively
DT[, ':=' (prod.SL.SW=Sepal.Length * Sepal.Width, 
           prod.PL.PW=Petal.Length * Petal.Width)]

DT[, prod.SL.SW := NULL] # update "prod.SL.SW" with NULL (deleting it)

c.names <- c("prod.PL.PW")
DT[, c.names := NULL]
DT[, (c.names) := NULL] # update columns that stored in variable c.names

# 對列進行計算
# compute on columns
DT[, mean(Sepal.Length)]
DT[, .(mean(Sepal.Length), sum(Sepal.Width))] # same as DT[, list(mean(Sepal.Length), sum(Sepal.Width))]
DT[, .(avg.SL=mean(Sepal.Length), sum.SW=sum(Sepal.Width))]
DT[Species == 'setosa', .(avg.SL=mean(Sepal.Length), sum.SW=sum(Sepal.Width))]

# .SD (subset of data.table) example
# .SD 例子
DT[, .SD, .SDcols = c(1, 2)] # same as DT[, .SD, .SDcols = c("Sepal.Length", "Sepal.Width")]
DT[, .SD[1:10]]

# 用 lapply & 計算每個 .SD 欄位 mean
# use lapply to compute mean for every columns
DT[, lapply(.SD, mean), .SDcols=-c("Species")]

norm <- function(x){
  (x-mean(x))/sd(x)
}
c.names <- names(DT)[1:4]
DT[, (paste0("norm.", c.names)) := lapply(.SD, norm), .SDcols=c.names]
DT[, (paste0("norm.", c.names)) := NULL]

### 基本操作 - by ====

# 搭配 j, 對 by 中的欄位/條件進行分組計算
# work with j,grouping computation using columns or expression in by
DT[, .SD[1:2], by=.(Species)]
DT[, lapply(.SD, mean), by=.(S=Species)]
DT[, lapply(.SD, mean), by=.(is.setosa=Species=="setosa")]
DT[, lapply(.SD, mean), .SDcols=-c("Species"),
   by=.(is.setosa=Species=="setosa", is.big.sepal=Sepal.Width > mean(Sepal.Width))]
DT[, SL.by.group := norm(Sepal.Length), by=.(Species)]

#### 以上的操作都可以直接搭配 i ####
#### operations above all work with i ####

# 搭配 .N 可以計數
# work with .N for counting
DT[, .(count=.N), by=.(Species)]
DT[, .(avg.SL=mean(Sepal.Length[1:.N-1])), by=.(Species)]

# 連鎖操作
# chaining operation
# Which species is the biggest under condition of Sepal.Length > 5?
DT[Sepal.Length > 5, .(count=.N), by=.(Species)][count==max(count)]

#### Note ====
# use copy(DT) if you don't want DT2 is just a reference of DT!
DT2 <- DT 
head(DT2)
DT[, test := "test"]
head(DT2)

#### extra ====
# use index as filter
id.with.outlier <- lapply(DT[, -5], function(x) which(x > quantile(x, 0.95)))
DT[-unique(unlist(id.with.outlier))]

# vectorized function is required while updating column using j
DT[, species.split := strsplit(Species, "o")] # should meet error

mystrsplit <- function(x, split, i=1){
  strsplit(x, split)[[1]][i]
}
vector.strsplit <- function(x, split, i = 1){
  sapply(x, FUN = mystrsplit, split=split, i = i)
}
DT[, species.split := vector.strsplit(as.character(Species), "o")]
