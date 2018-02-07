'
R 語言探索性資料分析與資料視覺化
檔案讀寫 (save and read files)
'
setwd("D:/Dropbox/Sinica_Project/Theta/AIA/lectures/R_basis/")

### read file with pure text or csv
# 純文字檔可以用 read.csv 讀入
readin.txt1 <- readLines("examples/python_pkgs1.txt") # will become a character vector
typeof(readin.txt1)
print(readin.txt1[2:5])

readin.txt2 <- read.csv("examples/python_pkgs1.txt", # file name
                    header = F, # file has header?
                    stringsAsFactors = FALSE # character to string?
                    ) # will transform to data frame
tail(readin.txt2)

readin.csv1 <- read.csv(file = "examples/DL_toolkits_results_google.csv",
                        header = TRUE, 
                        sep = ",",
                        stringsAsFactors = FALSE)
head(readin.csv1)

# 網路上的文字檔也可以直接讀取
# read.csv can read on-line txt file directly
readin.csv2 <- read.csv('http://www.stat.berkeley.edu/classes/s133/data/movies.txt', 
                        sep='|',
                        stringsAsFactors=FALSE)
head(readin.csv2)

### wrtie files (to csv)

# add new column and save it
readin.csv1$log.search_results <- log(readin.csv1$search_results + 1) # add 1 to prevent -Inf
head(readin.csv1, 4)
write.csv(readin.csv1, file = "tmp.csv", row.names = FALSE)

x <- read.csv("tmp.csv", header = T)
head(x, 4)

### write files (to rds, rda)
saveRDS(readin.csv1, file = "one_file_one_var.rds")
save(readin.csv1, readin.csv2, file = "one_file_multi_vars.rda")

# Test it
rm(list = ls())
x <- readRDS(file = "one_file_one_var.rds")
head(x,4)

rm(list = ls())
load("one_file_multi_vars.rda")
ls()

