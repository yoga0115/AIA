'
R 語言探索性資料分析與資料視覺化
資料操作: data.table

'

# 載入 data.table package 
library(data.table)

### 合併資料集 ====
Employees <- data.table(id=1:3, name=c("Alice", "Bob", "Carla"), 
                        department.id=c(11, 12, 14), salary=c(800, 900, 1000))
Departments <- data.table(department.id=c(11, 12, 99), 
                          department.name=c("Production", "Sales", "Research"))

Employees
Departments
# inner join
merge(Employees, Departments, by = "department.id")

# left outer join
merge(Employees, Departments, by = "department.id", all.x = T)

# right outer join
merge(Employees, Departments, by = "department.id", all.y = T)

# outer join
merge(Employees, Departments, by = "department.id", all = T)
