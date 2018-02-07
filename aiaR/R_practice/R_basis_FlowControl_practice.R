#Q1
#a
check.fizz_and_buss <- function(x){
  if (x %% 3 == 0 | x %% 5 == 0){
    k = 'FizzBuss'
  }else if (x %% 3 == 0){
    k = 'Fizz'
  }else if (x %% 5 == 0){
    k = 'Buss'
  }else{
    if (x %% 7 < 4){
      k = 'Hi'
    }else{
      k = 'Bye'
    }
  }
  return(k)
}

#b
start.time.loop <- proc.time()
for (i in 1:1000){
  print(check.fizz_and_buss(i))
}
end.time.loop <- proc.time()

#c
start.time.sapply <- proc.time()
sapply(1:1000, check.fizz_and_buss)
end.time.sapply <- proc.time()

start.time.loop - end.time.loop
#user  system elapsed 
#-0.050  -0.005  -0.801 
start.time.sapply - end.time.sapply
#user  system elapsed 
#-0.018  -0.006  -1.102 

#Q2
mtcars[which(mtcars$disp > median(mtcars$disp)),]






