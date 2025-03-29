# https://www.cuemath.com/algebra/fibonacci-numbers/
fib_ratio_for <- function(n) {
  fib <- numeric(n + 1)
  fib[1] <- 1
  fib[2] <- 1
  for (i in 3:(n + 1)) {
    fib[i] <- fib[i - 1] + fib[i - 2]
  }
  return(fib[2:(n + 1)] / fib[1:n])
}
print(fib_ratio_for(10))

fib_ratio_while <- function(n){
  fib1 <- numeric(n + 1)
  fib1[1] <- 1
  fib1[2] <- 1
  i <- 3
  while (i <= (n+1)){
    fib1[i] <- fib1[i - 1] + fib1[i - 2]
    i <- i+1
  }
  return(fib1[2:(n + 1)] / fib1[1:n])
}
print(fib_ratio_while(20))

#https://www.appsilon.com/post/r-microbenchmark
benchmark <- microbenchmark(
  fib_ratio_for_200 = fib_ratio_for(200),
  fib_ratio_while_200 = fib_ratio_while(200),
  fib_ratio_for_2000 = fib_ratio_for(2000),
  fib_ratio_while_2000 = fib_ratio_while(2000)
)
  print(benchmark)
#The for loop with 200 and 2000 was faster

plot(1:100,fib_ratio_for(100),type = "l",xlab = "n", ylab = "fib_ratio", main = "fib_ratio n=100") 
#For n=10 it start to stabilize
#The sequence converge at 1.6
  
  
  
  
  
  
  
  
  
