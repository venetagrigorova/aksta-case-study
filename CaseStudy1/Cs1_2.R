rho_n <- function(n) {
  if(n>=0 && n %% 1==0){
    result <- gamma((n - 1) / 2) / gamma(1/2)*gamma((n-2)/2)
    print(result)
  }
}

rho_n(2000)
#The result its inf and it does not show the result because we can not represent it wiht double precition.

#https://stackoverflow.com/questions/66149524/r-how-to-have-gamma-return-the-actual-number-instead-of-inf

rho_n_1000<- function(n) {
  if(n>=0 && n %% 1==0){
    #https://people.richland.edu/james/lecture/m116/logs/properties.html
    result_log <- lgamma((n - 1) / 2) - (lgamma(1/2) + lgamma((n - 2) / 2))    
    result <- exp(result_log)
    print(result)
  }
}
rho_n_1000(1000)

#https://www.guru99.com/r-apply-sapply-tapply.html
rho_n_values <- sapply(1:100,function(n){
  return(rho_n_1000(n) / sqrt(n))
})


plot(1:100 , rho_n_values, type = "l",xlab = "n", ylab = "rho_value", main = "rho_value /sqrt(n) n=100")
#For an increasing n rho(n) and sqrt(n) should stabilize at some point.