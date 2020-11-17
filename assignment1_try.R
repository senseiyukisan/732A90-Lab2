#########
### 1 ###
#########

# Functional
min_se <- function(data, par){
  with(data, sum((par[1]+ par[2] *x + par[3] * x^2) - y) ^2)
}


approximate <- function(x0,x1,x2){
  data <- rbind(x0,x1,x2)
  
  
  optimized <- optim(par = c(0.5,0.5,0.5), fn = min_se, data = data)
  return(optimized$par)
}



#########
### 2 ###
#########


approximate_cut <- function(n, fn){
  interval <- seq(from = 0, to = 1, by = n)
  
}
