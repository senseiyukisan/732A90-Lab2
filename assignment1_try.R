#########
### 1 ###
#########

# Functional
interp_fn <- function(boundaries, par){
  result <- c()
  for (i in 1:length(boundaries)){
    result[i] = c(par[1] + par[2] * boundaries[i] + (par[3] * boundaries[i]^2))
  }
  return(result)
}


min_se <- function(boundaries, par, target_fn){

  t_f = target_fn(boundaries)
  i_f = interp_fn(boundaries, par)
  sse = sum((t_f - i_f)^2)
  return(sse)
}


approximate <- function(boundaries, target_fn){
  optimized <- optim(par=c(0,0,0), fn=min_se, target_fn=target_fn, boundaries=boundaries)
  return(optimized$par)
}



#########
### 2 ###
#########


approximate_cut <- function(n, fn){
  interval <- seq(0, 1, length.out = n+1)
  interval_targets = list()
  for (i in 1:length(interval)) {
    first_val = interval[i]
    if (first_val == 1){stop}
    else{
      second_val = interval[i+1]
      mid_val = interval[i] + ((second_val - first_val) / 2)
      boundaries = c(first_val, mid_val, second_val)
      # print(boundaries)
      interval_targets = append(interval_targets, list(approximate(boundaries, fn)))
    }
  }
  return(interval_targets)
}



#########
### 3 ###
#########

f_1 <- function(x) {
  y = -x * (1-x)
  return(y)
}

f_2 <- function(x) {
  y = -x * sin(10 * pi * x)
  return(y)
}

# interp_fn <- function(boundaries, par) {
#   result <- c()
#   for (i in 1:length(boundaries)){
#     result[i] = c(par[1], par[2] * boundaries[i], (par[3] * boundaries[i]^2))
#   }
#   return(result)
# }


x <- seq(0, 1, by = 0.01)
plot(x,f_1(x), main="Prediction using piecewise - parabolic interpolator, n = 100", ylab = "y")

test <- approximate_cut(100, f_1)
scale <- 1/length(test)
for (i in 1:length(test)) {
  x <- seq((scale*i)- scale, scale*i, by = 0.01/length(test))
  yint <- sapply(x, interp_fn, par = test[[i]])
  lines(x,yint)
}

legend("bottomright", legend=c("f1(x)", "interpolator"), lty=c(NA,1),pch=c(1,NA))



################
### Martynas ###
################

interpolator <- function(a, x){
  X <- as.matrix(c(1, x, x^2), ncol = 1 )
  return(as.vector(a %*% X))
}

SSE <- function(x, a, method1, method2){
  real <- sapply(x, method1)
  pred <- sapply(x, method2, a = a)
  return(sum((real-pred)^2))
}

optimiser <- function(x, method){
  aInit <- c(0, 0, 0)
  res <- optim(aInit, SSE, x = x, method1 = method, method2 = interpolator)
  return(res$par)
}

aproximate <- function(n, method){
  scale <- 1/n
  midVal <- scale / 2
  result <- list()
  for (i in 1:n) {
    x <- c((scale*i)-scale, (scale * i) - midVal, scale * i)
    result <- append(result, list(optimiser(x, method)))
  }
  return(result)
}
# 
# x <- seq(0, 1, by = 0.01)
# plot(x,f_1(x), main="Prediction using piecewise - parabolic interpolator, n = 100", ylab = "y")
# 
# test <- aproximate(100, f_1)
# scale <- 1/length(test)
# for (i in 1:length(test)) {
#   x <- seq((scale*i)- scale, scale*i, by = 0.01/length(test))
#   yint <- sapply(x, interpolator, a = test[[i]])
#   lines(x,yint)
# }
# 
# legend("bottomright", legend=c("f1(x)", "interpolator"), lty=c(NA,1),pch=c(1,NA))