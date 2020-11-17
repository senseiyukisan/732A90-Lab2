#########
### 1 ###
#########

# Functional
min_se <- function(boundaries, par, target_fn){
  
  
  interp_fn <- function(boundaries, par) {
    result <- c()
    for (i in 1:length(boundaries)){
      result = c(boundaries[i] * par[1], par[2] * boundaries[i], (par[3] * boundaries[i]^2))
    }
    return(result)
  }
  print(boundaries)
  sse = sum((target_fn(boundaries) - interp_fn(boundaries, par))^2)
  return(sse)
}


approximate <- function(boundaries, target_fn){
  optimized <- optim(par = c(0.1,0.1,0.1), fn = min_se, target_fn=target_fn, boundaries = boundaries)
  return(optimized$par)
}


# Martynas

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
  # cat("res$par: ", res$par, "\n", "typeof(res$par): ", typeof(res$par), "\n")
  return(res$par)
}



#########
### 2 ###
#########


approximate_cut <- function(n, fn){
  interval <- seq(0, 1, length.out = n)
  interval_targets = c()
  for (i in 1:length(interval)) {
    first_val = interval[i]
    if (first_val == 1){stop}
    else{
      second_val = interval[i+1]
      mid_val = interval[i] + ((second_val - first_val) / 2)
      boundaries = c(first_val, mid_val, second_val)
      interval_targets = append(interval_targets, approximate(boundaries, fn))
    }
  }
  return(interval_targets)
}


# Martynas

aproximate <- function(n, method){
  scale <- 1/n
  # cat("Scale: ", scale, "\n")
  midVal <- scale / 2
  # cat("MidVal: ", midVal, "\n")
  result <- list()
  for (i in 1:n) {
    x <- c((scale*i)-scale, (scale * i) - midVal, scale * i)
    # cat("x: ", x, "\n")
    result <- append(result, list(optimiser(x, method)))
  }
  return(result)
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