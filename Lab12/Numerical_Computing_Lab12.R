f <- function(x) {
  return (10 * x * exp(-x^2))
}

trapezoidal <- function(a, b, n) {
  
  h <- (b-a)/n
  trapSum <- 0
  
  for(i in 1:(n-1))
    trapSum <- trapSum + f(a + i*h)
  return(((b-a)/(2*n)) * (f(a) + 2 * trapSum + f(b)))
}

simpsons <- function(a, b, n) {
  
  if(n %% 2 != 0)
    return (NA)
  
  h <- (b-a)/n
  x0 <- a
  xn <- b
  
  oddSum <- 0
  evenSum <- 0
  
  i <- 1
  while(i <= n-1) {
      oddSum <- oddSum + f(x0 + i*h)
      i <- i + 2
  }
  
  i <- 2
  while(i <= n-2) {
      evenSum <- evenSum + f(x0 + i*h)
      i <- i + 2
  }
  
  return (((b-a)/(3*n)) * (f(x0) + 4 * oddSum + 2 * evenSum + f(xn)))
}

main <- function() {
  
  a <- 0
  b <- 2
  n <- 6
  actVal <- 4.91
  
  # AE and RE
  print("     Trapezoidal   Simpson's 1/3     Absolute Error    Relative Error")
  print("---------------------------------------------------------------------")
  for(i in 1:n) {
    trapVal <- trapezoidal(a, b, i)
    simpVal <- simpsons(a, b, i)
    ae <- abs(actVal - trapVal)
    re <- (ae/actVal) * 100
    print(sprintf("%1d    %2.4f        %2.4f            %2.4f            %2.4f", i, trapVal, simpVal, ae, re))
  }
}

main()