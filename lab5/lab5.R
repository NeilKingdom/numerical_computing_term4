fn <- function(x, xk, y, n) {
  
  res <- c()
  for(i in 1:n) 
    res[length(res) + 1] <- Li(x, xk, i, n)*(y[i])
  return (sum(res))
}

Li <- function(x, xk, i, n) {
  
  # Define possible values of j such that j != i
  jVec <- c()
  for(j in 1:n) 
    if(j != i) 
      jVec[length(jVec) + 1] <- j
  
  res <- c()
  for(k in 1:n-1) 
    res[length(res) + 1] <- (xk-x[jVec[k]])/(x[i]-x[jVec[k]])
  return (prod(res))
}

lagInter <- function() {
  
  # User input for xVec
  print("Enter vector for xVec (space separated)")
  xVec<- as.numeric((strsplit(readline(), " ")[[1]]))
  message(print(xVec))
  
  # User input for yVec
  print("Enter vector for yVec (space separated)")
  yVec<- as.numeric((strsplit(readline(), " ")[[1]]))
  message(print(yVec))
  
  # User input for independent variable xk. Solve for f(xk)
  print("Enter value to be interpolated from xVec (xk)")
  xk <- as.numeric(readline())
  
  # Validate that xk is in the range of xVec
  if(xk < min(xVec) || xk > max(xVec)) {
    message(print("Sorry, xk must be within the range of xVec"))
    quit()
  }
  
  # Don't waste time with lagrange's interpolation formula if yk is already defined
  if(xk %in% xVec) {
    index <- which(xVec == xk)[[1]]
    return (yVec[index])
  }
  
  # Avoid redundantly retrieving length multiple times
  n <- length(xVec)
  
  yk <- fn(xVec, xk, yVec, n)
  return (yk)
}

res <- lagInter()
res

