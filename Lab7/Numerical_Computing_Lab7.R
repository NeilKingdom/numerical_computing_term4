a1 <- function(xVec, yVec, n) {
  
  numerator <- (n * sum(xVec * yVec) - sum(xVec) * sum(yVec))
  denominator <- (n * sum(xVec^2) - (sum(xVec))^2)
  return (numerator/denominator)
}

a0 <- function(a1, xVec, yVec, n) {
  return(sum(yVec)/n - a1 * (sum(xVec)/n)) 
}

main <- function() {
  
  # Part A
  
  print("Please enter the number of data pairs:")
  dPairs <- as.numeric(readline())
  
  xVec <- c()
  print("Enter the x-axis values:")
  for(i in 1:dPairs) 
    xVec[length(xVec) + 1] <- as.numeric(readline())
  
  yVec <- c()
  print("Enter the y-axis values:")
  for(i in 1:dPairs) 
    yVec[length(yVec) + 1] <- as.numeric(readline())
  
  a1 <- a1(xVec, yVec, dPairs)
  a0 <- a0(a1, xVec, yVec, dPairs)
  
  print("The best linear fit is of the form:")
  print(paste("y =", a1, "x +", a0))
  
  yFitted <- c()
  for(i in 1:dPairs) 
    yFitted[length(yFitted) + 1] <- (a0 + a1 * xVec[i])
  
  compare <- data.frame(
    no = c(1,2,3,4,5),
    x = xVec,
    yo = yVec,
    yf = yFitted
  )
  
  colnames(compare) <- list("No.", "x", "y(observed)", "y(fitted)")
  print(compare)
  
  # Part B
  
  Sr <- sum((yVec[1:dPairs] - (a0 + a1 * xVec[1:dPairs]))^2)
  print(paste("Sr =", Sr))
}

main()
