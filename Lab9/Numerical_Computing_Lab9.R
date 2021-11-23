#taylorCos <- function(x, n) {
#  return (((-1)^n * x^(2*n))/factorial(2*n))
#}

#taylorEPowX <- function(x, n) {
#  return ((x^n)/(factorial(n)))
#}

#multiplyFrac <- function(cosVec, eVec, nTerms) {
  
#  res <- c()
#  for(j in nTerms) {
#    for(i in nTerms) {
#      res[length(res) + 1] <- eVec[i+1] * cosVec[j+1]
#    }
#  }
  
#  return (sum(res))
#}

main <- function() {
  
  nTerms <- seq(0, 3)
  x <- as.numeric(readline(prompt="Please enter value of x"))
  
  # Calculate first n terms
  #cosVec <- c()
  #eVec <- c()
  #for(n in nTerms) {
  #  cosVec[length(cosVec) + 1] <- taylorCos(x, n)
  #  eVec[length(eVec) + 1] <- taylorEPowX(x, n)
  #}
  
  print(paste("Value of f(", x, ") = 10 + e^x cos(x) is:", (11 + x - ((x^3)/3) - ((x^4)/6))))
  
  # Calculate x's in range -5 and 5
  smallStep <- seq(-5, 5, by=(0.1))
  fx <- c()
  act <- c()
  
  for(x in smallStep) {
    #cosVec <- c()
    #eVec <- c()
    #for(n in nTerms) {
    #  cosVec[length(cosVec) + 1] <- taylorCos(x, n)
    #  eVec[length(eVec) + 1] <- taylorEPowX(x, n)
    #}
    fx[length(fx) + 1] <- 11 + x - ((x^3)/3) - ((x^4)/6) - ((x^5)/30)
    act[length(act) + 1] <- 10 + exp(1)^x * cos(x)
  }
  
  # Plot the graph
  plot(smallStep, fx, ylim=c(-30, 50), type="l", col="red", xlab="x values", ylab="f(x)")
  par(new=TRUE)
  plot(smallStep, act, ylim=c(-30, 50), type="l", col="orange", xlab="", ylab="", xaxt="n", yaxt="n")
  
  # Print AE and RE between small step and 10 + e^x * cos(x)
  
  print(paste("x","       Absolute error","        Relative error"))
  print("------------------------------------------------------")
  
  pointFive <- seq(-3.5, 3.5, by=(0.5))
  for(x in pointFive) {
    #cosVec <- c()
    #eVec <- c()
    #for(n in nTerms) {
    #  cosVec[length(cosVec) + 1] <- taylorCos(x, n)
    #  eVec[length(eVec) + 1] <- taylorEPowX(x, n)
    #}
    taylorResult <- (11 + x - ((x^3)/3) - ((x^4)/6))
    actualResult <- (10 + exp(1)^x * cos(x))
    
    ae <- abs(actualResult - taylorResult)
    re <- abs(ae/actualResult)*100
    print(sprintf("%2.1f          %4.5f               %4.5f", x, ae, re))
  }
}

main()

