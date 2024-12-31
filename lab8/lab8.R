myTaylor <- function(x, c, nTerms) {

  # Vector of taylor terms
  tVec <- c()
  for(n in 1:nTerms) {
      tVec[length(tVec) + 1] <- (((-1)^(n-1)/n) * (x-c)^n)
  }
  return (sum(tVec))
}

main <- function() {
  
  x <- as.numeric(readline(prompt="Please enter the value of x:"))
  nTerms <- seq(1, 10)
  
  # Print first 10 terms of Taylor Series
  
  print(paste("Term","ln(x)","                   Absolute error","        Relative error"))
  print("----------------------------------------------------------------------")
  
  for(n in nTerms) {
    taylorResult <- myTaylor(x, 1, n)
    ae <- abs((log(x) - taylorResult))
    re <- (ae/log(x))*100
    print(sprintf("%2i    %4.5f                     %4.5f            %4.5f", n, (((-1)^(n-1)/n) * (x-1)^n), ae, re))
  }
  
  nTerms <- seq(1,100)
  logTerms <- c()
  tayTerms <- c()
  
  for(n in nTerms) {
    logTerms[length(logTerms) + 1] <- log(x)
    tayTerms[length(tayTerms) + 1] <- myTaylor(x, 1, n)
  }
  
  plot(nTerms, logTerms, ylim=c(0.5, 1), type="l", col="blue", xlab="Number of Terms", ylab="ln(x) vs MacLaurin")
  par(new=TRUE)
  plot(nTerms, tayTerms, ylim=c(0.5, 1), type="l", col="red", xlab="", ylab="", xaxt="n", yaxt="n")
}

main()

