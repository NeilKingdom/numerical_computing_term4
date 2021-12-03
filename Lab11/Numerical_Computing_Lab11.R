# Functions
eulersMethod <- function(h, t) {
  
  # Initial condition
  yi <- 1.241
  
  # Calculate eulers given dx/dy = -ycos(t)
  yVec <- c()
  # Set yVec[1] to be Eulers at y0
  yVec[length(yVec) + 1] <- yi
  
  for(t in t) {
    fxy <- -yi * cos(t)
    yVec[length(yVec) + 1] <- yi + (fxy * h)
    yi <- yVec[length(yVec)]
  }
  
  return (yVec)
}

main <- function() {
  
  t.point5 <- seq(0, 6, 0.5)
  t.point25 <- seq(0, 6, 0.25)
  t.point1 <- seq(0, 6, 0.1)
  
  eulers1 <- eulersMethod(0.5, t.point5)
  eulers2 <- eulersMethod(0.25, t.point25)
  eulers3 <- eulersMethod(0.1, t.point1)
  
  # Actual ODE (Step of 0.5)
  ODE <- c()
  for(t in t.point5) 
    ODE[length(ODE) + 1] <- 0.5 * exp(sin(2)) * exp(-sin(t))
  
  # Actual and Relative Errors
  print(paste("x","       Absolute error","           Relative error"))
  print("------------------------------------------------------")
  for(i in 1:length(ODE)) {
    ae <- abs(ODE[i] - eulers1[i])
    re <- (ae/ODE[i]) * 100
    print(paste(t.point5[i], "     ", ae, "       ", re))
  }
  
  # Actual ODE (Step of 0.1)
  ODE <- c()
  for(t in t.point1) 
    ODE[length(ODE) + 1] <- 0.5 * exp(sin(2)) * exp(-sin(t))
  
  # Plot the graph
  plot(t.point1, ODE, xlim=c(0, 6), ylim=c(0, 4), type="l", col="red", xlab="Time", ylab="Displacement", main="Motion of a Mass Over Time")
  par(new=TRUE)
  plot(t.point5, eulers1[1:length(eulers1)-1], xlim=c(0, 6), ylim=c(0, 4), type="l", col="blue", xlab="", ylab="", xaxt="n", yaxt="n")
  par(new=TRUE)
  plot(t.point25, eulers2[1:length(eulers2)-1], xlim=c(0, 6), ylim=c(0, 4), type="l", col="orange", xlab="", ylab="", xaxt="n", yaxt="n")
  par(new=TRUE)
  plot(t.point1, eulers3[1:length(eulers3)-1], xlim=c(0, 6), ylim=c(0, 4), type="l", col="green", xlab="", ylab="", xaxt="n", yaxt="n")
}

main()

