# Library Files
library("openxlsx")

# Functions
firstDev <- function(xVec, yVec) {
  
  n <- length(xVec)
  first.dev <- c()
  
  # Range must be +1 from start and -1 from end for CDD
  for(i in 2:(n-1)) {
    first.dev[length(first.dev) + 1] <- ((yVec[(i+1)] - yVec[(i-1)])/(xVec[(i+1)] - xVec[(i-1)]))
  }
  return (first.dev)
}

secondDev <- function(xVec, yVec) {
  
  n <- length(xVec)
  second.dev <- c()
  
  # Range must be +1 from start and -1 from end for second derivative
  for(i in 2:(n-1)) {
    second.dev[length(second.dev) + 1] <- ((yVec[(i+1)] - 2*yVec[i] + yVec[(i-1)])/((xVec[(i+1)] - xVec[i])^2))
  }
  return (second.dev)
}

setwd("/home/neil/devel/school/2021_Fall/Numerical_Computing_Term4/Lab10/")
  
# Import CSV File
print("Please enter the name of the file to open:")
file <- readline()
rocket.data <- read.xlsx(file)
names(rocket.data) <- c("seconds", "distance")
  
xVec <- as.vector(rocket.data$seconds)
yVec <- as.vector(rocket.data$distance)
  
# Plot data
plot(xVec, yVec, xlim=c(0, 450), ylim=c(0, 200), type="l", col="red", xlab="Time (s)", ylab="Distance (km)", main="Distance Travelled by Rocket")
  
# Calc Velocity
newTime <- xVec[2:(length(xVec)-1)]
velocity <- firstDev(xVec, yVec)*1000
plot(newTime, velocity, xlim=c(0, 450), type="l", col="blue", xlab="Time (s)", ylab="Velocity (km)", main="Velocity of Rocket")
  
# Calc Acceleration
acceleration <- secondDev(xVec, yVec)*1000
plot(newTime, acceleration, xlim=c(0, 450), type="l", col="green", xlab="Time (s^2)", ylab="Acceleration (km)", main="Acceleration of Rocket")
  
  
