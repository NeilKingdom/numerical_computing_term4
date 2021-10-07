# Assignment 1
# Name: Neil Kingdom
# Student Number: 040967309
# Professor: Abdullah Kadri
# Due Date: Oct 4, 2021

# ----- Part 1 -----

# Import CSV File

# !Important! -- set path to your own working directory --
setwd("/home/neil/devel/school/2021_Fall/num_comp/Assignment1")
myCarsList <- read.csv("assignment1.csv", sep=",")
head(myCarsList)
# invisible() suppresses output
myCarsList <- invisible(myCarsList[-1,])
nrow(myCarsList)
ncol(myCarsList)

# Remove commas (Seems to only be an issue in my environment)
invisible(gsub(",", "", myCarsList))

# Type cast columns
myCarsList$MPG <- as.numeric(myCarsList$MPG)
myCarsList$Displacement <- as.numeric(myCarsList$Displacement)
myCarsList$Horsepower <- as.numeric(myCarsList$Horsepower)
myCarsList$Weight <- as.numeric(myCarsList$Weight)
myCarsList$Acceleration <- as.double(myCarsList$Acceleration)
myCarsList$Origin <- as.factor(myCarsList$Origin)

# Find Car(s) with: max MPG, Horsepower > 100, Acceleration < 15
maxMPG <- myCarsList[myCarsList$MPG==max(myCarsList$MPG),1]
paste("Maximum MPG:", maxMPG)
hp.gtHundred <- myCarsList[(myCarsList$Horsepower > 100),]
acc.ltFifteen <- myCarsList[(myCarsList$Acceleration < 15),]

# Output dataframes as CSV
write.csv(hp.gtHundred, paste(getwd(),"Horsepower.csv", sep = "/"))
write.csv(acc.ltFifteen, paste(getwd(),"Acceleration.csv", sep = "/"))

# Plot Histogram
# This will ignore zero values in MPG column
secondSmallest <- sort(unique(myCarsList$MPG),decreasing = FALSE)[2]
hist(myCarsList$MPG, main = "MPG Histogram", xlab = "MPG", col = "Red", breaks = 10, xlim = c(secondSmallest,max(myCarsList$MPG)))

# ----- Part 2 -----

# Set RNG seed to constant num 75
set.seed(75)
#aVec <- runif(50, min = 0, max = 500)
#bVec <- runif(50, min = 0, max = 500)
aVec <- sample(0:500, 50)
bVec <- sample(0:500, 50)
aVec
bVec
# Same length for aVec and bVec
n <- length(aVec)
cVec <- matrix(bVec[2:n] - aVec[1:n-1], nrow = 7, ncol = 7)
cVec

cVec <- c(cos(bVec[1:n-1])/sin(aVec[2:n]))
cVec

i <- 1
a <- aVec
n <- length(a)
res <- sum(c(exp(-a[(i+1):n])/(a[i:n-1] + 5)))
res

# ----- Part 3 -----

# Notes: This program assumes 32-bit IEEE-754 floating bit precision
# Since there are 23 bits for value, the epsilon is 2^23 = 8388608
# The assumption is that the user respects this range, and entering
# a decimal value > epsilon will result in undefined behaviour 

# Convert left half of decimal to binary
leftDecToBin <- function(dec) {
  
  rem <- 0
  bin <- c()
  
  while(dec != 0) {
    rem <- dec %% 2
    if(rem > 0) 
      bin[length(bin) + 1] <- 1
    else
      bin[length(bin) + 1] <- 0
    dec <- dec %/% 2
  }
  return (rev(bin))
}

# Convert right half of decimal to binary
rightDecToBin <- function(dec) {
  
  rem <- 0
  bin <- c()
  
  while(dec != 0) {
    rem <- (dec * 2) %% 2
    if((rem - (dec * 2) %% 1) > 0) 
      bin[length(bin) + 1] <- 1
    else
      bin[length(bin) + 1] <- 0
    dec <- (dec * 2) %% 1
  }
  return (bin)
}

main <- function() {

  print("Enter a decimal number to be converted to a IEEE-754 Floating Point Precision number: ")
  input <- as.numeric(readline())
  leftHalf <- floor(abs(input))
  rightHalf <- abs(input - leftHalf)
  
  leftHalf <- leftDecToBin(leftHalf)
  rightHalf <- rightDecToBin(rightHalf)
  
  # Calculate how many decimal places need to be moved
  delim <- -1
  bin <- c(leftHalf, delim, rightHalf)
  indexOfFirst <- which(bin %in% 1)[1]
  indexOfDelim <- which(bin %in% delim)
  posMoved <- indexOfDelim - indexOfFirst
  # Fix left shift error if leftHalf is 0
  if(!is.null(leftHalf))
    posMoved <- posMoved - 1
  
  # Sign bit
  sign <- 0
  if(input < 0)
    sign <- 1
  
  message(print("Sign (1-bit)", quote = FALSE))
  message(print(sign))
  
  # Exponent
  exp <- leftDecToBin(127 + posMoved)
  # Fix left shift error if leftHalf is 0
  if(is.null(leftHalf))
    exp <- c(0, exp)
  
  message(print("Exponent (8-bits)", quote = FALSE))
  message(print(exp))
  
  # Remove delimiter
  bin <- bin[!bin %in% delim]
  
  # Mantissa
  mantissa <- c()
  for(i in indexOfFirst:23)
    # Fix left shift error if leftHalf is 0
    if(!is.null(leftHalf))
      mantissa[length(mantissa) + 1] <- bin[i+1]
    else
      mantissa[length(mantissa) + 1] <- bin[i]
  
  mantissa[is.na(mantissa)] <- 0
  message(print("Mantissa (23-bits)", quote = FALSE))
  message(print(mantissa))
} 

main()


