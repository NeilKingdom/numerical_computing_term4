# Step 1

library(ggplot2)
data(mpg)
head(mpg)
summary(mpg)

# Step 2

myMean <- function(vec) {
  
  n <- length(vec)
  return (sum(c(vec[1:n]))/n)
}

myMedian <- function(vec) {
  
  n <- length(vec)
  
  vec <- sort(vec, decreasing = F)
  if((n %% 2) == 0) {
    return ((vec[(n/2)] + vec[((n/2) +1)])/2)
  }
  else 
    return (vec[ceiling(n/2)])
}

myMode <- function(vec) {
  
  # Early return if vector only contains 1 value
  if(length(vec) == 1)
    return (vec[1])
  
  vec <- sort(vec, decreasing = F)
  elements <- c()
  ocurrences <- c()
  for(x in vec) {
    if(!(x %in% elements)) {
      elements[length(elements) + 1] <- x
      ocurrences[length(ocurrences) + 1] <- 1
    }
    else {
      index <- which(elements == x)[[1]]
      ocurrences[index] <- ocurrences[index] + 1
    }
  }
  
  # Test if all elements occurred the same number of times
  i <- 2
  while(ocurrences[i] == ocurrences[(i-1)]) {
    i <- i + 1
    if(i == (length(ocurrences) + 1)) return (NA)
  }
  
  # Find mode(s)
  i <- 1
  greatest <- max(ocurrences)
  modes <- c()
  for(i in 1:length(ocurrences)) {
    if(ocurrences[i] == greatest)
      modes[length(modes) + 1] <- elements[i]
    i <- i + 1
  }
  return (modes)
}

myStaDev <- function(mean, vec, type) {
  
  n <- length(vec)
  
  # o = SD of population
  if(type == 'o')
    return (sqrt(sum(c(((vec[1:n] - mean) ** 2)/n))))
  # s = SD of sample
  if(type == 's')
    return (sqrt(sum(c(((vec[1:n] - mean) ** 2)/(n-1)))))
}

myVar <- function(sd) {
  
  return (sd ** 2)
}

# Step 3

set.seed(142)
myNormalData <- rnorm(10000, mean = 50, sd = 8)
hist(myNormalData, breaks = 80)
actualMean <- mean(myNormalData)
actualSD <- sd(myNormalData)

calcMean <- myMean(myNormalData)
calcSD <- myStaDev(calcMean, myNormalData, 's')

# Find absolute and relative errors
#actualMean
#calcMean
#actualSD
#calcSD

# Step 4 - A

myCtyMean <- myMean(mpg$cty)
paste("My mpg$cty mean:", myCtyMean)
paste("Built in mpg$cty mean:", mean(mpg$cty))
paste("My mpg$cty median:", myMedian(mpg$cty))
paste("Built in mpg$cty median:", median(mpg$cty))
paste("My mpg$cty mode:", myMode(mpg$cty))
paste("My mpg$cty SD:", myStaDev(myCtyMean, mpg$cty, 's'))
paste("Built in mpg$cty SD:", sd(mpg$cty))

# Step 4 - B 

BMI <- data.frame(
  
  gender = c("Male","Male", "Female", "Male", "Female", "Female"),
  height = c(81, 93, 78, 100, 92, 75),
  weight = c(152, 171.5, 165, 140, 192.1, 180.2),
  Age = c(42, 38, 26, 52, 18, 23)
)

print(BMI)

# mean and SD of height and weight
height.mean <- myMean(BMI$height)
weight.mean <- myMean(BMI$weight)
age.mean <- myMean(BMI$Age)
height.SD <- myStaDev(height.mean, BMI$height, 's')
weight.SD <- myStaDev(weight.mean, BMI$weight, 's')
age.SD <- myStaDev(age.mean, BMI$Age, 's')
paste("My BMI$height mean:", height.mean)
paste("My BMI$weight mean:", weight.mean)
paste("My BMI$height SD:", height.SD)
paste("My BMI$weight SD:", weight.SD)

# Prob. that height is < 85
zscore <- (85 - height.mean)/height.SD

prob.LT85 <- pnorm(85, mean = height.mean, sd = height.SD, lower.tail = T, log.p = F)
prob.LT85 <- prob.LT85*100
prob.LT85

# Prob. that weight > 166
zscore <- (166 - weight.mean)/weight.SD

prob.GT166 <- pnorm(166, mean = weight.mean, sd = weight.SD, lower.tail = T, log.p = F)
prob.GT166 <- 100 - (prob.GT166*100)
prob.GT166

# Prob. that 35 < age < 45
zscore35 <- (35 - age.mean)/age.SD
zscore45 <- (45 - age.mean)/age.SD

prob.GT35LT45 <- pnorm(45, mean = age.mean, sd = age.SD, lower.tail = T, log.p = F) - pnorm(35, mean = age.mean, sd = age.SD, lower.tail = T, log.p = F)
prob.GT35LT45 <- prob.GT35LT45*100
prob.GT35LT45

