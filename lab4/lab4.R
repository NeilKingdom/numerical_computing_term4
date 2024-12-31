# Lab 4

# --- Part A ---

vecSqr <- function(vec) {
  return(vec*vec)
}

# Tests  
vec.test <- c(1,2,3,4,5)
vecSqr(vec.test)

# --- Part B ---

first3let <- function(str){
  
  if(nchar(str) < 3) {
    return ("Length  of input string is less than 3")
  }
  
  return (substr(str, 1, 3))
}

# Tests
str <- "test"
str2 <- "ab"
first3let(str)
first3let(str2)

# --- Part C ---

oddOrEven <- function(vec) {
  
  ret <- c()
  for(x in vec) {
    if(is.na(x) || is.nan(x) || is.infinite(x))
      ret[length(ret) + 1] <- NA
    else if((x %% 2) == 0)
      ret[length(ret) + 1] <- T
    else
      ret[length(ret) + 1] <- F
  }
  
  return (ret)
}

# Tests
test <- c(1,2,NA,4,5,Inf,987,NaN, -Inf)
print(oddOrEven(test))

# --- Part D ---

factorial <- function(n) {
  
  if(n == 0 || n == 1)
    return (1)
  return (n * factorial(n-1))
}

# Tests
factorial(0)
factorial(1)
factorial(5)