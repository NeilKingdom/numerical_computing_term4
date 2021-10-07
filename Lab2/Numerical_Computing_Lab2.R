my.name <- readline(prompt="Enter name: ")
my.age <- readline(prompt="Enter age: ")
# Convert char into int
my.age <- as.integer(my.age)
print(paste("Hi,", my.name, ", next year you will be", my.age+1, "years old."))

v1 <- c(1,3,5)
v2 <- c(7,9,11)
v3 <- c(13,15,17)

m <- cbind(v1,v2,v3)
print(m)

students <- data.frame(
  Name = c("Michael A", "Jennifer R", "Sara B", "James H"),
  Gender = c("M", "F", "F", "M"),
  Age = c(18, 19, 20, 22),
  Designation = c("CET Student", "CP Student", "SSN Student", "CS Student"),
  NoCourses = c(5, 4, "<NA>", 3)
)

print(students)