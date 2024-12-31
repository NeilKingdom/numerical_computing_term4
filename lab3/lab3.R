vect.1 = c(0,1,2,3)

vect.2 <- c("Hello", "World")

c(TRUE, 1) -> vect.3

print(vect.1)
cat("Vect.1 is ",vect.1, "\n")
cat("Vect.2 is ",vect.2, "\n")
cat("Vect.3 is ",vect.3, "\n")

cat(class(vect.1), class(vect.2), class(vect.3), "\n")
ls(all.names = TRUE)

mat1 <- matrix(c(1,2,3,4,5,6,7,8,9), nrow = 3)
mat2 <- matrix(1:9, ncol = 3)
vect1 <- 1:9
mat3 <- matrix(vect1, nrow = 3)

mat1 + mat2
# The product of linear algebra matrix multiplication
mat1 %*% mat2
# Element by element multiplication
mat1 * mat2
# Find transpose of a matrix (switch cols and rows)
t(mat1)

mat1[1,3]
mat1[2,]
mat2[,-1]
mat3[-1,]
mat1[-1,-2]
mat1[1,2] <- 15

# Demo a)
mat1[1,] <- 15
mat2[,1] <- 6
mat2[,2] <- 6

mat1
mat2

#  
mat1 <- matrix(c(1,2,3,4,5,6,7,8,9), nrow = 3)
mat1[mat1 < 6]
mat1[mat1 >= 6]
mat1 > 4

#
t <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
u <- t[c(2,3,6)]
print(u)

v <- t[c(T,F,F,F,F,T,F)]
print(v)

x <- t[c(-2,-5)]
print(x)

y <- t[c(0,0,0,0,0,0,1)]
print(y)

v1 <- c(3,8,4,5,0,11)
v2 <- c(4,11,0,8,1,2)

add.result <- v1 + v2
print(add.result)

sub.result <- v1-v2
print(sub.result)

multi.result <- v1*v2
print(multi.result)

divi.result <- v1/v2
print(divi.result)

v <- c(13,8,41,2,0,11,-9)

sort.result <- sort(v)
print(sort.result)

revsort.result <- sort(v, decreasing = T)
print(revsort.result)

v <- c("green", "blue", "yellow", "violet", "green")
sort.result <- sort(v)
print(sort.result)

revsort.result <- sort(v, decreasing = T)
print(revsort.result)

# 
listA <- list(c("Jan", "Feb", "Mar"), matrix(c(3,9,5,1,-2,8), nrow = 2), list("green", 12.3))

names(listA) <- c("1st Quarter", "A_Matrix", "An_Inner_List")

print(listA)
print(listA[1])
print(listA[3])
print(listA$An_Inner_List)





# Lab 3
vec1 <- seq(0.1:1,by=0.1)
vec1
length(vec1)

row.names <- c("row1","row2","row3","row4","row5")
col.names <- c("col1","col2")
dnames <- list(row.names, col.names)
mat1 <- matrix(vec1, nrow=5, ncol=2, dimnames=dnames)
print(mat1)

mat1[3,2] <- 10
mat1[3,]
mat1[,2]

examDF <- data.frame (
  
  name = c("Anastasia", "Dima", "Katherine", "James", "Emily", "Micheal", "Matthew", "Laura", "Kevin", "Jonas"),
  score = c(12.5, 9.0, 16.5, 12.0, 9.0, 20.0, 14.5, 13.5, 8.0, 19.0),
  attempts = c(1, 3, 2, 3, 2, 3, 1, 1, 2, 1),
  qualify = c("yes", "no", "yes", "no", "no", "yes", "yes", "no", "no", "yes")
)
examDF

examRes <- subset(examDF, TRUE, select = c(-1,-2))
examRes

examQual <- subset(examDF, c(T,F,T,F,F,T,T,F,F,T))
examQual

test.ftemps <- c(45,77,19,101,120)
for(f in test.ftemps) {
  cat(f, "to celsius =", 5/9 * (f-32), "\n")
}
