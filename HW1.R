x <- c(1,2,3,4,5)
x
y <- seq(from=10, to=80, by=10)
y
z <- seq(from=0, by=2, length=4)
z

x*y

x*z

a <- x[x<=3]
a

b <- x==4
b

p <- x[b]
p

k <- c(1,2,3,4,5,6,7,8)
k
dim(k) <- c(4,2)
k

m = c(2,4,6,8)
n = c(3,5,7,9)
mat = cbind(m,n)
mat

colnames(mat)

mat[3,]

mat[,2]

mat[3,2]




