#HW 3

#1
y <- (c(105, 94, 108, 101, 100, 96, 119, 103, 107, 110, 112, 106, 120, 114, 121, 116, 128, 115, 118, 123))
 
x <- matrix(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
              1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
              0,0,0,0,0,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,
              0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1), 
              nrow=20, ncol=4)

names(x)[1] <- "mu"
names(x)[2] <- "A"
names(x)[3] <- "B"
names(x)[4] <- "C"


#a
design.a <- t(x) %*% x
xprimey <- t(x) %*% y


#b
x.b <- matrix(c(1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                0,0,0,0,0,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,
                0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1), 
                nrow=20, ncol=3)

names(x.b)[1] <- "A"
names(x.b)[2] <- "B"
names(x.b)[3] <- "C"

design.b <- t(x.b) %*% x.b
x.bprimey <- t(x.b) %*% y
beta.hat.b <- solve(design.b) %*% x.bprimey
names(beta.hat.b)[1] <- "Part b"

#c
x.c <- matrix(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                0,0,0,0,0,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0), 
                nrow=20, ncol=3)

names(x.c)[1] <- "mu"
names(x.c)[2] <- "A"
names(x.c)[3] <- "B"

design.c <- t(x.c) %*% x.c
x.cprimey <- t(x.c) %*% y
beta.hat.c <- solve(design.c) %*% x.cprimey
names(beta.hat.c)[1] <- "Part c"

#d
x.d <- matrix(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
              1,1,1,1,1,0,0,0,0,0,0,0,-1,-1,-1,-1,-1,-1,-1,-1,
              0,0,0,0,0,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1), 
              nrow=20, ncol=3)

names(x.d)[1] <- "mu"
names(x.d)[2] <- "A"
names(x.d)[3] <- "B"

design.d <- t(x.d) %*% x.d
x.dprimey <- t(x.d) %*% y
beta.hat.d <- solve(design.d) %*% x.dprimey
names(beta.hat.d)[1] <- "Part d"
