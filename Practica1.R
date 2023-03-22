# Ctrl + Shift + Enter
rm(list=ls())

print("Lanzamiento de una moneda")
x <- sample(x=c("c","+"), size=10, replace=TRUE,prob=c(1/3,2/3))
sum(x=="c")
### 1 == heads
### 2 == tails

x <- sample(x=1:0, size=10**4, replace=TRUE, prob=c(1/3,2/3))
sum(x)/length(x)
mean(x)
err <- 1
paso <- 1000
x0 <- mean(x)
while (err > 10**-4) {
  xx  <- sample(x=1:0, size=10**4, replace=TRUE, prob=c(1/3,2/3))
  x <- c(x,xx)
  print(x1 <- mean(x))
  
  err <- abs(x1-x0)
  x0 <- x1
  
}
## Usando runif()
u <- runif(10)
x <- u < 1/3
mean(x)
mean(runif(1000)< 1/3)
n <- seq(from=1000,to=100000,by=1000)
ll <- function(n=100, prob=0.5) return( mean( runif(n) < prob) )# < prob --> cara

prob <- sapply( n, ll, p=0.5)
# e usando agora a función plot temos
plot(n, prob, ylab=c(0,1),type="l",ylim=c(0.45,0.55), col='grey') # Axustar a ollo os límites do eixo
points(n, sapply(n,ll,p=0.5), type="l", col='grey')
# repito isto 18 veces máis
d <- replicate( 2, points( n, sapply(n,ll,p=0.5), type="l", col='grey'))
abline(h=0.5, col="red",lwd=2)





print("Simulación del funcionamiento de un circuito")
set.seed(100)
#x1 <- sample(x=1:0, size=200, replace=TRUE, prob=c(0.6,0.4))
x1 <- runif(200)<=0.6
x2 <- runif(200)<0.8
#x2 <- sample(x=1:0, size=200, replace=TRUE, prob=c(0.8,0.2))
#z <- x1+x2
#z[z == 2] <- 1
#mean(z)
mean(runif(200)<=0.6|runif(200)<0.8)
print("El valor teórico es 0.92")
print("Circuito más complejo")
x1 <- sample(x=1:0, size=200, replace=TRUE, prob=c(0.8,0.2))
x2 <- sample(x=1:0, size=200, replace=TRUE, prob=c(0.9,0.1))
x3 <- sample(x=1:0, size=200, replace=TRUE, prob=c(0.6,0.4))
x4 <- sample(x=1:0, size=200, replace=TRUE, prob=c(0.5,0.5))
x5 <- sample(x=1:0, size=200, replace=TRUE, prob=c(0.7,0.3))
z <- (x1+x2+x3+x4)*x5
z[z > 0] <- 1
mean(z)





print("Concurso de triples da NBA (2018)")
lb <- c(35.6 , 37.6 , 39.6 , 37.6 , 35.6 )/100
ch <- c(36.4 , 40.0 , 40.5 , 40.0 , 36.4 )/100
db <- c(32.9 , 35.7 , 38.5 , 35.7 , 32.9 )/100

triple <-  function(sdgfewdsfsdfeswf=0) {
tt <- function(prob=0.5) return( sample(x=1:0, size=5, replace=TRUE, prob=c(prob,1-prob)) )
x1 <- sapply(lb,tt)
x1[5,] <- x1[5,]*2
x2 <- sapply(ch,tt)
x2[5,] <- x2[5,]*2
x3 <- sapply(db,tt)
x3[5,] <- x3[5,]*2
if (  sum(max(c(sum(x1),sum(x2),sum(x3))) == c(sum(x1),sum(x2),sum(x3))) > 1 ) {
  return (triple())  
  
}

return ( which.max(c(sum(x1),sum(x2),sum(x3))) )

}
x <- sapply(X=1:100000,FUN=triple)
print(paste("Larry Bird = " , mean(x==1)))
print(paste("Craig Hodges = " , mean(x==2)))
print(paste("Devin Booker = " , mean(x==3)))







