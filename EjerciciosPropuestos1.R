rm(list = ls())
print("Ejercicios de vectores")
"Ejercicio 1"
sum(seq(4, 400, by = 4))

"Ejercicio 2"
rep(0:9, each = 100)

"Ejercicio 3"
x <- sample(1:1000, size = 100)
which(x %% 3 == 0)
x[which(x %% 3 == 0)]

"Ejercicio 4"
x <- 1:10
y <- 26:35
z <- 100:91
"a)"
x + y + z
"b)"
x * y * z
"c)"
x %*% z
sum(x * z)

"Ejercicio 5"
x <- sample(c("cara", "cruz"), size = 100, replace = TRUE)
paste("Número de caras = ", toString(sum(x == "cara")))
paste("Número de cruces = ", toString(sum(x == "cruz")))
paste("Porcentaje de caras = ", toString(sum(x == "cara") / length(x)))
##########################################################
print("Ejercicios de matrices y arrays")
"Ejercicio 1"
matrix(0:9, nrow = 100, ncol = 10, byrow = TRUE)

"Ejercicio 2"
"a)"
sample(c(1:6), size = 100, replace = TRUE)
"b)"
replicate(2, sample(c(1:6), size = 100, replace = TRUE))
"c)"
x <- replicate(5, sample(c(1:6), size = 100, replace = TRUE))
"d)"
colnames(x) <- c(paste("tirada-", 1:5, sep = ""))
"e)"
colSums(x < 3) / nrow(x)

"Ejercicio 3"
"a)"
sum(x[, 2] == 6)
which(x[, 2] == 6)
"b)"
x[20:25, c(2, 4)]
"c)"
x <- x[-(100:91), -2] # delete column 2
"d)"
x[x[, 1] > 4, ]

"Ejercicio 4"
A <- c(1, 1, 1)
B <- c(1, 2, 3)
C <- c(-1, 2, -1)
O <- c(-1, -1, -1)
sum(colSums(rbind(A, -O))**2)**(1 / 2)
dist(rbind(A, O))
sum(colSums(rbind(B, -O))**2)**(1 / 2)
dist(rbind(B, O))
sum(colSums(rbind(C, -O))**2)**(1 / 2)
dist(rbind(C, O))

##########################################################
print("Ejercicios de data.frames y listas")
print("Ejercicio 1")
print("a)")
library(datasets)
class(iris)
str(iris)
summary(iris)
sum(iris["Species"] == "setosa")
sum(iris["Species"] == "versicolor")
sum(iris["Species"] == "virginica")
print("b)")
setosa <- iris[iris["Species"] == "setosa", -5]
versicolor <- iris[iris["Species"] == "versicolor", -5]
virginica <- iris[iris["Species"] == "virginica", -5]
print("c)")
matrix(iris[5, "Petal.Width"])
c(iris[5, "Petal.Width"])
print("d)")
mean(iris[
  (iris["Species"] == "versicolor") & (iris["Sepal.Length"] > 6.3),
  "Petal.Length"
])
print("e)")
log(iris["Petal.Length"])
print("f)")
colMeans(iris[, -5])
print("g)")
iris[order(iris[, "Petal.Length"]), ]
print("h)")
iris[sample(nrow(iris), 10), ]
print("Ejercicio 1")
l <- list(
  Species = sample(iris$Species, size = 20),
  iris = iris,
  m = matrix(1:100, ncol = 4)
)
print("a)")
lapply(l, summary)
print("b)")
l <- append(l, "Hello world")
print("c)")
ll <- sample(l, 2, replace = FALSE)
print("d)")
tail(ll[[2]][length(ll[[2]])], n = 1)

##########################################################
print("Ejercicios de funciones")
print("Ejercicio 1")
Fun.Hello <- function(idioma) {
  if (idioma == "en") {
    return("Hello world")
  }
  if (idioma == "es") {
    return("Hola mundo")
  }
  if (idioma == "gl") {
    return("Ola mundo")
  }
}
Fun.Hello(idioma = "en")
Fun.Hello(idioma = "es")
Fun.Hello(idioma = "gl")
print("Ejercicio 2")
sol <- function(a, b, c) {
  x1 <- (-b + (b**2 - 4 * a * c)**(1 / 2)) / (2 * a)
  x2 <- (-b - (b**2 - 4 * a * c)**(1 / 2)) / (2 * a)
  return(cbind(x1, x2))
}
sol(a = 1, b = c(-3, 4), c = c(2, 4))
print("Ejercicio 3")
lanzamiento <- function(p = 0.5, n = 100) {
  return(mean(runif(n) < p))
}
print("a)")
lanzamiento(n = 100)
lanzamiento(n = 1000)
lanzamiento(n = 5000)
print("b)")
v.lanzamiento <- function(p = 0.5, n = 100) {
  return(Vectorize(FUN = lanzamiento)(p, n))
}
v.lanzamiento(n = c(100, 1000, 5000))
print("c)")
help(plot)
n <- c(100, seq(1000, 100000, 1000))
plot(n, v.lanzamiento(n = n), type = "p", cex = 1, pch = 21, bg = 'black')
replicate(
  1,
  points(n, v.lanzamiento(n = n), type = "p", cex = 1, pch = 21, bg = 'black')
)
abline(a = 0.5, b = 0, col = "red", lwd = 3)

##########################################################
print("Ejercicios de Estructura condicionales y bucles")
print("Ejercicio 1")
closestNumber <- function(n) {
  x1 <- trunc(n)
  if (n - x1 >= 0.5) {
    x1 <- x1 + 1
  }
  return(x1)
}
closestNumber(1.5)
closestNumber(1.2)
print("Ejercicio 2")
closestNumber2 <- function(n) {
  if (n - trunc(n) >= 0.5) {
    return(trunc(n + 1))
  } else {
    return(trunc(n))
  }
}
closestNumber2(1.5)
closestNumber2(1.2)
print("Ejercicio 3")
a <- c(1)
for (i in 2:10) {
  if (i %% 2 == 0) {
    a <- append(a, log((a[[i - 1]] + 2)))
  } else {
    a <- append(a, exp(a[[i - 1]] + 2))
  }
}
sum(a)

##########################################################
print("Ejercicios de Estructura condicionales y bucles")
print("Ejercicio 1")
wine.data <- read.table("Archivos/wine.data", sep = ",", dec = ".")
colnames(wine.data) <- c(
  "class",
  "Alcohol",
  "Malic acid",
  "Ash",
  "Alcalinity of ash",
  "Magnesium",
  "Total phenols",
  "Flavanoids",
  "Nonflavanoid phenols",
  "Proanthocyanins",
  "Color intensity",
  "Hue",
  "OD280/OD315 of diluted wines",
  "Proline"
)
wine.data[1:6, c(1:3, ncol(wine.data) - 1, ncol(wine.data))]
print("Ejercicio 2")
print("a)")
spambase.data <- read.table("Archivos/spambase.data", sep = ",", dec = ".")
class(spambase.data)
print("b)")
spambase.names <- read.table("Archivos/spambase.names", comment.char = "|")
colnames(spambase.data) <- c(spambase.names[-1, 1], "class")
print("c)")
mean(spambase.data[, ncol(spambase.data)])
spambase.data[, ncol(spambase.data)] <- factor(
  spambase.data[, ncol(spambase.data)],
  labels = c("spam", "non-spam"),
  levels = 1:0
)
mean(spambase.data[, ncol(spambase.data)] == "spam")
