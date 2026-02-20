# Load packages
if (!require(carData)) {
  install.packages("carData")
  library(carData)
}

x <- exp(100)
x
mi.funcion <- function(x = 1, y = 2, z = 3, ...) {
  return(sum(c(x, y, z, ...)))
}
mi.funcion()
x

mean(1:10)

mean <- function(x) {
  x <- c(x, 1)
  return(list(media = sum(x) / length(x), x = x))
}

mean(1:10)
base::mean(1:10)

an <- function(a1, r, n) {
  a1 * r^(n - 1)
}

an(1, 0.5, 10)
an(a1 = 1, r = 5, n = c(5, 10))


dd <- function(x) sum(x)
#help(Vectorize)

x <- 5
ifelse(x %% 2 == 0, paste(x, " es par"), paste(x, " no es par"))

list.files()

ll <- list.files()

for (arq in ll) {
  print(arq)
}

for (i in 1:length(ll)) {
  print(ll[i])
}

l <- vector("list", length = 3)
l[[1]] <- 1:10
l[[2]] <- "ddd"
l[[3]] <- list(l[[1]], l[[2]])

for (i in l) {
  print(i)
}

outer(1:10, 1:10, "*")


dd <- function(x, y) paste("a_", x, ",", y, sep = "")
outer(1:10, 1:10, dd)

data(Arrests)
str(Arrests)

help(read.table) #Use q to escape help
