# Ctrl + Shift + Enter

x <- 1:10
mean(x)
x
mean <- function(x = 1:10) {
  media2 <- base::mean(c(x, 1))
  media <- (sum(x) + 1) / (length(x) + 1)
  return(list(media, media2))
}

l <- mean()
str(l)
l[[1]]
l[[2]]


an <- function(a1, r, n) {
  a1 * r^(n - 1)
}

x <- 5
ifelse(x %% 2 == 0, paste(x, " par"), paste(x, "impar"))

arquivos <- list.files()
for (arq in arquivos) {
  print(arq)
}

outer(1:10, 1:10, "*")

mifuncion <- function(x) {
  return(sqrt(sum(1:x)))
}
sapply(1:10, mifuncion)


library(carData)
data(AMSsurvey)

write.table(
  AMSsurvey,
  file = "proba.txt",
  sep = "\t",
  row.names = FALSE,
  col.names = TRUE
)

dd <- read.table("proba.txt", header = TRUE, sep = "\t", dec = ".")
