# Ctrl + Shift + Enter
rm(list = ls())
library(startupmsg)
library("Rcmdr")
library("TeachingDemos")
library(sfsmisc)
library(distr)
print("Ejercicio 1")
print("a)")
print("Es una variable aleatoria discreta binomial")
print(
  "X es el número de éxitos en 25 pruebas independientes con probabilidad 0.5"
)
print("P(X=K) = (25,K)*0.5^K*(1-0.5)^(25-K)")
print("b)")
dbinom(0:25, size = 25, prob = 0.5)
sum(dbinom(0:25, size = 25, prob = 0.5) > 0)
sum(dbinom(0:25, size = 25, prob = 0.5))
print("c")
local({
  .x <- 5:20
  plotDistr(
    .x,
    dbinom(.x, size = 25, prob = 0.5),
    xlab = "Number of Successes",
    ylab = "Probability Mass",
    main = "Binomial Distribution:  Binomial trials=25, Probability of success=0.5",
    discrete = TRUE
  )
})
print(
  "Podemos ver que Media = Mediana porque podemos ver que la función es simétrica"
)
print("Estarían en el eje de simetría que es 12.5")
print("La moda es doble con 12 y 13")
print("d)")
local({
  .D <- Binom(size = 25, prob = 0.5)
  .fr <- c(12)
  .to <- c(Inf)
  .p <- p(.D)(.to) - p(.D)(.fr)
  cat(
    paste("P(", .fr, " < X <= ", .to, ") = ", .p, sep = "", collapse = "\n"),
    "\n"
  )
})
print("e)")
local({
  .D <- Binom(size = 25, prob = 0.5)
  .fr <- c(11)
  .to <- c(20)
  .p <- p(.D)(.to) - p(.D)(.fr)
  cat(
    paste("P(", .fr, " < X <= ", .to, ") = ", .p, sep = "", collapse = "\n"),
    "\n"
  )
})
print("f)")
local({
  .D <- Binom(size = 25, prob = 0.5)
  .fr <- c(12)
  .to <- c(20)
  .p <- p(.D)(.to) - p(.D)(.fr)
  cat(
    paste("P(", .fr, " < X <= ", .to, ") = ", .p, sep = "", collapse = "\n"),
    "\n"
  )
})
print(0.499544739723206 / 0.5)
local({
  .D <- Binom(size = 25, prob = 0.5)
  .fr <- c(19)
  .to <- c(Inf)
  .p <- p(.D)(.to) - p(.D)(.fr)
  cat(
    paste("P(", .fr, " < X <= ", .to, ") = ", .p, sep = "", collapse = "\n"),
    "\n"
  )
})
local({
  .D <- Binom(size = 25, prob = 0.5)
  .fr <- c(19)
  .to <- c(20)
  .p <- p(.D)(.to) - p(.D)(.fr)
  cat(
    paste("P(", .fr, " < X <= ", .to, ") = ", .p, sep = "", collapse = "\n"),
    "\n"
  )
})
print(0.00158339738845825 / 0.00203865766525269)
print("g)")
print(
  "Los apartados anteriores ya fueron hechos usando la probabilidad acumulada"
)
table <- local({
  .D <- Binom(size = 25, prob = 0.5)
  .p <- d(.D)(support(.D))
  .Table <- data.frame(Probability = .p)
  rownames(.Table) <- support(.D)
  return(.Table)
})
print(sum(table[14:26, ]))
0.5
print(sum(table[13:21, ]))
0.654525756835938
print(sum(table[14:21, ]) / sum(table[14:26, ]))
0.9990895
print(sum(table[21, ]) / sum(table[21:26, ]))
0.7766863
print("h)")
distr::q(Binom(size = 25, prob = 0.5))(
  c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9),
  lower.tail = TRUE
)
distr::q(Binom(size = 25, prob = 0.5))(c(0.25, 0.5, 0.75), lower.tail = TRUE)
print("P(X<=12) >= 0.4")
print("P(X<=12) >= 0.5")
print("P(X=12) >= 0.1")
print("i)")
distr::q(Binom(size = 25, prob = 0.5))(2 / 3, lower.tail = TRUE)
print("j)")
print(
  distr::q(Binom(size = 25, prob = 0.5))(0.75, lower.tail = TRUE) -
    distr::q(Binom(size = 25, prob = 0.5))(0.25, lower.tail = TRUE)
)
print("Es la diferencia máxima entre el 50% de las X centrales")
print((25 * 0.5 * (1 - 0.5)) * (0.5))
print("Dan similares porque no hay datos dispares")
print("k)")
BinomialSamples500 <- as.data.frame(matrix(
  r(Binom(size = 25, prob = 0.5))(500 * 1),
  ncol = 1
))
rownames(BinomialSamples500) <- paste("sample", 1:500, sep = "")
colnames(BinomialSamples500) <- "obs"
print("k)")
print(25 * 0.5)
print(25 * 0.5 * (1 - 0.5))
print(12)
print(12, 13)
print("m)")
summary(BinomialSamples500)
library(abind, pos = 19)
library(e1071, pos = 20)
#calcularResumenVariablesDiscretas(data=BinomialSamples500["obs"], statistics =c("mean", "sd", "quantiles"), quantiles = c(0.5), groups=NULL, tablaFrecuencia=TRUE, cortes=NULL)
print("n)")
BinomialSamples10000 <- as.data.frame(matrix(
  r(Binom(size = 25, prob = 0.5))(500 * 1),
  ncol = 1
))
rownames(BinomialSamples10000) <- paste("sample", 1:500, sep = "")
colnames(BinomialSamples10000) <- "obs"
summary(BinomialSamples10000)
#calcularResumenVariablesDiscretas(data=BinomialSamples10000["obs"], statistics =c("mean", "sd", "quantiles"), quantiles = c(0.5), groups=NULL, tablaFrecuencia=TRUE, cortes=NULL)

#######################################################################################################
print("Ejercicio 2")
print("a)")
local({
  .D <- Binom(size = 100, prob = 0.01)
  .fr <- c(12)
  .to <- c(Inf)
  .p <- p(.D)(.to) - p(.D)(.fr)
  cat(
    paste("P(", .fr, " < X <= ", .to, ") = ", .p, sep = "", collapse = "\n"),
    "\n"
  )
})
print("b)")
local({
  .D <- Binom(size = 100, prob = 0.01)
  .fr <- c(12)
  .to <- c(20)
  .p <- p(.D)(.to) - p(.D)(.fr)
  cat(
    paste("P(", .fr, " < X <= ", .to, ") = ", .p, sep = "", collapse = "\n"),
    "\n"
  )
})
print("c)")
distr::q(Binom(size = 100, prob = 0.01))(
  c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9),
  lower.tail = TRUE
)
distr::q(Binom(size = 100, prob = 0.01))(c(0.25, 0.5, 0.75), lower.tail = TRUE)
print("d)")
local({
  .D <- Pois(lambda = 1)
  .fr <- c(12)
  .to <- c(Inf)
  .p <- p(.D)(.to) - p(.D)(.fr)
  cat(
    paste("P(", .fr, " < X <= ", .to, ") = ", .p, sep = "", collapse = "\n"),
    "\n"
  )
})
local({
  .D <- Pois(lambda = 1)
  .fr <- c(12)
  .to <- c(20)
  .p <- p(.D)(.to) - p(.D)(.fr)
  cat(
    paste("P(", .fr, " < X <= ", .to, ") = ", .p, sep = "", collapse = "\n"),
    "\n"
  )
})
distr::q(Pois(lambda = 1))(
  c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9),
  lower.tail = TRUE
)
distr::q(Pois(lambda = 1))(c(0.25, 0.5, 0.75), lower.tail = TRUE)
print("e)")
Bi <- local({
  .D <- Binom(size = 100, prob = 0.01)
  .p <- d(.D)(support(.D))
  .Table <- data.frame(Probability = .p)
  rownames(.Table) <- support(.D)
  return(.Table)
})
Poiss <- local({
  .D <- Pois(lambda = 1)
  .p <- d(.D)(support(.D))
  .Table <- data.frame(Probability = .p)
  rownames(.Table) <- support(.D)
  return(.Table)
})
plot(Bi[0:11, ], Poiss[0:11, ], type = "l", col = 'grey')
points(Bi[0:11, ], Poiss[0:11, ], type = "l", col = 'red')
print("Sería una diagonal")

#######################################################################################################
print("Ejercicio 3")
print("a)")
local({
  .D <- Nbinom(size = 10, prob = 0.3)
  .fr <- c(-Inf)
  .to <- c(9)
  .p <- p(.D)(.to) - p(.D)(.fr)
  cat(
    paste("P(", .fr, " < X <= ", .to, ") = ", .p, sep = "", collapse = "\n"),
    "\n"
  )
})
print("b)")
local({
  .D <- Nbinom(size = 10, prob = 0.3)
  .fr <- c(11)
  .to <- c(Inf)
  .p <- p(.D)(.to) - p(.D)(.fr)
  cat(
    paste("P(", .fr, " < X <= ", .to, ") = ", .p, sep = "", collapse = "\n"),
    "\n"
  )
})
local({
  .D <- Nbinom(size = 10, prob = 0.3)
  .fr <- c(11)
  .to <- c(20)
  .p <- p(.D)(.to) - p(.D)(.fr)
  cat(
    paste("P(", .fr, " < X <= ", .to, ") = ", .p, sep = "", collapse = "\n"),
    "\n"
  )
})
print(0.343618547764243 / 0.932427233004966)
local({
  .D <- Nbinom(size = 10, prob = 0.3)
  .fr <- c(-Inf)
  .to <- c(8)
  .p <- p(.D)(.to) - p(.D)(.fr)
  cat(
    paste("P(", .fr, " < X <= ", .to, ") = ", .p, sep = "", collapse = "\n"),
    "\n"
  )
})
print("c)")
print(
  "Los apartados anteriores ya fueron hechos usando la probabilidad acumulada"
)
table <- local({
  .D <- Nbinom(size = 10, prob = 0.3)
  .p <- d(.D)(support(.D))
  .Table <- data.frame(Probability = .p)
  rownames(.Table) <- support(.D)
  return(.Table)
})
print(sum(table[1:10, ]))
print(sum(table[13:21, ]) / sum(table[13:79, ]))
print(sum(table[1:9, ]))
print("d)")
distr::q(Nbinom(size = 10, prob = 0.3))(c(0.95), lower.tail = TRUE)
print("e)")
distr::q(Nbinom(size = 10, prob = 0.3))(
  c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9),
  lower.tail = TRUE
)
distr::q(Nbinom(size = 10, prob = 0.3))(c(0.25, 0.5, 0.75), lower.tail = TRUE)
print("f)")
BNSamples500 <- as.data.frame(matrix(
  r(Nbinom(size = 10, prob = 0.3))(500 * 1),
  ncol = 1
))
print("g)")
print(10 * 0.3 / (1 - 0.3))
print(10 * 0.3 / ((1 - 0.3)**2))
print(22)
print((10 - 1) * (1 - 0.3) / 0.3)
print("h)")
summary(BNSamples500)
#calcularResumenVariablesDiscretas(data=BNSamples500["obs"], statistics =c("mean", "sd", "quantiles"), quantiles = c(0.5), groups=NULL, tablaFrecuencia=TRUE, cortes=NULL)
print("i)")
BNSamples1000 <- as.data.frame(matrix(
  r(Nbinom(size = 10, prob = 0.3))(1000 * 1),
  ncol = 1
))
summary(BNSamples1000)
#calcularResumenVariablesDiscretas(data=BNSamples1000["obs"], statistics =c("mean", "sd", "quantiles"), quantiles = c(0.5), groups=NULL, tablaFrecuencia=TRUE, cortes=NULL)
print("j)")
colnames(BNSamples1000) <- "obs"
with(BNSamples1000, Dotplot(obs, bin = FALSE))
local({
  .D <- Nbinom(size = 10, prob = 0.3)
  plot(
    .D,
    to.draw.arg = 1,
    mfColRow = FALSE,
    xlab = "Número de Fallos hasta Éxitos Objetivo",
    ylab = "Masa de Probabilidad",
    main = paste("NegativeBinomial Distribution:  Intentos=10, Prob=0.3")
  )
})

#######################################################################################################
print("Ejercicio 4")
print("a)")
local({
  .D <- Pois(lambda = 10)
  .fr <- c(-Inf)
  .to <- c(9)
  .p <- p(.D)(.to) - p(.D)(.fr)
  cat(
    paste("P(", .fr, " < X <= ", .to, ") = ", .p, sep = "", collapse = "\n"),
    "\n"
  )
})
print("b)")
local({
  .D <- Pois(lambda = 10)
  .fr <- c(-Inf)
  .to <- c(11)
  .p <- p(.D)(.to) - p(.D)(.fr)
  cat(
    paste("P(", .fr, " < X <= ", .to, ") = ", .p, sep = "", collapse = "\n"),
    "\n"
  )
})
local({
  .D <- Pois(lambda = 10)
  .fr <- c(-Inf)
  .to <- c(20)
  .p <- p(.D)(.to) - p(.D)(.fr)
  cat(
    paste("P(", .fr, " < X <= ", .to, ") = ", .p, sep = "", collapse = "\n"),
    "\n"
  )
})
print(0.998411739338142 - 0.696776146303107) / (1 - 0.696776146303107)
local({
  .D <- Pois(lambda = 10)
  .fr <- c(-Inf)
  .to <- c(8)
  .p <- p(.D)(.to) - p(.D)(.fr)
  cat(
    paste("P(", .fr, " < X <= ", .to, ") = ", .p, sep = "", collapse = "\n"),
    "\n"
  )
})
print("c)")
print(
  "Los apartados anteriores ya fueron hechos usando la probabilidad acumulada"
)
table <- local({
  .D <- Pois(lambda = 10)
  .p <- d(.D)(support(.D))
  .Table <- data.frame(Probability = .p)
  rownames(.Table) <- support(.D)
  return(.Table)
})
print(sum(table[1:10, ]))
print(sum(table[13:21, ]) / sum(table[13:29, ]))
print(sum(table[1:9, ]))
print("d)")
distr::q(Pois(lambda = 10))(
  c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9),
  lower.tail = TRUE
)
distr::q(Pois(lambda = 10))(c(0.25, 0.5, 0.75), lower.tail = TRUE)
print("e)")
Pois500 <- as.data.frame(matrix(r(Pois(lambda = 10))(500 * 1), ncol = 1))
print("f)")
print(10)
print(10)
print(10)
print(11)
print("g)")
summary(Pois500)
#calcularResumenVariablesDiscretas(data=Pois500["obs"], statistics =c("mean", "sd", "quantiles"), quantiles = c(0.5), groups=NULL, tablaFrecuencia=TRUE, cortes=NULL)
print("h)")
Pois1000 <- as.data.frame(matrix(r(Pois(lambda = 10))(1000 * 1), ncol = 1))
summary(Pois1000)
#calcularResumenVariablesDiscretas(data=Pois1000["obs"], statistics =c("mean", "sd", "quantiles"), quantiles = c(0.5), groups=NULL, tablaFrecuencia=TRUE, cortes=NULL)
print("i)")
colnames(Pois1000) <- "obs"
with(Pois1000, Dotplot(obs, bin = FALSE))
local({
  .D <- Pois(lambda = 10)
  plot(
    .D,
    to.draw.arg = 1,
    mfColRow = FALSE,
    xlab = "Número de Fallos hasta Éxitos Objetivo",
    ylab = "Masa de Probabilidad",
    main = paste("NegativeBinomial Distribution:  Intentos=10, Prob=0.3")
  )
})
print("j)")

Norm(mean = 10, sd = (10**(1 / 2)))
Poiss <- local({
  .D <- Pois(lambda = 10)
  .p <- d(.D)(support(.D))
  .Table <- data.frame(Probability = .p)
  rownames(.Table) <- support(.D)
  return(.Table)
})
plot(Norm(mean = 10, sd = (10**(1 / 2))), to.draw.arg = 1)
plot(0:40, Poiss[0:41, ], type = "l", col = 'grey')
