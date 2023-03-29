# Ctrl + Shift + Enter
rm(list=ls())
library(startupmsg)
library("Rcmdr")
library("TeachingDemos")
library(sfsmisc)
library(distr)
print("Ejercicio 1")
print("a)")
local({
  .D <- Unif(Min=-1, Max=1)
  .fr <- c(-Inf)
  .to <- c(0.1)
  .p <- p(.D)(.to) - p(.D)(.fr)
  cat(paste("P(",.fr," < X <= ",.to,") = ",.p,sep="",collapse="\n"),"\n")
})
print("b)")
local({
  .D <- Unif(Min=-1, Max=1)
  .fr <- c(0)
  .to <- c(Inf)
  .p <- p(.D)(.to) - p(.D)(.fr)
  cat(paste("P(",.fr," < X <= ",.to,") = ",.p,sep="",collapse="\n"),"\n")
})
local({
  .D <- Unif(Min=-1, Max=1)
  .fr <- c(0)
  .to <- c(0.2)
  .p <- p(.D)(.to) - p(.D)(.fr)
  cat(paste("P(",.fr," < X <= ",.to,") = ",.p,sep="",collapse="\n"),"\n")
})
print(0.1/0.5)
print("c")
distr::q(Unif(Min=-1, Max=1))(c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9), lower.tail=TRUE)
distr::q(Unif(Min=-1, Max=1))(c(0.25,0.5,0.75), lower.tail=TRUE)
print("d)")
Uniform500 <- as.data.frame(matrix(r(Unif(Min=-1, Max=1))(500*1), ncol=1))
rownames(Uniform500) <- paste("sample", 1:500, sep="")
colnames(Uniform500) <- "obs"
print("e)")
print((1+-1)/2)
print((1--1)**2/12)
print((-1+1)/2)
print("El rango (-1,1)")
print("f)")
#calcularResumenVariablesDiscretas(data=Uniform500["obs"], statistics =c("mean", "sd", "IQR", "quantiles"), quantiles = c(0,0.25,0.5,0.75,1), groups=NULL, tablaFrecuencia=TRUE, cortes=NULL)
print("g)")
Uniform1000 <- as.data.frame(matrix(r(Unif(Min=-1, Max=1))(500*1), ncol=1))
rownames(Uniform1000) <- paste("sample", 1:500, sep="")
colnames(Uniform1000) <- "obs"
#calcularResumenVariablesDiscretas(data=Uniform1000["obs"], statistics =c("mean", "sd", "IQR", "quantiles"), quantiles = c(0,0.25,0.5,0.75,1), groups=NULL, tablaFrecuencia=TRUE, cortes=NULL)
print("h)")
hist(Uniform1000)
local({
  .D <- Unif(Min=-1, Max=1)  
  plot(.D, to.draw.arg=1, mfColRow=FALSE, xlab="x", ylab="Densidad", main=paste("Uniform Distribution:  Minimum=-1, Maximum=1"))
})

#######################################################################################################
print("Ejercicio 2")
Exp10 = Exp(rate=0.1)
print("a)")
local({
  .D <- Exp10
  .fr <- c(-Inf)
  .to <- c(10)
  .p <- p(.D)(.to) - p(.D)(.fr)
  cat(paste("P(",.fr," < X <= ",.to,") = ",.p,sep="",collapse="\n"),"\n")
})
print("b)")
local({
  .D <- Exp10
  .fr <- c(12)
  .to <- c(Inf)
  .p <- p(.D)(.to) - p(.D)(.fr)
  cat(paste("P(",.fr," < X <= ",.to,") = ",.p,sep="",collapse="\n"),"\n")
})
local({
  .D <- Exp10
  .fr <- c(12)
  .to <- c(20)
  .p <- p(.D)(.to) - p(.D)(.fr)
  cat(paste("P(",.fr," < X <= ",.to,") = ",.p,sep="",collapse="\n"),"\n")
})
print(0.165858928675589/0.301194211912202)
local({
  .D <- Exp10
  .fr <- c(-Inf)
  .to <- c(8)
  .p <- p(.D)(.to) - p(.D)(.fr)
  cat(paste("P(",.fr," < X <= ",.to,") = ",.p,sep="",collapse="\n"),"\n")
})
print("Los resultados son iguales porque carece de memoria")
print("c)")
distr::q(Exp10)(c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9), lower.tail=TRUE)
distr::q(Exp10)(c(0.25,0.5,0.75), lower.tail=TRUE)
print("d)")
ExpSamples500 <- as.data.frame(matrix(r(Exp10)(500*1), ncol=1))
colnames(ExpSamples500) <- "obs"
print("e)")
print(1/0.1)
print(1/(0.1**2))
print(log(2)/0.1)
print(0)
print("f)")
summary(ExpSamples500)
#calcularResumenVariablesDiscretas(data=ExpSamples500["obs"], statistics =c("mean", "sd", "quantiles"), quantiles = c(0.5), groups=NULL, tablaFrecuencia=TRUE, cortes=NULL)
print("g)")
ExpSamples1000 <- as.data.frame(matrix(r(Exp10)(1000*1), ncol=1))
colnames(ExpSamples1000) <- "obs"
summary(ExpSamples1000)
#calcularResumenVariablesDiscretas(data=ExpSamples1000["obs"], statistics =c("mean", "sd", "quantiles"), quantiles = c(0.5), groups=NULL, tablaFrecuencia=TRUE, cortes=NULL)
print("h)")
hist(ExpSamples1000)
local({
  .D <- Exp10  
  plot(.D, to.draw.arg=1, mfColRow=FALSE, xlab="x", ylab="Densidad", main=paste("Exponential Distribution:  Rate=0.1"))
})

#######################################################################################################
print("Ejercicio 3")

N102 = Norm(mean=10, sd=2)
print("a)")
local({
  .D <- N102
  .fr <- c(10)
  .to <- c(Inf)
  .p <- p(.D)(.to) - p(.D)(.fr)
  cat(paste("P(",.fr," < X <= ",.to,") = ",.p,sep="",collapse="\n"),"\n")
})
local({
  .D <- N102
  .fr <- c(10)
  .to <- c(Inf)
  .p <- p(.D)(.to) - p(.D)(.fr)
  cat(paste("P(",.fr," <= X <= ",.to,") = ",.p,sep="",collapse="\n"),"\n")
})
print("Es una variable continua, cada punto tiene valor 0")
print("b)")
local({
  .D <- N102
  .fr <- c(13.91993)
  .to <- c(Inf)
  .p <- p(.D)(.to) - p(.D)(.fr)
  cat(paste("P(",.fr," < X <= ",.to,") = ",.p,sep="",collapse="\n"),"\n")
})
local({
  .D <- Norm(mean=0, sd=1)
  .fr <- c(1.959964)
  .to <- c(Inf)
  .p <- p(.D)(.to) - p(.D)(.fr)
  cat(paste("P(",.fr," < X <= ",.to,") = ",.p,sep="",collapse="\n"),"\n")
})
print((13.91993-10)/2)
print(1.959964)
print("c")
local({
  .D <- N102
  .fr <- c(12)
  .to <- c(Inf)
  .p <- p(.D)(.to) - p(.D)(.fr)
  cat(paste("P(",.fr," < X <= ",.to,") = ",.p,sep="",collapse="\n"),"\n")
})
local({
  .D <- N102
  .fr <- c(12)
  .to <- c(20)
  .p <- p(.D)(.to) - p(.D)(.fr)
  cat(paste("P(",.fr," < X <= ",.to,") = ",.p,sep="",collapse="\n"),"\n")
})
print(0.158654967279885/0.158655253931457)
local({
  .D <- N102
  .fr <- c(-Inf)
  .to <- c(8)
  .p <- p(.D)(.to) - p(.D)(.fr)
  cat(paste("P(",.fr," < X <= ",.to,") = ",.p,sep="",collapse="\n"),"\n")
})
print("d)")
distr::q(N102)(c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9), lower.tail=TRUE)
distr::q(N102)(c(0.25,0.5,0.75), lower.tail=TRUE)
print("e)")
NSamples500 <- as.data.frame(matrix(r(N102)(500*1), ncol=1))
colnames(NSamples500) <- "obs"
print("f)")
print(10)
print(2**2)
print(10)
print(10)
print("g)")
summary(NSamples500)
#calcularResumenVariablesDiscretas(data=NSamples500["obs"], statistics =c("mean", "sd", "quantiles"), quantiles = c(0.5), groups=NULL, tablaFrecuencia=TRUE, cortes=NULL)
print("h)")
NSamples1000 <- as.data.frame(matrix(r(N102)(1000*1), ncol=1))
colnames(NSamples1000) <- "obs"
summary(NSamples1000)
#calcularResumenVariablesDiscretas(data=NSamples1000["obs"], statistics =c("mean", "sd", "quantiles"), quantiles = c(0.5), groups=NULL, tablaFrecuencia=TRUE, cortes=NULL)
print("i)")
hist(NSamples1000)
local({
  .D <- N102  
  plot(.D, to.draw.arg=1, mfColRow=FALSE, xlab="x", ylab="Densidad", main=paste("Exponential Distribution:  Rate=0.1"))
})

#######################################################################################################
print("Ejercicio 4")
envase = Norm(mean=600, sd=0.1)

600 - distr::q(envase)(c(0.04), lower.tail=TRUE)
distr::q(envase)(c(0.96), lower.tail=TRUE) - 600
local({
  .D <- envase  
  plot(.D, regions=list(c(distr::q(envase)(c(0.04), lower.tail=TRUE), distr::q(envase)(c(0.96), lower.tail=TRUE))), col=c("#FF0000", "#BEBEBE"), legend.pos="topright", to.draw.arg=1, mfColRow=FALSE, xlab="x", 
              ylab="Densidad", main=paste("Normal Distribution:  Mean=600, Standard deviation=0.1"))
})
print(0.1750686)







