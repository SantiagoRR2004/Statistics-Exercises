# Ctrl + Shift + Enter
rm(list = ls())
library(startupmsg)
library("Rcmdr")
library("TeachingDemos")
library(sfsmisc)
library(distr)
print("Ejercicio 5")
print("a)")
local({
  .D <- Chisq(df = 150)
  .fr <- c(126)
  .to <- c(Inf)
  .p <- p(.D)(.to) - p(.D)(.fr)
  cat(
    paste("P(", .fr, " < X <= ", .to, ") = ", .p, sep = "", collapse = "\n"),
    "\n"
  )
})
print("b)")
local({
  .D <- Chisq(df = 65)
  .fr <- c(40)
  .to <- c(50)
  .p <- p(.D)(.to) - p(.D)(.fr)
  cat(
    paste("P(", .fr, " < X <= ", .to, ") = ", .p, sep = "", collapse = "\n"),
    "\n"
  )
})
print("c")
local({
  .D <- Chisq(df = 220)
  .fr <- c(260)
  .to <- c(Inf)
  .p <- p(.D)(.to) - p(.D)(.fr)
  cat(
    paste("P(", .fr, " < X <= ", .to, ") = ", .p, sep = "", collapse = "\n"),
    "\n"
  )
})
print("d)")
distr::q(Chisq(df = 220))(c(0.4), lower.tail = TRUE)


#######################################################################################################
print("Ejercicio 6")
print("a)")
local({
  .D <- Td(df = 8)
  .fr <- c(-Inf)
  .to <- c(0.25)
  .p <- p(.D)(.to) - p(.D)(.fr)
  cat(
    paste("P(", .fr, " < X <= ", .to, ") = ", .p, sep = "", collapse = "\n"),
    "\n"
  )
})
local({
  .D <- Td(df = 8)
  .fr <- c(-Inf)
  .to <- c(0.1645)
  .p <- p(.D)(.to) - p(.D)(.fr)
  cat(
    paste("P(", .fr, " < X <= ", .to, ") = ", .p, sep = "", collapse = "\n"),
    "\n"
  )
})
print("b)")
local({
  .D <- Td(df = 95)
  .fr <- c(-1.645)
  .to <- c(Inf)
  .p <- p(.D)(.to) - p(.D)(.fr)
  cat(
    paste("P(", .fr, " < X <= ", .to, ") = ", .p, sep = "", collapse = "\n"),
    "\n"
  )
})
print("c)")
distr::q(Td(df = 90))(c(0.05), lower.tail = TRUE)


#######################################################################################################
print("Ejercicio 7")
print("a)")
local({
  .D <- Fd(df1 = 6, df2 = 7)
  .fr <- c(-Inf)
  .to <- c(4.25)
  .p <- p(.D)(.to) - p(.D)(.fr)
  cat(
    paste("P(", .fr, " < X <= ", .to, ") = ", .p, sep = "", collapse = "\n"),
    "\n"
  )
})
print("b)")
local({
  .D <- Fd(df1 = 6, df2 = 7)
  .fr <- c(0.245)
  .to <- c(Inf)
  .p <- p(.D)(.to) - p(.D)(.fr)
  cat(
    paste("P(", .fr, " < X <= ", .to, ") = ", .p, sep = "", collapse = "\n"),
    "\n"
  )
})
print("c)")
distr::q(Fd(df1 = 6, df2 = 7))(c(0.025), lower.tail = TRUE)
