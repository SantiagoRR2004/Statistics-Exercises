# Ctrl + Shift + Enter
rm(list=ls())

library(datasets)
data(iris)
summary(iris)
save(iris,file="iris.RData")
rm(iris)
ls()
load(file="iris.RData")
ls()
getwd()
#### TXT
# save
write.table(iris,file="iris.txt",sep="\t",dec=".",
            quote=FALSE, row.names=FALSE,col.names=TRUE)

rm(iris)

iris2 <- read.table("iris.txt",header=TRUE,sep="\t",dec=".")
file.remove("iris.RData")
file.remove("iris.txt")
