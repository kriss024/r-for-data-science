rm(list=ls())
setwd("C:/Temp")
getwd()

library(sas7bdat)
sas_ds<-read.sas7bdat('rimporttest.sas7bdat')

sas_ds
