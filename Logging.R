rm(list = ls())

setwd("C:\\Work")
print(getwd())

library(logging)
basicConfig()
addHandler(writeToFile, logger="company", file="sample.log")

loginfo("hello world", logger="company")

logwarn("hello company", logger="company.module")

logerror("hello error", logger="company.error")
