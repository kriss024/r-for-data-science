rm(list = ls())

library(readxl)

dane <- read_excel("C:\\Work\\WE.xlsx", sheet = "Arkusz1")

setwd("C:\\Work")
print(getwd())

saveRDS(dane, file="dane.Rda")

nowy <- readRDS("dane.Rda")

save(dane, file = "dane.Rdata")
rm(list = ls())

load("dane.Rdata")