rm(list = ls())
library(survival)
library(discSurv)
library(survMisc)
library(MASS)
library(survminer)
library(dplyr)
library(tibble)
library(openxlsx)
library(xlsx)
library(tidyverse)

library(Ecdat)
data(UnempDur)
str(UnempDur)
summary(UnempDur)

czas <- UnempDur$spell
cens <- UnempDur$censor1
grupa <- as.factor(UnempDur$ui)
poziomy<-levels(grupa)
head(Surv(czas, cens), 20)

km <- survfit(Surv(czas, cens) ~ 1, conf.type = "log-log") # estymator KM
Nels <- survfit(Surv(czas, cens) ~ 1, conf.type = "log-log", type = "fh") # estymator NA
LifeTabUnempDur <- lifeTable(dataSet=UnempDur, timeColumn="spell", censColumn="censor1") #Estymator aktuarialny

plot(km, main = "Estymator Kaplana - Meyera")
plot(Nels, main = "Estymator Nelsona-Aalena")
plot(LifeTabUnempDur$Output$S, type = "s", ylim = c(0, 1), main = "Estymator aktuarialny")

km <- survfit(Surv(czas, cens) ~ grupa, conf.type = "log-log") # estymator KM
Nels <- survfit(Surv(czas, cens) ~ grupa, conf.type = "log-log", type = "fh") # estymator NA

plot(km, col = c("red", "blue"), main = "Estymator Kaplana - Meyera")
legend("bottomright", poziomy, lty = 1, col = c("red", "blue"))

plot(Nels, col = c("red", "blue"), main = "Estymator Nelsona-Aalena")
legend("bottomright", poziomy, lty = 1, col = c("red", "blue"))