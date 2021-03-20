rm(list=ls())
library(dplyr)
library(ggplot2)

german.credit <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data")

colnames(german.credit) <- c("chk_acct", "duration", "credit_his", "purpose", 
                             "amount", "saving_acct", "present_emp", "installment_rate", "sex", "other_debtor", 
                             "present_resid", "property", "age", "other_install", "housing", "n_credits", 
                             "job", "n_people", "telephone", "foreign", "response")

german.credit$response <- german.credit$response - 1
german.credit$target <- german.credit$response
german.credit$response <- as.factor(german.credit$response)

glimpse(german.credit)

summary(german.credit)

ggplot(german.credit, aes(purpose, ..count..)) + 
  geom_bar(aes(fill = response), position = "dodge")

german.credit.purpose.agg <- german.credit %>% group_by(purpose)
purpose.target.encoding <- german.credit.purpose.agg %>% summarise(target = mean(target))
summary(purpose.target.encoding)