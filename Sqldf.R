library(sqldf)

dane <- iris

agregacja <- sqldf("SELECT Species, count(*) as ile FROM dane GROUP BY Species")

unikalne <- sqldf("SELECT distinct Species FROM dane")
