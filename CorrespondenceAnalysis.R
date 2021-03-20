# Correspondence Analysis
library(ca)

data("USArrests")
mytable <- as.matrix(USArrests)
prop.table(mytable, 1) # row percentages
prop.table(mytable, 2) # column percentages

fit <- ca(mytable)
summary(fit) # extended results 
plot(fit) # symmetric map
plot(fit, mass = TRUE, contrib = "absolute", map =
    "rowgreen", arrows = c(FALSE, TRUE)) # asymmetric map