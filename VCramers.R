rm(list = ls())
condition1 <- c(27, 31, 12) 
condition2 <- c(45, 13, 23)
X <- cbind( condition1, condition2 )
rownames(X) <- c( 'choice1', 'choice2', 'choice3' )
print(X)

chisq.test(X)
# To estimate the effect size we can use Cramer's V:
library(lsr)
cramersV(X)