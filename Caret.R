rm(list = ls())

library(caret)

data(GermanCredit)

age <- GermanCredit$Age

library(e1071)
bc <- BoxCoxTrans(age)
print(bc)

age_bc <- predict(bc, age)

# 1. Open png file
png('rplot.png', width=600, height=350)
# 2. Create the plot
par(mfrow = c(1, 2))
hist(age)
hist(age_bc)
# 3. Close the file
dev.off() 