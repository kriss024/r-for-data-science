rm(list = ls())

url = "http://freakonometrics.free.fr/german_credit.csv"
credit = read.csv(url, header = TRUE, sep = ",")

str(credit)

Creditability <- credit$Creditability

CreditAmount <- credit$Credit.Amount

PaymentStatus <- as.factor(credit$Payment.Status.of.Previous.Credit)

credit.df <- data.frame(Creditability, CreditAmount)

credit.df2 <- data.frame(Creditability, PaymentStatus)

library(smbinning)

result = smbinning(credit.df, y = "Creditability", x = "CreditAmount", p = 0.05)
print(result$ivtable)
print(result$iv)
print(result$cuts)

table <- result$ivtable
print(table[, c("Cutpoint", "WoE")])

binning <- smbinning.gen(credit.df, result, chrname = "CreditAmountBin")

par(mfrow = c(2, 2))
smbinning.plot(result, option = "goodrate", sub = "Credit Amount")
smbinning.plot(result, option = "badrate", sub = "Credit Amount")
smbinning.plot(result, option = "dist", sub = "Credit Amount")
smbinning.plot(result, option = "WoE", sub = "Credit Amount")

#---------------------------------------------------------------------

result = smbinning.factor(credit.df2, y = "Creditability", x = "PaymentStatus")
print(result$ivtable)
print(result$iv)
print(result$cuts)

par(mfrow = c(2, 2))
smbinning.plot(result, option = "goodrate", sub = "Payment Status of Previous Credit")
smbinning.plot(result, option = "badrate", sub = "Payment Status of Previous Credit")
smbinning.plot(result, option = "dist", sub = "Payment Status of Previous Credit")
smbinning.plot(result, option = "WoE", sub = "Payment Status of Previous Credit")

table <- result$ivtable
print(table[,c("Cutpoint", "WoE")])