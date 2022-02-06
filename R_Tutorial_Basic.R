closeAllConnections()
rm(list=ls())

Died.At <- c(22,40,72,41)
Writer.At <- c(16, 18, 36, 36)
First.Name <- c("John", "Edgar", "Walt", "Jane")
Second.Name <- c("Doe", "Poe", "Whitman", "Austen")
Sex <- c("MALE", "MALE", "MALE", "FEMALE")
Date.Of.Death <- as.Date(c("2015-05-10", "1849-10-07", "1892-03-26","1817-07-18"))

writers_df <- data.frame(Died.At, Writer.At, First.Name, Second.Name, Sex, Date.Of.Death)

str(writers_df)
head(writers_df)
summary(writers_df)

names(writers_df)
names(writers_df) <- c("Age.At.Death", "Age.As.Writer", "Name", "Surname", "Gender", "Death")
names(writers_df)

dim(writers_df)
rows <- dim(writers_df)[1]
columns <- dim(writers_df)[1]
columns <- length(writers_df)

third.column <- writers_df[1:2,3]

writers.name  <- cbind(First.Name, Second.Name)
writers.two  <- rbind(writers_df, writers_df)

x <- cbind(x1 = 3, x2 = c(4:1, 2:5))
apply(x, 2, mean, trim = .2)
col.sums <- apply(x, 2, sum)
row.sums <- apply(x, 1, sum)

Age <- numeric()
Name <- character()
ID <- integer()
Gender <- factor()
Date <- as.Date(character())
ab <- data.frame(Age, Name, ID, Gender, Date)
str(ab)

writer_names_df <- writers_df[1:4, "Name", drop=FALSE]
young_writers_df <- subset(writers_df, Age.At.Death <= 40 & Age.As.Writer >= 18)
forty_sth_writers <- writers_df[writers_df$Age.At.Death > 40,]
writers_df$Age.As.Writer = ifelse(writers_df$Age.As.Writer>30,40,20)

writers_df.mean <- aggregate(Age.At.Death~Gender, writers_df, mean, na.rm = TRUE)

writer_names_df2 <- data.frame(a = seq(1,16,by=2), b = LETTERS[1:8], x= month.abb[1:8], y = sample(10:20,8, replace = TRUE), z=letters[1:8])

Age.At.Death <- NULL
writers_df$Location <- c("Belgium", "United Kingdom", "United States", "United Kingdom")

library(taRifx)
sorted_data <- sort(writers_df, decreasing=TRUE, ~Age.At.Death)

library(dplyr) 

food <- factor(c("low", "high", "medium", "high", "low", "medium", "high"))
levels(food)
print(is.factor(food))
nlevels(food)

print("Unordered Factors:") 
food <- factor(food, levels = c("low", "medium", "high"))
levels(food)

print("Ordered factors")
food <- factor(food, levels = c("low", "medium", "high"), ordered = TRUE, exclude = NULL)
levels(food)
print(is.ordered(food))

min(food)
max(food)

print("Converting factors")
num<-as.numeric(food)
print(food)
print(num)

print("Using factors")
dat <- read.csv(file = 'data/sample.csv', stringsAsFactors = TRUE)