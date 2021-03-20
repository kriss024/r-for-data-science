rm(list = ls())

library("dummies")

letters <- c( "a", "a", "b", "c", "d", "e", "f", "g", "h", "b", "b" )

letters <- c( "a", "b", "c", "d", "e", "f", "g", "h")

dummy.letters <- dummy(letters, sep = ".", verbose = TRUE)

print(dummy.letters)