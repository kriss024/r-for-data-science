rm(list = ls())
library(jsonlite)
all.equal(mtcars, fromJSON(toJSON(mtcars)))

print(toJSON(mtcars))

json <- '["Mario", "Peach", null, "Bowser"]'

print(fromJSON(json))

json <-
'[
  {"Name" : "Mario", "Age" : 32, "Occupation" : "Plumber"}, 
  {"Name" : "Peach", "Age" : 21, "Occupation" : "Princess"},
  {},
  {"Name" : "Bowser", "Occupation" : "Koopa"}
]'
mydf <- fromJSON(json)

print(mydf)

all.equal(mydf, fromJSON(toJSON(mydf)))

mydf$Ranking <- c(3, 1, 2, 4)
mydf.pretty <- toJSON(mydf, pretty = TRUE)
print(mydf.pretty)