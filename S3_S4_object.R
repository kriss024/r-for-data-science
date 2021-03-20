rm(list = ls())

j = list(name = 'Joe', salary = 55000, union = TRUE)
class(j) = 'employee'
attributes(j)
print(j)

print.employee = function(wkr) {
    cat(wkr$name, '\n')
    cat('salary', wkr$salary, '\n')
    cat('union member', wkr$union, '\n')
}

summary.employee = function(wkr) {
    cat(wkr$name, '\n')
    cat('salary', wkr$salary, '\n')
    cat('union member', wkr$union, '\n')
}

summary(j)

bubba <- list(first = "one", second = "two", third = "third")
class(bubba) = 'Flamboyancy'

GetFirst <- function(x) {
    UseMethod("GetFirst", x)
}

GetFirst.Flamboyancy <- function(x) {
    return(x$first)
}

GetFirst(bubba)

NorthAmerican <- function(eatsBreakfast = TRUE, myFavorite = "cereal") {

    me <- list(
    hasBreakfast = eatsBreakfast,
    favoriteBreakfast = myFavorite
  )

    class(me) = 'NorthAmerican'
    return(me)
}

bubba <- NorthAmerican()
bubba

louise <- NorthAmerican(eatsBreakfast = TRUE, myFavorite = "fried eggs")
louise

#####################################
rm(list = ls())

setClass('employee', representation(name = 'character', salary = 'numeric', union = 'logical'))
joe = new('employee', name = 'Joe', salary = 55000, union = TRUE)

joe@salary = 65000
show(joe)

setMethod('show', 'employee',
          function(object) {
              inorout = ifelse(object@union, 'is', 'is not')
              cat(object@name, 'has a salary of', object@salary, 'and', inorout, 'in the union \n')
          }
)
joe

typeof(joe)