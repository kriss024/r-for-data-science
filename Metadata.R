rm(list = ls())

double <- c(1.0, 1.5, 2.5)
double.na.inf <- c(NA, Inf, Inf)
double.na <- c(NA, NA, NA)
integer <- c(0, 1, 2)
factor <- as.factor(integer)
text <- c("aa", "bb", NA)
text.factor <- as.factor(text)
boolean <- c(TRUE, FALSE, TRUE)
complex <- 1+4i

data <- data.frame(double, double.na.inf, double.na, integer, factor, text, text.factor, boolean, complex)

GetMetadata <- function(data) {

    meta <- sapply(data, class)
    names <- names(data)
    l <- length(data)
    n <- nrow(data)

    name.columns <- data.frame()

    for (i in 1:l) {
        a <- names[i]
        b <- meta[i]
        d <- data[, a]
        m <- sum(is.na(d))
        u <- unique(d)
        t <- table(u)
        s <- sum(t)
        p = m / n
        tmp1 <- data.frame(Name = a, Class = b, Rows = n, NAs = m, "Missing percent" = p, Unique = s)
        name.columns <- rbind(name.columns, tmp1)
        rownames(name.columns) <- NULL
    }

    return(name.columns)
}

names <- GetMetadata(data)

print(names)