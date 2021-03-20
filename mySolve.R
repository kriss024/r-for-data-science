rm(list = ls(all = TRUE))

#https://en.wikipedia.org/wiki/Invertible_matrix#Blockwise_inversion
mySolve <- function(mat) {

    if (nrow(mat) == 1) {
        return(matrix(1.0 / det(mat)))
    }

    m <- floor(nrow(mat) / 2)
    m2 <- m + 1
    end <- nrow(mat)

    A <- mat[1:m, 1:m, drop = F]
    B <- mat[1:m, m2:end, drop = F]
    C <- mat[m2:end, 1:m, drop = F]
    D <- mat[m2:end, m2:end, drop = F]

    invA <- mySolve(A)
    invDCinvAB <- mySolve(D - C %*% invA %*% B)
    mat11 <- invA + invA %*% B %*% invDCinvAB %*% C %*% invA
    mat12 <- -invA %*% B %*% invDCinvAB
    mat21 <- -invDCinvAB %*% C %*% invA
    mat22 <- invDCinvAB
    
    mat_out <- cbind(rbind(mat11, mat21), rbind(mat12, mat22))

    return(mat_out)

}

set.seed(1)
s <- 5
mat <- matrix(rnorm(s^2), nrow = s)
print("My Solve")
print(mySolve(mat))
print("Built-in Solve:")
solve(mat)