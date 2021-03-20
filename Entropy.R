rm(list = ls())
library(entropy)
library(ggplot2)

x1 = runif(10000)
df <- data.frame(x1)
qplot(df$x1,
      geom="histogram",
      bins = 10,  
      main = "Histogram for x1", 
      xlab = "x1",  
      fill=I("blue"), 
      col=I("red"), 
      alpha=I(.2))

# discretize into 10 categories
y1 = discretize(x1, numBins=10, r=c(0,1))
df.y <- data.frame(y1)
print(df.y)

entropy(df.y$Freq) # empirical estimate near theoretical maximum
log(10) # theoretical value for discrete uniform distribution with 10 bins

### 2D example ####
x1 = runif(10000)
x2 = runif(10000)
y2d = discretize2d(x1, x2, numBins1=10, numBins2=10)
df.y2d <- data.frame(y2d)
print(df.y2d)
sum(df.y2d$Freq)

# joint entropy
H12 = entropy(df.y2d$Freq)
print(H12)
log(100) # theoretical maximum for 10x10 table
# mutual information
mi.empirical(df.y2d$Freq) # approximately zero
# another way to compute mutual information
# compute marginal entropies
H1 = entropy(rowSums(y2d))
H2 = entropy(colSums(y2d))
print(H1+H2-H12) # mutual entropy

### 2D example v2 ####
x1 = (1:10000)/10000
x2 = x1 + rnorm(10000)/5
df <- data.frame(x1,x2)
ggplot(df, aes(x1, x2)) + geom_point(alpha = 1/10) + geom_rug()

y2d = discretize2d(x1, x2, numBins1=10, numBins2=10)
df.y2d <- data.frame(y2d)

print(y2d)
sum(y2d)

# joint entropy
H12 = entropy(y2d)
print(H12)
log(100) # theoretical maximum for 10x10 table
# mutual information
mi.empirical(y2d) # approximately zero
# another way to compute mutual information
# compute marginal entropies
H1 = entropy(rowSums(y2d))
H2 = entropy(colSums(y2d))
print(H1+H2-H12) # mutual entropy


library(reldist)
# generate vector (of incomes)
x <- c(541, 1463, 2445, 3438, 4437, 5401, 6392, 8304, 11904, 22261)

# compute Gini coefficient
gini(x)

# generate a vector of weights.
w <- runif(n=length(x))
gini(x,w)

# working with weights
fl <- c(2.5, 7.5, 15, 35, 75, 150)    # midpoints of classes
n  <- c(25, 13, 10, 5, 5, 2)          # frequencies

# with confidence intervals
gini(fl, n)