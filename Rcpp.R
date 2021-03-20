rm(list = ls())
library(Rcpp)

sourceCpp(code = '
  #include <Rcpp.h>

  // [[Rcpp::export]]
  int fibonacci(const int x) {
    if (x == 0) return(0);
    if (x == 1) return(1);
    return (fibonacci(x - 1)) + fibonacci(x - 2);
  }'
)

cppFunction(
    'int fibonacci2(const int x) {
        if (x == 0) return(0); 
        if (x == 1) return(1);
        return (fibonacci2(x - 1)) + fibonacci2(x - 2);
    }
    ')

cppFunction(plugins = c("cpp11"), '
    int useCpp11() {
        auto x = 10;
        return x;
    }
    ')

print(fibonacci(25))
print(fibonacci2(25))
print(useCpp11())