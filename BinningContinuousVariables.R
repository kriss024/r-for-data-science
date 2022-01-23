rm(list = ls())

library(dplyr)
library(tidyverse)
library(ggplot2)


adult.data <- read_delim('https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data', delim = ',', col_names = FALSE, na = c('','?'))

colnames(adult.data) <- c('age','workclass', 'fnlwgt', 'education', 
                          'education_num', 'marital_status', 'occupation', 'relationship', 
                          'race','sex', 'capital_gain', 'capital_loss', 'hours_per_week', 
                          'native_country', 'income_class')

numeric_col <- c('age', 'fnlwgt', 'education_num', 'capital_gain', 'capital_loss', 'hours_per_week')

str_col <- c('workclass', 'education', 'marital_status', 'occupation', 'relationship', 
             'race','sex', 'native_country', 'income_class')

adult.data.train <- adult.data %>% 
  mutate(across(all_of(str_col), str_trim)) %>% 
  mutate(across(all_of(numeric_col), as.numeric))

#------------

data.col.sel <- adult.data.train %>% select(age)
avector <- as.vector(unlist(data.col.sel))

#------------

Binning <- function(avector, n_binn){
  
  if (is.vector(avector)==TRUE) {
    
    avector.factor <- as.factor(cut_number(avector, n_binn))
    
    # levels(avector.factor)
    n <- nlevels(avector.factor)
    ls <- as.list(levels(avector.factor))
    bins <- c(-Inf)
    
    for(i in 1:n) {
      row <- ls[[i]]
      elem <- str_split(row, ',' , simplify = TRUE)
      elem_size <- length(elem)
      for(j in 1:elem_size) {
        elem_str <- elem[1,j]
        elem_number <- parse_number(elem_str)
        bins <- append(bins, elem_number)
      }
    }
    bins <- unique(append(bins, Inf))
    bins <- bins[order(bins)]
    return(bins)
    
  } else {
    
    return(NULL)
  }
  
}

#------------

bin <- Binning(avector, 6)
adult.data.train$age_bin <- cut(adult.data.train$age, breaks = bin)

table(adult.data.train$age_bin, adult.data.train$income_class)