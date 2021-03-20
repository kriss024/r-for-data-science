library(h2o)
h2o.init(nthreads = -1, max_mem_size = "8G")
h2o.removeAll()

loan_csv <- "https://raw.githubusercontent.com/h2oai/app-consumer-loan/master/data/loan.csv"
data <- h2o.importFile(loan_csv) 
dim(data)

data$bad_loan <- as.factor(data$bad_loan) 
h2o.levels(data$bad_loan)

splits <- h2o.splitFrame(data = data, ratios = c(0.7, 0.15), seed = 1) 
train <- splits[[1]]
valid <- splits[[2]]
test <- splits[[3]]

nrow(train) 
nrow(valid) 
nrow(test) 

y <- "bad_loan"
#remove the interest rate column because it's correlated with the outcome
x <- setdiff(names(data), c(y, "int_rate"))  
print(x)

gbm_params1 <- list(learn_rate = c(0.01, 0.1),
                    max_depth = c(3, 5, 9),
                    sample_rate = c(0.8, 1.0),
                    col_sample_rate = c(0.2, 0.5, 1.0))

gbm_grid1 <- h2o.grid("gbm", x = x, y = y,
                      grid_id = "gbm_grid1",
                      training_frame = train,
                      validation_frame = valid,
                      ntrees = 100,
                      seed = 1,
                      hyper_params = gbm_params1)

gbm_gridperf1 <- h2o.getGrid(grid_id = "gbm_grid1", 
                             sort_by = "auc", 
                             decreasing = TRUE)
print(gbm_gridperf1)

best_gbm_model_id <- gbm_gridperf1@model_ids[[1]]
best_gbm <- h2o.getModel(best_gbm_model_id)

print(best_gbm)

best_gbm_perf <- h2o.performance(model = best_gbm, newdata = test)
h2o.auc(best_gbm_perf)

pred <- h2o.predict(best_gbm, newdata = test)
head(pred)