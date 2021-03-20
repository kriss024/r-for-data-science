rm(list = ls())
library(datasets)
data(iris)
summary(iris)

library(h2o)
h2o.init(nthreads = -1, max_mem_size = "4G")

iris.hex <- as.h2o(iris)

ntrees_opts <- c(1, 5)
learn_rate_opts <- c(0.1, 0.01)
hyper_parameters <- list(ntrees = ntrees_opts, learn_rate = learn_rate_opts)

grid_id<-"gbm_grid_test"

grid <- h2o.grid("gbm", grid_id=grid_id, x=1:4, y=5, training_frame=iris.hex, hyper_params = hyper_parameters)
summary(grid)

model_ids <- grid@model_ids

grid_models <- lapply(grid@model_ids, function(mid) {
  model = h2o.getModel(mid)
})

print(grid_models)

stopping_metric <- 'accuracy'
sorted_models <- h2o.getGrid(
  grid_id = grid_id, 
  sort_by = stopping_metric,
  decreasing = TRUE
)
best_model <- h2o.getModel(sorted_models@model_ids[[1]])
print(best_model)




