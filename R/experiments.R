########## Experiments ##########

## Packages ##
library(parallel)

source("R/models.R")

# Functions ---------------------------------------------------------------

## Parallel experiment function

parallel_experiment = function(param_list, dt_curves, dt_rate, dt_scalar){
  min_obs = param_list[["min_obs"]]
  n_fpcs = param_list[["n_fpcs"]]
  lambda_smooth = param_list[["lambda_smooth"]]
  binary = param_list[["binary"]]
  exp = experiment(dt_curves, dt_rate, dt_scalar, min_obs, n_fpcs, 10^(-lambda_smooth), binary)
  saveRDS(exp, paste0("models/multiclass/min_obs_", min_obs, "/lambda", lambda_smooth, "_nfpcs", n_fpcs,".rds"))
  return(NULL)
}


# RUN 10 obs --------------------------------------------------------------

set.seed(123)

dt_scalar = readRDS("data/treated/scalar/scalar_features_10obs.rds")
dt_rate = readRDS("data/treated/rates/poisson_rates_10obs.rds")
dt_curves = readRDS("data/treated/wallets_log_smoothed_balanced_10obs.rds")

param_list_10obs = list()
i = 1
for (lambda_smooth in c(1,10)){
  for (n_fpcs in c(1,3,5,7)){
    param_list_10obs[[i]] = list("min_obs" = 10, "lambda_smooth" = lambda_smooth, "n_fpcs" = n_fpcs, "binary" = FALSE)
    i = i + 1
  }
}

start_time = Sys.time()
mclapply(param_list_10obs, parallel_experiment, copy(dt_curves), copy(dt_rate), copy(dt_scalar))
print(Sys.time() - start_time)


# RUN 20 obs --------------------------------------------------------------

set.seed(123)

dt_scalar = readRDS("data/treated/scalar/scalar_features_20obs.rds")
dt_rate = readRDS("data/treated/rates/poisson_rates_20obs.rds")
dt_curves = readRDS("data/treated/wallets_log_smoothed_balanced_20obs.rds")

param_list_20obs = list()
i = 1
for (lambda_smooth in c(1,10)){
  for (n_fpcs in c(1,3,5,7)){
    param_list_20obs[[i]] = list("min_obs" = 20, "lambda_smooth" = lambda_smooth, "n_fpcs" = n_fpcs, "binary" = FALSE)
    i = i + 1
  }
}

start_time = Sys.time()
mclapply(param_list_20obs, parallel_experiment, copy(dt_curves), copy(dt_rate), copy(dt_scalar))
print(Sys.time() - start_time)
