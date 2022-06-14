########## Pre smoothing ##########

# Packages
library(dplyr)

# Auxiliary functions
source("R/aux_functions.R")

# Functions ---------------------------------------------------------------

# Function to evaluate function points 
points_of_smooth_function = function(address_hash, df, n_points, limit){
    df = df %>% filter(address == address_hash)
    credit = df %>% filter(increment_type == "credit")
    debt = df %>% filter(increment_type == "debt")
    x_vec_credit = credit$ts_normal
    y_vec_credit = credit$int_increments
    x_vec_debt = debt$ts_normal
    y_vec_debt = debt$int_increments
    ts_equal_vec = seq(from = 0, to = 1, length.out = n_points)
    eval_points_cred = unique(sort(c(ts_equal_vec, x_vec_credit)))
    eval_points_cred = clean_vec(eval_points_cred, limit)
    eval_points_debt = unique(sort(c(ts_equal_vec, debt$ts_normal)))
    eval_points_debt = clean_vec(eval_points_debt, limit)
    eval_step_function_credit = lapply(eval_points_cred, step_function, x_vec_credit, y_vec_credit) %>% 
        unlist()
    eval_step_function_debt = lapply(eval_points_debt, step_function, x_vec_debt, y_vec_debt) %>% 
        unlist()
    df_cred = tibble(address = address_hash, label = df$label[1], category = df$category[1],
                     increment_type = "credit", ts_normal = eval_points_cred,
                     int_increments = eval_step_function_credit, count = df$count[1])
    df_debt = tibble(address = address_hash, label = df$label[1], category = df$category[1],
                     increment_type = "debt", ts_normal = eval_points_debt,
                     int_increments = eval_step_function_debt, count = df$count[1])
    
    return(bind_rows(df_cred, df_debt))
}


# RUN ---------------------------------------------------------------------

start_time = Sys.time()

## 10 obs ##
wallets = readRDS("data/treated/cred_debt_balanced_10obs.rds")
wallets = lapply(unique(wallets$address), points_of_smooth_function, wallets, 501, 0.05) %>% 
    bind_rows() %>% group_by(address, increment_type) %>% arrange(ts_normal) %>% ungroup()
saveRDS(wallets, "data/treated/wallets_presmooth_balanced_10obs.rds")
print(Sys.time() - start_time)

# For full sample:
# wallets = readRDS("data/treated/cred_debt_complete_10obs.rds")
# wallets = lapply(unique(wallets$address), points_of_smooth_function, wallets, 501, 0.05) %>%
#   bind_rows() %>% group_by(address, increment_type) %>% arrange(ts_normal) %>% ungroup()
# saveRDS(wallets, "data/treated/wallets_presmooth_complete_10obs.rds")
# print(Sys.time() - start_time)

## 20 obs ##
wallets = readRDS("data/treated/cred_debt_balanced_20obs.rds")
wallets = lapply(unique(wallets$address), points_of_smooth_function, wallets, 501, 0.05) %>% 
  bind_rows() %>% group_by(address, increment_type) %>% arrange(ts_normal) %>% ungroup()
saveRDS(wallets, "data/treated/wallets_presmooth_balanced_20obs.rds")
print(Sys.time() - start_time)

# For full sample:
# wallets = readRDS("data/treated/cred_debt_complete_20obs.rds")
# wallets = lapply(unique(wallets$address), points_of_smooth_function, wallets, 501, 0.05) %>% 
#   bind_rows() %>% group_by(address, increment_type) %>% arrange(ts_normal) %>% ungroup()
# saveRDS(wallets, "data/treated/wallets_presmooth_complete_20obs.rds")
# print(Sys.time() - start_time)
