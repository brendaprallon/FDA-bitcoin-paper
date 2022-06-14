#### Features for the Vector Model ####

# Packages
library(data.table)


# Functions ---------------------------------------------------------------
scalar_features = function(dt){
    # Creating scalar features
    # count for credits and debts
    dt[, N := .N, by = c("address", "increment_type")]
    # accumulated sum
    setorder(dt, address, category, ts_normal)
    dt[, cumsum := last(int_increments), by = c("address", "increment_type")]
    # quantiles
    dt[, q1 := quantile(increments)[2], by = c("address", "increment_type")]
    dt[, median := quantile(increments)[3], by = c("address", "increment_type")]
    dt[, q3 := quantile(increments)[4], by = c("address", "increment_type")]
    dt[, iqr := q3 - q1]
    # min max
    dt[, min := min(increments), by = c("address", "increment_type")]
    dt[, max := max(increments), by = c("address", "increment_type")]
    # var
    dt[, var := var(increments), by = c("address", "increment_type")]
    # difference between first and last movements
    dt[, interval_time := (last(ts_normal) - first(ts_normal))*3000, by = c("address", "increment_type")]
    # constant poisson rate
    dt[interval_time != 0, constant_poisson_rate := N/interval_time]
    dt[interval_time == 0, constant_poisson_rate := 0]
    
    # selecting columns
    dt = unique(dt[, .(address, category, increment_type, count, N, cumsum, q1, median, q3, iqr, 
                       min, max, var, interval_time, constant_poisson_rate)])
    # wide format
    dt = dcast(dt, address + category + count ~ increment_type, value.var = names(dt)[5:ncol(dt)], fill = 0)
    # for wallets with no debts, fpcs are 0
    dt[is.na(dt)] = 0
    return(dt)
}


# RUN ---------------------------------------------------------------------

# 10 obs
dt = readRDS("data/treated/cred_debt_balanced_10obs.rds")
saveRDS(scalar_features(copy(dt)), "data/treated/scalar/scalar_features_10obs.rds")

# 20 obs
dt = readRDS("data/treated/cred_debt_balanced_20obs.rds")
saveRDS(scalar_features(copy(dt)), "data/treated/scalar/scalar_features_20obs.rds")
