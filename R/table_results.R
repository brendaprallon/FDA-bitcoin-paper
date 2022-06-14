#### Model Results ####

# Packages
library(data.table)

# Functions ---------------------------------------------------------------

# function to get data table for accuracy in categories
table_acc_cat = function(x, out = TRUE){
    if (out == TRUE){
        x = data.table(x[["acc_out_of_sample_cat"]], keep.rownames = TRUE)
        x[, rn := paste(rn, "out")]
    }else{
        x = data.table(x[["acc_in_sample_cat"]], keep.rownames = TRUE)
        x[, rn := paste(rn, "in")]
    }
    x = transpose(x, make.names = 'rn')
    return(x)
}

# function to create data table with model results
table_model_results = function(model, type){
    dt = data.table(model = names(model),
                    algorithm = type,
                    acc_out = unlist(lapply(model,  function(x) x[["acc_out_of_sample"]])),
                    acc_in = unlist(lapply(model,  function(x) x[["acc_in_sample"]])))
    dt = cbind(dt, rbindlist(lapply(model, table_acc_cat)))
    dt = cbind(dt, rbindlist(lapply(model, table_acc_cat, out = FALSE)))
    return(dt)
}


# function to create data table with all model results for aggregation and number of observations
all_results = function(min_obs){
    path = paste0("models/multiclass/min_obs_", min_obs, "/")
    files = list.files(path)
    models = lapply(files, function(x) readRDS(paste0(path, x)))
    names(models) = files
    dt_list = list()
    for (name in names(models)){
        rf_model = models[[name]]$rf
        dt_rf = table_model_results(rf_model, type = "random forest")
        svm_model = models[[name]]$svm
        dt_svm = table_model_results(svm_model, type = "svm")
        multi_model = models[[name]]$multi
        dt_multi = table_model_results(multi_model, type = "multi logit")
        gbm_model = models[[name]]$gbm
        dt_gbm = table_model_results(gbm_model, type = "gbm")
        dt = rbindlist(list(dt_rf, dt_svm, dt_multi, dt_gbm))
        dt[, names(dt)[3:ncol(dt)] := lapply(.SD, round, 3), .SDcols = names(dt)[3:ncol(dt)]]
        model_spec = gsub(".*?([0-9]+).*", "\\1", unlist(strsplit(name, "_", fixed = TRUE)))
        dt[, `:=` (lambda = as.numeric(model_spec[1]), 
                   n_fpcs = as.numeric(model_spec[2]), 
                   varprop_curves_credit = models[[name]]$varprop$credit$curves,
                   varprop_curves_debt = models[[name]]$varprop$debt$curves,
                   varprop_derivatives_credit = models[[name]]$varprop$credit$derivatives,
                   varprop_derivatives_debt = models[[name]]$varprop$debt$derivatives,
                   varprop_rates_credit = models[[name]]$varprop$credit$rates,
                   varprop_rates_debt = models[[name]]$varprop$debt$rates)]
        setcolorder(dt, names(dt)[c(15,16,1:14,17:22)]) 
        dt_list[[name]] = dt
    }
    return(rbindlist(dt_list))
}


# RUN ---------------------------------------------------------------------

aggr_10_obs = all_results(10)
saveRDS(setorder(aggr_10_obs[model != "median_functional_rate"], -acc_out), "results/min_10_obs.rds")

aggr_20_obs = all_results(20)
saveRDS(setorder(aggr_20_obs[model != "median_functional_rate"], -acc_out), "results/min_20_obs.rds")
