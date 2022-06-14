#### Models ####

# Packages 
library(data.table)
library(randomForest)
library(nnet)
library(gbm)
library(e1071)


# Functions ---------------------------------------------------------------
source("R/fpca_model_frame.R")

# Updating model response
# Updating model response
results = function(results_list, model_name, model, i, dt_train, dt_val, binary, gbm = FALSE, ntrees = NULL){
    dt_train = copy(dt_train)
    dt_val = copy(dt_val)
    if(gbm == TRUE){
        pred_val = predict(model, dt_val, n.trees = ntrees)
        pred_train = predict(model, dt_train, n.trees = ntrees)
        if (binary == TRUE){
            dt_val[, pred := ifelse(pred_val> 0.5, 1, 0)]
            dt_train[, pred := ifelse(pred_train> 0.5, 1, 0)]
        }else{
            dt_val[, pred := attributes(pred_val)$dimnames[[2]][apply(pred_val, 1, which.max)]]
            dt_train[, pred := attributes(pred_train)$dimnames[[2]][apply(pred_train, 1, which.max)]]
        }
    }else{
        dt_val[, pred := predict(model, dt_val)]
        dt_train[, pred := predict(model, dt_train)]
    }
    results_list[[model_name]][["acc_out_of_sample"]][[i]] = sum(dt_val$pred == dt_val$category)/nrow(dt_val)
    acc_out_of_sample_cat = data.frame(setorder(dt_val[, sum(category == pred)/.N, by = category], category))
    rownames(acc_out_of_sample_cat) = acc_out_of_sample_cat$category
    acc_out_of_sample_cat$category =  NULL
    results_list[[model_name]][["acc_out_of_sample_cat"]][[i]] = acc_out_of_sample_cat
    results_list[[model_name]][["acc_in_sample"]][[i]] = sum(dt_train$pred == dt_train$category)/nrow(dt_train)
    acc_in_sample_cat = data.frame(setorder(dt_train[, sum(category == pred)/.N, by = category], category))
    rownames(acc_in_sample_cat) = acc_in_sample_cat$category
    acc_in_sample_cat$category =  NULL
    results_list[[model_name]][["acc_in_sample_cat"]][[i]] = acc_in_sample_cat
    cats = unique(dt_train$category)
    results_list[[model_name]][["confusion_matrix_out_of_sample"]][[i]] = as.data.frame.matrix(
        table(factor(dt_val$category, levels = cats), factor(dt_val$pred, levels = cats)))
    results_list[[model_name]][["confusion_matrix_in_sample"]][[i]] = as.data.frame.matrix(
        table(factor(dt_train$category, levels = cats), factor(dt_train$pred, levels = cats)))
    return(results_list)
}

# Evaluating Models
experiment = function(dt_curves, dt_rate, dt_scalar, min_obs, n_fpcs, lambda_smooth, binary = FALSE){
    if (binary == TRUE){
        dt_scalar[category != "Darknet Marketplace", category := "0"]
        dt_rate[category != "Darknet Marketplace", category := "0"]
        dt_curves[category != "Darknet Marketplace", category := "0"]
        dt_scalar[category == "Darknet Marketplace", category := "1"]
        dt_rate[category == "Darknet Marketplace", category := "1"]
        dt_curves[category == "Darknet Marketplace", category := "1"]
        kfolds_addr = readRDS(paste0("data/treated/partitions/binary_kfold_addr_k5_", min_obs, "obs.rds"))
    }else{
        kfolds_addr = readRDS(paste0("data/treated/partitions/kfold_addr_k5_", min_obs, "obs.rds"))
    }
    metrics = list("acc_in_sample" = c(), "acc_out_of_sample" = c(), 
                   "acc_in_sample_cat" = list(), "acc_out_of_sample_cat" = list(), "confusion_matrix_in_sample" = list(),
                   "confusion_matrix_out_of_sample" = list())
    results_rf = list("vector" = metrics , "vector_const_rate" = metrics,  "vector_functional_rate" = metrics, 
                      "median_functional_rate" = metrics, "vector_functional" = metrics, "functional" = metrics, 
                      "vector_deriv_functional_rate" = metrics)
    results_svm = list("vector" = metrics , "vector_const_rate" = metrics,  "vector_functional_rate" = metrics, 
                       "median_functional_rate" = metrics, "vector_functional" = metrics, "functional" = metrics,
                       "vector_deriv_functional_rate" = metrics)
    results_multi = list("vector" = metrics , "vector_const_rate" = metrics,  "vector_functional_rate" = metrics, 
                         "median_functional_rate" = metrics, "vector_functional" = metrics, "functional" = metrics,
                         "vector_deriv_functional_rate" = metrics)
    results_gbm = list("vector" = metrics , "vector_const_rate" = metrics,  "vector_functional_rate" = metrics, 
                       "median_functional_rate" = metrics, "vector_functional" = metrics, "functional" = metrics,
                       "vector_deriv_functional_rate" = metrics)
    varprop = list()
    for (i in 1:length(kfolds_addr)){
        addr_val = kfolds_addr[[i]]
        addr_train = as.character(unlist(kfolds_addr[c(1:5)[-i]]))
        model_frame = build_model_frame(dt_scalar, dt_rate, dt_curves, min_obs, addr_train, addr_val, lambda_smooth, n_fpcs)
        dt_train = model_frame$train
        dt_val = model_frame$test
        varprop[[i]] = model_frame$varprop
        ### Variables
        ## vector (no constant poisson rate)
        # linear
        linear_vector_credit_ft = c("N_credit", "cumsum_credit", "q1_credit", "median_credit", "q3_credit",
                                    "min_credit", "max_credit", "var_credit", "interval_time_credit")
        linear_vector_debt_ft = c("N_debt", "cumsum_debt", "q1_debt", "median_debt", "q3_debt",
                                  "min_debt", "max_debt", "var_debt", "interval_time_debt")
        # RF
        rf_vector_credit_ft = c(linear_vector_credit_ft, "iqr_credit")
        rf_vector_debt_ft = c(linear_vector_debt_ft, "iqr_debt")
        # SVM
        svm_vector_credit_ft = rf_vector_credit_ft
        svm_vector_debt_ft = rf_vector_debt_ft
        # GBM
        gbm_vector_credit_ft = rf_vector_credit_ft
        gbm_vector_debt_ft = rf_vector_debt_ft
        
        ## Functional Curves (no difference between random forest and linear)
        functional_curves_credit_ft = names(dt_train)[grepl("(?=.*FPC)(?=.*curves)(?=.*credit)", names(dt_train), perl = T)]
        functional_curves_debt_ft = names(dt_train)[grepl("(?=.*FPC)(?=.*curves)(?=.*debt)", names(dt_train), perl = T)]
        
        ## Functional Derivatives
        functional_deriv_credit_ft = names(dt_train)[grepl("(?=.*FPC)(?=.*deriv)(?=.*credit)", names(dt_train), perl = T)]
        functional_deriv_debt_ft = names(dt_train)[grepl("(?=.*FPC)(?=.*deriv)(?=.*debt)", names(dt_train), perl = T)]
        
        ## Functional Rates
        functional_rates_credit_ft = names(dt_train)[grepl("(?=.*FPC)(?=.*rates)(?=.*credit)", names(dt_train), perl = T)]
        functional_rates_debt_ft = names(dt_train)[grepl("(?=.*FPC)(?=.*rates)(?=.*debt)", names(dt_train), perl = T)]
        
        ### Random Forest Models ###
        # Vector
        vector_rf = randomForest(as.formula(paste("category ~", 
                                                  paste(rf_vector_credit_ft, collapse = " + "), "+",
                                                  paste(rf_vector_debt_ft, collapse = " + "))),
                                 data = dt_train, ntree = 100)
        results_rf = results(results_rf, "vector", vector_rf, i, dt_train, dt_val, binary)
        # Vector + const. Poisson rate
        vector_const_rate_rf = randomForest(as.formula(paste("category ~", 
                                                             paste(rf_vector_credit_ft, collapse = " + "), "+",
                                                             paste(rf_vector_debt_ft, collapse = " + "), 
                                                             "+ constant_poisson_rate_credit + constant_poisson_rate_debt")),
                                            data = dt_train, ntree = 100)
        results_rf = results(results_rf, "vector_const_rate", vector_const_rate_rf, i, dt_train, dt_val, binary)
        # Vector + func. Poisson rate
        vector_functional_rate_rf = randomForest(as.formula(paste("category ~", 
                                                                  paste(rf_vector_credit_ft, collapse = " + "), "+",
                                                                  paste(rf_vector_debt_ft, collapse = " + "), "+",
                                                                  paste(functional_rates_credit_ft, collapse = " + "), "+",
                                                                  paste(functional_rates_debt_ft, collapse = " + "))),
                                                 data = dt_train, ntree = 100)
        results_rf = results(results_rf, "vector_functional_rate", vector_functional_rate_rf, i, dt_train, dt_val, binary)
        # median + func. Poisson rate
        median_functional_rate_rf = randomForest(as.formula(paste("category ~", 
                                                                  "median_credit + median_debt +",
                                                                  paste(rf_vector_debt_ft, collapse = " + "), "+",
                                                                  paste(functional_rates_credit_ft, collapse = " + "), "+",
                                                                  paste(functional_rates_debt_ft, collapse = " + "))),
                                                 data = dt_train, ntree = 100)
        results_rf = results(results_rf, "median_functional_rate", median_functional_rate_rf, i, dt_train, dt_val, binary)
        # Vector + Functional
        vector_functional_rf = randomForest(as.formula(paste("category ~", 
                                                             paste(rf_vector_credit_ft, collapse = " + "), "+",
                                                             paste(rf_vector_debt_ft, collapse = " + "), "+",
                                                             paste(functional_rates_credit_ft, collapse = " + "), "+",
                                                             paste(functional_rates_debt_ft, collapse = " + "), "+",
                                                             paste(functional_curves_credit_ft, collapse = " + "), "+",
                                                             paste(functional_curves_debt_ft, collapse = " + "), "+",
                                                             paste(functional_deriv_credit_ft, collapse = " + "), "+",
                                                             paste(functional_deriv_debt_ft, collapse = " + "))),
                                            data = dt_train, ntree = 100)
        results_rf = results(results_rf, "vector_functional", vector_functional_rf, i, dt_train, dt_val, binary)
        # Functional 
        functional_rf = randomForest(as.formula(paste("category ~", 
                                                      paste(functional_rates_credit_ft, collapse = " + "), "+",
                                                      paste(functional_rates_debt_ft, collapse = " + "), "+",
                                                      paste(functional_curves_credit_ft, collapse = " + "), "+",
                                                      paste(functional_curves_debt_ft, collapse = " + "), "+",
                                                      paste(functional_deriv_credit_ft, collapse = " + "), "+",
                                                      paste(functional_deriv_debt_ft, collapse = " + "))),
                                     data = dt_train, ntree = 100)
        results_rf = results(results_rf, "functional", functional_rf, i, dt_train, dt_val, binary)
        
        # Vector + derivative + functional poisson rate
        vector_deriv_functional_rate_rf = randomForest(as.formula(paste("category ~", 
                                                                        paste(rf_vector_credit_ft, collapse = " + "), "+",
                                                                        paste(rf_vector_debt_ft, collapse = " + "), "+",
                                                                        paste(functional_rates_credit_ft, collapse = " + "), "+",
                                                                        paste(functional_rates_debt_ft, collapse = " + "), "+",
                                                                        paste(functional_deriv_credit_ft, collapse = " + "), "+",
                                                                        paste(functional_deriv_debt_ft, collapse = " + "))),
                                                       data = dt_train, ntree = 100)
        results_rf = results(results_rf, "vector_deriv_functional_rate", vector_deriv_functional_rate_rf, i, dt_train, dt_val, binary)
        
        ### SVM Models
        # Vector
        vector_svm = svm(as.formula(paste("category ~", 
                                                  paste(svm_vector_credit_ft, collapse = " + "), "+",
                                                  paste(svm_vector_debt_ft, collapse = " + "))),
                                 data = dt_train)
        results_svm = results(results_svm, "vector", vector_svm, i, dt_train, dt_val, binary)
        # Vector + const. Poisson rate
        vector_const_rate_svm = svm(as.formula(paste("category ~", 
                                                             paste(svm_vector_credit_ft, collapse = " + "), "+",
                                                             paste(svm_vector_debt_ft, collapse = " + "), 
                                                             "+ constant_poisson_rate_credit + constant_poisson_rate_debt")),
                                            data = dt_train)
        results_svm = results(results_svm, "vector_const_rate", vector_const_rate_svm, i, dt_train, dt_val, binary)
        # Vector + func. Poisson rate
        vector_functional_rate_svm = svm(as.formula(paste("category ~", 
                                                                  paste(svm_vector_credit_ft, collapse = " + "), "+",
                                                                  paste(svm_vector_debt_ft, collapse = " + "), "+",
                                                                  paste(functional_rates_credit_ft, collapse = " + "), "+",
                                                                  paste(functional_rates_debt_ft, collapse = " + "))),
                                                 data = dt_train)
        results_svm = results(results_svm, "vector_functional_rate", vector_functional_rate_svm, i, dt_train, dt_val, binary)
        # median + func. Poisson rate
        median_functional_rate_svm = svm(as.formula(paste("category ~", 
                                                                  "median_credit + median_debt +",
                                                                  paste(svm_vector_debt_ft, collapse = " + "), "+",
                                                                  paste(functional_rates_credit_ft, collapse = " + "), "+",
                                                                  paste(functional_rates_debt_ft, collapse = " + "))),
                                                 data = dt_train)
        results_svm = results(results_svm, "median_functional_rate", median_functional_rate_svm, i, dt_train, dt_val, binary)
        # Vector + Functional
        vector_functional_svm = svm(as.formula(paste("category ~", 
                                                             paste(svm_vector_credit_ft, collapse = " + "), "+",
                                                             paste(svm_vector_debt_ft, collapse = " + "), "+",
                                                             paste(functional_rates_credit_ft, collapse = " + "), "+",
                                                             paste(functional_rates_debt_ft, collapse = " + "), "+",
                                                             paste(functional_curves_credit_ft, collapse = " + "), "+",
                                                             paste(functional_curves_debt_ft, collapse = " + "), "+",
                                                             paste(functional_deriv_credit_ft, collapse = " + "), "+",
                                                             paste(functional_deriv_debt_ft, collapse = " + "))),
                                            data = dt_train)
        results_svm = results(results_svm, "vector_functional", vector_functional_svm, i, dt_train, dt_val, binary)
        # Functional 
        functional_svm = svm(as.formula(paste("category ~", 
                                                      paste(functional_rates_credit_ft, collapse = " + "), "+",
                                                      paste(functional_rates_debt_ft, collapse = " + "), "+",
                                                      paste(functional_curves_credit_ft, collapse = " + "), "+",
                                                      paste(functional_curves_debt_ft, collapse = " + "), "+",
                                                      paste(functional_deriv_credit_ft, collapse = " + "), "+",
                                                      paste(functional_deriv_debt_ft, collapse = " + "))),
                                     data = dt_train)
        results_svm = results(results_svm, "functional", functional_svm, i, dt_train, dt_val, binary)
        
        # Vector + derivative + functional poisson rate
        vector_deriv_functional_rate_svm = svm(as.formula(paste("category ~", 
                                                                        paste(svm_vector_credit_ft, collapse = " + "), "+",
                                                                        paste(svm_vector_debt_ft, collapse = " + "), "+",
                                                                        paste(functional_rates_credit_ft, collapse = " + "), "+",
                                                                        paste(functional_rates_debt_ft, collapse = " + "), "+",
                                                                        paste(functional_deriv_credit_ft, collapse = " + "), "+",
                                                                        paste(functional_deriv_debt_ft, collapse = " + "))),
                                                       data = dt_train)
        results_svm = results(results_svm, "vector_deriv_functional_rate", vector_deriv_functional_rate_svm, i, dt_train, dt_val, binary)
        
        ### Multinomial Logit Models ###
        # Vector
        vector_multi = multinom(as.formula(paste("category ~", 
                                                 paste(linear_vector_credit_ft, collapse = " + "), "+",
                                                 paste(linear_vector_debt_ft, collapse = " + "))),
                                data = dt_train)
        results_multi = results(results_multi, "vector", vector_multi, i, dt_train, dt_val, binary)
        # Vector + const. Poisson rate
        vector_const_rate_multi = multinom(as.formula(paste("category ~", 
                                                            paste(linear_vector_credit_ft, collapse = " + "), "+",
                                                            paste(linear_vector_debt_ft, collapse = " + "), 
                                                            "+ constant_poisson_rate_credit + constant_poisson_rate_debt")),
                                           data = dt_train)
        results_multi = results(results_multi, "vector_const_rate", vector_const_rate_multi, i, dt_train, dt_val, binary)
        # Vector + func. Poisson rate
        vector_functional_rate_multi = multinom(as.formula(paste("category ~", 
                                                                 paste(linear_vector_credit_ft, collapse = " + "), "+",
                                                                 paste(linear_vector_debt_ft, collapse = " + "), "+",
                                                                 paste(functional_rates_credit_ft, collapse = " + "), "+",
                                                                 paste(functional_rates_debt_ft, collapse = " + "))),
                                                data = dt_train)
        results_multi = results(results_multi, "vector_functional_rate", vector_functional_rate_multi, i, dt_train, dt_val, binary)
        # median + func. Poisson rate
        median_functional_rate_multi = multinom(as.formula(paste("category ~", 
                                                                 "median_credit + median_debt +",
                                                                 paste(linear_vector_debt_ft, collapse = " + "), "+",
                                                                 paste(functional_rates_credit_ft, collapse = " + "), "+",
                                                                 paste(functional_rates_debt_ft, collapse = " + "))),
                                                data = dt_train)
        results_multi = results(results_multi, "median_functional_rate", median_functional_rate_multi, i, dt_train, dt_val, binary)
        # Vector + Functional
        vector_functional_multi = multinom(as.formula(paste("category ~", 
                                                            paste(linear_vector_credit_ft, collapse = " + "), "+",
                                                            paste(linear_vector_debt_ft, collapse = " + "), "+",
                                                            paste(functional_rates_credit_ft, collapse = " + "), "+",
                                                            paste(functional_rates_debt_ft, collapse = " + "), "+",
                                                            paste(functional_curves_credit_ft, collapse = " + "), "+",
                                                            paste(functional_curves_debt_ft, collapse = " + "), "+",
                                                            paste(functional_deriv_credit_ft, collapse = " + "), "+",
                                                            paste(functional_deriv_debt_ft, collapse = " + "))),
                                           data = dt_train)
        results_multi = results(results_multi, "vector_functional", vector_functional_multi, i, dt_train, dt_val, binary)
        # Functional 
        functional_multi = multinom(as.formula(paste("category ~", 
                                                     paste(functional_rates_credit_ft, collapse = " + "), "+",
                                                     paste(functional_rates_debt_ft, collapse = " + "), "+",
                                                     paste(functional_curves_credit_ft, collapse = " + "), "+",
                                                     paste(functional_curves_debt_ft, collapse = " + "), "+",
                                                     paste(functional_deriv_credit_ft, collapse = " + "), "+",
                                                     paste(functional_deriv_debt_ft, collapse = " + "))),
                                    data = dt_train, )
        results_multi = results(results_multi, "functional", functional_multi, i, dt_train, dt_val, binary)
        
        # Vector + derivative + functional poisson rate
        vector_deriv_functional_rate_multi = multinom(as.formula(paste("category ~", 
                                                                       paste(linear_vector_credit_ft, collapse = " + "), "+",
                                                                       paste(linear_vector_debt_ft, collapse = " + "), "+",
                                                                       paste(functional_rates_credit_ft, collapse = " + "), "+",
                                                                       paste(functional_rates_debt_ft, collapse = " + "), "+",
                                                                       paste(functional_deriv_credit_ft, collapse = " + "), "+",
                                                                       paste(functional_deriv_debt_ft, collapse = " + "))),
                                                      data = dt_train)
        results_multi = results(results_multi, "vector_deriv_functional_rate", 
                                vector_deriv_functional_rate_multi, i, dt_train, dt_val, binary)
        
        ### Gradient Boosting Models ###
        if (binary == TRUE){
            dt_train[, category := as.numeric(as.character(category))]
            dt_val[, category := as.numeric(as.character(category))]
        }
        # Vector
        vector_gbm = gbm(as.formula(paste("category ~",
                                          paste(gbm_vector_credit_ft, collapse = " + "), "+",
                                          paste(gbm_vector_debt_ft, collapse = " + "))),
                         data = dt_train, n.trees = 100)
        results_gbm = results(results_gbm, "vector", vector_gbm, i, dt_train, dt_val, binary, gbm = TRUE, ntrees = 100)
        # Vector + const. Poisson rate
        vector_const_rate_gbm = gbm(as.formula(paste("category ~",
                                                     paste(gbm_vector_credit_ft, collapse = " + "), "+",
                                                     paste(gbm_vector_debt_ft, collapse = " + "),
                                                     "+ constant_poisson_rate_credit + constant_poisson_rate_debt")),
                                    data = dt_train, n.trees = 100)
        results_gbm = results(results_gbm, "vector_const_rate", vector_const_rate_gbm, i, dt_train, dt_val, binary, gbm = TRUE, ntrees = 100)
        # Vector + func. Poisson rate
        vector_functional_rate_gbm = gbm(as.formula(paste("category ~",
                                                          paste(gbm_vector_credit_ft, collapse = " + "), "+",
                                                          paste(gbm_vector_debt_ft, collapse = " + "), "+",
                                                          paste(functional_rates_credit_ft, collapse = " + "), "+",
                                                          paste(functional_rates_debt_ft, collapse = " + "))),
                                         data = dt_train, n.trees = 100)
        results_gbm = results(results_gbm, "vector_functional_rate", vector_functional_rate_gbm, i, dt_train, dt_val, binary, gbm = TRUE, ntrees = 100)
        # median + func. Poisson rate
        median_functional_rate_gbm = gbm(as.formula(paste("category ~",
                                                          "median_credit + median_debt +",
                                                          paste(gbm_vector_debt_ft, collapse = " + "), "+",
                                                          paste(functional_rates_credit_ft, collapse = " + "), "+",
                                                          paste(functional_rates_debt_ft, collapse = " + "))),
                                         data = dt_train, n.trees = 100)
        results_gbm = results(results_gbm, "median_functional_rate", median_functional_rate_gbm, i, dt_train, dt_val, binary, gbm = TRUE, ntrees = 100)
        # Vector + Functional
        vector_functional_gbm = gbm(as.formula(paste("category ~",
                                                     paste(gbm_vector_credit_ft, collapse = " + "), "+",
                                                     paste(gbm_vector_debt_ft, collapse = " + "), "+",
                                                     paste(functional_rates_credit_ft, collapse = " + "), "+",
                                                     paste(functional_rates_debt_ft, collapse = " + "), "+",
                                                     paste(functional_curves_credit_ft, collapse = " + "), "+",
                                                     paste(functional_curves_debt_ft, collapse = " + "), "+",
                                                     paste(functional_deriv_credit_ft, collapse = " + "), "+",
                                                     paste(functional_deriv_debt_ft, collapse = " + "))),
                                    data = dt_train, n.trees = 100)
        results_gbm = results(results_gbm, "vector_functional", vector_functional_gbm, i, dt_train, dt_val, binary, gbm = TRUE, ntrees = 100)
        # Functional
        functional_gbm = gbm(as.formula(paste("category ~",
                                              paste(functional_rates_credit_ft, collapse = " + "), "+",
                                              paste(functional_rates_debt_ft, collapse = " + "), "+",
                                              paste(functional_curves_credit_ft, collapse = " + "), "+",
                                              paste(functional_curves_debt_ft, collapse = " + "), "+",
                                              paste(functional_deriv_credit_ft, collapse = " + "), "+",
                                              paste(functional_deriv_debt_ft, collapse = " + "))),
                             data = dt_train, n.trees = 100)
        results_gbm = results(results_gbm, "functional", functional_gbm, i, dt_train, dt_val, binary, gbm = TRUE, ntrees = 100)
        
        # Vector + derivative + functional poisson rate
        vector_deriv_functional_rate_gbm = gbm(as.formula(paste("category ~", 
                                                                paste(gbm_vector_credit_ft, collapse = " + "), "+",
                                                                paste(gbm_vector_debt_ft, collapse = " + "), "+",
                                                                paste(functional_rates_credit_ft, collapse = " + "), "+",
                                                                paste(functional_rates_debt_ft, collapse = " + "), "+",
                                                                paste(functional_deriv_credit_ft, collapse = " + "), "+",
                                                                paste(functional_deriv_debt_ft, collapse = " + "))),
                                               data = dt_train, n.trees = 100)
        results_gbm = results(results_gbm, "vector_deriv_functional_rate", 
                              vector_deriv_functional_rate_gbm, i, dt_train, dt_val, binary, gbm = TRUE, ntrees = 100)
    }
    # Mean of varprop
    mean_varprop = list("credit" = list(), "debt" = list())
    for (name in names(varprop[[1]]$credit)){
        mean_varprop$credit[[name]] = mean(unlist(lapply(varprop, function(x) x$credit[[name]])))
        mean_varprop$debt[[name]] = mean(unlist(lapply(varprop, function(x) x$debt[[name]])))
    }
    
    # Mean of cross validated results
    mean_results_rf = list()
    mean_results_svm = list()
    mean_results_gbm = list()
    mean_results_multi = list()
    for (name in names(results_rf)){
        for (metric in names(results_rf[[name]])){
            mean_results_rf[[name]][[metric]] = Reduce("+", results_rf[[name]][[metric]]) / length(results_rf[[name]][[metric]])
            mean_results_gbm[[name]][[metric]] = Reduce("+", results_gbm[[name]][[metric]]) / length(results_gbm[[name]][[metric]])
            mean_results_multi[[name]][[metric]] = Reduce("+", results_multi[[name]][[metric]]) / length(results_multi[[name]][[metric]])
            mean_results_svm[[name]][[metric]] = Reduce("+", results_svm[[name]][[metric]]) / length(results_svm[[name]][[metric]])
        }
    }
    return(list("rf" = mean_results_rf, "gbm" = mean_results_gbm, "multi" = mean_results_multi,
                "svm" = mean_results_svm, "varprop" = mean_varprop))
}





