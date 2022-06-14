#### Feature Importance plot of the winning model ###

# Packages #
library(data.table)
library(randomForest)
library(ggplot2)


# Functions ---------------------------------------------------------------

source("R/models.R")

# Evaluating Models

eval_model = function(min_obs, dt_curves, dt_rate, dt_scalar, n_fpcs, lambda_smooth, binary = FALSE,
                      algorithm = "random_forest"){
    if (binary == TRUE){
        dt_scalar[category != "Darknet Marketplace", category := "0"]
        dt_rate[category != "Darknet Marketplace", category := "0"]
        dt_curves[category != "Darknet Marketplace", category := "0"]
        dt_scalar[category == "Darknet Marketplace", category := "1"]
        dt_rate[category == "Darknet Marketplace", category := "1"]
        dt_curves[category == "Darknet Marketplace", category := "1"]
        kfolds_addr = readRDS(paste0("data/treated/partitions/binary_kfold_addr_k5_", min_obs, "obs.rds"))
        addr_test = readRDS(paste0("data/treated/partitions/binary_test_addr_p0.2_", min_obs, "obs.rds"))
    }else{
        kfolds_addr = readRDS(paste0("data/treated/partitions/kfold_addr_k5_", min_obs, "obs.rds"))
        addr_test = readRDS(paste0("data/treated/partitions/test_addr_p0.2_", min_obs, "obs.rds"))
    }

    models_rf = list()
    addr_train = as.character(unlist(kfolds_addr))
    model_frame = build_model_frame(dt_scalar, dt_rate, dt_curves, min_obs, addr_train, addr_test, lambda_smooth, n_fpcs)
    dt_train = model_frame$train
    dt_val = model_frame$test
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
    models_rf[["vector"]] = vector_rf
    # Vector + const. Poisson rate
    vector_const_rate_rf = randomForest(as.formula(paste("category ~", 
                                                         paste(rf_vector_credit_ft, collapse = " + "), "+",
                                                         paste(rf_vector_debt_ft, collapse = " + "), 
                                                         "+ constant_poisson_rate_credit + constant_poisson_rate_debt")),
                                        data = dt_train, ntree = 100)
    models_rf[["vector_const_rate"]] = vector_const_rate_rf
    # Vector + func. Poisson rate
    vector_functional_rate_rf = randomForest(as.formula(paste("category ~", 
                                                              paste(rf_vector_credit_ft, collapse = " + "), "+",
                                                              paste(rf_vector_debt_ft, collapse = " + "), "+",
                                                              paste(functional_rates_credit_ft, collapse = " + "), "+",
                                                              paste(functional_rates_debt_ft, collapse = " + "))),
                                             data = dt_train, ntree = 100)
    models_rf[["vector_functional_rate"]] = vector_functional_rate_rf
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
    models_rf[["vector_functional"]] = vector_functional_rf
    # Functional 
    functional_rf = randomForest(as.formula(paste("category ~", 
                                                  paste(functional_rates_credit_ft, collapse = " + "), "+",
                                                  paste(functional_rates_debt_ft, collapse = " + "), "+",
                                                  paste(functional_curves_credit_ft, collapse = " + "), "+",
                                                  paste(functional_curves_debt_ft, collapse = " + "), "+",
                                                  paste(functional_deriv_credit_ft, collapse = " + "), "+",
                                                  paste(functional_deriv_debt_ft, collapse = " + "))),
                                 data = dt_train, ntree = 100)
    models_rf[["functional"]] = functional_rf
        
    # Vector + derivative + functional poisson rate
    vector_deriv_functional_rate_rf = randomForest(as.formula(paste("category ~", 
                                                                    paste(rf_vector_credit_ft, collapse = " + "), "+",
                                                                    paste(rf_vector_debt_ft, collapse = " + "), "+",
                                                                    paste(functional_rates_credit_ft, collapse = " + "), "+",
                                                                    paste(functional_rates_debt_ft, collapse = " + "), "+",
                                                                    paste(functional_deriv_credit_ft, collapse = " + "), "+",
                                                                    paste(functional_deriv_debt_ft, collapse = " + "))),
                                                   data = dt_train, ntree = 100)
    models_rf[["vector_deriv_functional_rate"]] = vector_deriv_functional_rate_rf
    
    return(models_rf)
}

ft_importance_plt_aux = function(model_name, model_list, min_obs){
    # Plot feature importance
    plot_df = as.data.frame(importance(model_list[[model_name]]))
    plot_df$variable = factor(rownames(plot_df), levels = rownames(plot_df))
    
    plt = ggplot(plot_df, aes(x=reorder(variable, -MeanDecreaseGini), y=MeanDecreaseGini)) + 
        ggtitle(paste('Feature Importance:', model_name)) + 
        xlab("Variable") +
        geom_bar(stat = 'identity') +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
    model_filename = tolower(gsub(" ", "_",  
                                  gsub("\\s+", " ", 
                                       gsub("[^[:alnum:][:space:]]","", model_name))))
    ggsave(paste0("graphics/feature importance/feature_importance_model_", 
                  model_filename, "_", min_obs, "obs.png"), plt)
}

feature_importance_plt = function(min_obs){
    # data
    dt_scalar = readRDS(paste0("data/treated/scalar/scalar_features_", min_obs, "obs.rds"))
    dt_rate = readRDS(paste0("data/treated/rates/poisson_rates_", min_obs, "obs.rds"))
    dt_curves = readRDS(paste0("data/treated/wallets_log_smoothed_balanced_", min_obs, "obs.rds"))
    # result tables
    results = readRDS(paste0("results/min_", min_obs, "_obs.rds"))
    # winning models
    best_rf = results[algorithm == "random forest"][1]
    
    set.seed(123)
    
    rf_model = eval_model(min_obs, copy(dt_curves), copy(dt_rate), copy(dt_scalar),
                          n_fpcs = best_rf$n_fpcs, lambda_smooth = 10^(-best_rf$lambda),
                          binary = FALSE)
    names(rf_model) = c("Vec.", "Vec. + Const. Rate", "Vec. + Fun. Rate","Vec + Fun.", "Fun.", 
                        "Vec. + Deriv. + Fun. Rate")
    
    lapply(names(rf_model), ft_importance_plt_aux, rf_model, min_obs)
}


# RUN ---------------------------------------------------------------------

# Min 10 obs
feature_importance_plt(10)
#Min 20 obs
feature_importance_plt(20)
