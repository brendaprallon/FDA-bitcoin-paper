### Latex Tables: Winning models ###

# Packages
library(data.table)
library(xtable)


# Functions ---------------------------------------------------------------

source("R/aux_functions.R")

acc_cat_table = function(x){
    acc_in = data.table(x[["acc_in_sample_cat"]], keep.rownames = TRUE)
    acc_in$variable = "In Sample"
    acc_out = data.table(x[["acc_out_of_sample_cat"]], keep.rownames = TRUE)
    acc_out$variable = "Out of Sample"
    return(rbind(acc_in, acc_out))
}

latex_table_results = function(min_obs, algorithm = c("rf","svm", "gbm", "multi")){
    label = paste0(algorithm, "_", min_obs, "obs")
    if (algorithm == "rf"){
        title = "Random Forest"
    }else if (algorithm == "svm"){
        title = "Support Vector Machine"
    }else if (algorithm == "gbm"){
        title = "Gradient Boosting"
    }else if (algorithm == "multi"){
        title = "Mutinomial Logit"
    }
    # Accuracy table
    model = readRDS(paste0("models/test/multiclass/winning_", algorithm, "_", min_obs, "obs.rds"))
    model = model[[algorithm]]
    model = model[names(model) != "median_functional_rate"]
    acc_out = lapply(model, function(x) x[["acc_out_of_sample"]])
    acc_in = lapply(model, function(x) x[["acc_in_sample"]])
    acc_dt = rbind(data.frame(acc_in), data.frame(acc_out))
    names(acc_dt) = c("Vec.", "Vec. + Const. Rate", "Vec. + Fun. Rate","Vec. + Fun.", "Fun.", "Vec. + Deriv. + Fun. Rate")
    rownames(acc_dt) = c("In Sample", "Out of Sample")
    print(xtable(acc_dt, label = paste0(label, "_acc"), caption = paste("Accuracy of", title, "Models - Min",
                                                                        min_obs, "Obs."), digits =3),
          file = paste0("tables/latex/", label, "_acc.tex"))
    # Accuracy by category table
    acc_cat_list = lapply(model, acc_cat_table)
    for (i in 1:length(acc_cat_list)){
        names(acc_cat_list[[i]])[2] = names(model)[i]
    }
    acc_cat_dt = Reduce(merge, acc_cat_list)
    acc_cat_dt$rn[c(2,4,6,8,10)] = rep("", 5)
    names(acc_cat_dt)[1] = "category"
    names(acc_cat_dt) = c("", "", "Vec.", "Vec. + Const. Rate", "Vec. + Fun. Rate","Vec + Fun.", "Fun.", 
                          "Vec. + Deriv. + Fun. Rate")
    print(xtable(acc_cat_dt, label = paste0(label, "_acc_cat"), caption = paste("Accuracy of", title, "Models by Category- Min",
                                                                        min_obs, "Obs."), digits =3),
          file = paste0("tables/latex/", label, "_acc_cat.tex"), include.rownames = FALSE)
}


# RUN ---------------------------------------------------------------------

# 10 obs
latex_table_results(min_obs = 10, algorithm = "rf")
latex_table_results(min_obs = 10, algorithm = "svm")
latex_table_results(min_obs = 10, algorithm = "gbm")
latex_table_results(min_obs = 10, algorithm = "multi")

# 20 obs
latex_table_results(min_obs = 20, algorithm = "rf")
latex_table_results(min_obs = 20, algorithm = "svm")
latex_table_results(min_obs = 20, algorithm = "gbm")
latex_table_results(min_obs = 20, algorithm = "multi")
