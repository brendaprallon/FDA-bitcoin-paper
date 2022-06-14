### Accuracy plots: winning models ###

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

plot_accuracy = function(min_obs, algorithm = c("rf","svm", "gbm", "multi")){
    label = paste0(algorithm, "_", min_obs, "obs")
    if (algorithm == "rf"){
        title = "Accuracy of Random Forest Models"
    }else if (algorithm == "svm"){
        title = "Accuracy of Support Vector Machine Models"
    }else if (algorithm == "gbm"){
        title = "Accuracy of Gradient Boosting Models"
    }else if (algorithm == "multi"){
        title = "Accuracy of Mutinomial Logit Models"
    }
    # Accuracy plot
    model = readRDS(paste0("models/test/multiclass/winning_", algorithm, "_", min_obs, "obs.rds"))
    model = model[[algorithm]]
    model = model[names(model) != "median_functional_rate"]
    acc_out = lapply(model, function(x) x[["acc_out_of_sample"]])
    acc_in = lapply(model, function(x) x[["acc_in_sample"]])
    acc_dt = rbind(data.frame(acc_in), data.frame(acc_out))
    names(acc_dt) = c("Vec.", "Vec. + Const. Rate", "Vec. + Fun. Rate","Vec. + Fun.", "Fun.", "Vec. + Deriv. + Fun. Rate")
    acc_dt$type = c("In Sample", "Out of Sample")
    acc_dt = melt(data.table(acc_dt), id.vars = "type")
    acc_plt = ggplot(acc_dt, aes(x = variable, y = value, fill = type)) + 
        ggtitle(paste(title, "Min", min_obs, "Obs.")) +
        xlab("") + 
        ylab("") +
        geom_bar(position = "dodge", stat = "identity") + 
        scale_fill_manual(name = "", values = c("In Sample" = "#A6CEE3", "Out of Sample" = "#1F78B4")) + 
        geom_text(aes(label=round(value,3)), vjust=1.6, color="white",
                  position = position_dodge(0.9), size=3.5)+
        theme_minimal()
    ggsave(paste0("graphics/accuracy/acc_", algorithm, "_", min_obs, "obs.png"), acc_plt, width = 10, height = 8)
    # Accuracy by category plot - simplifying only for winning model
    acc_cat_list = lapply(model, acc_cat_table)
    for (i in 1:length(acc_cat_list)){
        names(acc_cat_list[[i]])[2] = names(model)[i]
    }
    acc_cat_dt = Reduce(merge, acc_cat_list)
    names(acc_cat_dt)[1] = "category"
    acc_cat_dt = data.table(rename_to_english(acc_cat_dt))
    if (algorithm == "multi"){
        acc_cat_dt = acc_cat_dt[, .(category, variable, functional)]
        title_cat = paste(title, "by Category (Fun.) - Min", min_obs, "Obs.")
        acc_cat_plt = ggplot(acc_cat_dt, aes(x = category, y = functional, fill = variable)) + 
            ggtitle(title_cat) +
            xlab("") + 
            ylab("") +
            geom_bar(position = "dodge", stat = "identity") + 
            scale_fill_manual(name = "", values = c("In Sample" = "#A6CEE3", "Out of Sample" = "#1F78B4")) + 
            geom_text(aes(label=round(functional,3)), vjust=1.6, color="white",
                      position = position_dodge(0.9), size=3.5)+
            theme_minimal()
    }else{
        acc_cat_dt = acc_cat_dt[, .(category, variable, vector_functional)]
        title_cat = paste(title, "by Category (Vec. + Fun.) - Min", min_obs, "Obs.")
        acc_cat_plt = ggplot(acc_cat_dt, aes(x = category, y = vector_functional, fill = variable)) + 
            ggtitle(title_cat) +
            xlab("") + 
            ylab("") +
            geom_bar(position = "dodge", stat = "identity") + 
            scale_fill_manual(name = "", values = c("In Sample" = "#A6CEE3", "Out of Sample" = "#1F78B4")) + 
            geom_text(aes(label=round(vector_functional,3)), vjust=1.6, color="white",
                      position = position_dodge(0.9), size=3.5)+
            theme_minimal()
    }
    ggsave(paste0("graphics/accuracy/acc_cat_", algorithm, "_", min_obs, "obs.png"), acc_cat_plt, width = 12, height = 8)  
}


# RUN ---------------------------------------------------------------------

# 10 obs
plot_accuracy(min_obs = 10, algorithm = "rf")
plot_accuracy(min_obs = 10, algorithm = "svm")
plot_accuracy(min_obs = 10, algorithm = "gbm")
plot_accuracy(min_obs = 10, algorithm = "multi")

# 20 obs
plot_accuracy(min_obs = 20, algorithm = "rf")
plot_accuracy(min_obs = 20, algorithm = "svm")
plot_accuracy(min_obs = 20, algorithm = "gbm")
plot_accuracy(min_obs = 20, algorithm = "multi")