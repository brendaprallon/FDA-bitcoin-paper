### Plot FPCs ###

library(ggplot2)
library(data.table)


# Functions ---------------------------------------------------------------
# number of functional principal components vs. variance
plot_nfpcs_vs_acc = function(min_obs){
    fpcs = c(1,3,5,7)
    fpc_models = lapply(paste0("models/test/multiclass/fpc", fpcs, "_rf_", min_obs, "obs.rds"), function(x) readRDS(x))
    acc_out = unlist(lapply(fpc_models, function(x) x[["rf"]][["vector_functional"]][["acc_out_of_sample"]]))
    varprop_curves_credit = unlist(lapply(fpc_models, function(x) x[["varprop"]][["credit"]][["curves"]]))
    varprop_curves_debt = unlist(lapply(fpc_models, function(x) x[["varprop"]][["debt"]][["curves"]]))
    varprop_derivatives_credit = unlist(lapply(fpc_models, function(x) x[["varprop"]][["credit"]][["derivatives"]]))
    varprop_derivatives_debt = unlist(lapply(fpc_models, function(x) x[["varprop"]][["debt"]][["derivatives"]]))
    varprop_rates_credit = unlist(lapply(fpc_models, function(x) x[["varprop"]][["credit"]][["rates"]]))
    varprop_rates_debt = unlist(lapply(fpc_models, function(x) x[["varprop"]][["debt"]][["rates"]]))
    dt = data.table(fpcs, acc_out, varprop_curves_credit, varprop_curves_debt, varprop_rates_debt, 
                    varprop_derivatives_credit, varprop_derivatives_debt, varprop_rates_credit)
    dt = melt(dt, id.vars = c("fpcs", "acc_out"))
    dt[variable == "varprop_curves_credit", variable := "Credit curves"]
    dt[variable == "varprop_curves_debt", variable := "Debt curves"]
    dt[variable == "varprop_derivatives_credit", variable := "Credit derivatives"]
    dt[variable == "varprop_derivatives_debt", variable := "Debt derivatives"]
    dt[variable == "varprop_rates_credit", variable := "Credit rates"]
    dt[variable == "varprop_rates_debt", variable := "Debt rates"]
    cols = c("Accuracy"="#F8766B","% Variance"="#00BFC4")
    plt = ggplot(dt) + 
        ggtitle("Explained variance and accuracy vs. number of principal components") +
        scale_x_continuous("N. functional principal components", breaks = c(1,3,5,7)) +
        ylab("Percentage") + 
        geom_point(aes(x = fpcs, y = acc_out, color = "Accuracy")) +
        geom_line(aes(x = fpcs, y = acc_out, color = "Accuracy")) +
        geom_point(aes(x = fpcs, y = value, color = "% Variance")) +
        geom_line(aes(x = fpcs, y = value, color = "% Variance")) +
        scale_colour_manual(values=cols) +
        theme(legend.title=element_blank()) + 
        facet_wrap("variable", scales = "free_y")
    ggsave(paste0("graphics/n fpcs/acc_vs_variance_", min_obs, "obs.png"), plt, width = 11, height = 6)
}


# RUN ---------------------------------------------------------------------

# 10 obs
plot_nfpcs_vs_acc(10)

# 20 obs
plot_nfpcs_vs_acc(20)
    
