### Plotting eigenfunctions ###

# Packages #
library(data.table)
library(fda)
library(ggplot2)
library(ggpubr)


# Functions ---------------------------------------------------------------

plot_fpcas = function(min_obs, curve_type = c("curves", "derivatives", "rates"), 
                      cred_debt = c("credit", "debt"), lambda_smooth = 10^(-10)){
    if (curve_type == "curves" | curve_type == "derivatives"){
        dt = readRDS(paste0("data/treated/wallets_log_smoothed_balanced_",
                            min_obs, "obs.rds"))[increment_type == cred_debt]
    }else{
        dt = readRDS(paste0("data/treated/rates/poisson_rates_", min_obs, "obs.rds"))[increment_type == cred_debt]
    }
    
    kfolds_addr = readRDS(paste0("data/treated/partitions/kfold_addr_k5_", min_obs, "obs.rds"))
    addr_test = readRDS(paste0("data/treated/partitions/test_addr_p0.2_", min_obs, "obs.rds"))
    addr_train = as.character(unlist(kfolds_addr))
    dt_train = dt[address %in% addr_train]
    if (curve_type == "curves"){
        dt_to_smooth_train = dcast(dt_train[, .(address, ts_normal, int_increments_sm)],
                                   ts_normal ~ address, value.var = "int_increments_sm")
    }else if (curve_type == "derivatives"){
        dt_to_smooth_train = dcast(dt_train[, .(address, ts_normal, deriv_increments_sm)],
                                   ts_normal ~ address, value.var = "deriv_increments_sm")
    }else{
        dt_to_smooth_train = dcast(dt_train[, .(address, ts_normal, lambda)],
                                   ts_normal ~ address, value.var = "lambda")
    }
    eval_points_train = dt_to_smooth_train$ts_normal
    dt_to_smooth_train[, ts_normal := NULL]
    basisobj = create.bspline.basis(breaks = eval_points_train)
    fdParobj = fdPar(fdobj=basisobj, Lfdobj=2, lambda = lambda_smooth)
    smoothlist = smooth.basis(eval_points_train, as.matrix(dt_to_smooth_train), fdParobj)
    fpca = pca.fd(smoothlist$fd, nharm =4)
    
    basis_mat = eval.basis(eval_points_train, fpca$harmonics$basis)
    eigenfuns_coefs = fpca$harmonics$coefs
    eigenbasis_mat = basis_mat %*% eigenfuns_coefs
    mean_fun = basis_mat %*% fpca$meanfd$coefs
    
    percentvar_1 = round(100 * fpca$varprop[1], 1)
    percentvar_2 = round(100 * fpca$varprop[2], 1)
    percentvar_3 = round(100 * fpca$varprop[3], 1)
    percentvar_4 = round(100 * fpca$varprop[4], 1)
    
    fac1 = sqrt(fpca$values[1])
    fac2 = sqrt(fpca$values[2])
    fac3 = sqrt(fpca$values[3])
    fac4 = sqrt(fpca$values[4])
    
    plt_dt = cbind(data.table(ts_normal = eval_points_train, mean_fun = as.numeric(mean_fun[,1])), eigenbasis_mat)
    plt_dt[, PC1_positive := mean_fun + fac1*(PC1)]
    plt_dt[, PC2_positive := mean_fun + fac2*(PC2)]
    plt_dt[, PC3_positive := mean_fun + fac3*(PC3)]
    plt_dt[, PC4_positive := mean_fun + fac4*(PC4)]
    plt_dt[, PC1_negative := mean_fun - fac1*(PC1)]
    plt_dt[, PC2_negative := mean_fun - fac2*(PC2)]
    plt_dt[, PC3_negative := mean_fun - fac3*(PC3)]
    plt_dt[, PC4_negative := mean_fun - fac4*(PC4)]
    
    plt_dt = melt(plt_dt[, .(ts_normal, mean_fun, PC1_positive, PC1_negative, PC2_positive, PC2_negative, PC3_positive,
                             PC3_negative, PC4_positive, PC4_negative)], id.vars = "ts_normal")
    
    plt_FPC1 = ggplot(data = plt_dt[variable == "mean_fun" | variable == "PC1_positive" | variable == "PC1_negative"],
                      aes(ts_normal, value, color = variable, linetype = variable)) +
        geom_line() + 
        xlab("Time") + 
        ylab("") + 
        ggtitle(paste0("First eigenfunction: ", percentvar_1, "% of variability")) +
        scale_color_manual(labels = c("PC1_positive" = "+ eigen fun.", "PC1_negative" = "- eigen fun.", "mean_fun" = "Mean fun."), 
                           values = c("PC1_positive" = "#00BFC4", "PC1_negative" = "#F8766B", "mean_fun" = "black"),
                           name = "") + 
        scale_linetype_manual(labels = c("PC1_positive" = "+ eigen fun.", "PC1_negative" = "- eigen fun.", "mean_fun" = "Mean fun."),
                              values = c("PC1_positive" = "solid", "PC1_negative" = "solid", "mean_fun" = "dashed"),
                              name = "") 
    
    plt_FPC2 = ggplot(data = plt_dt[variable == "mean_fun" | variable == "PC2_positive" | variable == "PC2_negative"],
                      aes(ts_normal, value, color = variable, linetype = variable)) +
        geom_line() + 
        xlab("Time") + 
        ylab("") + 
        ggtitle(paste0("Second eigenfunction: ", percentvar_2, "% of variability")) +
        scale_color_manual(labels = c("PC2_positive" = "+ eigen fun.", "PC2_negative" = "- eigen fun.", "mean_fun" = "Mean fun."), 
                           values = c("PC2_positive" = "#00BFC4", "PC2_negative" = "#F8766B", "mean_fun" = "black"),
                           name = "") + 
        scale_linetype_manual(labels = c("PC2_positive" = "+ eigen fun.", "PC2_negative" = "- eigen fun.", "mean_fun" = "Mean fun."),
                              values = c("PC2_positive" = "solid", "PC2_negative" = "solid", "mean_fun" = "dashed"),
                              name = "") 
    
    plt_FPC3 = ggplot(data = plt_dt[variable == "mean_fun" | variable == "PC3_positive" | variable == "PC3_negative"],
                      aes(ts_normal, value, color = variable, linetype = variable)) +
        geom_line() + 
        xlab("Time") + 
        ylab("") + 
        ggtitle(paste0("Third eigenfunction: ", percentvar_3, "% of variability")) +
        scale_color_manual(labels = c("PC3_positive" = "+ eigen fun.", "PC3_negative" = "- eigen fun.", "mean_fun" = "Mean fun."), 
                           values = c("PC3_positive" = "#00BFC4", "PC3_negative" = "#F8766B", "mean_fun" = "black"),
                           name = "") + 
        scale_linetype_manual(labels = c("PC3_positive" = "+ eigen fun.", "PC3_negative" = "- eigen fun.", "mean_fun" = "Mean fun."),
                              values = c("PC3_positive" = "solid", "PC3_negative" = "solid", "mean_fun" = "dashed"),
                              name = "") 
    
    plt_FPC4 = ggplot(data = plt_dt[variable == "mean_fun" | variable == "PC4_positive" | variable == "PC4_negative"],
                      aes(ts_normal, value, color = variable, linetype = variable)) +
        geom_line() + 
        xlab("Time") + 
        ylab("") + 
        ggtitle(paste0("Fourth eigenfunction: ", percentvar_4, "% of variability")) +
        scale_color_manual(labels = c("PC4_positive" = "+ eigen fun.", "PC4_negative" = "- eigen fun.", "mean_fun" = "Mean fun."), 
                           values = c("PC4_positive" = "#00BFC4", "PC4_negative" = "#F8766B", "mean_fun" = "black"),
                           name = "") + 
        scale_linetype_manual(labels = c("PC4_positive" = "+ eigen fun.", "PC4_negative" = "- eigen fun.", "mean_fun" = "Mean fun."),
                              values = c("PC4_positive" = "solid", "PC4_negative" = "solid", "mean_fun" = "dashed"),
                              name = "") 
    
    plt_all = ggarrange(plt_FPC1, plt_FPC2, plt_FPC3, plt_FPC4, common.legend = TRUE, legend = "right")
    plt_all = annotate_figure(plt_all, top = text_grob(paste("FPCA of", cred_debt, curve_type), face = "bold", size = 14))
    ggexport(plt_all, filename = paste0("graphics/fpca/", curve_type,"_", cred_debt, "_", min_obs,
                                        "obs.png"), width = 800, height = 700)
}


# RUN ---------------------------------------------------------------------

plot_fpcas(10, "curves", "credit")
plot_fpcas(10, "curves", "debt")
plot_fpcas(10, "derivatives", "credit")
plot_fpcas(10, "derivatives", "debt")
plot_fpcas(10, "rates", "credit")
plot_fpcas(10, "rates", "debt")

plot_fpcas(20, "curves", "credit")
plot_fpcas(20, "curves", "debt")
plot_fpcas(20, "derivatives", "credit")
plot_fpcas(20, "derivatives", "debt")
plot_fpcas(20, "rates", "credit")
plot_fpcas(20, "rates", "debt")
