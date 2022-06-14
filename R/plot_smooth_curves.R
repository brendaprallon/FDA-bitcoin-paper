####### Plotting Smooth Curves ########

# Packages
library(data.table)
library(fda)
library(ggplot2)

# Functions ---------------------------------------------------------------

smooth_curves_for_plotting = function(address_hash, dt, lambda_presmooth){
    print(address_hash)
    dt = dt[address == address_hash]
    credit = dt[increment_type == "credit"]
    debt = dt[increment_type == "debt"]
    x_vec_credit = credit$ts_normal
    y_vec_credit = log(abs(credit$int_increments) + 1)
    x_vec_debt = debt$ts_normal
    y_vec_debt = log(abs(debt$int_increments) + 1)
    
    # smoothing with bsplines
    basisobj_credit = create.bspline.basis(breaks = x_vec_credit)
    fdParobj_credit = fdPar(fdobj=basisobj_credit, Lfdobj=2, lambda = lambda_presmooth)
    smoothlist_credit = smooth.basis(x_vec_credit, y_vec_credit, fdParobj_credit)
    smooth_pred_credit = as.numeric(eval.fd(x_vec_credit, smoothlist_credit$fd))
    
    basisobj_debt = create.bspline.basis(breaks = x_vec_debt)
    fdParobj_debt = fdPar(fdobj=basisobj_debt, Lfdobj=2, lambda = lambda_presmooth)
    smoothlist_debt = smooth.basis(x_vec_debt, y_vec_debt, fdParobj_debt)
    smooth_pred_debt = as.numeric(eval.fd(x_vec_debt, smoothlist_debt$fd))

    #final data table
    dt_cred = data.table(address = address_hash, label = dt$label[1], category = dt$category[1],
                         increment_type = "credit", ts_normal = x_vec_credit, int_increments = y_vec_credit,
                         int_increments_sm = smooth_pred_credit, count = dt$count[1])
    dt_debt = data.table(address = address_hash, label = dt$label[1], category = dt$category[1],
                         increment_type = "debt", ts_normal = x_vec_debt, int_increments = y_vec_debt,
                         int_increments_sm = smooth_pred_debt, count = dt$count[1])
    return(rbindlist(list(dt_cred, dt_debt)))
}

## Function to plot smooth curves ##
plot_smooth_curves = function(wallets, addr_sample, nobs, free_scale = FALSE){
    for (category in names(addr_sample)){
        addr_cat = addr_sample[[category]]
        dt = wallets[address %in% addr_cat]
        dt = rbindlist(lapply(unique(dt$address), smooth_curves_for_plotting, dt, 0.001))
        credit_plt = ggplot(data = dt[increment_type == "credit"]) +
            ggtitle(paste(category), "- Credit") +
            xlab("Time") + 
            ylab("Log(Cumsum(X(t)))") + 
            geom_point(aes(x = ts_normal, y = int_increments), color = "#00BFC4") +
            geom_line(aes(x = ts_normal, y = int_increments_sm), color = "black") 
        if(free_scale == TRUE){
            credit_plt = credit_plt + facet_wrap(~address, nrow = 2, scales = "free_y")
        }else{
            credit_plt = credit_plt + facet_wrap(~address, nrow = 2)
        }
        debt_plt = ggplot(data = dt[increment_type == "debt"]) +
            ggtitle(paste(category), "- Debt") +
            xlab("Time") + 
            ylab("Log(Cumsum(X(t)))") + 
            geom_point(aes(x = ts_normal, y = int_increments), color = "#F8766B") +
            geom_line(aes(x = ts_normal, y = int_increments_sm), color = "black") 
        if(free_scale == TRUE){
            debt_plt = debt_plt + facet_wrap(~address, nrow = 2, scales = "free_y")
            file_plt = paste0("graphics/curves/smooth/free scale/smooth_curves_", gsub("/", "_", gsub(" ", "_", tolower(category)), fixed = TRUE))
            wd = 11
        }else{
            debt_plt = debt_plt + facet_wrap(~address, nrow = 2)
            file_plt = paste0("graphics/curves/smooth/fixed scale/smooth_curves_",  gsub("/", "_", gsub(" ", "_", tolower(category)), fixed = TRUE))
            wd = 10
        }
        ggsave(paste0(file_plt, "_", nobs, "obs_credit.png"), credit_plt, width = wd, height = 6)
        ggsave(paste0(file_plt, "_", nobs, "obs_debt.png"), debt_plt, width = wd, height = 6)
    }
}


# RUN ---------------------------------------------------------------------

## 10 obs ##
wallets = data.table(readRDS("data/treated/wallets_presmooth_balanced_10obs.rds"))
setorder(wallets, address, increment_type, ts_normal)
# sample to plot
addr_sample = readRDS("data/treated/plt_sample_addr_10obs.rds")
addr_sample = lapply(addr_sample, function(x) x[1:6])
plot_smooth_curves(wallets, addr_sample, 10)
plot_smooth_curves(wallets, addr_sample, 10, free_scale = TRUE)

## 20 obs ##
wallets = data.table(readRDS("data/treated/wallets_presmooth_balanced_20obs.rds"))
setorder(wallets, address, increment_type, ts_normal)
# sample to plot
addr_sample = readRDS("data/treated/plt_sample_addr_20obs.rds")
addr_sample = lapply(addr_sample, function(x) x[1:6])
plot_smooth_curves(wallets, addr_sample, 20)
plot_smooth_curves(wallets, addr_sample, 20, free_scale = TRUE)
