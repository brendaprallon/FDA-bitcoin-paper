####### Plotting Step Functions ########

# Packages
library(data.table)
library(ggplot2)

# Functions ---------------------------------------------------------------

## Function to plot step curves ##
plot_step_curves = function(wallets, addr_sample, nobs, free_scale=FALSE){
    for (category in names(addr_sample)){
        addr_cat = addr_sample[[category]]
        dt = wallets[address %in% addr_cat]
        #dt = rbindlist(lapply(unique(dt$address), points_of_step_function, dt))
        dt[increment_type == "credit", increment_type := "Credit"]
        dt[increment_type == "debt", increment_type := "Debt"]
        plt = ggplot(data = dt) +
            ggtitle(category) +
            xlab("Time") + 
            ylab("") + 
            geom_step(aes(x = ts_normal, y = int_increments, color = increment_type)) +
            labs(color="Curve type") + 
            scale_colour_manual(values = c("Credit" = "#00BFC4", "Debt" = "#F8766B")) 
        if(free_scale == TRUE){
            plt = plt + facet_wrap(~address, nrow = 2, scales = "free_y")
            file_plt = paste0("graphics/curves/step/free scale/step_curves_", gsub("/", "_", gsub(" ", "_", tolower(category))))
            wd = 11
        }else{
            plt = plt + facet_wrap(~address, nrow = 2)
            file_plt = paste0("graphics/curves/step/fixed scale/step_curves_", gsub("/", "_", gsub(" ", "_", tolower(category))))
            wd = 10
        }
        ggsave(paste0(file_plt, "_", nobs, "obs.png"), plt, width = wd, height = 6)
    }
}


# RUN ---------------------------------------------------------------------

## 10 obs ##
wallets = data.table(readRDS("data/treated/wallets_presmooth_balanced_10obs.rds"))
# sample to plot
addr_sample = readRDS("data/treated/balanced samples/plt_sample_addr_10obs.rds")
addr_sample = lapply(addr_sample, function(x) x[1:6])
plot_step_curves(wallets, addr_sample, 10)
plot_step_curves(wallets, addr_sample, 10, free_scale = TRUE)

## 20 obs ##
wallets = data.table(readRDS("data/treated/wallets_presmooth_balanced_20obs.rds"))
# sample to plot
addr_sample = readRDS("data/treated/balanced samples/plt_sample_addr_20obs.rds")
addr_sample = lapply(addr_sample, function(x) x[1:6])
plot_step_curves(wallets, addr_sample, 20)
plot_step_curves(wallets, addr_sample, 20, free_scale = TRUE)