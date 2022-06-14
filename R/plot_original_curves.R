###### Plotting Original Curves ######

# Packages
library(data.table)
library(ggplot2)

# Auxiliary functions
source("R/aux_functions.R")


# Functions ---------------------------------------------------------------

# Function to evaluate function points; adds points evaluated in the beggining and ending of the interval
points_of_original_function = function(address_hash, dt){
    dt = dt[address == address_hash]
    x_vec = dt$ts_normal
    y_vec = dt$value
    eval_points = unique(sort(c(c(0,1), x_vec)))
    eval_step_function = unlist(lapply(eval_points, step_function, x_vec, y_vec))
    dt = data.table(address = address_hash, label = dt$label[1], category = dt$category[1],
                    ts_normal = eval_points, value = eval_step_function, count = dt$count[1])
    return(dt)
}

## Function to plot step curves ##
plot_original_curves = function(wallets, addr_sample, nobs, free_scale=FALSE){
    for (category in names(addr_sample)){
        addr_cat = addr_sample[[category]]
        dt = wallets[address %in% addr_cat]
        dt = rbindlist(lapply(unique(dt$address), points_of_original_function, dt))
        plt = ggplot(data = dt) +
            ggtitle(category) +
            xlab("Time") + 
            ylab("Balance") + 
            geom_step(aes(x = ts_normal, y = value)) 
        if(free_scale == TRUE){
            plt = plt + facet_wrap(~address, nrow = 2, scales = "free_y")
            file_plt = paste0("graphics/curves/original/free scale/original_curves_", gsub("/", "_", gsub(" ", "_", tolower(category))))
            wd = 11
        }else{
            plt = plt + facet_wrap(~address, nrow = 2)
            file_plt = paste0("graphics/curves/original/fixed scale/original_curves_", gsub("/", "_", gsub(" ", "_", tolower(category))))
            wd = 10
        }
        ggsave(paste0(file_plt, "_", nobs, "obs.png"), plt, width = wd, height = 6)
    }
}



# RUN ---------------------------------------------------------------------

wallets = data.table(readRDS("data/treated/cred_debt_complete.rds"))
setorder(wallets, address, ts_normal)
wallets[, value := cumsum(increments), by = "address"]

## 10 obs ##
# sample to plot
addr_sample = readRDS("data/treated/plt_sample_addr_10obs.rds")
addr_sample = lapply(addr_sample, function(x) x[1:6])
plot_original_curves(wallets, addr_sample, 10)
plot_original_curves(wallets, addr_sample, 10, free_scale = TRUE)

## 20 obs ##
# sample to plot
addr_sample = readRDS("data/treated/plt_sample_addr_20obs.rds")
addr_sample = lapply(addr_sample, function(x) x[1:6])
plot_original_curves(wallets, addr_sample, 20)
plot_original_curves(wallets, addr_sample, 20, free_scale = TRUE)
