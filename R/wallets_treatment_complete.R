### Wallets data base ###

# Packages #
library(data.table)
library(xtable)
library(ggplot2)

# Functions ---------------------------------------------------------------

# Function to convert btc to usd
btc_to_usd = function(first_date, btc_price, lifespan){
    first_date = as.Date(first_date)
    last_date = first_date + as.difftime(lifespan, units = "hours")
    usd_mean = mean(btc_price[date >= first_date & date <= last_date, usd])
}

# Function to treat the data
wallets_treatment = function(dt, btc_price, lifespan){

    # Converting time stamps to date
    setnames(dt, "timestamp", "time_stamp")
    dt[, date := as.POSIXct(time_stamp, origin = '1970-01-01', tz = 'UCT')]
    
    # first dates
    setorder(dt, address, date)
    dt[, first_date := first(date), by = address]
    
    # Normalizing values by the mean avarege of dollar price
    usd_dt = unique(dt[, .(first_date)])
    usd_dt$usd_mean = unlist(lapply(usd_dt$first_date, btc_to_usd, copy(btc_price), lifespan))
    dt = merge(dt, usd_dt, by = "first_date")
    dt[, value := value*usd_mean]
    
    # Filtering only values of the first lifespan hours
    dt = dt[date <= first_date + as.difftime(lifespan, units = "hours")]
    
    # creating new count variable
    dt[, count := .N, by = address]
    
    # changing value name to increment
    setnames(dt, "value", "increments")
    
    # integral
    dt[, int_increments := cumsum(increments), by = .(address, increment_type)]
    
    # normalizing time stamps
    dt[, ts := as.numeric(date)]
    dt[, `:=` (first_ts = min(ts), last_ts = max(ts)), by = address]
    dt[, ts_normal := (ts - first_ts)/(as.numeric(first_date + as.difftime(lifespan, units = "hours")) - first_ts)]
    
    # Table of original counts
    table_ltx_original(dt)
    
    # saving the data table
    saveRDS(dt, "data/treated/cred_debt_complete.rds")
    return(dt)
}

# Table addresses and entities, original data
table_ltx_original = function(dt){
    addr = unique(dt[, .(address, category)])
    addr = addr[, .N, by = "category"]
    addr = rbind(addr, data.table(category = "Total obs.", N = sum(addr$N)))
    entities = unique(dt[, .(label, category)])
    entities = entities[, .N, by = "category"]
    entities = rbind(entities, data.table(category = "Total obs.", N = sum(entities$N)))
    original_tbl = merge(addr, entities, by = "category")
    names(original_tbl) = c("Category", "Addresses", "Entities")
    original_tbl = xtable(original_tbl, caption = "Original Number of Addresses and Entities, by Category", 
                     label = "n_obs_cat_orig")
    print(original_tbl, file= "tables/latex/n_obs_cat_orig", include.rownames = FALSE,
          format.args=list(big.mark = ",", decimal.mark = "."))
}

# Table addresses per category
table_ltx_sh_cat = function(dt, min_obs){
    cat = unique(dt[, .(address, category)])
    # latex table
    cat_tbl = cat[, .N, by = category]
    total_obs = sum(cat_tbl$N)
    cat_tbl[, N := round(N/sum(N), 2)]
    cat_tbl = rbind(cat_tbl, data.frame("category" = "Total obs.", "N" = total_obs))
    names(cat_tbl) = c("Category", "Share")
    cat_tbl = xtable(cat_tbl, caption = paste("Share of Categories -", min_obs, "obs."), 
                     label = paste0("sh_cat_", min_obs, "obs"))
    print(cat_tbl, file= paste0("tables/latex/sh_cat_", min_obs, "obs.tex"), include.rownames = FALSE,
          format.args=list(big.mark = ",", decimal.mark = "."))
}

table_ltx_n_cat = function(dt, min_obs){
    cat = unique(dt[, .(address, category)])
    # latex table
    cat_tbl = cat[, .N, by = category]
    total_obs = sum(cat_tbl$N)
    cat_tbl = rbind(cat_tbl, data.frame("category" = "Total obs.", "N" = total_obs))
    names(cat_tbl) = c("Category", "count")
    cat_tbl = xtable(cat_tbl, caption = paste("Number of Addresses by Category -", min_obs, "obs."), 
                     label = paste0("n_cat_", min_obs, "obs"))
    print(cat_tbl, file= paste0("tables/latex/n_cat_", min_obs, "obs.tex"), include.rownames = FALSE,
          format.args = list(big.mark = ","))
}

# Table labels per category
table_ltx_count_cat = function(dt){
    cat = unique(dt[, .(label, category)])
    # latex table
    cat_tbl = cat[, .N, by = category]
    total_obs = sum(cat_tbl$N)
    cat_tbl = rbind(cat_tbl, data.frame("category" = "Total obs.", "N" = total_obs))
    names(cat_tbl) = c("Category", "Count")
    cat_tbl = xtable(cat_tbl, caption = "Number of Labels per Category", 
                     label = "label_cat")
    print(cat_tbl, file= paste0("tables/latex/count_label_cat.tex"), include.rownames = FALSE,
          format.args=list(big.mark = ",", decimal.mark = "."))
}

# Table and plot start dates per category
table_plt_ltx_date_cat = function(dt){
    cat = unique(dt[, .(first_date, category)])
    cat[, first_date := as.Date(first_date)]
    # Density plot
    cat_plt = ggplot(cat, aes(x=first_date)) +
        ggtitle ("Density of first dates by category") +
        geom_density() + 
        facet_wrap(~category, scales = "free_x") + 
        xlab("") +
        ylab("") 
    ggsave("graphics/first dates/first_dates_density.png", cat_plt, width = 7, height = 6)
    # latex table
    cat_tbl = cat[, .(min = min(first_date), first_quantile = quantile(first_date, type = 1)[2], 
                      median = median(first_date), third_quantile = quantile(first_date, type = 1)[4],
                      max = max(first_date)),
                  by = category]
    cat_tbl[, min := as.character(min)]
    cat_tbl[, first_quantile := as.character(first_quantile)]
    cat_tbl[, median := as.character(median)]
    cat_tbl[, third_quantile := as.character(third_quantile)]
    cat_tbl[, max := as.character(max)]
    names(cat_tbl) = c("Category", "Min.", "1st quantile", "Median", "3rd quantile", "Max.")
    cat_tbl = xtable(cat_tbl, caption = "Distribution of Starting Dates by Category", label = "date_cat")
    print(cat_tbl, file= "tables/latex/start_dates_cat.tex", include.rownames = FALSE)
}

#Saving balanced sample
balanced_sample = function(dt, min_obs, plt_sample_size){
    # getting the minimum between categories
    cat_dt = unique(dt[, .(address, category)])
    cat_dt[, count := .N, by = category]
    cat_dt = unique(cat_dt[, .(category, count)])
    min_obs_cat = min(cat_dt$count) 
    # sampling from each category
    balanced_sample_addr = list()
    plt_sample_addr = list()
    for (cat in cat_dt$category){
        cat_count = cat_dt[category == cat, count] 
        if (cat_count < min_obs_cat){ 
            balanced_sample_addr[[cat]] = sample(unique(dt[category == cat, address]), cat_count) 
        }else{ 
            balanced_sample_addr[[cat]] = sample(unique(dt[category == cat, address]), min_obs_cat)
        } 
        plt_sample_addr[[cat]] = sample(balanced_sample_addr[[cat]], plt_sample_size)
    }
    # save lists
    saveRDS(plt_sample_addr, paste0("data/treated/plt_sample_addr_", min_obs, "obs.rds"))
    # save dt
    saveRDS(dt[address %in% unlist(balanced_sample_addr)], paste0("data/treated/cred_debt_balanced_", min_obs, "obs.rds"))
}

# specific function to filter by number of observations
filter_min_obs = function(dt, min_obs, lifespan){
    # filtering
    dt = dt[count >= min_obs]
    # latex table
    table_ltx_sh_cat(copy(dt), min_obs)
    table_ltx_n_cat(copy(dt), min_obs)
    # saving data
    saveRDS(dt, paste0("data/treated/cred_debt_complete_", min_obs, "obs.rds"))
    # balanced data
    balanced_sample(copy(dt), min_obs, 12)
}

# RUN ---------------------------------------------------------------------

# Timing script 
start_time = Sys.time()
# Reading data
dt = readRDS("data/raw/wallets_from_blockchain.rds")
# satoshi to btc
dt[, value := as.numeric(value)/(10^8)]
# labels
labels = data.table(read.csv("data/labels/label_addresses.csv"))
names(labels)[c(1,2)] = c("address", "label")
dt = merge(dt, unique(labels[, .(address, label)]), by = "address", all.x = TRUE)

# Categories:
dict = data.table(read.csv("data/labels/labels.csv"))
# We took the categories from wallet explorer, but also did our own investigation, in special to identify illegal labels. 
# We keep the wallet explorer class, except from the "old/historic" cases that we identified as being illegal.
dict[, new_wallet_explorer := wallet_explorer]
dict[wallet_explorer == 'Old/historic', new_wallet_explorer := NA]
dict[new_category == 'Darknet Marketplace', new_wallet_explorer := new_category]
dt = merge(dt, dict[, .(label, new_wallet_explorer)], by = "label", all.x = TRUE)
setnames(dt, "new_wallet_explorer", "category")
dt = dt[!is.na(category)]
dt[, category := as.character(category)]

# BTC USD table
btc_price = data.table(read.csv("data/raw/btc.csv", header = FALSE))
names(btc_price) = c("date", "usd")
btc_price[, date := as.Date(as.character(date), "%m/%d/%Y")]
setorder(btc_price, date)

# Treating the data: Using lifespan of 3000
dt = wallets_treatment(copy(dt), copy(btc_price), lifespan = 3000)
table_ltx_count_cat(copy(dt))
table_plt_ltx_date_cat(copy(dt))

## Filtering Min Obs
set.seed(123)
# 10 obs
filter_min_obs(copy(dt), 10, 3000)
# 20 obs
filter_min_obs(copy(dt), 20, 3000)

end_time = Sys.time()
print(start_time - end_time)
