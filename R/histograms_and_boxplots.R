### Plotting histograms of wallets length ###

# Packages
library(data.table)
library(ggplot2)

# Auxiliary functions
source("R/aux_functions.R")

# Reading 
wallets = readRDS("data/raw/wallets_time_series_appended.rds")

# Remove wallets with NA values
wallets = wallets[!is.na(value)]

# Translate categorys and remove user and advertising
wallets = wallets[!(category %in% c("Usuário", "Advertising"))]
wallets = data.table(rename_to_english(wallets))

# Converting time stamps to date
wallets[, date := as.POSIXct(time_stamp, origin = '1970-01-01', tz = 'UCT')]

# first dates
setorder(wallets, address, date)
wallets[, first_date := first(date), by = address]

# Normalizing values by the mean avarege of dollar price
btc_price = data.table(read.csv("data/raw/btc.csv", header = FALSE))
names(btc_price) = c("date", "usd")
btc_price[, date := as.Date(as.character(date), "%m/%d/%Y")]
setorder(btc_price, date)

btc_to_usd = function(first_date, btc_price){
    first_date = as.Date(first_date)
    last_date = first_date + as.difftime(3000, units = "hours")
    usd_mean = mean(btc_price[date >= first_date & date <= last_date, usd])
}

usd_wallets = unique(wallets[, .(first_date)])
usd_wallets$usd_mean = unlist(lapply(usd_wallets$first_date, btc_to_usd, copy(btc_price)))
wallets = merge(wallets, usd_wallets, by = "first_date")
wallets[, value := value*usd_mean]

# increments column
setorder(wallets, address, date)
wallets[, lag := shift(value, n=1, fill=NA, type="lag"), by = address]
wallets[!is.na(lag), increments := value - lag]
wallets[is.na(lag), increments := value]

# Credit and debt
wallets[increments >= 0, increment_type :=  "credit"]
wallets[increments < 0, increment_type := "debt"]

# last dates and length in hours
wallets[, last_date := last(date), by = address]
wallets[, length_hours := as.numeric(difftime(last_date, first_date, units = "hours"))]

# Aggregating creds and debts at the same time
wallets[,`:=` (value = sum(value), lag = sum(lag), increments = sum(increments)), by = .(address, increment_type, date)]
wallets = unique(wallets)

# if missing the number of hours, it is zero
wallets[is.na(length_hours), length_hours := 0]

# integral
wallets[, int_increments := cumsum(increments), by = .(address, increment_type)]

# last credit and debts
wallets = wallets[date <= first_date + as.difftime(3000, units = "hours")]
setorder(wallets, address, increment_type, date)
wallets[, last_int_increments := last(int_increments), by = c("address", "increment_type")]
# new number of observations
wallets[, n_obs := .N, by = address]

# data table to plot values of credit and debt int increments
wallets_by_addr = unique(wallets[, .(address, category, n_obs, length_hours, increment_type, last_int_increments)])

hist_hours = ggplot(wallets_by_addr) +
    ggtitle("Histogram - Duration in hours") + 
    xlab("Hours") + 
    ylab("Count") + 
    geom_histogram(aes(length_hours), bins = 50,
                   show.legend = FALSE) +
    geom_freqpoly(aes(length_hours), bins = 50)

hist_n_obs = ggplot(wallets_by_addr) +
    ggtitle("Histogram - Number of observations") + 
    xlab("N obs.") + 
    ylab("Count") + 
    geom_histogram(aes(n_obs), bins = 50,
                   show.legend = FALSE) +
    geom_freqpoly(aes(n_obs), bins = 50)

hist_hours_by_cat = ggplot(wallets_by_addr) +
    ggtitle("Histogram - Duration in hours") + 
    xlab("Hours") + 
    ylab("Count") + 
    geom_histogram(aes(length_hours, fill = category), bins = 50,
                   show.legend = FALSE) +
    geom_freqpoly(aes(length_hours), bins = 50) + 
    facet_wrap(~category, ncol = 3, scales = "free_y")

hist_n_obs_by_cat = ggplot(wallets_by_addr) +
    ggtitle("Histogram - Number of observations") + 
    xlab("N obs.") + 
    ylab("Count") + 
    geom_histogram(aes(n_obs, fill = category), bins = 50,
                   show.legend = FALSE) +
    geom_freqpoly(aes(n_obs), bins = 50) + 
    facet_wrap(~category, ncol = 3, scales = "free_y")

# hist_hours_less_20000 = ggplot(wallets_by_addr[length_hours < 20000 & length_hours > 0]) +
#     ggtitle("Histogram - Duration in hours (> 0 & < 2000)") + 
#     xlab("Hours") + 
#     ylab("Count") + 
#     geom_histogram(aes(length_hours), bins = 50,
#                    show.legend = FALSE) +
#     geom_freqpoly(aes(length_hours), bins = 50)
# 
# hist_n_obs_less_500 = ggplot(wallets_by_addr[n_obs < 500 & n_obs > 1]) +
#     ggtitle("Histogram - Number of observations (> 1 & < 500)") + 
#     xlab("N obs.") + 
#     ylab("Count") + 
#     geom_histogram(aes(n_obs), bins = 50,
#                    show.legend = FALSE) +
#     geom_freqpoly(aes(n_obs), bins = 50)
# 
# hist_hours_by_cat_less_20000 = ggplot(wallets_by_addr[length_hours < 20000 & length_hours > 0]) +
#     ggtitle("Histogram - Duration in hours (> 0 & < 2000)") + 
#     xlab("Hours") + 
#     ylab("Count") + 
#     geom_histogram(aes(length_hours, fill = category), bins = 50,
#                    show.legend = FALSE) +
#     geom_freqpoly(aes(length_hours), bins = 50) + 
#     facet_wrap(~category, ncol = 3)
# 
# hist_n_obs_by_cat_less_500 = ggplot(wallets_by_addr[n_obs < 500 & n_obs > 1]) +
#     ggtitle("Histogram - Number of observations (> 1 & < 500)") + 
#     xlab("N obs.") + 
#     ylab("Count") + 
#     geom_histogram(aes(n_obs, fill = category), bins = 50,
#                    show.legend = FALSE) +
#     geom_freqpoly(aes(n_obs), bins = 50) + 
#     facet_wrap(~category, ncol = 3)
# 
# hist_hours_more_10 = ggplot(wallets_by_addr[n_obs < 500 & n_obs >= 10]) +
#     ggtitle("Histogram - Duration in hours (min. 10 obs.)") + 
#     xlab("Hours") + 
#     ylab("Count") + 
#     geom_histogram(aes(length_hours), bins = 50,
#                    show.legend = FALSE) +
#     geom_freqpoly(aes(length_hours), bins = 50)
# 
# hist_n_obs_more_10 = ggplot(wallets_by_addr[n_obs < 500 & n_obs >= 10]) +
#     ggtitle("Histogram - Number of observations (min. 10 obs.)") + 
#     xlab("N obs.") + 
#     ylab("Count") + 
#     geom_histogram(aes(n_obs), bins = 50,
#                    show.legend = FALSE) +
#     geom_freqpoly(aes(n_obs), bins = 50)
# 
# hist_hours_by_cat_more_10 = ggplot(wallets_by_addr[n_obs < 500 & n_obs >= 10]) +
#     ggtitle("Histogram - Duration in hours (min. 10 obs.)") + 
#     xlab("Hours") + 
#     ylab("Count") + 
#     geom_histogram(aes(length_hours, fill = category), bins = 50,
#                    show.legend = FALSE) +
#     geom_freqpoly(aes(length_hours), bins = 50) + 
#     facet_wrap(~category, ncol = 3)
# 
# hist_n_obs_by_cat_more_10 = ggplot(wallets_by_addr[n_obs < 500 & n_obs >= 10]) +
#     ggtitle("Histogram - Number of observations (min. 10 obs.)") + 
#     xlab("N obs.") + 
#     ylab("Count") + 
#     geom_histogram(aes(n_obs, fill = category), bins = 50,
#                    show.legend = FALSE) +
#     geom_freqpoly(aes(n_obs), bins = 50) + 
#     facet_wrap(~category, ncol = 3)
# 
# hist_hours_more_20 = ggplot(wallets_by_addr[n_obs < 500 & n_obs >= 20]) +
#     ggtitle("Histogram - Duration in hours (min. 20 obs.)") + 
#     xlab("Hours") + 
#     ylab("Count") + 
#     geom_histogram(aes(length_hours), bins = 50,
#                    show.legend = FALSE) +
#     geom_freqpoly(aes(length_hours), bins = 50)
# 
# hist_n_obs_more_20 = ggplot(wallets_by_addr[n_obs < 500 & n_obs >= 20]) +
#     ggtitle("Histogram - Number of observations (min. 20 obs.)") + 
#     xlab("N obs.") + 
#     ylab("Count") + 
#     geom_histogram(aes(n_obs), bins = 50,
#                    show.legend = FALSE) +
#     geom_freqpoly(aes(n_obs), bins = 50)
# 
# hist_hours_by_cat_more_20 = ggplot(wallets_by_addr[n_obs < 500 & n_obs >= 20]) +
#     ggtitle("Histogram - Duration in hours (min. 20 obs.)") + 
#     xlab("Hours") + 
#     ylab("Count") + 
#     geom_histogram(aes(length_hours, fill = category), bins = 50,
#                    show.legend = FALSE) +
#     geom_freqpoly(aes(length_hours), bins = 50) + 
#     facet_wrap(~category, ncol = 3)
# 
# hist_n_obs_by_cat_more_20 = ggplot(wallets_by_addr[n_obs < 500 & n_obs >= 20]) +
#     ggtitle("Histogram - Number of observations (min. 20 obs.)") + 
#     xlab("N obs.") + 
#     ylab("Count") + 
#     geom_histogram(aes(n_obs, fill = category), bins = 50,
#                    show.legend = FALSE) +
#     geom_freqpoly(aes(n_obs), bins = 50) + 
#     facet_wrap(~category, ncol = 3)

# Boxplots for credits and debts 
wallets_by_addr[increment_type == "credit", increment_type := "Credit"]
wallets_by_addr[increment_type == "debt", increment_type := "Debt"]
boxplot = ggplot(wallets_by_addr, aes(increment_type, abs(last_int_increments))) + 
    ggtitle("Boxplots - Accumulated Values") + 
    xlab("") + 
    ylab("Value") + 
    geom_boxplot() + 
    coord_flip()
boxplot_more_10 = ggplot(wallets_by_addr[n_obs >= 10 & n_obs < 500], aes(increment_type, abs(last_int_increments))) + 
    ggtitle("Boxplots - Accumulated Values (min. 10 obs.)") + 
    xlab("") + 
    ylab("Value") + 
    geom_boxplot() + 
    coord_flip()
boxplot_more_20 = ggplot(wallets_by_addr[n_obs >= 20 & n_obs < 500], aes(increment_type, abs(last_int_increments))) + 
    ggtitle("Boxplots - Accumulated Values (min. 20 obs.)") + 
    xlab("") + 
    ylab("Value") + 
    geom_boxplot() + 
    coord_flip()
boxplot_zoom_5000 = ggplot(wallets_by_addr, aes(increment_type, abs(last_int_increments))) + 
    ggtitle("Boxplots - Accumulated Values") + 
    xlab("") + 
    ylab("Value") + 
    geom_boxplot() + 
    coord_flip(ylim = c(0, 5000))
boxplot_more_10_zoom_5000 = ggplot(wallets_by_addr[n_obs >= 10 & n_obs < 500], aes(increment_type, abs(last_int_increments))) + 
    ggtitle("Boxplots - Accumulated Values (min. 10 obs.)") + 
    xlab("") + 
    ylab("Value") + 
    geom_boxplot() + 
    coord_flip(ylim = c(0, 5000))
boxplot_more_20_zoom_5000 = ggplot(wallets_by_addr[n_obs >= 20 & n_obs < 500], aes(increment_type, abs(last_int_increments))) + 
    ggtitle("Boxplots - Accumulated Values (min. 20 obs.)") + 
    xlab("") + 
    ylab("Value") + 
    geom_boxplot() + 
    coord_flip(ylim = c(0, 5000))
boxplot_cat = ggplot(wallets_by_addr, aes(increment_type, abs(last_int_increments))) + 
    ggtitle("Boxplots - Accumulated Values") + 
    xlab("") + 
    ylab("Value") + 
    geom_boxplot() + 
    coord_flip() + 
    facet_wrap(~category, ncol = 3)
boxplot_cat_more_10 = ggplot(wallets_by_addr[n_obs >= 10 & n_obs < 500], aes(increment_type, abs(last_int_increments))) + 
    ggtitle("Boxplots - Accumulated Values (min. 10 obs.)") + 
    xlab("") + 
    ylab("Value") + 
    geom_boxplot() + 
    coord_flip() + 
    facet_wrap(~category, ncol = 3)
boxplot_cat_more_20 = ggplot(wallets_by_addr[n_obs >= 20 & n_obs < 500], aes(increment_type, abs(last_int_increments))) + 
    ggtitle("Boxplots - Accumulated Values (min. 20 obs.)") + 
    xlab("") + 
    ylab("Value") + 
    geom_boxplot() + 
    coord_flip() + 
    facet_wrap(~category, ncol = 3)
boxplot_cat_zoom_5000 = ggplot(wallets_by_addr, aes(increment_type, abs(last_int_increments))) + 
    ggtitle("Boxplots - Accumulated Values") + 
    xlab("") + 
    ylab("Value") + 
    geom_boxplot() + 
    coord_flip(ylim = c(0, 5000)) + 
    facet_wrap(~category, ncol = 3)
boxplot_cat_more_10_zoom_5000 = ggplot(wallets_by_addr[n_obs >= 10 & n_obs < 500], aes(increment_type, abs(last_int_increments))) + 
    ggtitle("Boxplots - Accumulated Values (min. 10 obs.)") + 
    xlab("") + 
    ylab("Value") + 
    geom_boxplot() + 
    coord_flip(ylim = c(0, 5000)) + 
    facet_wrap(~category, ncol = 3)
boxplot_cat_more_20_zoom_5000 = ggplot(wallets_by_addr[n_obs >= 20 & n_obs < 500], aes(increment_type, abs(last_int_increments))) + 
    ggtitle("Boxplots - Accumulated Values (min. 20 obs.)") + 
    xlab("") + 
    ylab("Value") + 
    geom_boxplot() + 
    coord_flip(ylim = c(0, 5000)) + 
    facet_wrap(~category, ncol = 3)

# Log
boxplot_log = ggplot(wallets_by_addr, aes(increment_type, log(abs(last_int_increments)+1))) + 
    ggtitle("Boxplots - Log Accumulated Values") + 
    xlab("") + 
    ylab("Log Value") + 
    geom_boxplot() + 
    coord_flip()
boxplot_more_10_log = ggplot(wallets_by_addr[n_obs >= 10 & n_obs < 500], aes(increment_type, log(abs(last_int_increments)+1))) + 
    ggtitle("Boxplots - Log Accumulated Values (min. 10 obs.)") + 
    xlab("") + 
    ylab("Log Value") + 
    geom_boxplot() + 
    coord_flip()
boxplot_more_20_log = ggplot(wallets_by_addr[n_obs >= 20 & n_obs < 500], aes(increment_type, log(abs(last_int_increments)+1))) + 
    ggtitle("Boxplots - Log Accumulated Values (min. 20 obs.)") + 
    xlab("") + 
    ylab("Log Value") + 
    geom_boxplot() + 
    coord_flip()
boxplot_cat_log = ggplot(wallets_by_addr, aes(increment_type, log(abs(last_int_increments)+1))) + 
    ggtitle("Boxplots - Log Accumulated Values") + 
    xlab("") + 
    ylab("Log Value") + 
    geom_boxplot() + 
    coord_flip() + 
    facet_wrap(~category, ncol = 3)
boxplot_cat_more_10_log = ggplot(wallets_by_addr[n_obs >= 10 & n_obs < 500], aes(increment_type, log(abs(last_int_increments)+1))) + 
    ggtitle("Boxplots - Log Accumulated Values (min. 10 obs.)") + 
    xlab("") + 
    ylab("Log Value") + 
    geom_boxplot() + 
    coord_flip() + 
    facet_wrap(~category, ncol = 3)
boxplot_cat_more_20_log = ggplot(wallets_by_addr[n_obs >= 20 & n_obs < 500], aes(increment_type, log(abs(last_int_increments)+1))) + 
    ggtitle("Boxplots - Log Accumulated Values (min. 20 obs.)") + 
    xlab("") + 
    ylab("Log Value") + 
    geom_boxplot() + 
    coord_flip() + 
    facet_wrap(~category, ncol = 3)

ggsave("graphics/histograms/hist_hours.png", hist_hours, 
       width = 7, height = 4.5)
ggsave("graphics/histograms/hist_n_obs.png", hist_n_obs, 
       width = 7, height = 4.5)
ggsave("graphics/histograms/hist_hours_by_cat.png", hist_hours_by_cat, 
       width = 7, height = 4.5)
ggsave("graphics/histograms/hist_n_obs_by_cat.png", hist_n_obs_by_cat, 
       width = 7, height = 4.5)
# ggsave("graphics/histograms/hist_hours_less_20000.png", hist_hours_less_20000, 
#        width = 7, height = 4.5)
# ggsave("graphics/histograms/hist_n_obs_less_500.png", hist_n_obs_less_500, 
#        width = 7, height = 4.5)
# ggsave("graphics/histograms/hist_hours_by_cat_less_20000.png", hist_hours_by_cat_less_20000, 
#        width = 7, height = 4.5)
# ggsave("graphics/histograms/hist_n_obs_by_cat_less_500.png", hist_n_obs_by_cat_less_500, 
#        width = 7, height = 4.5)
# # Filter: more than 10 observations
# ggsave("graphics/histograms/hist_hours_more_10.png", hist_hours_more_10, 
#        width = 7, height = 4.5)
# ggsave("graphics/histograms/hist_n_obs_more_10.png", hist_n_obs_more_10, 
#        width = 7, height = 4.5)
# ggsave("graphics/histograms/hist_hours_by_cat_more_10.png", hist_hours_by_cat_more_10, 
#        width = 7, height = 4.5)
# ggsave("graphics/histograms/hist_n_obs_by_cat_more_10.png", hist_n_obs_by_cat_more_10, 
#        width = 7, height = 4.5)
# # Filter: more than 20 observations
# ggsave("graphics/histograms/hist_hours_more_20.png", hist_hours_more_20, 
#        width = 7, height = 4.5)
# ggsave("graphics/histograms/hist_n_obs_more_20.png", hist_n_obs_more_20, 
#        width = 7, height = 4.5)
# ggsave("graphics/histograms/hist_hours_by_cat_more_20.png", hist_hours_by_cat_more_20, 
#        width = 7, height = 4.5)
# ggsave("graphics/histograms/hist_n_obs_by_cat_more_20.png", hist_n_obs_by_cat_more_20, 
#        width = 7, height = 4.5)
# Accumulated credits and debts Boxplots
ggsave("graphics/boxplots/boxplot.png", boxplot, 
       width = 7, height = 4.5)
ggsave("graphics/boxplots/boxplot_more_10.png", boxplot_more_10, 
       width = 7, height = 4.5)
ggsave("graphics/boxplots/boxplot_more_20.png", boxplot_more_20, 
       width = 7, height = 4.5)
ggsave("graphics/boxplots/boxplot_log.png", boxplot_log, 
       width = 7, height = 4.5)
ggsave("graphics/boxplots/boxplot_more_10_log.png", boxplot_more_10_log, 
       width = 7, height = 4.5)
ggsave("graphics/boxplots/boxplot_more_20_log.png", boxplot_more_20_log, 
       width = 7, height = 4.5)
ggsave("graphics/boxplots/boxplot_cat.png", boxplot_cat, 
       width = 7, height = 4.5)
ggsave("graphics/boxplots/boxplot_cat_more_10.png", boxplot_cat_more_10, 
       width = 7, height = 4.5)
ggsave("graphics/boxplots/boxplot_cat_more_20.png", boxplot_cat_more_20, 
       width = 7, height = 4.5)
ggsave("graphics/boxplots/boxplot_cat_log.png", boxplot_cat_log, 
       width = 7, height = 4.5)
ggsave("graphics/boxplots/boxplot_cat_more_10_log.png", boxplot_cat_more_10_log, 
       width = 7, height = 4.5)
ggsave("graphics/boxplots/boxplot_cat_more_20_log.png", boxplot_cat_more_20_log, 
       width = 7, height = 4.5)
ggsave("graphics/boxplots/boxplot_zoom_5000.png", boxplot_zoom_5000, 
       width = 7, height = 4.5)
ggsave("graphics/boxplots/boxplot_more_10_zoom_5000.png", boxplot_more_10_zoom_5000, 
       width = 7, height = 4.5)
ggsave("graphics/boxplots/boxplot_more_20_zoom_5000.png", boxplot_more_20_zoom_5000, 
       width = 7, height = 4.5)
ggsave("graphics/boxplots/boxplot_cat_zoom_5000.png", boxplot_cat_zoom_5000, 
       width = 7, height = 4.5)
ggsave("graphics/boxplots/boxplot_cat_more_10_zoom_5000.png", boxplot_cat_more_10_zoom_5000, 
       width = 7, height = 4.5)
ggsave("graphics/boxplots/boxplot_cat_more_20_zoom_5000.png", boxplot_cat_more_20_zoom_5000, 
       width = 7, height = 4.5)