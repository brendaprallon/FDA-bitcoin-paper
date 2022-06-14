#### MASTER ####

# Description of scripts used in the final paper with input and output files

# Data in Google database -------------------------------------------------
# Data gathered from: https://cloud.google.com/blog/products/data-analytics/introducing-six-new-cryptocurrencies-in-bigquery-public-datasets-and-how-to-analyze-them
## Input files
# data/raw/bitcoin_address_inputs_outputs_v2_indexed.sqlite3 (needs to be decompressed)
## Output files
# data/raw/wallets_from_blockchain.rds
source("R/read_data_from_blockchain.R")


# Plotting number of wallets ----------------------------------------------
# Plotting number of wallets with minimum of 10/20 observations by different life spans ranging from 500h to 5000h, by 500h
# Using this plot we chose to use 3000h as the lifespan
## Input files
# data/raw/wallets_from_blockchain.rds
# data/labels/label_addresses.csv # dictionary with labels/addresses
# data/labels/labels.csv # dictionary with labels/categories
## Output files
# graphics/life span/min_20obs.png
# graphics/life span/min_20obs.png
source('R/plot_life_span.R')


# Data treatment ----------------------------------------------------------
# Appending all intermedate files and treated the data: creating credits and debts, filtering for the first 2000 hours, more than 10 and ess than 500 observations
## Input files
# data/raw/wallets_from_blockchain.rds
# data/labels/label_addresses.csv # dictionary with labels/addresses
# data/labels/labels.csv # dictionary with labels/categories
# data/raw/btc.csv # btc to USD table
## Output files
# data/treated/cred_debt_complete.rds # all treated data
# data/treated/cred_debt_complete_10obs.rds # all treated data, min 10 obs
# data/treated/cred_debt_complete_20obs.rds # all treated data, min 20 obs
# data/treated/plt_addr_list_10obs.rds # list with 12 sample addresses per category, to use for plotting, min 10 obs
# data/treated/plt_addr_list_20obs.rds # list with 12 sample addresses per category, to use for plotting, min 20 obs
# data/treated/cred_debt_balanced_10obs.rds # balanced sample of treated data, min 10 obs. Obs.: the balanced sample could be selected later, but since there is heavy computation in the following codes, we select it now to run the next scripts only with them.
# data/treated/cred_debt_balanced_20obs.rds # balanced sample of treated data, min 20 obs
# tables/latex/sh_cat_10obs.tex # table of category shares after treatment, min 10 obs
# tables/latex/sh_cat_20obs.tex # table of category shares after treatment, min 20 obs
# tables/latex/n_cat_10obs.tex # table of number of addresses after treatment, min 10 obs
# tables/latex/n_cat_20obs.tex # table of number of addresses after treatment, min 20 obs
# tables/latex/start_dates_cat.tex  # starting dates of wallets per category
# tables/latex/count_label_cat.tex  # number of labels per category 
# graphics/first dates/first_dates_density.png # density of first dates per category
source("R/wallets_treatment_complete.R")
# Time difference of -17.0238 mins


# Plots: original functions (values) --------------------------------------
## Input files
# data/treated/cred_debt_complete.rds # all treated data
# data/treated/plt_addr_list_10obs.rds # list with 12 sample addresses per category, to use for plotting, min 10 obs
# data/treated/plt_addr_list_20obs.rds # list with 12 sample addresses per category, to use for plotting, min 20 obs
# R/aux_functions.R # script with some auxiliary functions
## Ouput files
# (y scale is fixed for plots)
# graphics/curves/original/fixed scale/original_curves_darknet_marketplace_10obs.png
# graphics/curves/original/fixed scale/original_curves_darknet_marketplace_20obs.png
# graphics/curves/original/fixed scale/original_curves_gambling_10obs.png
# graphics/curves/original/fixed scale/original_curves_gambling_20obs.png
# graphics/curves/original/fixed scale/original_curves_exchanges_10obs.png
# graphics/curves/original/fixed scale/original_curves_exchanges_20obs.png
# graphics/curves/original/fixed scale/original_curves_pools_10obs.png
# graphics/curves/original/fixed scale/original_curves_pools_20obs.png
# graphics/curves/original/fixed scale/original_curves_services_others_10obs.png
# graphics/curves/original/fixed scale/original_curves_services_others_20obs.png
# (y scale varys between plots)
# graphics/curves/original/free scale/original_curves_darknet_marketplace_10obs.png
# graphics/curves/original/free scale/original_curves_darknet_marketplace_20obs.png
# graphics/curves/original/free scale/original_curves_gambling_10obs.png
# graphics/curves/original/free scale/original_curves_gambling_20obs.png
# graphics/curves/original/free scale/original_curves_exchanges_10obs.png
# graphics/curves/original/free scale/original_curves_exchanges_20obs.png
# graphics/curves/original/free scale/original_curves_pools_10obs.png
# graphics/curves/original/free scale/original_curves_pools_20obs.png
# graphics/curves/original/free scale/original_curves_services_others_10obs.png
# graphics/curves/original/free scale/original_curves_services_others_20obs.png
source("R/plot_original_curves.R")


# Pre-smoothing -----------------------------------------------------------
# Pre-smoothing data: evaluating credits and debts on the original points + 501 equaly spaced points
## Input files
# data/treated/cred_debt_balanced_10obs.rds # balanced sample of treated data, min 10 obs
# data/treated/cred_debt_balanced_20obs.rds # balanced sample of treated data, min 20 obs
## Output files
# data/treated/wallets_presmooth_balanced_10obs.rds # balanced sample with step functions, min 10 obs
# data/treated/wallets_presmooth_balanced_20obs.rds # balanced sample with step functions, min 20 obs
source("R/wallets_pre_smooth.R")


# Plots: step functions, credits and debts --------------------------------
## Input files
# data/treated/plt_addr_list_10obs.rds # list with 12 sample addresses per category, to use for plotting, min 10 obs
# data/treated/plt_addr_list_20obs.rds # list with 12 sample addresses per category, to use for plotting, min 20 obs
# data/treated/cred_debt_balanced_10obs.rds # balanced sample of treated data, min 10 obs. Obs.: the balanced sample could be selected later, but since there is heavy computation in the following codes, we select it now to run the next scripts only with them.
# data/treated/cred_debt_balanced_20obs.rds # balanced sample of treated data, min 20 obs
## Output files
# (y scale is fixed for plots)
# graphics/curves/step/fixed scale/step_curves_darknet_marketplace_10obs.png
# graphics/curves/step/fixed scale/step_curves_darknet_marketplace_20obs.png
# graphics/curves/step/fixed scale/step_curves_exchanges_10obs.png
# graphics/curves/step/fixed scale/step_curves_exchanges_20obs.png
# graphics/curves/step/fixed scale/step_curves_gambling_10obs.png
# graphics/curves/step/fixed scale/step_curves_gambling_20obs.png
# graphics/curves/step/fixed scale/step_curves_pools_10obs.png
# graphics/curves/step/fixed scale/step_curves_pools_20obs.png
# graphics/curves/step/fixed scale/step_curves_services_others_10obs.png
# graphics/curves/step/fixed scale/step_curves_services_others_20obs.png
# (y scale varys between plots)
# graphics/curves/step/free scale/step_curves_darknet_marketplace_10obs.png
# graphics/curves/step/free scale/step_curves_darknet_marketplace_20obs.png
# graphics/curves/step/free scale/step_curves_exchanges_10obs.png
# graphics/curves/step/free scale/step_curves_exchanges_20obs.png
# graphics/curves/step/free scale/step_curves_gambling_10obs.png
# graphics/curves/step/free scale/step_curves_gambling_20obs.png
# graphics/curves/step/free scale/step_curves_pools_10obs.png
# graphics/curves/step/free scale/step_curves_pools_20obs.png
# graphics/curves/step/free scale/step_curves_services_others_10obs.png
# graphics/curves/step/free scale/step_curves_services_others_20obs.png
source("R/plot_step_functions.R")


# Smoothing credits and debts with b-splines ------------------------------
## Input files
# data/treated/wallets_presmooth_balanced_10obs.rds # balanced sample with step functions, min 10 obs
# data/treated/wallets_presmooth_balanced_10obs.rds # balanced sample with step functions, min 20 obs
## Ouput files
# data/treated/wallets_log_smoothed_balanced_10obs.rds # balanced sample with smoothed cruves, min 10 obs
# data/treated/wallets_log_smoothed_balanced_20obs.rds # balanced sample with smoothed cruves, min 20 obs
source("R/smoothing.R")
# Time difference of 14.09227 hours


# Plots: smooth functions, credits and debts ------------------------------
## Input files
# data/treated/plt_addr_list_10obs.rds # list with 12 sample addresses per category, to use for plotting, min 10 obs
# data/treated/plt_addr_list_20obs.rds # list with 12 sample addresses per category, to use for plotting, min 20 obs
# data/treated/wallets_presmooth_balanced_10obs.rds # balanced sample with step functions, min 10 obs
# data/treated/wallets_presmooth_balanced_10obs.rds # balanced sample with step functions, min 20 obs
## Output files
# (y scale is fixed for plots)
# Credit
# graphics/curves/smooth/fixed scale/smooth_curves_darknet_marketplace_10obs_credit.png
# graphics/curves/smooth/fixed scale/smooth_curves_darknet_marketplace_20obs_credit.png
# graphics/curves/smooth/fixed scale/smooth_curves_exchanges_10obs_credit.png
# graphics/curves/smooth/fixed scale/smooth_curves_exchanges_20obs_credit.png
# graphics/curves/smooth/fixed scale/smooth_curves_gambling_10obs_credit.png
# graphics/curves/smooth/fixed scale/smooth_curves_gambling_20obs_credit.png
# graphics/curves/smooth/fixed scale/smooth_curves_pools_10obs_credit.png
# graphics/curves/smooth/fixed scale/smooth_curves_pools_20obs_credit.png
# graphics/curves/smooth/fixed scale/smooth_curves_service_others_10obs_credit.png
# graphics/curves/smooth/fixed scale/smooth_curves_service_others_20obs_credit.png
# Debt
# graphics/curves/smooth/fixed scale/smooth_curves_darknet_marketplace_10obs_debt.png
# graphics/curves/smooth/fixed scale/smooth_curves_darknet_marketplace_20obs_debt.png
# graphics/curves/smooth/fixed scale/smooth_curves_exchanges_10obs_debt.png
# graphics/curves/smooth/fixed scale/smooth_curves_exchanges_20obs_debt.png
# graphics/curves/smooth/fixed scale/smooth_curves_gambling_10obs_debt.png
# graphics/curves/smooth/fixed scale/smooth_curves_gambling_20obs_debt.png
# graphics/curves/smooth/fixed scale/smooth_curves_pools_10obs_debt.png
# graphics/curves/smooth/fixed scale/smooth_curves_pools_20obs_debt.png
# graphics/curves/smooth/fixed scale/smooth_curves_service_others_10obs_debt.png
# graphics/curves/smooth/fixed scale/smooth_curves_service_others_20obs_debt.png
# (y scale varys between plots)
# Credit
# graphics/curves/smooth/free scale/smooth_curves_darknet_marketplace_10obs_credit.png
# graphics/curves/smooth/free scale/smooth_curves_darknet_marketplace_20obs_credit.png
# graphics/curves/smooth/free scale/smooth_curves_exchanges_10obs_credit.png
# graphics/curves/smooth/free scale/smooth_curves_exchanges_20obs_credit.png
# graphics/curves/smooth/free scale/smooth_curves_gambling_10obs_credit.png
# graphics/curves/smooth/free scale/smooth_curves_gambling_20obs_credit.png
# graphics/curves/smooth/free scale/smooth_curves_pools_10obs_credit.png
# graphics/curves/smooth/free scale/smooth_curves_pools_20obs_credit.png
# graphics/curves/smooth/free scale/smooth_curves_service_others_10obs_credit.png
# graphics/curves/smooth/free scale/smooth_curves_service_others_20obs_credit.png
# Debt
# graphics/curves/smooth/free scale/smooth_curves_darknet_marketplace_10obs_debt.png
# graphics/curves/smooth/free scale/smooth_curves_darknet_marketplace_20obs_debt.png
# graphics/curves/smooth/free scale/smooth_curves_exchanges_10obs_debt.png
# graphics/curves/smooth/free scale/smooth_curves_exchanges_20obs_debt.png
# graphics/curves/smooth/free scale/smooth_curves_gambling_10obs_debt.png
# graphics/curves/smooth/free scale/smooth_curves_gambling_20obs_debt.png
# graphics/curves/smooth/free scale/smooth_curves_pools_10obs_debt.png
# graphics/curves/smooth/free scale/smooth_curves_pools_20obs_debt.png
# graphics/curves/smooth/free scale/smooth_curves_service_others_10obs_debt.png
# graphics/curves/smooth/free scale/smooth_curves_service_others_20obs_debt.png
source("R/plot_smooth_curves.R")


# Plots: smooth derivatives, credits and debts ----------------------------
## Input files
# data/treated/plt_addr_list_10obs.rds # list with 12 sample addresses per category, to use for plotting, min 10 obs
# data/treated/plt_addr_list_20obs.rds # list with 12 sample addresses per category, to use for plotting, min 20 obs
# data/treated/wallets_log_smoothed_balanced_10obs.rds # balanced sample with smooth functions and their derivatives, min 10 obs
# data/treated/wallets_log_smoothed_balanced_20obs.rds # balanced sample with smooth functions and their derivatives, min 20 obs
## Output files
# (y scale is fixed for plots)
# Credit
# graphics/derivatives/fixed scale/smooth_derivatives_darknet_marketplace_10obs_credit.png
# graphics/derivatives/fixed scale/smooth_derivatives_darknet_marketplace_20obs_credit.png
# graphics/derivatives/fixed scale/smooth_derivatives_exchanges_10obs_credit.png
# graphics/derivatives/fixed scale/smooth_derivatives_exchanges_20obs_credit.png
# graphics/derivatives/fixed scale/smooth_derivatives_gambling_10obs_credit.png
# graphics/derivatives/fixed scale/smooth_derivatives_gambling_20obs_credit.png
# graphics/derivatives/fixed scale/smooth_derivatives_pools_10obs_credit.png
# graphics/derivatives/fixed scale/smooth_derivatives_pools_20obs_credit.png
# graphics/derivatives/fixed scale/smooth_derivatives_service_others_10obs_credit.png
# graphics/derivatives/fixed scale/smooth_derivatives_service_others_20obs_credit.png
# Debt
# graphics/derivatives/fixed scale/smooth_derivatives_darknet_marketplace_10obs_debt.png
# graphics/derivatives/fixed scale/smooth_derivatives_darknet_marketplace_20obs_debt.png
# graphics/derivatives/fixed scale/smooth_derivatives_exchanges_10obs_debt.png
# graphics/derivatives/fixed scale/smooth_derivatives_exchanges_20obs_debt.png
# graphics/derivatives/fixed scale/smooth_derivatives_gambling_10obs_debt.png
# graphics/derivatives/fixed scale/smooth_derivatives_gambling_20obs_debt.png
# graphics/derivatives/fixed scale/smooth_derivatives_pools_10obs_debt.png
# graphics/derivatives/fixed scale/smooth_derivatives_pools_20obs_debt.png
# graphics/derivatives/fixed scale/smooth_derivatives_service_others_10obs_debt.png
# graphics/derivatives/fixed scale/smooth_derivatives_service_others_20obs_debt.png
# (y scale varys between plots)
# Credit
# graphics/derivatives/free scale/smooth_derivatives_darknet_marketplace_10obs_credit.png
# graphics/derivatives/free scale/smooth_derivatives_darknet_marketplace_20obs_credit.png
# graphics/derivatives/free scale/smooth_derivatives_exchanges_10obs_credit.png
# graphics/derivatives/free scale/smooth_derivatives_exchanges_20obs_credit.png
# graphics/derivatives/free scale/smooth_derivatives_gambling_10obs_credit.png
# graphics/derivatives/free scale/smooth_derivatives_gambling_20obs_credit.png
# graphics/derivatives/free scale/smooth_derivatives_pools_10obs_credit.png
# graphics/derivatives/free scale/smooth_derivatives_pools_20obs_credit.png
# graphics/derivatives/free scale/smooth_derivatives_service_others_10obs_credit.png
# graphics/derivatives/free scale/smooth_derivatives_service_others_20obs_credit.png
# Debt
# graphics/derivatives/free scale/smooth_derivatives_darknet_marketplace_10obs_debt.png
# graphics/derivatives/free scale/smooth_derivatives_darknet_marketplace_20obs_debt.png
# graphics/derivatives/free scale/smooth_derivatives_exchanges_10obs_debt.png
# graphics/derivatives/free scale/smooth_derivatives_exchanges_20obs_debt.png
# graphics/derivatives/free scale/smooth_derivatives_gambling_10obs_debt.png
# graphics/derivatives/free scale/smooth_derivatives_gambling_20obs_debt.png
# graphics/derivatives/free scale/smooth_derivatives_pools_10obs_debt.png
# graphics/derivatives/free scale/smooth_derivatives_pools_20obs_debt.png
# graphics/derivatives/free scale/smooth_derivatives_service_others_10obs_debt.png
# graphics/derivatives/free scale/smooth_derivatives_service_others_20obs_debt.png
source("R/plot_smooth_derivatives.R")


# Calculating poisson rates  ----------------------------------------------
## Input files
# data/treated/cred_debt_balanced_10obs.rds # balanced sample of treated data, min 10 obs
# data/treated/cred_debt_balanced_20obs.rds # balanced sample of treated data, min 20 obs
## Output files
# data/treated/rates/poisson_rates_10obs.rds 
# data/treated/rates/poisson_rates_20obs.rds 
source("R/poisson_rates.R") 


# Plots for the poisson rates, credits and debts --------------------------
## Input files
# data/treated/plt_addr_list_10obs.rds # list with 12 sample addresses per category, to use for plotting, min 10 obs
# data/treated/plt_addr_list_20obs.rds # list with 12 sample addresses per category, to use for plotting, min 20 obs
## Output files
# (y scale is fixed for plots)
# Credit
# graphics/poisson rates/fixed scale/poisson_rates_darknet_marketplace_10obs_credit.png
# graphics/poisson rates/fixed scale/poisson_rates_darknet_marketplace_20obs_credit.png
# graphics/poisson rates/fixed scale/poisson_rates_exchanges_10obs_credit.png
# graphics/poisson rates/fixed scale/poisson_rates_exchanges_20obs_credit.png
# graphics/poisson rates/fixed scale/poisson_rates_gambling_10obs_credit.png
# graphics/poisson rates/fixed scale/poisson_rates_gambling_20obs_credit.png
# graphics/poisson rates/fixed scale/poisson_rates_pools_10obs_credit.png
# graphics/poisson rates/fixed scale/poisson_rates_pools_20obs_credit.png
# graphics/poisson rates/fixed scale/poisson_rates_service_others_10obs_credit.png
# graphics/poisson rates/fixed scale/poisson_rates_service_others_20obs_credit.png
# Debt
# graphics/poisson rates/fixed scale/poisson_rates_darknet_marketplace_10obs_debt.png
# graphics/poisson rates/fixed scale/poisson_rates_darknet_marketplace_20obs_debt.png
# graphics/poisson rates/fixed scale/poisson_rates_exchanges_10obs_debt.png
# graphics/poisson rates/fixed scale/poisson_rates_exchanges_20obs_debt.png
# graphics/poisson rates/fixed scale/poisson_rates_gambling_10obs_debt.png
# graphics/poisson rates/fixed scale/poisson_rates_gambling_20obs_debt.png
# graphics/poisson rates/fixed scale/poisson_rates_pools_10obs_debt.png
# graphics/poisson rates/fixed scale/poisson_rates_pools_20obs_debt.png
# graphics/poisson rates/fixed scale/poisson_rates_service_others_10obs_debt.png
# graphics/poisson rates/fixed scale/poisson_rates_service_others_20obs_debt.png
# (y scale varys between plots)
# Credit
# graphics/poisson rates/free scale/poisson_rates_darknet_marketplace_10obs_credit.png
# graphics/poisson rates/free scale/poisson_rates_darknet_marketplace_20obs_credit.png
# graphics/poisson rates/free scale/poisson_rates_exchanges_10obs_credit.png
# graphics/poisson rates/free scale/poisson_rates_exchanges_20obs_credit.png
# graphics/poisson rates/free scale/poisson_rates_gambling_10obs_credit.png
# graphics/poisson rates/free scale/poisson_rates_gambling_20obs_credit.png
# graphics/poisson rates/free scale/poisson_rates_pools_10obs_credit.png
# graphics/poisson rates/free scale/poisson_rates_pools_20obs_credit.png
# graphics/poisson rates/free scale/poisson_rates_service_others_10obs_credit.png
# graphics/poisson rates/free scale/poisson_rates_service_others_20obs_credit.png
# Debt
# graphics/poisson rates/free scale/poisson_rates_darknet_marketplace_10obs_debt.png
# graphics/poisson rates/free scale/poisson_rates_darknet_marketplace_20obs_debt.png
# graphics/poisson rates/free scale/poisson_rates_exchanges_10obs_debt.png
# graphics/poisson rates/free scale/poisson_rates_exchanges_20obs_debt.png
# graphics/poisson rates/free scale/poisson_rates_gambling_10obs_debt.png
# graphics/poisson rates/free scale/poisson_rates_gambling_20obs_debt.png
# graphics/poisson rates/free scale/poisson_rates_pools_10obs_debt.png
# graphics/poisson rates/free scale/poisson_rates_pools_20obs_debt.png
# graphics/poisson rates/free scale/poisson_rates_service_others_10obs_debt.png
# graphics/poisson rates/free scale/poisson_rates_service_others_20obs_debt.png
source("R/plot_poisson_rates.R")


# Scalar features ---------------------------------------------------------
## Input files
# data/treated/cred_debt_balanced_10obs.rds # balanced sample of treated data, min 10 obs
# data/treated/cred_debt_balanced_20obs.rds # balanced sample of treated data, min 20 obs
## Output files
# data/treated/scalar/scalar_features_10obs.rds # scalar features for balanced sample of treated data, min 10 obs
# data/treated/scalar/scalar_features_20obs.rds # scalar features for balanced sample of treated data, min 20 obs
source("R/scalar_features.R")


# Creating partitions  ----------------------------------------------------
## Input files
# data/treated/scalar/scalar_features_10obs.rds
# data/treated/scalar/scalar_features_20obs.rds
## Output files
# data/treated/partitions/binary_kfold_addr_k5_10obs.rds 
# data/treated/partitions/binary_kfold_addr_k5_20obs.rds 
# data/treated/partitions/binary_test_addr_p0.2_10obs.rds 
# data/treated/partitions/binary_test_addr_p0.2_20obs.rds 
# data/treated/partitions/kfold_addr_k5_10obs.rds 
# data/treated/partitions/kfold_addr_k5_20obs.rds 
# data/treated/partitions/test_addr_p0.2_10obs.rds 
# data/treated/partitions/test_addr_p0.2_20obs.rds
source("R/creating_partitions.R")


# Models --------------------------------------------------------------
## Input files
# R/models.R (which uses R/fpca_model_frame.R)
# data/treated/scalar/scalar_features_10obs.rds
# data/treated/rates/poisson_rates_10obs.rds
# data/treated/wallets_log_smoothed_balanced_10obs.rds
# data/treated/scalar/scalar_features_20obs.rds
# data/treated/rates/poisson_rates_20obs.rds
# data/treated/wallets_log_smoothed_balanced_20obs.rds
## Output files:
# All in folder models/multiclass/min_obs_10/
# All in folder models/multiclass/min_obs_20/
source("R/experiments.R")


# Latex table results -----------------------------------------------------
## Input files
# All in folder models/multiclass/min_obs_10/
# All in folder models/multiclass/min_obs_20/
## Output files:
#results/min_10_obs.rds 
#results/min_20_obs.rds
source("R/table_results.R")


# Latex table results -----------------------------------------------------
## Input files
# data/treated/scalar/scalar_features_10obs.rds
# results/min_10obs.rds
#
# data/treated/rates/poisson_rates_10obs.rds
#
# data/treated/wallets_log_smoothed_balanced_10obs.rds
# data/treated/partitions/kfold_addr_k5_10obs.rds
# data/treated/partitions/test_addr_p0.2_10obs.rds
#
# data/treated/scalar/scalar_features_20obs.rds
# results/min_20obs.rds
#
# data/treated/rates/poisson_rates_20obs.rds
#
# data/treated/wallets_log_smoothed_balanced_20obs.rds
# data/treated/partitions/kfold_addr_k5_20obs.rds
# data/treated/partitions/test_addr_p0.2_20obs.rds
# Output files
# All in models/test/multiclass
source("R/testing_models.R")


# Latex table results -----------------------------------------------------
## Input Files
# R/aux_functions.R
# models/test/multiclass/winning_rf_10obs.rds
# models/test/multiclass/winning_gbm_10obs.rds
# models/test/multiclass/winning_multi_10obs.rds
# models/test/multiclass/winning_rf_20obs.rds
# models/test/multiclass/winning_gbm_20obs.rds
# models/test/multiclass/winning_multi_20obs.rds
## Output files
# tables/latex/rf_10obs_acc.tex
# tables/latex/gbm_10obs_acc.tex
# tables/latex/multi_10obs_acc.tex
# tables/latex/rf_10obs_acc_cat.tex
# tables/latex/gbm_10obs_acc_cat.tex
# tables/latex/multi_10obs_acc_cat.tex
# tables/latex/rf_20obs_acc.tex
# tables/latex/gbm_20obs_acc.tex
# tables/latex/multi_20obs_acc.tex
# tables/latex/rf_20obs_acc_cat.tex
# tables/latex/gbm_20obs_acc_cat.tex
# tables/latex/multi_20obs_acc_cat.tex
source("R/latex_table_results.R")


# Plot: Accuracy vs number of FPCs ----------------------------------------
## Input files 
# models/test/multiclass/fpc1_rf_10obs.rds
# models/test/multiclass/fpc3_rf_10obs.rds
# models/test/multiclass/fpc5_rf_10obs.rds
# models/test/multiclass/fpc7_rf_10obs.rds
# models/test/multiclass/fpc1_rf_20obs.rds
# models/test/multiclass/fpc3_rf_20obs.rds
# models/test/multiclass/fpc5_rf_20obs.rds
# models/test/multiclass/fpc7_rf_20obs.rds
## Output files
# graphics/n fpcs/acc_vs_variance_10obs.png
# graphics/n fpcs/acc_vs_variance_20obs.png
source("R/plot_nfpcs_vs_variance.R")


# Plot: Eigenfunctions ----------------------------------------------------
## Input files
# data/treated/wallets_log_smoothed_balanced_10obs.rds
# data/treated/rates/poisson_rates_10obs.rds
# data/treated/partitions/kfold_addr_k5_10obs.rds
# data/treated/partitions/test_addr_p0.2_10obs.rds
# data/treated/wallets_log_smoothed_balanced_20obs.rds
# data/treated/rates/poisson_rates_20obs.rds
# data/treated/partitions/kfold_addr_k5_20obs.rds
# data/treated/partitions/test_addr_p0.2_20obs.rds
## Output files
# all in graphics/fpca/
source("R/plot_fpcas.R")


# Plot: Accuracy  ---------------------------------------------------------
## Input files 
# R/aux_functions.R
# models/test/multiclass/winning_gbm_10obs.rds
# models/test/multiclass/winning_multi_10obs.rds
# models/test/multiclass/winning_rf_10obs.rds
# models/test/multiclass/winning_svm_10obs.rds
# models/test/multiclass/winning_gbm_20obs.rds
# models/test/multiclass/winning_multi_20obs.rds
# models/test/multiclass/winning_rf_20obs.rds
# models/test/multiclass/winning_svm_20obs.rds
## Output files
# All in graphics/accuracy/
source("R/plot_accuracy.R")


# Plot: Feature Importance of winning model -------------------------------
## Input files
## Input files
# R/models.R (which uses R/fpca_model_frame.R)
# data/treated/scalar/scalar_features_10obs.rds
# data/treated/rates/poisson_rates_10obs.rds
# data/treated/wallets_log_smoothed_balanced_10obs.rds
# results/min_10obs.rds
# data/treated/scalar/scalar_features_20obs.rds
# data/treated/rates/poisson_rates_20obs.rds
# data/treated/wallets_log_smoothed_balanced_20obs.rds
# results/min_20obs.rds
## Output files
# All in graphics/feature importance
source("R/plot_feature_importance.R")