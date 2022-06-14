##### Smoothing with B-splines #####

# Packages
library(data.table) # faster computing
library(parallel) # parallelization
library(fda)

# Functions ---------------------------------------------------------------

# Each curve will be smoothed  individually - we are using different number of points for each curve -
# the original points plus 501 equally spaced points. 
smooth_curves = function(address_hash, dt, lambda_presmooth, lambda_deriv){
    #print(address_hash)
    dt = dt[address == address_hash]
    credit = dt[increment_type == "credit"]
    debt = dt[increment_type == "debt"]
    x_vec_credit = credit$ts_normal
    y_vec_credit = log(abs(credit$int_increments) + 1)
    x_vec_debt = debt$ts_normal
    y_vec_debt = log(abs(debt$int_increments) + 1)
    ts_normal_vec = seq(from = 0, to = 1, length.out = 501)
    
    # smoothing with bsplines
    basisobj_credit = create.bspline.basis(breaks = x_vec_credit)
    fdParobj_credit = fdPar(fdobj=basisobj_credit, Lfdobj=2)
    fdParobj_credit = fdPar(fdobj=basisobj_credit, Lfdobj=2, lambda = lambda_presmooth)
    smoothlist_credit = smooth.basis(x_vec_credit, y_vec_credit, fdParobj_credit)
    smooth_pred_credit = as.numeric(eval.fd(ts_normal_vec, smoothlist_credit$fd))
    
    basisobj_debt = create.bspline.basis(breaks = x_vec_debt)
    fdParobj_debt = fdPar(fdobj=basisobj_debt, Lfdobj=2, lambda = lambda_presmooth)
    smoothlist_debt = smooth.basis(x_vec_debt, y_vec_debt, fdParobj_debt)
    smooth_pred_debt = as.numeric(eval.fd(ts_normal_vec, smoothlist_debt$fd))
    
    # derivatives
    basis_deriv = create.bspline.basis(breaks = ts_normal_vec, norder = 5)
    fdPar_deriv = fdPar(fdobj = basis_deriv, Lfdobj = 3, lambda = lambda_deriv)
    smoothlist_deriv_credit = smooth.basis(ts_normal_vec, smooth_pred_credit, fdPar_deriv)
    deriv_credit = deriv.fd(smoothlist_deriv_credit$fd, 1)
    deriv_pred_credit = as.numeric(eval.fd(ts_normal_vec, deriv_credit))
    smoothlist_deriv_debt = smooth.basis(ts_normal_vec, smooth_pred_debt, fdPar_deriv)
    deriv_debt = deriv.fd(smoothlist_deriv_debt$fd, 1)
    deriv_pred_debt = as.numeric(eval.fd(ts_normal_vec, deriv_debt))
    
    #final data table
    dt_cred = data.table(address = address_hash, label = dt$label[1], category = dt$category[1],
                     increment_type = "credit", ts_normal = ts_normal_vec,
                     int_increments_sm = smooth_pred_credit, deriv_increments_sm = deriv_pred_credit,
                     count = dt$count[1])
    dt_debt = data.table(address = address_hash, label = dt$label[1], category = dt$category[1],
                     increment_type = "debt", ts_normal = ts_normal_vec, 
                     int_increments_sm = smooth_pred_debt, deriv_increments_sm = deriv_pred_debt,
                     count = dt$count[1])
    return(rbindlist(list(dt_cred, dt_debt)))
}

gcv = function(lamrange, lamdel, argvals, y, fdParobj){
    lamlow = lamrange[1]
    lamhi = lamrange[2]
    loglamvec = seq(lamlow, lamhi, lamdel)
    gcv_result = c()
    i = 1
    for (loglambda in loglamvec) {
        fdParobj$lambda = 10^(loglambda)
        smoothlist = smooth.basis(argvals, y, fdParobj)
        gcv_result[i] = as.numeric(smoothlist$gcv)  #  the mean of the N gcv values
        i = i + 1
    }
    return(loglamvec[which.min(gcv_result)])
}


# RUN ---------------------------------------------------------------------

start_time = Sys.time()
## 10 obs ##
wallets = data.table(readRDS("data/treated/wallets_presmooth_balanced_10obs.rds"))
setorder(wallets, address, increment_type, ts_normal)
wallets = rbindlist(mclapply(unique(wallets$address), smooth_curves, wallets, 0.001, 0.0001))
setorder(wallets, address, increment_type, ts_normal)
saveRDS(wallets, "data/treated/wallets_log_smoothed_balanced_10obs.rds")
print(Sys.time() - start_time)

## 20 obs ##
wallets = data.table(readRDS("data/treated/wallets_presmooth_balanced_20obs.rds"))
setorder(wallets, address, increment_type, ts_normal)
wallets = rbindlist(mclapply(unique(wallets$address), smooth_curves, wallets, 0.001, 0.0001))
setorder(wallets, address, increment_type, ts_normal)
saveRDS(wallets, "data/treated/wallets_log_smoothed_balanced_20obs.rds")
print(Sys.time() - start_time)
