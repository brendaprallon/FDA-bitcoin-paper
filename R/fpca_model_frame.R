#### FPCA  and building data frame for models #####

# Packages
library(data.table)
library(fda)


# Functions ---------------------------------------------------------------

# FPCA for rates
fpca_poisson_rates = function(dt_train, dt_test, h, 
                              lambda_smooth,
                              n_fpcs, increment_type = NULL){
    # Train
    dic_add_cat_train = unique(dt_train[, .(address, category)])
    dt_to_smooth_train = dcast(dt_train[, .(address, ts_normal, lambda)],
                               ts_normal ~ address, value.var = "lambda")
    eval_points_train = dt_to_smooth_train$ts_normal
    dt_to_smooth_train[, ts_normal := NULL]
    basisobj = create.bspline.basis(breaks = eval_points_train)
    fdParobj = fdPar(fdobj=basisobj, Lfdobj=2, lambda = lambda_smooth)
    smoothlist = smooth.basis(eval_points_train, as.matrix(dt_to_smooth_train), fdParobj)
    fpca = pca.fd(smoothlist$fd, nharm = n_fpcs)
    fpca_dt_train = cbind(dic_add_cat_train, fpca$scores)
    names(fpca_dt_train)[3:ncol(fpca_dt_train)] = paste0("FPC", c(1:n_fpcs), "_rates_", increment_type)
    # Test
    dic_add_cat_test = unique(dt_test[, .(address, category)])
    dt_to_smooth_test = dcast(dt_test[, .(address, ts_normal, lambda)],
                              ts_normal ~ address, value.var = "lambda")
    eval_points_test = dt_to_smooth_test$ts_normal
    dt_to_smooth_test[, ts_normal := NULL]
    basis_mat = eval.basis(eval_points_test, fpca$harmonics$basis)
    eigenfuns_coefs = fpca$harmonics$coefs
    eigenbasis_mat = basis_mat %*% eigenfuns_coefs
    penalty_mat = t(eigenfuns_coefs) %*% eval.penalty(basisobj, 2) %*% eigenfuns_coefs
    y_hat = as.matrix(dt_to_smooth_test)
    mean_fun = basis_mat %*% fpca$meanfd$coefs
    y_hat_cen = apply(y_hat, 2, function(x) x - mean_fun)
    fpca_est_coefs_test = solve(t(eigenbasis_mat)%*%eigenbasis_mat + lambda_smooth*penalty_mat)%*%
       t(eigenbasis_mat)%*%y_hat_cen
    fpca_dt_test = cbind(dic_add_cat_test, t(fpca_est_coefs_test))
    names(fpca_dt_test)[3:ncol(fpca_dt_test)] = paste0("FPC", c(1:n_fpcs), "_rates_", increment_type)
    return(list("train" = fpca_dt_train, "test" = fpca_dt_test, "fpca_varprop" = sum(fpca$varprop)))
}

# FPCA for curves
fpca_curves = function(dt_train, dt_test, 
                       lambda_smooth, 
                       n_fpcs, increment_type = NULL){
    # Train
    dic_add_cat_train = unique(dt_train[, .(address, category)])
    dt_to_smooth_train = dcast(dt_train[, .(address, ts_normal, int_increments_sm)],
                         ts_normal ~ address, value.var = "int_increments_sm")
    eval_points_train = dt_to_smooth_train$ts_normal
    dt_to_smooth_train[, ts_normal := NULL]
    basisobj = create.bspline.basis(breaks = eval_points_train)
    fdParobj = fdPar(fdobj=basisobj, Lfdobj=2, lambda = lambda_smooth)
    smoothlist = smooth.basis(eval_points_train, as.matrix(dt_to_smooth_train), fdParobj)
    fpca = pca.fd(smoothlist$fd, nharm = n_fpcs)
    fpca_dt_train = cbind(dic_add_cat_train, fpca$scores)
    names(fpca_dt_train)[3:ncol(fpca_dt_train)] = paste0("FPC", c(1:n_fpcs), "_curves_", increment_type)
    # Test
    dic_add_cat_test = unique(dt_test[, .(address, category)])
    dt_to_smooth_test = dcast(dt_test[, .(address, ts_normal, int_increments_sm)],
                               ts_normal ~ address, value.var = "int_increments_sm")
    eval_points_test = dt_to_smooth_test$ts_normal
    dt_to_smooth_test[, ts_normal := NULL]
    basis_mat = eval.basis(eval_points_test, fpca$harmonics$basis)
    eigenfuns_coefs = fpca$harmonics$coefs
    eigenbasis_mat = basis_mat %*% eigenfuns_coefs
    penalty_mat = t(eigenfuns_coefs) %*% eval.penalty(basisobj, 2) %*% eigenfuns_coefs
    y_hat = as.matrix(dt_to_smooth_test)
    mean_fun = basis_mat %*% fpca$meanfd$coefs
    y_hat_cen = apply(y_hat, 2, function(x) x - mean_fun)
    fpca_est_coefs_test = solve(t(eigenbasis_mat)%*%eigenbasis_mat + lambda_smooth*penalty_mat)%*%
        t(eigenbasis_mat)%*%y_hat_cen
    fpca_dt_test = cbind(dic_add_cat_test, t(fpca_est_coefs_test))
    names(fpca_dt_test)[3:ncol(fpca_dt_test)] = paste0("FPC", c(1:n_fpcs), "_curves_", increment_type)
    return(list("train" = fpca_dt_train, "test" = fpca_dt_test, "fpca_varprop" = sum(fpca$varprop)))
}

# FPCA for derivatives
fpca_derivatives = function(dt_train, dt_test,
                            lambda_smooth, 
                            n_fpcs, increment_type = NULL){
    # Train
    dic_add_cat_train = unique(dt_train[, .(address, category)])
    dt_to_smooth_train = dcast(dt_train[, .(address, ts_normal, deriv_increments_sm)],
                               ts_normal ~ address, value.var = "deriv_increments_sm")
    eval_points_train = dt_to_smooth_train$ts_normal
    dt_to_smooth_train[, ts_normal := NULL]
    basisobj = create.bspline.basis(breaks = eval_points_train)
    fdParobj = fdPar(fdobj=basisobj, Lfdobj=2, lambda = lambda_smooth)
    smoothlist = smooth.basis(eval_points_train, as.matrix(dt_to_smooth_train), fdParobj)
    fpca = pca.fd(smoothlist$fd, nharm = n_fpcs)
    fpca_dt_train = cbind(dic_add_cat_train, fpca$scores)
    names(fpca_dt_train)[3:ncol(fpca_dt_train)] = paste0("FPC", c(1:n_fpcs), "_deriv_", increment_type)
    # Test
    dic_add_cat_test = unique(dt_test[, .(address, category)])
    dt_to_smooth_test = dcast(dt_test[, .(address, ts_normal, deriv_increments_sm)],
                              ts_normal ~ address, value.var = "deriv_increments_sm")
    eval_points_test = dt_to_smooth_test$ts_normal
    dt_to_smooth_test[, ts_normal := NULL]
    basis_mat = eval.basis(eval_points_test, fpca$harmonics$basis)
    eigenfuns_coefs = fpca$harmonics$coefs
    eigenbasis_mat = basis_mat %*% eigenfuns_coefs
    penalty_mat = t(eigenfuns_coefs) %*% eval.penalty(basisobj, 2) %*% eigenfuns_coefs
    y_hat = as.matrix(dt_to_smooth_test)
    mean_fun = basis_mat %*% fpca$meanfd$coefs
    y_hat_cen = apply(y_hat, 2, function(x) x - mean_fun)
    fpca_est_coefs_test = solve(t(eigenbasis_mat)%*%eigenbasis_mat + lambda_smooth*penalty_mat)%*%
        t(eigenbasis_mat)%*%y_hat_cen
    # fpca_est_coefs_test = solve(t(eigenbasis_mat)%*%eigenbasis_mat)%*% t(eigenbasis_mat)%*%y_hat_cen #
    fpca_dt_test = cbind(dic_add_cat_test, t(fpca_est_coefs_test))
    names(fpca_dt_test)[3:ncol(fpca_dt_test)] = paste0("FPC", c(1:n_fpcs), "_deriv_", increment_type)
    return(list("train" = fpca_dt_train, "test" = fpca_dt_test, "fpca_varprop" = sum(fpca$varprop)))
}

build_model_frame = function(dt_scalar, dt_rate, dt_curves, min_obs, addr_train, addr_test, lambda_smooth, n_fpcs){
    # fpcas
    dt_rate_credit = fpca_poisson_rates(dt_rate[increment_type == "credit" & address %in% addr_train],
                                        dt_rate[increment_type == "credit" & address %in% addr_test],
                                        h, lambda_smooth, n_fpcs, increment_type = "credit")
    dt_rate_debt = fpca_poisson_rates(dt_rate[increment_type == "debt" & address %in% addr_train],
                                        dt_rate[increment_type == "debt" & address %in% addr_test],
                                        h, lambda_smooth, n_fpcs, increment_type = "debt")
    dt_curves_credit = fpca_curves(dt_curves[increment_type == "credit" & address %in% addr_train],
                                   dt_curves[increment_type == "credit" & address %in% addr_test],
                                   lambda_smooth, n_fpcs, increment_type = "credit")
    dt_curves_debt = fpca_curves(dt_curves[increment_type == "debt" & address %in% addr_train],
                                   dt_curves[increment_type == "debt" & address %in% addr_test],
                                   lambda_smooth, n_fpcs, increment_type = "debt")
    dt_deriv_credit = fpca_derivatives(dt_curves[increment_type == "credit" & address %in% addr_train],
                                       dt_curves[increment_type == "credit" & address %in% addr_test],
                                       lambda_smooth, n_fpcs, increment_type = "credit")
    dt_deriv_debt = fpca_derivatives(dt_curves[increment_type == "debt" & address %in% addr_train],
                                       dt_curves[increment_type == "debt" & address %in% addr_test],
                                       lambda_smooth, n_fpcs, increment_type = "debt")
    dt_train_list = list(dt_scalar[address %in% addr_train], dt_rate_credit$train, dt_rate_debt$train,
                         dt_curves_credit$train, dt_curves_debt$train, dt_deriv_credit$train, dt_deriv_debt$train)
    dt_test_list = list(dt_scalar[address %in% addr_test], dt_rate_credit$test, dt_rate_debt$test,
                         dt_curves_credit$test, dt_curves_debt$test, dt_deriv_credit$test, dt_deriv_debt$test)
    dt_train = Reduce(function(...) merge(..., all = TRUE, by = c("address", "category")), dt_train_list)
    dt_train = dt_train[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]
    dt_train[, category:=as.factor(category)]
    dt_test = Reduce(function(...) merge(..., all = TRUE, by = c("address", "category")), dt_test_list)
    dt_test = dt_test[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]
    dt_test[, category:=as.factor(category)]
    varprop = list("credit" = list("curves" = dt_curves_credit$fpca_varprop, 
                                   "derivatives" = dt_deriv_credit$fpca_varprop, 
                                   "rates" = dt_rate_credit$fpca_varprop),
                   "debt" = list("curves" = dt_curves_debt$fpca_varprop, 
                                 "derivatives" = dt_deriv_debt$fpca_varprop, 
                                 "rates" = dt_rate_debt$fpca_varprop))
    return(list("train" = dt_train, "test" = dt_test, "varprop" = varprop))
}

