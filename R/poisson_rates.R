######### Poisson Rate #############

# Packages
library(data.table)
library(fda)
library(parallel)


# Functions ---------------------------------------------------------------

# Create functional poisson curve
smooth_rates = function(addr_hash, dt, lambda_presmooth){
    print(addr_hash)
    dt_credit = dt[address == addr_hash & increment_type == "credit"]
    dt_debt = dt[address == addr_hash & increment_type == "debt"]
    ts_normal_vec = seq(from = 0, to = 1, length.out = 501)
    # credit
    if (nrow(dt_credit) == 1){
        poisson_rate_credit = rep(0, 501)
    }else if (nrow(dt_credit) == 2){
        tvec_credit = dt_credit$ts_normal[2:length(dt_credit$ts_normal)]
        breaks_credit =  c(0,tvec_credit,1)
        tbasis_credit = create.bspline.basis(rangeval = c(0,1), breaks = breaks_credit, norder = 3)
        Wfd0_credit = fd(matrix(0,length(breaks_credit)+1,1),tbasis_credit)
        WfdParobj_credit = fdPar(Wfd0_credit, 1, lambda_presmooth)
        Wfdobj_credit = intensity.fd(tvec_credit, WfdParobj_credit, dbglev = 0)$Wfdobj
        poisson_rate_credit = exp(eval.fd(ts_normal_vec,Wfdobj_credit))[,1]
    }else{
        tvec_credit = dt_credit$ts_normal[2:length(dt_credit$ts_normal)]
        breaks_credit =  c(0,tvec_credit,1)
        tbasis_credit = create.bspline.basis(rangeval = c(0,1), breaks = breaks_credit)
        Wfd0_credit = fd(matrix(0,length(breaks_credit)+2,1),tbasis_credit)
        WfdParobj_credit = fdPar(Wfd0_credit, 1, lambda_presmooth)
        Wfdobj_credit = intensity.fd(tvec_credit, WfdParobj_credit, dbglev = 0)$Wfdobj
        poisson_rate_credit = exp(eval.fd(ts_normal_vec,Wfdobj_credit))[,1] 
    }
    #debt
    if (nrow(dt_debt)==0){
        poisson_rate_debt = rep(0, 501)
    }else if(nrow(dt_debt)==1){
        tvec_debt = dt_debt$ts_normal
        breaks_debt = unique(c(0,tvec_debt,1))
        tbasis_debt = create.bspline.basis(rangeval = c(0,1),breaks = breaks_debt, norder = 3)
        Wfd0_debt = fd(matrix(0,length(breaks_debt)+1,1),tbasis_debt)
        WfdParobj_debt = fdPar(Wfd0_debt, 1, lambda_presmooth)
        Wfdobj_debt = intensity.fd(tvec_debt, WfdParobj_debt, dbglev = 0)$Wfdobj
        poisson_rate_debt = exp(eval.fd(ts_normal_vec,Wfdobj_debt))[,1]
    }else{
        tvec_debt = dt_debt$ts_normal
        breaks_debt = unique(c(0,tvec_debt,1))
        tbasis_debt = create.bspline.basis(rangeval = c(0,1),breaks = breaks_debt)
        Wfd0_debt = fd(matrix(0,length(breaks_debt)+2,1),tbasis_debt)
        WfdParobj_debt = fdPar(Wfd0_debt, 1, lambda_presmooth)
        Wfdobj_debt = intensity.fd(tvec_debt, WfdParobj_debt, dbglev = 0)$Wfdobj
        poisson_rate_debt = exp(eval.fd(ts_normal_vec,Wfdobj_debt))[,1]
    }
    #final data table
    dt_cred = data.table(address = addr_hash, label = dt_credit$label[1], category = dt_credit$category[1],
                         increment_type = "credit", ts_normal = ts_normal_vec,
                         lambda = poisson_rate_credit, count = dt_credit$count[1])
    dt_debt = data.table(address = addr_hash, label = dt_credit$label[1], category = dt_credit$category[1],
                         increment_type = "debt", ts_normal = ts_normal_vec, 
                         lambda = poisson_rate_debt, count = dt_credit$count[1])
    return(rbindlist(list(dt_cred, dt_debt)))
}

# RUN ---------------------------------------------------------------------

start_time = Sys.time()
# 10 obs
dt = readRDS("data/treated/cred_debt_balanced_10obs.rds")
setorder(dt, address, increment_type, ts_normal)
rates = rbindlist(mclapply(unique(dt$address), smooth_rates, copy(dt), 0.1))
saveRDS(rates, "data/treated/rates/poisson_rates_10obs.rds")
print(Sys.time() - start_time)

# 20 obs
dt = readRDS("data/treated/cred_debt_balanced_20obs.rds")
setorder(dt, address, increment_type, ts_normal)
rates = rbindlist(mclapply(unique(dt$address), smooth_rates, copy(dt), 0.1))
saveRDS(rates, "data/treated/rates/poisson_rates_20obs.rds")
print(Sys.time() - start_time)
