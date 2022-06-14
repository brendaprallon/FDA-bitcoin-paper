### Creating Partitions for Cross-Validation ###

# Packages 
library(data.table)
library(caret)


# Functions ---------------------------------------------------------------

# creating partitions for training and testing
create_partition = function(min_obs, p_test, n_kfold_train, binary = FALSE){
    set.seed(123)
    dt = readRDS(paste0("data/treated/scalar/scalar_features_", min_obs, "obs.rds"))

    if (binary == TRUE){
        dt[category != "Darknet Marketplace", category := "Other"]
        dt = dt[category == "Darknet Marketplace" | address %in% sample(dt[category == "Other", address],
                                                                        nrow(dt[category == "Darknet Marketplace"]))]
    }
    testIndex = createDataPartition(dt$category, p = p_test, list =F)
    test_addr = dt$address[testIndex]
    kfold_index = createFolds(dt$category[-testIndex], k = n_kfold_train)
    kfold_addr = list()
    for (i in 1:n_kfold_train){
        kfold_addr[[i]] = dt$address[-testIndex][kfold_index[[i]]]
    }
    if(binary == TRUE){
        saveRDS(test_addr, paste0("data/treated/partitions/binary_test_addr_p", p_test, "_", min_obs, "obs.rds"))
        saveRDS(kfold_addr, paste0("data/treated/partitions/binary_kfold_addr_k", n_kfold_train, "_", min_obs, "obs.rds"))
    }else{
        saveRDS(test_addr, paste0("data/treated/partitions/test_addr_p", p_test, "_", min_obs, "obs.rds"))
        saveRDS(kfold_addr, paste0("data/treated/partitions/kfold_addr_k", n_kfold_train, "_", min_obs, "obs.rds"))
    }
}


# RUN ---------------------------------------------------------------------

# Partitions: 20% test, 5 kfolds for training
# min 10 obs
create_partition(10, 0.2, 5)
create_partition(10, 0.2, 5, binary = TRUE)
# min 20 obs
create_partition(20, 0.2, 5)
create_partition(20, 0.2, 5, binary = TRUE)
