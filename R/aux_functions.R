### Auxiliary Functions ###

## Packages ##
library(dplyr)

# Adding 500 points
# Step function
# ordered x_vec
step_function = function(x, x_vec, y_vec){
    i = length(which(x_vec <= x))
    if (i == 0){
        return(0)
    }else{
        return(as.numeric(y_vec[i]))
    }
}

# function to select x_vec values not too close to each_other, takes threshold in hours
clean_vec = function(x, limit = 0.05){
    x = x*2000
    x_clean = c(x[1])
    #idx = c(1)
    diff = x - lag(x, default = 0)
    cum_sum = 0
    for (i in 2:length(x)){
        j = length(x_clean) + 1
        if (diff[i] >= limit){
            x_clean[j] = x[i]
            #idx = c(idx, i)
            cum_sum = 0
        }else{
            cum_sum = cum_sum + diff[i]
            if(cum_sum >= limit){
                x_clean[j] = x[i]
                #idx = c(idx, i)
                cum_sum = 0
            }
        }
    }
    x_clean[length(x_clean)] = x[length(x)]
    return(x_clean/2000)
    #return(idx)
}