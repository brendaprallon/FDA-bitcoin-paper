##### Reading new data #######

library(RSQLite)
library(data.table)

## connect to db
con = dbConnect(drv=RSQLite::SQLite(), dbname="data/raw/bitcoin_address_inputs_outputs_v2_indexed.sqlite3")

## list all tables
tables = dbListTables(con)

## exclude sqlite_sequence (contains table information)
tables = tables[tables != "sqlite_sequence"]

get_data_from_database = function(increment = c("credit", "debt")){
  if (increment == "credit"){
    ## create a data.frame for each table
    dt = dbGetQuery(conn=con, statement=paste("SELECT * FROM '", "outputs", "'", sep=""))
  }else{
    ## create a data.frame for each table
    dt = dbGetQuery(conn=con, statement=paste("SELECT * FROM '", "inputs", "'", sep=""))
  }
  
  # as data table for fast computing
  dt = data.table(dt)
  
  # Ordering
  setorder(dt, address, timestamp)
  
  # Aggregating by timestamp
  dt = dt[, sum(value), by = .(address, timestamp)]
  setnames(dt, "V1", "value")
  
  # Adding increment_type column 
  dt[, increment_type := increment]
  
  if (increment == "debt"){
    dt[, value := -value]
  }
  
  return(dt)
}

# Credit
credit = get_data_from_database("credit")

# Debt
debt = get_data_from_database("debt")

# saving
saveRDS(setorder(rbind(credit, debt), address, timestamp), "data/raw/wallets_from_blockchain.rds")

dbDisconnect(conn = con)
