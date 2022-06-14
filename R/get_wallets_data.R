### Retriving wallet information ###

# Packages #
library(readr)
library(dplyr)
library(xtable)
library(httr)
library(jsonlite)

# Auxiliary functions #
source("R/aux_functions.R")

## Setting seed ##
set.seed(123)

## Reading addresses labels ##
wallets = read_csv('data/vivian/label_addresses.csv', col_types = 'cccc')
names(wallets) = tolower(names(wallets))
# exploration
{
    print(paste("nrow", nrow(wallets)))
    print(paste("unique addresses", length(unique(wallets$address))))
    print(paste("unique labels", length(unique(wallets$label))))
    print(paste("num -old", sum(grepl("-old", wallets$label, fixed = TRUE))))
    print(paste("unique labels -old", sum(grepl("-old", unique(wallets$label), fixed = TRUE))))
}
# 1720377 obs, 938 labels, 72 have "-old", in total 527351 have "-old", 1606943 unique addresses

## Reading categories ##
categories = read_csv2('data/vivian/Categorias_novas.csv', col_names = FALSE, 
                       col_types = 'ccccc', locale = locale(encoding = "ISO-8859-1"))
# removing duplicated columns with location
categories = categories %>% select(-X3)
# column names
names(categories) = c("label", "category", "location", "observations")
# exploration
{
    print(paste("nrow", nrow(categories)))
    print(paste("-old", sum(grepl("-old", categories$label, fixed = TRUE))))
    print(paste("brazil", categories %>% filter(location =="Brazil") %>% nrow()))
}
# 128 labels that are classified, 19 have "-old", 4 from Brazil

## Merging ##
wallets_cat = merge(wallets, categories, by = "label")
# exploration
{
    print(paste("nrow", nrow(wallets_cat)))
    print(paste("unique addresses", length(unique(wallets_cat$address))))
    print(paste("unique labels", length(unique(wallets_cat$label))))
    print(paste("num -old", sum(grepl("-old", wallets_cat$label, fixed = TRUE))))
    print(paste("unique labels -old", sum(grepl("-old", unique(wallets_cat$label), fixed = TRUE))))
    print(table(wallets_cat$category))
}
# "nrow 1052796" "unique addresses 1052793" "unique labels 127" "num -old 183150" "unique labels -old 19"
# filtering out the old
wallets_cat = wallets_cat %>% filter(!grepl("-old", label, fixed = TRUE))
# exploration
{
    print(paste("nrow", nrow(wallets_cat)))
    print(paste("unique addresses", length(unique(wallets_cat$address))))
    print(paste("unique labels", length(unique(wallets_cat$label))))
    print(paste("num -old", sum(grepl("-old", wallets_cat$label, fixed = TRUE))))
    print(paste("unique labels -old", sum(grepl("-old", unique(wallets_cat$label), fixed = TRUE))))
    print(table(wallets_cat$category))
    print(table(unique(wallets_cat[, c("label", "category")])$category))
}
## latex descriptive tables
# removing user and advertising
final_tbl = wallets_cat %>% filter(!(category %in% c("Advertising", "Usuário")))
# number of addresses and labels
n_obs_tbl = data.frame("Category" = "Total obs.", "Addresses" = length(unique(final_tbl$address)), "Entities" = length(unique(final_tbl$label)))
# renaming
final_tbl = final_tbl %>% mutate(category = case_when(category == "Jogos/Apostas" ~ "Gambling",
                                                      category == "Exchange/Investimentos" ~ "Exchange",
                                                      category == "Serviço de Mixer" ~ "Mixer Service",
                                                      category == "Sistema de pagamento" ~"Payment System",
                                                      TRUE ~ category))
sh_addr = final_tbl %>% select(address, category) %>% unique() %>% pull(category) %>%
    table() %>% as.data.frame() %>% mutate(Freq = round(Freq/sum(Freq), 2))
names(sh_addr) = c("Category", "Addresses")
sh_labels = final_tbl %>% select(label, category) %>% unique() %>% pull(category) %>%
    table() %>% as.data.frame() %>% mutate(Freq = round(Freq/sum(Freq), 2))
names(sh_labels) = c("Category", "Entities")
sh_obs_cat = merge(sh_addr, sh_labels, by = "Category")
sh_obs_cat = rbind(sh_obs_cat, n_obs_tbl)
sh_obs_cat = xtable(sh_obs_cat, caption = "Original Share of Categories in Addresses and Entities", label = "sh_obs_cat_orig")
print(sh_obs_cat, file = "tables/latex/sh_obs_cat_orig.tex", include.rownames = FALSE)
n_addr = final_tbl %>% select(address, category) %>% unique() %>% pull(category) %>%
    table() %>% as.data.frame() 
names(n_addr) = c("Category", "Addresses")
n_labels = final_tbl %>% select(label, category) %>% unique() %>% pull(category) %>%
    table() %>% as.data.frame() 
names(n_labels) = c("Category", "Entities")
n_obs_cat = merge(n_addr, n_labels, by = "Category")
n_obs_cat = xtable(n_obs_cat, caption = "Original Number of Addresses and Entities, by Category", label = "n_obs_cat_orig")
print(n_obs_cat, file = "tables/latex/n_obs_cat_orig.tex", include.rownames = FALSE, format.args = list(big.mark = ","))
rm(list(final_tbl, n_obs_tbl, sh_addr, sh_labels, sh_obs_cat))

# removing wallets
rm(wallets)

# renaming wallets_cat
wallets = wallets_cat
rm(wallets_cat)

# Selecting necessary columns
wallets = wallets %>% select(c(address, label, category))

df_list = list()
i = 1 # we change this in case of trouble to pick up from the last 10.000 addresses
len = length(unique(wallets$address))
for (wallet_address in unique(wallets$address)[i:len]){
    print(i)
    API_URL = paste0("https://api.blockchain.info/charts/balance?address=",
                     wallet_address, "&timespan=all&format=json")
    
    rawResponse = GET(API_URL)
    
    if (rawResponse$status_code == 200){
        response = fromJSON(content(rawResponse, as = 'text'))[['values']] %>%
            as.data.frame()
        if (nrow(response) == 0){
            response = data.frame(address = wallet_address, x = NA, y = NA)
        }else{
            response$address = wallet_address
        }
    }else{
        response = data.frame(address = wallet_address, x = NA, y = NA)
    }
    
    df_list[[i]] = response
    i = i + 1
    Sys.sleep(runif(1, min = 2, max = 4))
    
    if (i %% 10000 == 0){
        time_series = bind_rows(df_list[(i-10000 + 1):i])
        names(time_series)[c(1,2)] = c("time_stamp", "value")
        
        wallets_series = merge(wallets, time_series, by = "address")
        
        saveRDS(wallets_series, paste0("data/raw/wallets_series_sample_", i/10000,
                                       ".rds"))
    }
    
    
}

