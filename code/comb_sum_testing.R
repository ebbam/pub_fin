library(here)
library(tidyverse)
library(janitor)
library(gridExtra)
library(readr)
library(data.table)
library(openxlsx)

# Testing to find which combinations of categories add up to ANY available total
test2 <- county_short %>% filter(year == 2000 & fips == "36055" & large_category != "Total" & retain_total)
nums <- test2 %>% filter(amount != 0) %>% pull(amount) %>% unique

# Get all possible combinations of values
comb <- lapply(1:length(nums), function(x) combn(nums, x))

totals <- county_totals %>% filter(year == 2000 & fips == "36055" & will_category != "total interest on general debt") %>% filter(amount != 0) %>% pull(amount) %>% unique

# Main insight here: no combination of individual expenditure categories adds up to 
for(k in 1:length(comb)){
  for(j in 1:length(totals)){
    if(length(unlist(comb[k])[unlist(comb[k]) == totals[j]]) != 0){
      print(totals[j])
      print(k)
      print(unlist(comb[k])[unlist(comb[k]) == totals[j]])
    }
  }
}


