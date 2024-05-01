library(tidyverse)
library(quantmod)
library(janitor)


index_symbols <- c("^SP500TR", "EFA", "VBLIX", "AGG")


index_prices <- getSymbols(index_symbols,
                                   src = "yahoo",
                                   from = "1998-12-31",
                                   to = "2023-12-31",
                                   auto.assign = T,
                                   warnings = F) %>% 
  #get adjusted prices only because adjusted prices incorporate dividends, splits, and other corporate actions
  map(~Ad(get(.))) %>% 
  reduce(merge) %>% 
  `colnames<-`(index_symbols)

index_prices_monthly <- index_prices %>% 
  #get monthly prices
  to.monthly(OHLC = F) %>%
  data.frame(date = index(.)) %>% 
  remove_rownames() %>% 
  #get year and month and remove the date column
  mutate(fy = year(date), month = month(date), .keep = "unused") %>% 
  select(fy, month, everything()) %>% 
  clean_names()

#20 year annualized returns
index_annual_returns_2022 <- index_prices_monthly %>% 
  filter(month == 6,
         fy %in% 2002:2022) %>% 
  mutate(across(3:6, ~ .x/lag(.x) - 1, .names = "{.col}_return"),
         across(3:6, ~ (last(.x)/ first(.x)) ^ (1/(length(.x) - 1)) - 1, .names = "{.col}_annualized_return"))
  

index_annual_returns_2023 <- index_prices_monthly %>% 
  filter(month == 6,
         fy %in% 2003:2023) %>% 
  mutate(across(3:6, ~ .x/lag(.x) - 1, .names = "{.col}_return"),
         across(3:6, ~ (last(.x)/ first(.x)) ^ (1/(length(.x) - 1)) - 1, .names = "{.col}_annualized_return"))

