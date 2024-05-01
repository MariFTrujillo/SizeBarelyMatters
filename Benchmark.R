# rm(list = ls())


# library(readxl)
# library(plotly)
# library(formattable)
# library(packcircles)
# library(ggrepel)

# # Import pension data
# pension_data_all <- read.csv("ppd-oct-2023.csv")
# 
# # Clean pension data and retain relevant columns only
# pension_data_clean <- pension_data_all %>% 
#   mutate(PlanFullName = gsub("\x92", "'", PlanFullName),
#          PlanName = gsub("\x92", "'", PlanName)) %>% 
#   select(PlanName, PlanFullName, AdministeringGovt, fy, fye, ActLiabilities_GASB, InvestmentReturn_1yr) %>% 
#   rename(
#     plan_name = PlanName,            #rename columns for easier reference
#     plan_full_name = PlanFullName,
#     gov_type = AdministeringGovt,
#     aal = ActLiabilities_GASB,
#     return = InvestmentReturn_1yr
#   ) %>% 
#   mutate(fye = ymd(fye),
#          month = month(fye),
#          return = as.numeric(return)) %>% 
#   group_by(plan_name) %>%
#   fill(month, .direction = "downup") %>% 
#   # filter(n() == max_range, all(!is.na(c(return)))) %>% 
#   ungroup() %>% 
#   filter(gov_type == 0)

#Get monthly index prices
#We need a US stock market index fund, a US bond market index fund, and an international stock market index fund
#SWTSX: Schwab Total Stock Market Index Fund (US Equity)
#VBMFX: Vanguard Total Bond Market Index Fund (US Fixed Income)
#ACWI ex US: International equity excluding US Index (International Equity)
index_symbols <- c("SWTSX", "VBMFX", "VGSLX", "^SP500TR")

index_prices_monthly <- index_symbols %>% 
  #get adjusted prices only because adjusted prices incorporate dividends, splits, and other corporate actions
  map(~Ad(getSymbols(.x, 
                     src = "yahoo",
                     from = "1998-12-31",
                     to = "2022-12-31",
                     auto.assign = F,
                     warnings = F))) %>% 
  reduce(merge) %>% 
  setNames(index_symbols) %>%
  #get monthly prices
  apply.monthly(last) %>% 
  data.frame(date = index(.)) %>%
  remove_rownames() %>% 
  #get year and month and remove the date column
  mutate(fy = year(date), month = month(date), .keep = "unused") %>% 
  select(fy, month, everything()) %>% 
  clean_names()


acwi_ex_us_monthly <- import("acwi_ex_us_stock.xls") %>% 
  row_to_names(6) %>% 
  clean_names() %>% 
  rename(acwi_ex_us = acwi_ex_usa_standard_large_mid_cap) %>% 
  filter(!is.na(acwi_ex_us)) %>% 
  mutate(date = mdy(date),
         fy = year(date),
         month = month(date),
         acwi_ex_us = as.numeric(acwi_ex_us)) %>% 
  select(fy, month, acwi_ex_us)
  

# Calculate annual returns for the individual securities for each month
index_returns <- index_prices_monthly %>%
  left_join(acwi_ex_us_monthly) %>% 
  pivot_longer(cols = -(fy:month), names_to = "security", values_to = "prices") %>%
  arrange(security, month, fy) %>%
  group_by(security, month) %>%
  mutate(returns = prices/lag(prices) - 1, .keep = "unused") %>%
  pivot_wider(names_from = security, values_from = returns) %>%
  ungroup() %>% 
  rename(us_stock = swtsx, us_bond = vbmfx, ex_us_stock = acwi_ex_us, us_real_estate = vgslx, sp500 = x_sp500tr) %>% 
  #create an index for private equity, which seems to have a 3-month reporting lag
  arrange(fy) %>%
  mutate(lag_us_stock = lag(us_stock, n = 3)) %>% 
  select(fy, month, us_stock, us_bond, ex_us_stock, us_real_estate, lag_us_stock, sp500)



# rows_with_na <-pension_index_data[!complete.cases(pension_index_data),]

# write.csv(pension_data_, "pension_data.csv")

#Choose number of years to perform the analysis


#Calculate average geometric returns and excess average returns
#Function to calculate average returns
geo_return <- function(x, na.rm = F) {
  if (na.rm == T) {
    x = na.omit(x)
  }
  avg_return <- prod(1+x)^(1/length(x)) - 1
  return(avg_return)
}


#Function to calculate the empirical benchmark portfolio (old function)
benchmark_portfolio <- function(return_, ...) {
  
  x0 <- 1                                     #for the intercept (alpha)
  x <- as.matrix(cbind(x0, ...))
  
  Dmat <- crossprod(x)
  dvec <- crossprod(return_, x)                  # vector to be minimized: product:transpose return and x
  Amat <- t(cbind(0, rbind(rep(1, ncol(x) - 1), diag(ncol(x) - 1))))   # matrix defining the constraints
  bvec <- c(1, rep(0, ncol(x) - 1))                      # vector of b coefficient; meq = 1 is equality constraint: coefs sum to 1  
  
  result <- solve.QP(Dmat = Dmat, dvec = dvec, Amat = Amat, bvec = bvec, meq = 1) 
  
  return(list(result$solution))
}


#Function to calculate the empirical benchmark portfolio for a pension plan (new function)
benchmark_pension <- function(single_plan_pension_data, index_names) {
  index_return_matrix <- single_plan_pension_data %>% 
    select(all_of(index_names)) %>% 
    as.matrix()
  
  pension_return_vec <- single_plan_pension_data$return
             
  x0 <- 1                           #for the intercept (alpha)
  x <- cbind(x0, index_return_matrix)   
  
  Dmat <- crossprod(x)
  dvec <- crossprod(pension_return_vec, x)                  # vector to be minimized: product:transpose return and x
  Amat <- t(cbind(0, rbind(rep(1, ncol(x) - 1), diag(ncol(x) - 1))))   # matrix defining the constraints
  bvec <- c(1, rep(0, ncol(x) - 1))                      # vector of b coefficient; meq = 1 is equality constraint: coefs sum to 1  
  
  result <- solve.QP(Dmat = Dmat, dvec = dvec, Amat = Amat, bvec = bvec, meq = 1) 
  
  #Empirical benchmark weights (excluding alpha)
  index_weights <- result$solution[-1]
  
  #Alpha
  alpha <- result$solution[1]
  
  #Calculate benchmark returns (since the matrix multiplication below produces a matrix, we need to extract the first column to make a vector)
  benchmark_return <- (index_return_matrix %*% index_weights)[,1]
  
  #Calculate cumulative benchmark returns
  benchmark_cumulative_return <- cumprod(1 + benchmark_return)
  
  #Calculate plan's cumulative returns
  plan_cumulative_return <- cumprod(1 + pension_return_vec)
  
  #Cumulative excess returns
  excess_benchmark_cumulative_return <- plan_cumulative_return - benchmark_cumulative_return
  
  single_plan_pension_data$alpha = alpha
  single_plan_pension_data$emp_benchmark_weight <- list(index_weights)
  single_plan_pension_data$emp_benchmark_return <- benchmark_return
  single_plan_pension_data$plan_cumulative_return <- plan_cumulative_return
  single_plan_pension_data$emp_benchmark_cumulative_return <- benchmark_cumulative_return
  single_plan_pension_data$excess_benchmark_cumulative_return <- excess_benchmark_cumulative_return
  
  #add the base fy before the first fy
  single_plan_pension_data <- single_plan_pension_data %>% 
    add_row(.before = 0) %>% 
    mutate(fy = ifelse(is.na(fy), min(fy, na.rm = T) - 1, fy),
           across(contains("name"), ~ ifelse(is.na(.x), lead(.x), .x)),
           across(contains("cumulative") & !contains("excess"), ~ ifelse(is.na(.x), 1, .x)),
           excess_benchmark_cumulative_return = ifelse(is.na(excess_benchmark_cumulative_return), 0, excess_benchmark_cumulative_return))
  
  return(single_plan_pension_data)
}


#Function to calculate the empirical benchmark portfolio for the whole pension data set
benchmark_pension_all <- function(benchmark_period, pension_data, index_data, index_names, end_fy, return_type = "cumulative") {
  #filter the pension data to only include the specified period
  pension_data_filtered <- pension_data %>% 
    filter(fy <= end_fy, fy >= end_fy - benchmark_period + 1)
  
  #join index data
  pension_index_data_filtered <- pension_data_filtered %>%
    left_join(index_data, by = c("fy", "month"))
  
  #filter out plans that don't have returns every year for the whole period
  pension_index_data_filtered <- pension_index_data_filtered %>% 
    group_by(plan_name) %>%
    filter(all(!is.na(return))) %>%
    ungroup()
  
  #split the data by plan name --> this creates a list of data frames
  pension_data_split <- split(pension_index_data_filtered, pension_index_data_filtered$plan_name)
  #apply the benchmark_pension function to each plan
  pension_data_emp_benchmark <- lapply(pension_data_split, benchmark_pension, index_names = index_names)
  #turn the list back into a single data frame
  pension_data_emp_benchmark_df <- bind_rows(pension_data_emp_benchmark)
  
  if (return_type == "cumulative") {
    return(pension_data_emp_benchmark_df %>% 
             #remove all columns with any NA values
             select(where(~all(!is.na(.x)))) %>% 
             #rename columns to clarify the benchmarking period
             rename_with(~ paste0(.x, "_", benchmark_period), contains("cumulative"))
           )
  } else {
    return(pension_data_emp_benchmark_df %>% 
             group_by(plan_name) %>% 
             summarise(plan_avg_return = geo_return(return, na.rm = T),
                       emp_benchmark_avg_return = geo_return(emp_benchmark_return, na.rm = T)) %>% 
             ungroup() %>% 
             mutate(excess_benchmark_avg_return = plan_avg_return - emp_benchmark_avg_return) %>% 
             #rename columns to clarify the benchmarking period
             rename_with(~ paste0(.x, "_", benchmark_period), contains("avg"))
    )
  }
  
}


#######Testing
# 
# benchmark_all_15 <- benchmark_pension_all(pension_data_clean, index_returns, 
#                                               index_names = c("us_stock", "us_bond", "ex_us_stock", "lag_us_stock"), 
#                                               end_fy = 2022, benchmark_period = 20,
#                                               return_type = "average"
#                                           )
# 
# benchmark_all_15_below_benchmark <- benchmark_all_15 %>% 
#   summarise(percent_below_0 = sum(avg_emp_benchmark_excess < 0)/n())
# 
# 
# benchmark_all_cumulative_15 <- benchmark_pension_all(pension_data_clean, index_returns, 
#                                               index_names = c("us_stock", "us_bond", "ex_us_stock", "lag_us_stock"), 
#                                               end_fy = 2022, benchmark_period = 20,
#                                               return_type = "cumulative"
# )



#########Visualization

# 
# #Visualize the average excess returns from lowest to highest; add a line at 0; 
# benchmark_all_15 %>%
#   arrange(avg_emp_benchmark_excess) %>%
#   mutate(plan_name = factor(plan_name, levels = plan_name)) %>%
#   ggplot(aes(x = plan_name, y = avg_emp_benchmark_excess)) +
#   geom_point(aes(size = last_aal), alpha = 0.3) +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   coord_flip() +
#   labs(x = "Plan Name", y = "Excess Average Return", title = "Excess Average Returns by Plan") +
#   theme_bw() +
#   theme(axis.text.y = element_text(size = 8))
# 
# 
# #Visualize the cumulative excess returns; add a horizontal line at 0
# benchmark_all_cumulative_15 %>%
#   ggplot(aes(x = fy, y = excess_benchmark_cumulative_return, group = plan_name)) +
#   geom_line(alpha = 0.1, size = 1) +
#   geom_hline(yintercept = 0, linetype = "dashed", col = "red", size = 1) +
#   labs(x = "Fiscal Year", y = "Cumulative Excess Return", title = "Cumulative Excess Returns by Plan") +
#   theme_bw()
# 
# 
# #Visualize cumulative returns of the target plan
# target_plan <- 'California PERF'
# ppd_cum_returns_plan <- benchmark_all_cumulative_15 %>%
#   filter(plan_name == target_plan) %>%
#   select(plan_name, plan_full_name, fy, plan_cumulative_return, emp_benchmark_cumulative_return) %>%
#   pivot_longer(cols = plan_cumulative_return:emp_benchmark_cumulative_return,
#                names_to = "cum_return_type",
#                values_to = "value") %>%
#   mutate(cum_return_type = case_when(
#     cum_return_type == "plan_cumulative_return" ~ target_plan,
#     cum_return_type == "emp_benchmark_cumulative_return" ~ "Empirical Benchmark",
#   ))
# 
# ggplot(ppd_cum_returns_plan, aes(x = fy, y = value, col = cum_return_type)) +
#   geom_line(size = 1) +
#   geom_text_repel(data = ppd_cum_returns_plan %>% filter(fy == 2022),    #Annotate the two lines
#                   aes(label = cum_return_type),
#                   nudge_x = 0.5,
#                   segment.color = NA) +
#   # scale_color_manual(values = c("orange", "#0E86D4"),
#   #                    breaks = c(target_plan, benchmark_type)) +
#   scale_x_continuous(breaks = (2022 - 20):2022,
#                      expand = expansion(mult = c(0, 0.1))) +
#   scale_y_continuous(breaks = pretty_breaks(n = 10)) +
#   labs(x = NULL, y = NULL,
#        title = paste0("Cumulative Returns", " (", 2022 - 20 + 1, " - ", 2022, ")",
#                       " - ", target_plan)) +
#   theme_classic() +
#   theme(axis.line.y = element_blank(),
#         panel.grid.major.y = element_line(size = 0.3),
#         legend.position = "none")
#   




