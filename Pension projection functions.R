#Fiscal year projection function
fy_f <- function(fy){       
  for(i in 2:length(fy)) {
    if(is.na(fy[i])){
      fy[i] <- fy[i-1] + 1
    }
  }
  return(fy)
}

#Fiscal year projection function for ymd format
fye_f <- function(fye){       
  for(i in 2:length(fye)) {
    if(is.na(fye[i])){
      fye[i] <- fye[i-1] + years(1)
    }
  }
  return(fye)
}

#Function to fill na values in a vector with previous non-na values
get_last_f <- function(x) {
  for(i in 2:length(x)) {
    if(is.na(x[i])) {
      x[i] <- x[i-1]
    }
  }
  return(x)
}

#Payroll growth function 
#if the payroll growth assumption isn't available in any year, use the average payroll growth rate. Otherwise, use the latest available payroll growth assumption
payroll_growth_f <- function(payroll_growth, payroll_growth_avg) {    
  if(sum(!is.na(payroll_growth)) == 0) {   
    payroll_growth <- payroll_growth_avg
  } else {
    payroll_growth <- get_last_f(payroll_growth)
  }
  return(payroll_growth)
}

#Growth function
growth_f <- function(x, g) {
  for(i in 2:length(x)) {
    if(is.na(x[i])) {
      x[i] <- x[i-1] * (1 + g[i])
    }
  }
  return(x)
}



#Function to create a synthetic benchmark portfolio using quadratic programming (to find the "best fit" benchmark portfolio with three indexes)
#See examples in the two links below:
#https://henrywang.nl/quadratic-programming-with-r/
#https://henrywang.nl/another-quadratic-programming-example-with-r/

#The synthetic benchmark portfolios then are used to estimate returns in missing years
benchmark_portfolio <- function(return, x1, x2, x3) {
  
  end_pos <- max(which(!is.na(return)))       #find the latest year with available return data
  y_actual <- return[(end_pos - 9):end_pos]   #use only the last 10 years (with available return data) to create the benchmark
  x0 <- 1                                     #for the intercept (alpha)
  x1 <- x1[(end_pos - 9):end_pos]
  x2 <- x2[(end_pos - 9):end_pos]
  x3 <- x3[(end_pos - 9):end_pos]
  x = cbind(x0, x1, x2, x3)
  
  Dmat <- crossprod(x)
  dvec <- crossprod(y_actual, x)                  # vector to be minimized: product:transpose y_actual and x
  # Amat <- cbind(rep(1,3), diag(3))   
  Amat <- t(cbind(0, rbind(rep(1,3), diag(3))))   # matrix defining the constraints
  bvec <- c(1,0,0,0)                              # vector of b coefficient; meq = 1 is equality constraint: coefs sum to 1  
  
  result <- solve.QP(Dmat = Dmat, dvec = dvec, Amat = Amat, bvec = bvec, meq = 1) 
  
  return(list(result$solution))
}


#Investment return function
return_f <- function(return, fy, latest_return, predict_return, input_return) {  
  for (i in 1:length(return)) {
    #if official returns are not available
    if (is.na(return[i])) {
      #but the plan has released the return number else where that has been recorded in the latest return data set, use the latest return
      if (!is.na(latest_return[i])) {
        return[i] <- latest_return[i]
        #for "current" fiscal year or years before that, use "predict returns" 
      } else if (fy[i] <= current_fy) {
        return[i] <- predict_return[i]
        #for "next year", use the input return when all the options above are not available
      } else {
        return[i] <- input_return
      }
      #use official returns when they're available
    } else {
      return[i] <- return[i]
    }
  }
  return(return)
}

#AAL function
aal_f <- function(aal, tpl, arr, payroll, nc, ben_pay) {
  for(i in 2:length(aal)) {
    #if aal is not available or the aal number is identical to the aal number in the previous year (indicating a data error)
    if(is.na(aal[i]) || isTRUE(aal[i] == aal[i-1])) {
      #but if total pension liability is available
      if (!is.na(tpl[i])) {
        #then use the total pension liability figure
        aal[i] <- tpl[i]
      } else {
        #if tpl isn't available, estimate the aal by the roll forward method
        aal[i] <- aal[i-1]*(1 + arr[i]) + (nc[i]*payroll[i] + ben_pay[i])*(1 + arr[i])^0.5  
      }
    }
  }
  return(aal)
}

#MVA function
mva_f <- function(mva, return, payroll, cont_rate, ben_pay) {
  for(i in 2:length(mva)) {
    if(is.na(mva[i])) {
      mva[i] <- mva[i-1]*(1 + return[i]) + (cont_rate[i]*payroll[i] + ben_pay[i])*(1 + return[i])^0.5
    }
  }
  return(mva)
}


#Final projection function 
projection_f <- function(input_return, inf_adj = F) {
  
  #plan projection
  ppd_project_plan <- ppd_benchmark %>% 
    left_join(cpi) %>% 
    group_by(plan_name) %>% 
    mutate(return = return_f(return, fy, latest_return, predict_return, input_return),
           inf_adj = inf_adj,
           aal = aal_f(aal, tpl, arr, payroll, nc, ben_pay),
           aal = ifelse(inf_adj == T, aal * cpi[fy == latest_update_year] / cpi, aal),
           mva = mva_f(mva, return, payroll, cont_rate, ben_pay),
           mva = ifelse(inf_adj == T, mva * cpi[fy == latest_update_year] / cpi, mva)) %>% 
    mutate(ual = aal - mva, 
           funded_ratio = mva/aal,
           .after = mva) %>% 
    ungroup() %>% 
    select(fy, plan_name, plan_full_name, state, ual_official, returns_official,
           aal, mva, ual, funded_ratio) %>% 
    mutate(type = "plan")
  
  #Threshold to determine if UAL and returns are "official". 
  #If the threshold is 0.9, it means the state's (or national) numbers are considered "official" if more than 90% of its UAL (or returns), weighted by AAL, has been reported.
  official_threshold = 0.9
  
  #state projection
  ppd_project_state <- ppd_project_plan %>% 
    group_by(state, fy) %>% 
    summarise(
      ual_official = weighted.mean(x = ual_official, w = aal),
      returns_official = weighted.mean(returns_official, w = aal),
      aal = sum(aal, na.rm = T),
      mva = sum(mva, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(ual = aal - mva,
           funded_ratio = mva/aal,
           ual_official = ifelse(ual_official > official_threshold, 1, 0),
           returns_official = ifelse(returns_official > official_threshold, 1, 0),
           type = "state")
  
  #national (us) projection
  ppd_project_us <- ppd_project_plan %>% 
    group_by(fy) %>% 
    summarise(
      ual_official = weighted.mean(x = ual_official, w = aal),
      returns_official = weighted.mean(returns_official, w = aal),
      # ual_official = mean(ual_official),
      # returns_official = mean(returns_official),
      aal = sum(aal, na.rm = T),
      mva = sum(mva, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(ual = aal - mva,
           funded_ratio = mva/aal,
           ual_official = ifelse(ual_official > official_threshold, 1, 0),
           returns_official = ifelse(returns_official > official_threshold, 1, 0),
           state = "USA",
           type = "national")
  
  
  
  #combined projections (plan, state, and national)
  ppd_project_combined <- bind_rows(ppd_project_plan,
                                    ppd_project_state,
                                    ppd_project_us)
  
  
  
  return(ppd_project_combined)
  
  
  
  # if (output_type == "plan") {
  #   return(ppd_project_plan)
  # } else if(output_type == "state") {
  #   return(ppd_project_state)
  # } else if(output_type == "us") {
  #   return(ppd_project_us)
  # }
  
}


