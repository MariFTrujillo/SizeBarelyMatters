#Color palette for top funded ratio
top_fund_pal <- function(x) rgb(colorRamp(c("#e0eff7", "#3399cc"))(x), maxColorValue = 255)

#Color palette for bottom funded ratio
bottom_fund_pal <- function(x) rgb(colorRamp(c("#FF6633", "#ffdcd0"))(x), maxColorValue = 255)

#Color palette for funded ratio table
fund_pal <- function(x) {
  colors <- double(length(x))
  
  # Handle NA values
  non_na_pos <- which(!is.na(x))
  na_pos <- which(is.na(x))
  
  # Compute the colors for the non-NA values
  colors[non_na_pos] <- rgb(colorRamp(c("#FF6633", "#ffcc33", "#3399cc"))(x[non_na_pos]), maxColorValue = 255)
  
  # Replace the NAs with a specified color, e.g., "grey50"
  colors[na_pos] <- "grey"
  
  return(colors)
}

#Function to create bar charts inside a reactable
bar_style <- function(width = 1, fill = "#e6e6e6", height = "75%",
                      align = c("left", "right"), color = NULL) {
  align <- match.arg(align)
  if (align == "left") {
    position <- paste0(width * 100, "%")
    image <- sprintf("linear-gradient(90deg, %1$s %2$s, transparent %2$s)", fill, position)
  } else {
    position <- paste0(100 - width * 100, "%")
    image <- sprintf("linear-gradient(90deg, transparent %1$s, %2$s %1$s)", position, fill)
  }
  list(
    backgroundImage = image,
    backgroundSize = paste("100%", height),
    backgroundRepeat = "no-repeat",
    backgroundPosition = "center",
    color = color
  )
}

#Function to render plan funding status in reactable
funding_table <- function(data, position) {

  max_mva_aal <- max(data$mva, data$aal)
  
  reactable(
    data = data,
    filterable = T,
    defaultSorted = list(funded_ratio_mva = "desc"),
    columns = list(
      fy = colDef(name = "Year"),
      plan_name = colDef(name = "Plan"), 
      gov_type2 = colDef(name = "Type",
                         cell = function(value) {
                           class = paste0("tag type-", str_replace_all(tolower(value), " ", ""))
                           div(class = class, value)
                         }),
      
      aal = colDef(name = "AAL",
                   cell = function(value) {dollar(value, scale = 1/1000000, suffix = "B")},
                   style = function(value) {
                     bar_style(width = value / max_mva_aal,
                               fill = "#ffcc33")
                   }
      ),
      
      mva = colDef(name = "MVA",
                   cell = function(value) {dollar(value, scale = 1/1000000, suffix = "B")},
                   style = function(value) {
                     bar_style(width = value / max_mva_aal,
                               fill = "#669933")
                   }
      ),
      
      funded_ratio_mva = colDef(name = "Funded Ratio", 
                            format = colFormat(percent = T, digits = 0),
                            style = function(value) {
                              normalized <- (value - min(data$funded_ratio_mva)) / (max(data$funded_ratio_mva) - min(data$funded_ratio_mva))
                              color <- fund_pal(normalized)
                              list(background = color)
                            })
    )
  )
}


# test <- ppd_clean_2022 %>% 
#   select(fy, plan_name, aal, mva, funded_ratio_mva) %>% 
#   filter(!is.na(funded_ratio_mva)) %>% 
#   arrange(desc(funded_ratio_mva))
# 
# funding_table(test)


#Function to calculate rolling average returns
rolling_return <- function(return_vec, window) {
  avg_returns <- double(length = length(return_vec))
  for (i in 1:length(return_vec)) {
    if (i < window) {
      avg_returns[i] <- NA
    } else {
      avg_returns[i] <- prod(1 + return_vec[(i-window+1):i])^(1 / window) - 1
    }
  }
  return(avg_returns)
}

#Function to get mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


#Color palette for return table
base_color <- "#3399cc"
end_color <- "#FFFFFF"

return_palette_function <- colorRampPalette(c(base_color, end_color))

return_tones <- return_palette_function(5)


