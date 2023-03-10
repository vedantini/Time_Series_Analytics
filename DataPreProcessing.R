DP <- function(data){
  
  data$periods <- format(as.Date(data$Date), "%Y-%m")
  
  unique_periods = list(unique(data$period))
  
  clean_data = data.frame('Periods' = unique_periods[[1]],
                          'Stock_Prices' = unique_periods[[1]])
  
  for(i in 1:length(unique_periods[[1]])){
    clean_data$Stock_Prices[i] = data[data$period == unique_periods[[1]][i],5][length(data[data$period == unique_periods[[1]][i],5])]
  }
  
  clean_data$Stock_Prices <- sapply(clean_data$Stock_Prices, as.numeric)
  return(clean_data)
}