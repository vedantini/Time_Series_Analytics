DP <- function(input.data){
  
  # Creating Period column
  input.data$Period <- format(as.Date(input.data$Date), "%Y-%m")
  
  # Selecting required columns
  input.data <- input.data[,c("Period","Volume")]
  
  # Aggregating the data by month
  input.data <- aggregate(x = input.data[c("Volume")],
                          FUN = mean,
                          by = list(Group.date = input.data$Period))
  
  return(input.data)
}