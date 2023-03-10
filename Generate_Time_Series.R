Gen_TS <- function(data,s1,s2,e1,e2)
{
  data.ts <- ts(data$Stock_Prices,start =c(s1, s2), end =c(e1, e2), freq = 12)
  
  return(data.ts)
} 