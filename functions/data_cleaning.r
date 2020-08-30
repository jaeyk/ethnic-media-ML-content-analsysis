
# Extract percent agreement

extract_percent_agreement <- function(data) {
  # Calculatign the percent agreement
  test <- agree(data, tolerance = 0) %>% as.numeric()
  # Extracting the number
  percent_agreement <- test[5]
  # Return the number
  percent_agreement
}

extract_kappa <- function(data){
  # Calculatign the kappa statistic 
  test <- Kappa(table(data))
  
  # Extracting the kappa statistic
  kappa <- test$Unweighted[1] %>% as.numeric()
  
  # Extracting confidence intervals
  lwr <- confint(test)[1,1]
  upr <- confint(test)[1,2]
  
  # Return the number
  data.frame("kappa" = kappa, 
             "lwr" = lwr, 
             "upr" = upr)}