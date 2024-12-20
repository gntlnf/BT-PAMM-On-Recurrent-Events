
source("initial_run.R")








load("Data_generation/data_all.rda")





# bias_rmse <- function(mean_all, true_mean)
# {
# 
#   estimate<-mean(unlist(mean_all))
# 
#   bias<-mean(unlist(lapply(seq_along(mean_all), function(j) {
#     return(((mean_all[[j]] - true_mean)/true_mean)*100)
#   })))
# 
# 
#   rmse<-sqrt(var(unlist(mean_all))  + bias^2)/estimate
# 
# 
#   ##### felipe
#   sd_bias<-sd(unlist(lapply(seq_along(mean_all), function(j) {
#     return(((mean_all[[j]] - true_mean)/true_mean)*100)
#   })))
#   ##### felipe
#   return(c(estimate, bias, rmse, sd_bias))
# }


