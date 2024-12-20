
# this function is based of Erdmanns code, just extended slightly
bias_rmse <- function(mean_all, true_mean)
{

  estimate<-mean(unlist(mean_all))

  bias<-mean(unlist(lapply(seq_along(mean_all), function(j) {
    return(((mean_all[[j]] - true_mean)/true_mean)*100)
  })))


  rmse<-sqrt(var(unlist(mean_all))  + bias^2)/estimate


  ##### felipe
  sd_bias<-sd(unlist(lapply(seq_along(mean_all), function(j) {
    return(((mean_all[[j]] - true_mean)/true_mean)*100)
  })))
  ##### felipe
  return(c(estimate, bias, rmse, sd_bias))
}

# these files are generated using Erdmanns code
load("Erdmann/saves/True1.rda")
True1 <- true_mean
load("Erdmann/saves/True2.rda")
True2 <- true_mean
load("Erdmann/saves/True5.rda")
True5 <- true_mean
load("Erdmann/saves/True6.rda")
True6 <- true_mean
load("Erdmann/saves/True9.rda")
True9 <- true_mean
load("Erdmann/saves/True10.rda")
True10 <- true_mean
trueMeanMaster <- list(True1, True2, True1, True2, True5, True6, True5, True6, True9, True10, True9, True10)


source("Analysis/final_res_scripts/res_final_pois100.R")
source("Analysis/final_res_scripts/res_final_pois100.R")
source("Analysis/final_res_scripts/res_final_mark100.R")
source("Analysis/final_res_scripts/res_final_mark200.R")
source("Analysis/final_res_scripts/res_final_non_mark100.R")
source("Analysis/final_res_scripts/res_final_non_mark200.R")