


library(survival)
library(etm)
library(mvna)
library(data.table)
library(dplyr)
library(MASS)
library(xtable)

source("Erdmann/fct/simulation_recurrent_events.R")
# source("aus_sim_repo/fct/Utils.R")
source("Erdmann/ScenarioMasterfile.R")
source("Analysis/mu_functions.R")


#

# load("aus_sim_repo/saves/True1.rda")
# True1 <- true_mean
# load("aus_sim_repo/saves/True2.rda")
# True2 <- true_mean
# load("aus_sim_repo/saves/True5.rda")
# True5 <- true_mean
# load("aus_sim_repo/saves/True6.rda")
# True6 <- true_mean
# load("aus_sim_repo/saves/True9.rda")
# True9 <- true_mean
# load("aus_sim_repo/saves/True10.rda")
# True10 <- true_mean
# trueMeanMaster <- list(True1, True2, True1, True2, True5, True6, True5, True6, True9, True10, True9, True10)
# # 
# 
# 
# 
# setwd("pammtools-multi-state")
# devtools::load_all()
# setwd("..")
# 
# 
# load("Analysis/Data_generation/data_all.rda")
# 
# 
# 
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
