
# load results of this thesis settings: Poisson N=100
load("Analysis/final_res_scripts/final_results/poisson100_final_results.RData")

# evaluate results of this thesis 
res_normal1 <- normal1 %>% m_s_b_r(1)

res_normal1_notrans <- normal1_notrans %>% m_s_b_r(1)

res_cutoff1 <- cutoff1 %>% clean_results() %>% m_s_b_r(1)  

res_normal2 <- normal2 %>% m_s_b_r(2)

res_normal2_notrans <- normal2_notrans %>% m_s_b_r(2)

res_cutoff2 <- cutoff2 %>% clean_results() %>% m_s_b_r(2)  

res_normal3 <- normal3 %>% m_s_b_r(3)

res_normal3_notrans <- normal3_notrans %>% m_s_b_r(3)

res_cutoff3 <- cutoff3  %>% clean_results() %>% m_s_b_r(3)  

res_normal4 <- normal4 %>% m_s_b_r(4)

res_normal4_notrans <- normal4_notrans %>% m_s_b_r(4)

res_cutoff4 <- cutoff4 %>% clean_results() %>% m_s_b_r(4)  


# evaluate results of replication of Erdmann's paper results
load("Erdmann/results/ResultData_Scenario1_N100.rda")

plot_df <- rbind(data.frame(t(res_normal1),
                            t = c(40, 60, 80, 100), scenario = "rand_noterm",
                            method = "normal_spline", from = "new"),
                 data.frame(t(res_cutoff1),
                            t = c(40, 60, 80, 100), scenario = "rand_noterm",
                            method = "spline_by=transition", from = "new"),
                 data.frame(t(res_normal1_notrans),
                            t = c(40, 60, 80, 100), scenario = "rand_noterm",
                            method = "normal_spline_notrans", from = "new"),
                 data.frame("mean" = scenario$AJ[1,],
                            "sd"   = NA,
                            "bias" = scenario$AJ[2,],
                            "sd_b" = NA,
                            "rmse" = scenario$AJ[3,],
                            "True" = trueMeanMaster[[1]],
                            "t"    = c(40, 60, 80, 100),
                            "scenario"  = "rand_noterm",
                            "method"  = "AJ",
                            "from" = "old"),
                 data.frame("mean" = scenario$`NA`[1,],
                            "sd"   = NA,
                            "bias" = scenario$`NA`[2,],
                            "sd_b" = NA,
                            "rmse" = scenario$`NA`[3,],
                            "True" = trueMeanMaster[[1]],
                            "t"    = c(40, 60, 80, 100),
                            "scenario"  = "rand_noterm",
                            "method"  = "NA",
                            "from" = "old"),
                 data.frame("mean" = scenario$EB1[1,],
                            "sd"   = NA,
                            "bias" = scenario$EB1[2,],
                            "sd_b" = NA,
                            "rmse" = scenario$EB1[3,],
                            "True" = trueMeanMaster[[1]],
                            "t"    = c(40, 60, 80, 100),
                            "scenario"  = "rand_noterm",
                            "method"  = "EB1",
                            "from" = "old"),
                 data.frame("mean" = scenario$EB2[1,],
                            "sd"   = NA,
                            "bias" = scenario$EB2[2,],
                            "sd_b" = NA,
                            "rmse" = scenario$EB2[3,],
                            "True" = trueMeanMaster[[1]],
                            "t"    = c(40, 60, 80, 100),
                            "scenario"  = "rand_noterm",
                            "method"  = "EB2",
                            "from" = "old"))


# setwd("..")
load("Erdmann/results/ResultData_Scenario2_N100.rda")

plot_df <- rbind(plot_df,
                 data.frame(t(res_normal2),
                            t = c(40, 60, 80, 100), scenario = "rand_term",
                            method = "normal_spline", from = "new"),
                 data.frame(t(res_cutoff2),
                            t = c(40, 60, 80, 100), scenario = "rand_term",
                            method = "spline_by=transition", from = "new"),
                 data.frame(t(res_normal2_notrans),
                            t = c(40, 60, 80, 100), scenario = "rand_term",
                            method = "normal_spline_notrans", from = "new"),
                 data.frame("mean" = scenario$AJ[1,],
                            "sd"   = NA,
                            "bias" = scenario$AJ[2,],
                            "sd_b" = NA,
                            "rmse" = scenario$AJ[3,],
                            "True" = trueMeanMaster[[2]],
                            "t"    = c(40, 60, 80, 100),
                            "scenario"  = "rand_term",
                            "method"  = "AJ",
                            "from" = "old"),
                 data.frame("mean" = scenario$`NA`[1,],
                            "sd"   = NA,
                            "bias" = scenario$`NA`[2,],
                            "sd_b" = NA,
                            "rmse" = scenario$`NA`[3,],
                            "True" = trueMeanMaster[[2]],
                            "t"    = c(40, 60, 80, 100),
                            "scenario"  = "rand_term",
                            "method"  = "NA",
                            "from" = "old"),
                 data.frame("mean" = scenario$EB1[1,],
                            "sd"   = NA,
                            "bias" = scenario$EB1[2,],
                            "sd_b" = NA,
                            "rmse" = scenario$EB1[3,],
                            "True" = trueMeanMaster[[2]],
                            "t"    = c(40, 60, 80, 100),
                            "scenario"  = "rand_term",
                            "method"  = "EB1",
                            "from" = "old"),
                 data.frame("mean" = scenario$EB2[1,],
                            "sd"   = NA,
                            "bias" = scenario$EB2[2,],
                            "sd_b" = NA,
                            "rmse" = scenario$EB2[3,],
                            "True" = trueMeanMaster[[2]],
                            "t"    = c(40, 60, 80, 100),
                            "scenario"  = "rand_term",
                            "method"  = "EB2",
                            "from" = "old"))

# setwd("..")
load("Erdmann/results/ResultData_Scenario3_N100.rda")

plot_df <- rbind(plot_df,
                 data.frame(t(res_normal3),
                            t = c(40, 60, 80, 100), scenario = "state_noterm",
                            method = "normal_spline", from = "new"),
                 data.frame(t(res_cutoff3),
                            t = c(40, 60, 80, 100), scenario = "state_noterm",
                            method = "spline_by=transition", from = "new"),
                 data.frame(t(res_normal3_notrans),
                            t = c(40, 60, 80, 100), scenario = "state_noterm",
                            method = "normal_spline_notrans", from = "new"),
                 data.frame("mean" = scenario$AJ[1,],
                            "sd"   = NA,
                            "bias" = scenario$AJ[2,],
                            "sd_b" = NA,
                            "rmse" = scenario$AJ[3,],
                            "True" = trueMeanMaster[[3]],
                            "t"    = c(40, 60, 80, 100),
                            "scenario"  = "state_noterm",
                            "method"  = "AJ",
                            "from" = "old"),
                 data.frame("mean" = scenario$`NA`[1,],
                            "sd"   = NA,
                            "bias" = scenario$`NA`[2,],
                            "sd_b" = NA,
                            "rmse" = scenario$`NA`[3,],
                            "True" = trueMeanMaster[[3]],
                            "t"    = c(40, 60, 80, 100),
                            "scenario"  = "state_noterm",
                            "method"  = "NA",
                            "from" = "old"),
                 data.frame("mean" = scenario$EB1[1,],
                            "sd"   = NA,
                            "bias" = scenario$EB1[2,],
                            "sd_b" = NA,
                            "rmse" = scenario$EB1[3,],
                            "True" = trueMeanMaster[[3]],
                            "t"    = c(40, 60, 80, 100),
                            "scenario"  = "state_noterm",
                            "method"  = "EB1",
                            "from" = "old"),
                 data.frame("mean" = scenario$EB2[1,],
                            "sd"   = NA,
                            "bias" = scenario$EB2[2,],
                            "sd_b" = NA,
                            "rmse" = scenario$EB2[3,],
                            "True" = trueMeanMaster[[3]],
                            "t"    = c(40, 60, 80, 100),
                            "scenario"  = "state_noterm",
                            "method"  = "EB2",
                            "from" = "old"))

# setwd("..")
load("Erdmann/results/ResultData_Scenario4_N100.rda")

plot_df <- rbind(plot_df,
                 data.frame(t(res_normal4),
                            t = c(40, 60, 80, 100), scenario = "state_term",
                            method = "normal_spline", from = "new"),
                 data.frame(t(res_cutoff4),
                            t = c(40, 60, 80, 100), scenario = "state_term",
                            method = "spline_by=transition", from = "new"),
                 data.frame(t(res_normal4_notrans),
                            t = c(40, 60, 80, 100), scenario = "state_term",
                            method = "normal_spline_notrans", from = "new"),
                 data.frame("mean" = scenario$AJ[1,],
                            "sd"   = NA,
                            "bias" = scenario$AJ[2,],
                            "sd_b" = NA,
                            "rmse" = scenario$AJ[3,],
                            "True" = trueMeanMaster[[4]],
                            "t"    = c(40, 60, 80, 100),
                            "scenario"  = "state_term",
                            "method"  = "AJ",
                            "from" = "old"),
                 data.frame("mean" = scenario$`NA`[1,],
                            "sd"   = NA,
                            "bias" = scenario$`NA`[2,],
                            "sd_b" = NA,
                            "rmse" = scenario$`NA`[3,],
                            "True" = trueMeanMaster[[4]],
                            "t"    = c(40, 60, 80, 100),
                            "scenario"  = "state_term",
                            "method"  = "NA",
                            "from" = "old"),
                 data.frame("mean" = scenario$EB1[1,],
                            "sd"   = NA,
                            "bias" = scenario$EB1[2,],
                            "sd_b" = NA,
                            "rmse" = scenario$EB1[3,],
                            "True" = trueMeanMaster[[4]],
                            "t"    = c(40, 60, 80, 100),
                            "scenario"  = "state_term",
                            "method"  = "EB1",
                            "from" = "old"),
                 data.frame("mean" = scenario$EB2[1,],
                            "sd"   = NA,
                            "bias" = scenario$EB2[2,],
                            "sd_b" = NA,
                            "rmse" = scenario$EB2[3,],
                            "True" = trueMeanMaster[[4]],
                            "t"    = c(40, 60, 80, 100),
                            "scenario"  = "state_term",
                            "method"  = "EB2",
                            "from" = "old"))


pois_100_plot <- plot_df
pois_100_plot <- pois_100_plot %>% 
  mutate(method=ifelse(method=="normal_spline",
                       "s(tend)+transition",
                       ifelse(method=="normal_spline_notrans",
                              "s(tend)",
                              ifelse(method=="spline_by=transition",
                                     "s(tend, by=transition)+transition",
                                     method)))) %>% 
  mutate(from=ifelse(from=="new",
                      "PAMM",
                      "non-parametric")) %>% 
  rename(Estimator=method, Source=from)

save(pois_100_plot, file = "Analysis/final_res_scripts/final_results/pois_100_plot.rda")

load("Analysis/final_res_scripts/final_results/pois_100_plot.rda")

    
