


load("Analysis/final_res_scripts/final_results/non_mark200_final_results.RData")
# setwd("..")
load("Erdmann/results/ResultData_Scenario9_N200.rda")


res_normal1 <- normal1 %>% m_s_b_r(9)

# res_normal1_notrans <- normal1_notrans %>% m_s_b_r(9)

res_cutoff1 <- cutoff1 %>% clean_results() %>% m_s_b_r(9)

# res_wait1 <- wait1 %>% clean_results() %>% m_s_b_r(9)

# res_wait1_nonmarkov <- wait1_nonmarkov %>% clean_results() %>% m_s_b_r(9)

# res_tspline1 <- tspline1 %>% clean_results() %>% m_s_b_r(9)


plot_df <- rbind(data.frame(t(res_normal1),
                            t = c(40, 60, 80, 100), scenario = "rand_noterm",
                            method = "normal_spline", from = "new"),
                 data.frame(t(res_cutoff1),
                            t = c(40, 60, 80, 100), scenario = "rand_noterm",
                            method = "spline_by=transition", from = "new"),
                 # data.frame(t(res_normal1_notrans),
                 #            t = c(40, 60, 80, 100), scenario = "rand_noterm",
                 #            method = "normal_spline_notrans", from = "new"),
                 # data.frame(t(res_wait1),
                 #            t = c(40, 60, 80, 100), scenario = "rand_noterm",
                 #            method = "spline_by=transition_wait", from = "new"),
                 # data.frame(t(res_wait1_nonmarkov),
                 #            t = c(40, 60, 80, 100), scenario = "rand_noterm",
                 #            method = "spline_by=transition_wait_nonmarkov", from = "new"),
                 # data.frame(t(res_tspline1),
                 #            t = c(40, 60, 80, 100), scenario = "rand_noterm",
                 #            method = "tensor_spline", from = "new"),
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

load("Erdmann/results/ResultData_Scenario10_N200.rda")

res_normal2 <- normal2 %>% m_s_b_r(10)

# res_normal2_notrans <- normal2_notrans %>% m_s_b_r(10)

res_cutoff2 <- cutoff2 %>% clean_results() %>% m_s_b_r(10)

# res_wait2 <- wait2 %>% clean_results() %>% m_s_b_r(10)
# 
# res_wait2_nonmarkov <- wait2_nonmarkov %>% clean_results() %>% m_s_b_r(10)
# 
# res_tspline2 <- tspline2 %>% clean_results() %>% m_s_b_r(10)





plot_df <- rbind(plot_df,
                 data.frame(t(res_normal2),
                            t = c(40, 60, 80, 100), scenario = "rand_term",
                            method = "normal_spline", from = "new"),
                 data.frame(t(res_cutoff2),
                            t = c(40, 60, 80, 100), scenario = "rand_term",
                            method = "spline_by=transition", from = "new"),
                 # data.frame(t(res_normal2_notrans),
                 #            t = c(40, 60, 80, 100), scenario = "rand_term",
                 #            method = "normal_spline_notrans", from = "new"),
                 # data.frame(t(res_wait2),
                 #            t = c(40, 60, 80, 100), scenario = "rand_term",
                 #            method = "spline_by=transition_wait", from = "new"),
                 # data.frame(t(res_wait2_nonmarkov),
                 #            t = c(40, 60, 80, 100), scenario = "rand_term",
                 #            method = "spline_by=transition_wait_nonmarkov", from = "new"),
                 # data.frame(t(res_tspline2),
                 #            t = c(40, 60, 80, 100), scenario = "rand_term",
                 #            method = "tensor_spline", from = "new"),
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
load("Erdmann/results/ResultData_Scenario12_N100.rda")

res_normal3 <- normal3 %>% m_s_b_r(11)

# res_normal3_notrans <- normal3_notrans %>% m_s_b_r(11)

res_cutoff3 <- cutoff3 %>% clean_results() %>% m_s_b_r(11)

# res_wait3 <- wait3 %>% clean_results() %>% m_s_b_r(11)
# 
# res_wait3_nonmarkov <- wait3_nonmarkov %>% clean_results() %>% m_s_b_r(11)
# 
# res_tspline3 <- tspline3 %>% clean_results() %>% m_s_b_r(11)




plot_df <- rbind(plot_df,
                 data.frame(t(res_normal3),
                            t = c(40, 60, 80, 100), scenario = "state_noterm",
                            method = "normal_spline", from = "new"),
                 data.frame(t(res_cutoff3),
                            t = c(40, 60, 80, 100), scenario = "state_noterm",
                            method = "spline_by=transition", from = "new"),
                 # data.frame(t(res_normal3_notrans),
                 #            t = c(40, 60, 80, 100), scenario = "state_noterm",
                 #            method = "normal_spline_notrans", from = "new"),
                 # data.frame(t(res_wait3),
                 #            t = c(40, 60, 80, 100), scenario = "state_noterm",
                 #            method = "spline_by=transition_wait", from = "new"),
                 # data.frame(t(res_wait3_nonmarkov),
                 #            t = c(40, 60, 80, 100), scenario = "state_noterm",
                 #            method = "spline_by=transition_wait_nonmarkov", from = "new"),
                 # data.frame(t(res_tspline3),
                 #            t = c(40, 60, 80, 100), scenario = "state_noterm",
                 #            method = "tensor_spline", from = "new"),
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
load("Erdmann/results/ResultData_Scenario12_N200.rda")

res_normal4 <- normal4 %>% clean_results() %>% m_s_b_r(12)

# res_normal4_notrans <- normal4_notrans %>% m_s_b_r(12)

res_cutoff4 <- cutoff4 %>% clean_results() %>% m_s_b_r(12)

# res_wait4 <- wait4 %>% clean_results() %>% m_s_b_r(12)
# 
# res_wait4_nonmarkov <- wait4_nonmarkov %>% clean_results() %>% m_s_b_r(12)
# 
# res_tspline4 <- tspline4 %>% clean_results() %>% m_s_b_r(12)



plot_df <- rbind(plot_df,
                 data.frame(t(res_normal4),
                            t = c(40, 60, 80, 100), scenario = "state_term",
                            method = "normal_spline", from = "new"),
                 data.frame(t(res_cutoff4),
                            t = c(40, 60, 80, 100), scenario = "state_term",
                            method = "spline_by=transition", from = "new"),
                 # data.frame(t(res_normal4_notrans),
                 #            t = c(40, 60, 80, 100), scenario = "state_term",
                 #            method = "normal_spline_notrans", from = "new"),
                 # data.frame(t(res_wait4),
                 #            t = c(40, 60, 80, 100), scenario = "state_term",
                 #            method = "spline_by=transition_wait", from = "new"),
                 # data.frame(t(res_wait4_nonmarkov),
                 #            t = c(40, 60, 80, 100), scenario = "state_term",
                 #            method = "spline_by=transition_wait_nonmarkov", from = "new"),
                 # data.frame(t(res_tspline4),
                 #            t = c(40, 60, 80, 100), scenario = "state_term",
                 #            method = "tensor_spline", from = "new"),
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


plot_non_markov200 <- plot_df


plot_non_markov200 <- plot_non_markov200 %>% 
  mutate(from=ifelse(from=="new",
                     "PAMM",
                     "non-parametric")) %>%
  mutate(method=ifelse(method=="normal_spline",
                       "s(tend)+transition",
                       ifelse(method=="normal_spline_notrans",
                              "s(tend)",
                              ifelse(method=="spline_by=transition",
                                     "s(tend, by=transition)+transition",
                                     ifelse(method=="spline_by=transition_wait",
                                            "s(tend, by=transition)+wait",
                                            ifelse(method=="spline_by=transition_wait_nonmarkov",
                                                   "s(tend, by=transition)+wait(nonmark)",
                                                   method)))))) %>% 
  rename(Estimator=method, Source=from) %>% 
  filter(Estimator != "tensor_spline")

save(plot_non_markov200, file = "Analysis/final_res_scripts/final_results/plot_non_markov200.rda")

load("Analysis/final_res_scripts/final_results/plot_non_markov200.rda")


