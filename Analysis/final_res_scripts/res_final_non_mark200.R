

# load results of this thesis settings: Non-Markov N=200
load("Analysis/final_res_scripts/final_results/results/non_mark200_final_results.RData")

# evaluate results of this thesis 
res_normal1 <- normal1 %>% m_s_b_r(9)

res_cutoff1 <- cutoff1 %>% clean_results() %>% m_s_b_r(9)

res_normal2 <- normal2 %>% m_s_b_r(10)

res_cutoff2 <- cutoff2 %>% clean_results() %>% m_s_b_r(10)

res_normal3 <- normal3 %>% m_s_b_r(11)

res_cutoff3 <- cutoff3 %>% clean_results() %>% m_s_b_r(11)

res_normal4 <- normal4 %>% clean_results() %>% m_s_b_r(12)

res_cutoff4 <- cutoff4 %>% clean_results() %>% m_s_b_r(12)

# evaluate results of replication of Erdmann's paper results
load("Erdmann/results/ResultData_Scenario9_N200.rda")

plot_df <- rbind(data.frame(t(res_normal1),
                            t = c(40, 60, 80, 100), scenario = "rand_noterm",
                            method = "normal_spline", from = "new"),
                 data.frame(t(res_cutoff1),
                            t = c(40, 60, 80, 100), scenario = "rand_noterm",
                            method = "spline_by=transition", from = "new"),
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

plot_df <- rbind(plot_df,
                 data.frame(t(res_normal2),
                            t = c(40, 60, 80, 100), scenario = "rand_term",
                            method = "normal_spline", from = "new"),
                 data.frame(t(res_cutoff2),
                            t = c(40, 60, 80, 100), scenario = "rand_term",
                            method = "spline_by=transition", from = "new"),
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


load("Erdmann/results/ResultData_Scenario12_N100.rda")

plot_df <- rbind(plot_df,
                 data.frame(t(res_normal3),
                            t = c(40, 60, 80, 100), scenario = "state_noterm",
                            method = "normal_spline", from = "new"),
                 data.frame(t(res_cutoff3),
                            t = c(40, 60, 80, 100), scenario = "state_noterm",
                            method = "spline_by=transition", from = "new"),
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

load("Erdmann/results/ResultData_Scenario12_N200.rda")

plot_df <- rbind(plot_df,
                 data.frame(t(res_normal4),
                            t = c(40, 60, 80, 100), scenario = "state_term",
                            method = "normal_spline", from = "new"),
                 data.frame(t(res_cutoff4),
                            t = c(40, 60, 80, 100), scenario = "state_term",
                            method = "spline_by=transition", from = "new"),
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

save(plot_non_markov200, file = "Analysis/final_res_scripts/final_results/evaluated/plot_non_markov200.rda")

load("Analysis/final_res_scripts/final_results/evaluated/plot_non_markov200.rda")


