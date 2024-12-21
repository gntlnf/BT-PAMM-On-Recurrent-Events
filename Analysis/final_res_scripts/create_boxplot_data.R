## create boxplot data N=100

# ohne terminating event
{load("Analysis/final_res_scripts/final_results/results/poisson100_final_results.RData")
  
  pois_1_100_res <- rbind(
    data.frame("100" = normal1[seq(3, length(normal1), by = 4)],
               Estimator = "s(tend)+transition"),
    data.frame("100" = normal1_notrans[seq(3, length(normal1_notrans), by = 4)],
               Estimator = "s(tend)"),
    data.frame("100" = (cutoff1 %>% clean_results())[seq(3, length(cutoff1 %>% clean_results()), by = 4)],
               Estimator = "s(tend, by=transition)+transition")
  )
  
  pois_1_100_res <- cbind(pois_1_100_res, Scenario = "pois_1", Source = "New")
  
  pois_3_100_res <- rbind(
    data.frame("100" = normal3[seq(3, length(normal3), by = 4)],
               Estimator = "s(tend)+transition"),
    data.frame("100" = normal3_notrans[seq(3, length(normal3_notrans), by = 4)],
               Estimator = "s(tend)"),
    data.frame("100" = (cutoff3 %>% clean_results())[seq(3, length(cutoff3 %>% clean_results()), by = 4)],
               Estimator = "s(tend, by=transition)+transition")
  )
  
  pois_3_100_res <- cbind(pois_3_100_res, Scenario = "pois_3", Source = "New")
  
  load("Analysis/final_res_scripts/final_results/results/mark100_final_results.RData")
  
  mark_1_100_res <- rbind(
    data.frame("100" = normal1[seq(3, length(normal1), by = 4)],
               Estimator = "s(tend)+transition"),
    data.frame("100" = (cutoff1 %>% clean_results())[seq(3, length(cutoff1 %>% clean_results()), by = 4)],
               Estimator = "s(tend, by=transition)+transition")
  )
  
  mark_1_100_res <- cbind(mark_1_100_res, Scenario = "markov_1", Source = "New")
  
  mark_3_100_res <- rbind(
    data.frame("100" = normal3[seq(3, length(normal3), by = 4)],
               Estimator = "s(tend)+transition"),
    data.frame("100" = (cutoff3 %>% clean_results())[seq(3, length(cutoff3 %>% clean_results()), by = 4)],
               Estimator = "s(tend, by=transition)+transition")
  )
  
  mark_3_100_res <- cbind(mark_3_100_res, Scenario = "markov_3", Source = "New")
  
  load("Analysis/final_res_scripts/final_results/results/non_mark100_final_results.RData")
  
  non_mark_1_100_res <- rbind(
    data.frame("100" = normal1[seq(3, length(normal1), by = 4)],
               Estimator = "s(tend)+transition"),
    data.frame("100" = (cutoff1 %>% clean_results())[seq(3, length(cutoff1 %>% clean_results()), by = 4)],
               Estimator = "s(tend, by=transition)+transition")
  )
  
  non_mark_1_100_res <- cbind(non_mark_1_100_res, Scenario = "non_markov_1", Source = "New")
  
  non_mark_3_100_res <- rbind(
    data.frame("100" = normal3[seq(3, length(normal3), by = 4)],
               Estimator = "s(tend)+transition"),
    data.frame("100" = (cutoff3 %>% clean_results())[seq(3, length(cutoff3 %>% clean_results()), by = 4)],
               Estimator = "s(tend, by=transition)+transition")
  )
  
  non_mark_3_100_res <- cbind(non_mark_3_100_res, Scenario = "non_markov_3", Source = "New")
  
  df <- rbind(pois_1_100_res, pois_3_100_res,
              mark_1_100_res, mark_3_100_res
              , non_mark_1_100_res, non_mark_3_100_res
  )
  
  
  load("Erdmann/results/Boxplot_Scenario1_N100.rda")
  erd_pois_1_100 <- boxplotData %>% 
    mutate(scenario = "pois_1")
  erd_pois_1_100 <- erd_pois_1_100 %>% 
    pivot_longer(
      cols = c(AJ, NAE, EB1, EB2), 
      names_to = "Estimator",      
      values_to = "X100"           
    ) %>% 
    rename(Scenario = scenario) %>% 
    mutate(Source = "Old")
  
  df <- rbind(df, erd_pois_1_100)
  
  load("Erdmann/results/Boxplot_Scenario3_N100.rda")
  erd_pois_3_100 <- boxplotData %>% 
    mutate(scenario = "pois_3")
  erd_pois_3_100 <- erd_pois_3_100 %>% 
    pivot_longer(
      cols = c(AJ, NAE, EB1, EB2), 
      names_to = "Estimator",      
      values_to = "X100"           
    ) %>% 
    rename(Scenario = scenario) %>% 
    mutate(Source = "Old")
  
  df <- rbind(df, erd_pois_3_100)
  
  load("Erdmann/results/Boxplot_Scenario5_N100.rda")
  
  erd_markov_1_100 <- boxplotData %>% 
    mutate(scenario = "markov_1")
  erd_markov_1_100 <- erd_markov_1_100 %>% 
    pivot_longer(
      cols = c(AJ, NAE, EB1, EB2),
      names_to = "Estimator",     
      values_to = "X100"          
    ) %>% 
    rename(Scenario = scenario) %>% 
    mutate(Source = "Old")
  
  df <- rbind(df, erd_markov_1_100)
  
  load("Erdmann/results/Boxplot_Scenario7_N100.rda")
  
  erd_markov_3_100 <- boxplotData %>% 
    mutate(scenario = "markov_3")
  erd_markov_3_100 <- erd_markov_3_100 %>% 
    pivot_longer(
      cols = c(AJ, NAE, EB1, EB2), 
      names_to = "Estimator",      
      values_to = "X100"           
    ) %>% 
    rename(Scenario = scenario) %>% 
    mutate(Source = "Old")
  
  df <- rbind(df, erd_markov_3_100)
  
  load("Erdmann/results/Boxplot_Scenario9_N100.rda")
  
  erd_non_markov_1_100 <- boxplotData %>% 
    mutate(scenario = "non_markov_1")
  erd_non_markov_1_100 <- erd_non_markov_1_100 %>% 
    pivot_longer(
      cols = c(AJ, NAE, EB1, EB2),
      names_to = "Estimator",     
      values_to = "X100"          
    ) %>% 
    rename(Scenario = scenario) %>% 
    mutate(Source = "Old")
  
  df <- rbind(df, erd_non_markov_1_100)
  
  load("Erdmann/results/Boxplot_Scenario11_N100.rda")
  
  erd_non_markov_3_100 <- boxplotData %>% 
    mutate(scenario = "non_markov_3")
  erd_non_markov_3_100 <- erd_non_markov_3_100 %>% 
    pivot_longer(
      cols = c(AJ, NAE, EB1, EB2), 
      names_to = "Estimator",      
      values_to = "X100"           
    ) %>% 
    rename(Scenario = scenario) %>% 
    mutate(Source = "Old")
  
  df <- rbind(df, erd_non_markov_3_100)
  
  df$Scenario <- factor(df$Scenario, levels = c("pois_1", "pois_3",
                                                "markov_1", "markov_3",
                                                "non_markov_1", "non_markov_3"))
  df$Estimator[df$Estimator=="NAE"]<-"NA"
  save(df, file = "Analysis/final_res_scripts/final_results/boxplot_100_final.rda")}

load("Analysis/final_res_scripts/final_results/evaluated/boxplot_100_final.rda")

boxplot_all_data <- df


# mit terminating event
{load("Analysis/final_res_scripts/final_results/results/poisson100_final_results.RData")
  
  pois_2_100_res <- rbind(
    data.frame("100" = normal2[seq(3, length(normal2), by = 4)],
               Estimator = "s(tend)+transition"),
    data.frame("100" = normal2_notrans[seq(3, length(normal2_notrans), by = 4)],
               Estimator = "s(tend)"),
    data.frame("100" = (cutoff2 %>% clean_results())[seq(3, length(cutoff2 %>% clean_results()), by = 4)],
               Estimator = "s(tend, by=transition)+transition")
  )
  
  pois_2_100_res <- cbind(pois_2_100_res, Scenario = "pois_2", Source = "New")
  
  pois_4_100_res <- rbind(
    data.frame("100" = normal4[seq(3, length(normal4), by = 4)],
               Estimator = "s(tend)+transition"),
    data.frame("100" = normal4_notrans[seq(3, length(normal4_notrans), by = 4)],
               Estimator = "s(tend)"),
    data.frame("100" = (cutoff4 %>% clean_results())[seq(3, length(cutoff4 %>% clean_results()), by = 4)],
               Estimator = "s(tend, by=transition)+transition")
  )
  
  pois_4_100_res <- cbind(pois_4_100_res, Scenario = "pois_4", Source = "New")
  
  load("Analysis/final_res_scripts/final_results/results/mark100_final_results.RData")
  
  mark_2_100_res <- rbind(
    data.frame("100" = normal2[seq(3, length(normal2), by = 4)],
               Estimator = "s(tend)+transition"),
    data.frame("100" = (cutoff2 %>% clean_results())[seq(3, length(cutoff2 %>% clean_results()), by = 4)],
               Estimator = "s(tend, by=transition)+transition")
  )
  
  mark_2_100_res <- cbind(mark_2_100_res, Scenario = "markov_2", Source = "New")
  
  mark_4_100_res <- rbind(
    data.frame("100" = normal4[seq(3, length(normal4), by = 4)],
               Estimator = "s(tend)+transition"),
    data.frame("100" = (cutoff4 %>% clean_results())[seq(3, length(cutoff4 %>% clean_results()), by = 4)],
               Estimator = "s(tend, by=transition)+transition")
  )
  
  mark_4_100_res <- cbind(mark_4_100_res, Scenario = "markov_4", Source = "New")
  
  load("Analysis/final_res_scripts/final_results/results/non_mark100_final_results.RData")
  
  non_mark_2_100_res <- rbind(
    data.frame("100" = normal2[seq(3, length(normal2), by = 4)],
               Estimator = "s(tend)+transition"),
    data.frame("100" = (cutoff2 %>% clean_results())[seq(3, length(cutoff2 %>% clean_results()), by = 4)],
               Estimator = "s(tend, by=transition)+transition")
  )
  
  
  non_mark_2_100_res <- cbind(non_mark_2_100_res, Scenario = "non_markov_2", Source = "New")
  
  non_mark_4_100_res <- rbind(
    data.frame("100" = (normal4 %>% clean_results())[seq(3, length(normal4 %>% clean_results()), by = 4)],
               Estimator = "s(tend)+transition"),
    data.frame("100" = (cutoff4 %>% clean_results())[seq(3, length(cutoff4 %>% clean_results()), by = 4)],
               Estimator = "s(tend, by=transition)+transition")
  )
  
  non_mark_4_100_res <- cbind(non_mark_4_100_res, Scenario = "non_markov_4", Source = "New")
  
  df <- rbind(pois_2_100_res, pois_4_100_res,
              mark_2_100_res, mark_4_100_res
              , non_mark_2_100_res, non_mark_4_100_res
  )
  
  
  load("Erdmann/results/Boxplot_Scenario2_N100.rda")
  erd_pois_2_100 <- boxplotData %>% 
    mutate(scenario = "pois_2")
  erd_pois_2_100 <- erd_pois_2_100 %>% 
    pivot_longer(
      cols = c(AJ, NAE, EB1, EB2), 
      names_to = "Estimator",      
      values_to = "X100"           
    ) %>% 
    rename(Scenario = scenario) %>% 
    mutate(Source = "Old")
  
  df <- rbind(df, erd_pois_2_100)
  
  load("Erdmann/results/Boxplot_Scenario4_N100.rda")
  erd_pois_4_100 <- boxplotData %>% 
    mutate(scenario = "pois_4")
  erd_pois_4_100 <- erd_pois_4_100 %>% 
    pivot_longer(
      cols = c(AJ, NAE, EB1, EB2), 
      names_to = "Estimator",      
      values_to = "X100"           
    ) %>% 
    rename(Scenario = scenario) %>% 
    mutate(Source = "Old")
  
  df <- rbind(df, erd_pois_4_100)
  
  load("Erdmann/results/Boxplot_Scenario6_N100.rda")
  
  erd_markov_2_100 <- boxplotData %>% 
    mutate(scenario = "markov_2")
  erd_markov_2_100 <- erd_markov_2_100 %>% 
    pivot_longer(
      cols = c(AJ, NAE, EB1, EB2), 
      names_to = "Estimator",      
      values_to = "X100"           
    ) %>% 
    rename(Scenario = scenario) %>% 
    mutate(Source = "Old")
  
  df <- rbind(df, erd_markov_2_100)
  
  load("Erdmann/results/Boxplot_Scenario8_N100.rda")
  
  erd_markov_4_100 <- boxplotData %>% 
    mutate(scenario = "markov_4")
  erd_markov_4_100 <- erd_markov_4_100 %>% 
    pivot_longer(
      cols = c(AJ, NAE, EB1, EB2),
      names_to = "Estimator",     
      values_to = "X100"          
    ) %>% 
    rename(Scenario = scenario) %>% 
    mutate(Source = "Old")
  
  df <- rbind(df, erd_markov_4_100)
  
  load("Erdmann/results/Boxplot_Scenario10_N100.rda")
  
  erd_non_markov_2_100 <- boxplotData %>% 
    mutate(scenario = "non_markov_2")
  erd_non_markov_2_100 <- erd_non_markov_2_100 %>% 
    pivot_longer(
      cols = c(AJ, NAE, EB1, EB2),
      names_to = "Estimator",     
      values_to = "X100"          
    ) %>% 
    rename(Scenario = scenario) %>% 
    mutate(Source = "Old")
  
  df <- rbind(df, erd_non_markov_2_100)
  
  load("Erdmann/results/Boxplot_Scenario12_N100.rda")
  
  erd_non_markov_4_100 <- boxplotData %>% 
    mutate(scenario = "non_markov_4")
  erd_non_markov_4_100 <- erd_non_markov_4_100 %>% 
    pivot_longer(
      cols = c(AJ, NAE, EB1, EB2), 
      names_to = "Estimator",      
      values_to = "X100"           
    ) %>% 
    rename(Scenario = scenario) %>% 
    mutate(Source = "Old")
  
  df <- rbind(df, erd_non_markov_4_100)
  
  df$Scenario <- factor(df$Scenario, levels = c("pois_2", "pois_4",
                                                "markov_2", "markov_4",
                                                "non_markov_2", "non_markov_4"))
  df$Estimator[df$Estimator=="NAE"]<-"NA"
  save(df, file = "Analysis/final_res_scripts/final_results/evaluated/boxplot_term_100_final.rda")}

load("Analysis/final_res_scripts/final_results/evaluated/boxplot_term_100_final.rda")

# save both in one
boxplot_all_data <- rbind(boxplot_all_data, df)
save(boxplot_all_data, file = "Analysis/final_res_scripts/final_results/evaluated/boxplot_all_data_100_final.rda")


# create boxplot N=200

# no terminating event
{
  load("Analysis/final_res_scripts/final_results/results/poisson200_final_results.RData")
  
  pois_1_200_res <- rbind(
    data.frame("40" = normal1[seq(1, length(normal1), by = 4)],
               "60" = normal1[seq(2, length(normal1), by = 4)],
               "80" = normal1[seq(3, length(normal1), by = 4)],
               "100" = normal1[seq(4, length(normal1), by = 4)],
               Estimator = "s(tend)+transition"),
    data.frame("40" = normal1_notrans[seq(1, length(normal1_notrans), by = 4)],
               "60" = normal1_notrans[seq(2, length(normal1_notrans), by = 4)],
               "80" = normal1_notrans[seq(3, length(normal1_notrans), by = 4)],
               "100" = normal1_notrans[seq(4, length(normal1_notrans), by = 4)],
               Estimator = "s(tend)"),
    data.frame("40" = (cutoff1 %>% clean_results())[seq(1, length(cutoff1 %>% clean_results()), by = 4)],
               "60" = (cutoff1 %>% clean_results())[seq(2, length(cutoff1 %>% clean_results()), by = 4)],
               "80" = (cutoff1 %>% clean_results())[seq(3, length(cutoff1 %>% clean_results()), by = 4)],
               "100" = (cutoff1 %>% clean_results())[seq(4, length(cutoff1 %>% clean_results()), by = 4)],
               Estimator = "s(tend, by=transition)+transition")
  )
  
  
  pois_1_200_res <- cbind(pois_1_200_res, Scenario = "pois_1", Source = "New")
  
  pois_3_200_res <- rbind(
    data.frame("40" = normal3[seq(1, length(normal3), by = 4)],
               "60" = normal3[seq(2, length(normal3), by = 4)],
               "80" = normal3[seq(3, length(normal3), by = 4)],
               "100" = normal3[seq(4, length(normal3), by = 4)],
               Estimator = "s(tend)+transition"),
    data.frame("40" = normal3_notrans[seq(1, length(normal3_notrans), by = 4)],
               "60" = normal3_notrans[seq(2, length(normal3_notrans), by = 4)],
               "80" = normal3_notrans[seq(3, length(normal3_notrans), by = 4)],
               "100" = normal3_notrans[seq(4, length(normal3_notrans), by = 4)],
               Estimator = "s(tend)"),
    data.frame("40" = (cutoff3 %>% clean_results())[seq(1, length(cutoff3 %>% clean_results()), by = 4)],
               "60" = (cutoff3 %>% clean_results())[seq(2, length(cutoff3 %>% clean_results()), by = 4)],
               "80" = (cutoff3 %>% clean_results())[seq(3, length(cutoff3 %>% clean_results()), by = 4)],
               "100" = (cutoff3 %>% clean_results())[seq(4, length(cutoff3 %>% clean_results()), by = 4)],
               Estimator = "s(tend, by=transition)+transition")
  ) 
  
  
  pois_3_200_res <- cbind(pois_3_200_res, Scenario = "pois_3", Source = "New")
  
  load("Analysis/final_res_scripts/final_results/results/mark200_final_results.RData")
  
  mark_1_200_res <- rbind(
    data.frame("40" = normal1[seq(1, length(normal1), by = 4)],
               "60" = normal1[seq(2, length(normal1), by = 4)],
               "80" = normal1[seq(3, length(normal1), by = 4)],
               "100" = normal1[seq(4, length(normal1), by = 4)],
               Estimator = "s(tend)+transition"),
    data.frame("40" = (cutoff1 %>% clean_results())[seq(1, length(cutoff1 %>% clean_results()), by = 4)],
               "60" = (cutoff1 %>% clean_results())[seq(2, length(cutoff1 %>% clean_results()), by = 4)],
               "80" = (cutoff1 %>% clean_results())[seq(3, length(cutoff1 %>% clean_results()), by = 4)],
               "100" = (cutoff1 %>% clean_results())[seq(4, length(cutoff1 %>% clean_results()), by = 4)],
               Estimator = "s(tend, by=transition)+transition")# ,
  )
  
  
  mark_1_200_res <- cbind(mark_1_200_res, Scenario = "markov_1", Source = "New")
  
  mark_3_200_res <- rbind(
    data.frame("40" = normal3[seq(1, length(normal3), by = 4)],
               "60" = normal3[seq(2, length(normal3), by = 4)],
               "80" = normal3[seq(3, length(normal3), by = 4)],
               "100" = normal3[seq(4, length(normal3), by = 4)],
               Estimator = "s(tend)+transition"),
    data.frame("40" = (cutoff3 %>% clean_results())[seq(1, length(cutoff3 %>% clean_results()), by = 4)],
               "60" = (cutoff3 %>% clean_results())[seq(2, length(cutoff3 %>% clean_results()), by = 4)],
               "80" = (cutoff3 %>% clean_results())[seq(3, length(cutoff3 %>% clean_results()), by = 4)],
               "100" = (cutoff3 %>% clean_results())[seq(4, length(cutoff3 %>% clean_results()), by = 4)],
               Estimator = "s(tend, by=transition)+transition")# ,
  )
  
  
  mark_3_200_res <- cbind(mark_3_200_res, Scenario = "markov_3", Source = "New")
  
  load("Analysis/final_res_scripts/final_results/results/non_mark200_final_results.RData")
  
  non_mark_1_200_res <- rbind(
    data.frame("40" = normal1[seq(1, length(normal1), by = 4)],
               "60" = normal1[seq(2, length(normal1), by = 4)],
               "80" = normal1[seq(3, length(normal1), by = 4)],
               "100" = normal1[seq(4, length(normal1), by = 4)],
               Estimator = "s(tend)+transition"),
    data.frame("40" = (cutoff1 %>% clean_results())[seq(1, length(cutoff1 %>% clean_results()), by = 4)],
               "60" = (cutoff1 %>% clean_results())[seq(2, length(cutoff1 %>% clean_results()), by = 4)],
               "80" = (cutoff1 %>% clean_results())[seq(3, length(cutoff1 %>% clean_results()), by = 4)],
               "100" = (cutoff1 %>% clean_results())[seq(4, length(cutoff1 %>% clean_results()), by = 4)],
               Estimator = "s(tend, by=transition)+transition")
  )
  
  non_mark_1_200_res <- cbind(non_mark_1_200_res, Scenario = "non_markov_1", Source = "New")
  
  
  non_mark_3_200_res <- rbind(
    data.frame("40" = normal3[seq(1, length(normal3), by = 4)],
               "60" = normal3[seq(2, length(normal3), by = 4)],
               "80" = normal3[seq(3, length(normal3), by = 4)],
               "100" = normal3[seq(4, length(normal3), by = 4)],
               Estimator = "s(tend)+transition"),
    data.frame("40" = (cutoff3 %>% clean_results())[seq(1, length(cutoff3 %>% clean_results()), by = 4)],
               "60" = (cutoff3 %>% clean_results())[seq(2, length(cutoff3 %>% clean_results()), by = 4)],
               "80" = (cutoff3 %>% clean_results())[seq(3, length(cutoff3 %>% clean_results()), by = 4)],
               "100" = (cutoff3 %>% clean_results())[seq(4, length(cutoff3 %>% clean_results()), by = 4)],
               Estimator = "s(tend, by=transition)+transition")
  )
  
  non_mark_3_200_res <- cbind(non_mark_3_200_res, Scenario = "non_markov_3", Source = "New")
  
  df <- rbind(pois_1_200_res, pois_3_200_res,
              mark_1_200_res, mark_3_200_res
              , non_mark_1_200_res, non_mark_3_200_res
  )
  
  
  
  df$Scenario <- factor(df$Scenario, levels = c("pois_1", "pois_3",
                                                "markov_1", "markov_3",
                                                "non_markov_1", "non_markov_3"))
  
  save(df, file = "Analysis/final_res_scripts/final_results/evaluated/boxplot_200_all_t.rda")}



# with terminating event
{
  load("Analysis/final_res_scripts/final_results/results/poisson200_final_results.RData")
  
  pois_2_200_res <- rbind(
    data.frame("40" = normal2[seq(1, length(normal2), by = 4)],
               "60" = normal2[seq(2, length(normal2), by = 4)],
               "80" = normal2[seq(3, length(normal2), by = 4)],
               "100" = normal2[seq(4, length(normal2), by = 4)],
               Estimator = "s(tend)+transition"),
    data.frame("40" = normal2_notrans[seq(1, length(normal2_notrans), by = 4)],
               "60" = normal2_notrans[seq(2, length(normal2_notrans), by = 4)],
               "80" = normal2_notrans[seq(3, length(normal2_notrans), by = 4)],
               "100" = normal2_notrans[seq(4, length(normal2_notrans), by = 4)],
               Estimator = "s(tend)"),
    data.frame("40" = (cutoff2 %>% clean_results())[seq(1, length(cutoff2 %>% clean_results()), by = 4)],
               "60" = (cutoff2 %>% clean_results())[seq(2, length(cutoff2 %>% clean_results()), by = 4)],
               "80" = (cutoff2 %>% clean_results())[seq(3, length(cutoff2 %>% clean_results()), by = 4)],
               "100" = (cutoff2 %>% clean_results())[seq(4, length(cutoff2 %>% clean_results()), by = 4)],
               Estimator = "s(tend, by=transition)+transition")
  )
  
  
  pois_2_200_res <- cbind(pois_2_200_res, Scenario = "pois_2", Source = "New")
  
  pois_4_200_res <- rbind(
    data.frame("40" = normal3[seq(1, length(normal3), by = 4)],
               "60" = normal3[seq(2, length(normal3), by = 4)],
               "80" = normal3[seq(3, length(normal3), by = 4)],
               "100" = normal3[seq(4, length(normal3), by = 4)],
               Estimator = "s(tend)+transition"),
    data.frame("40" = normal4_notrans[seq(1, length(normal4_notrans), by = 4)],
               "60" = normal4_notrans[seq(2, length(normal4_notrans), by = 4)],
               "80" = normal4_notrans[seq(3, length(normal4_notrans), by = 4)],
               "100" = normal4_notrans[seq(4, length(normal4_notrans), by = 4)],
               Estimator = "s(tend)"),
    data.frame("40" = (cutoff4 %>% clean_results())[seq(1, length(cutoff4 %>% clean_results()), by = 4)],
               "60" = (cutoff4 %>% clean_results())[seq(2, length(cutoff4 %>% clean_results()), by = 4)],
               "80" = (cutoff4 %>% clean_results())[seq(3, length(cutoff4 %>% clean_results()), by = 4)],
               "100" = (cutoff4 %>% clean_results())[seq(4, length(cutoff4 %>% clean_results()), by = 4)],
               Estimator = "s(tend, by=transition)+transition")
  ) 
  
  
  pois_4_200_res <- cbind(pois_4_200_res, Scenario = "pois_4", Source = "New")
  
  load("Analysis/final_res_scripts/final_results/results/mark200_final_results.RData")
  
  mark_2_200_res <- rbind(
    data.frame("40" = normal2[seq(1, length(normal2), by = 4)],
               "60" = normal2[seq(2, length(normal2), by = 4)],
               "80" = normal2[seq(3, length(normal2), by = 4)],
               "100" = normal2[seq(4, length(normal2), by = 4)],
               Estimator = "s(tend)+transition"),
    data.frame("40" = (cutoff2 %>% clean_results())[seq(1, length(cutoff2 %>% clean_results()), by = 4)],
               "60" = (cutoff2 %>% clean_results())[seq(2, length(cutoff2 %>% clean_results()), by = 4)],
               "80" = (cutoff2 %>% clean_results())[seq(3, length(cutoff2 %>% clean_results()), by = 4)],
               "100" = (cutoff2 %>% clean_results())[seq(4, length(cutoff2 %>% clean_results()), by = 4)],
               Estimator = "s(tend, by=transition)+transition")# ,
  )
  
  
  mark_2_200_res <- cbind(mark_2_200_res, Scenario = "markov_2", Source = "New")
  
  mark_4_200_res <- rbind(
    data.frame("40" = normal4[seq(1, length(normal4), by = 4)],
               "60" = normal4[seq(2, length(normal4), by = 4)],
               "80" = normal4[seq(3, length(normal4), by = 4)],
               "100" = normal4[seq(4, length(normal4), by = 4)],
               Estimator = "s(tend)+transition"),
    data.frame("40" = (cutoff4 %>% clean_results())[seq(1, length(cutoff4 %>% clean_results()), by = 4)],
               "60" = (cutoff4 %>% clean_results())[seq(2, length(cutoff4 %>% clean_results()), by = 4)],
               "80" = (cutoff4 %>% clean_results())[seq(3, length(cutoff4 %>% clean_results()), by = 4)],
               "100" = (cutoff4 %>% clean_results())[seq(4, length(cutoff4 %>% clean_results()), by = 4)],
               Estimator = "s(tend, by=transition)+transition")# ,
  )
  
  
  mark_4_200_res <- cbind(mark_4_200_res, Scenario = "markov_4", Source = "New")
  
  load("Analysis/final_res_scripts/final_results/results/non_mark200_final_results.RData")
  
  non_mark_2_200_res <- rbind(
    data.frame("40" = normal1[seq(1, length(normal1), by = 4)],
               "60" = normal1[seq(2, length(normal1), by = 4)],
               "80" = normal1[seq(3, length(normal1), by = 4)],
               "100" = normal1[seq(4, length(normal1), by = 4)],
               Estimator = "s(tend)+transition"),
    data.frame("40" = (cutoff2 %>% clean_results())[seq(1, length(cutoff2 %>% clean_results()), by = 4)],
               "60" = (cutoff2 %>% clean_results())[seq(2, length(cutoff2 %>% clean_results()), by = 4)],
               "80" = (cutoff2 %>% clean_results())[seq(3, length(cutoff2 %>% clean_results()), by = 4)],
               "100" = (cutoff2 %>% clean_results())[seq(4, length(cutoff2 %>% clean_results()), by = 4)],
               Estimator = "s(tend, by=transition)+transition")
  )
  
  non_mark_2_200_res <- cbind(non_mark_2_200_res, Scenario = "non_markov_2", Source = "New")
  
  
  non_mark_4_200_res <- rbind(
    data.frame("40" = normal4[seq(1, length(normal4), by = 4)],
               "60" = normal4[seq(2, length(normal4), by = 4)],
               "80" = normal4[seq(3, length(normal4), by = 4)],
               "100" = normal4[seq(4, length(normal4), by = 4)],
               Estimator = "s(tend)+transition"),
    data.frame("40" = (cutoff4 %>% clean_results())[seq(1, length(cutoff4 %>% clean_results()), by = 4)],
               "60" = (cutoff4 %>% clean_results())[seq(2, length(cutoff4 %>% clean_results()), by = 4)],
               "80" = (cutoff4 %>% clean_results())[seq(3, length(cutoff4 %>% clean_results()), by = 4)],
               "100" = (cutoff4 %>% clean_results())[seq(4, length(cutoff4 %>% clean_results()), by = 4)],
               Estimator = "s(tend, by=transition)+transition")
  )
  
  non_mark_4_200_res <- cbind(non_mark_4_200_res, Scenario = "non_markov_4", Source = "New")
  
  df <- rbind(pois_2_200_res, pois_4_200_res,
              mark_2_200_res, mark_4_200_res
              , non_mark_2_200_res, non_mark_4_200_res
  )
  
  
  
  df$Scenario <- factor(df$Scenario, levels = c("pois_2", "pois_4",
                                                "markov_2", "markov_4",
                                                "non_markov_2", "non_markov_4"))
  
  save(df, file = "Analysis/final_res_scripts/final_results/evaluated/boxplot_200_all_term_t.rda")}



# boxplot all N=200 for comparison plot


# no terminating event
{
  load("Analysis/final_res_scripts/final_results/results/poisson200_final_results.RData")

  pois_1_200_res <- rbind(
    data.frame("100" = normal1[seq(3, length(normal1), by = 4)],
               Estimator = "s(tend)+transition"),
    data.frame("100" = normal1_notrans[seq(3, length(normal1_notrans), by = 4)],
               Estimator = "s(tend)"),
    data.frame("100" = (cutoff1 %>% clean_results())[seq(3, length(cutoff1 %>% clean_results()), by = 4)],
               Estimator = "s(tend, by=transition)+transition")
  )

  pois_1_200_res <- cbind(pois_1_200_res, Scenario = "pois_1", Source = "New")

  pois_3_200_res <- rbind(
    data.frame("100" = normal3[seq(3, length(normal3), by = 4)],
               Estimator = "s(tend)+transition"),
    data.frame("100" = normal3_notrans[seq(3, length(normal3_notrans), by = 4)],
               Estimator = "s(tend)"),
    data.frame("100" = (cutoff3 %>% clean_results())[seq(3, length(cutoff3 %>% clean_results()), by = 4)],
               Estimator = "s(tend, by=transition)+transition")
  )

  pois_3_200_res <- cbind(pois_3_200_res, Scenario = "pois_3", Source = "New")

  load("Analysis/final_res_scripts/final_results/results/mark200_final_results.RData")

  mark_1_200_res <- rbind(
    data.frame("100" = normal1[seq(3, length(normal1), by = 4)],
               Estimator = "s(tend)+transition"),
    data.frame("100" = (cutoff1 %>% clean_results())[seq(3, length(cutoff1 %>% clean_results()), by = 4)],
               Estimator = "s(tend, by=transition)+transition")
  )

  mark_1_200_res <- cbind(mark_1_200_res, Scenario = "markov_1", Source = "New")

  mark_3_200_res <- rbind(
    data.frame("100" = normal3[seq(3, length(normal3), by = 4)],
               Estimator = "s(tend)+transition"),
    data.frame("100" = (cutoff3 %>% clean_results())[seq(3, length(cutoff3 %>% clean_results()), by = 4)],
               Estimator = "s(tend, by=transition)+transition")
  )

  mark_3_200_res <- cbind(mark_3_200_res, Scenario = "markov_3", Source = "New")

  load("Analysis/final_res_scripts/final_results/results/non_mark200_final_results.RData")

  non_mark_1_200_res <- rbind(
    data.frame("100" = normal1[seq(3, length(normal1), by = 4)],
               Estimator = "s(tend)+transition"),
    data.frame("100" = (cutoff1 %>% clean_results())[seq(3, length(cutoff1 %>% clean_results()), by = 4)],
               Estimator = "s(tend, by=transition)+transition")
  )

  non_mark_1_200_res <- cbind(non_mark_1_200_res, Scenario = "non_markov_1", Source = "New")

  non_mark_3_200_res <- rbind(
    data.frame("100" = normal3[seq(3, length(normal3), by = 4)],
               Estimator = "s(tend)+transition"),
    data.frame("100" = (cutoff3 %>% clean_results())[seq(3, length(cutoff3 %>% clean_results()), by = 4)],
               Estimator = "s(tend, by=transition)+transition")
  )

  non_mark_3_200_res <- cbind(non_mark_3_200_res, Scenario = "non_markov_3", Source = "New")

  df <- rbind(pois_1_200_res, pois_3_200_res,
              mark_1_200_res, mark_3_200_res
              , non_mark_1_200_res, non_mark_3_200_res
  )
  
  
  load("Erdmann/results/Boxplot_Scenario1_N200.rda")
  erd_pois_1_200 <- boxplotData %>% 
    mutate(scenario = "pois_1")
  erd_pois_1_200 <- erd_pois_1_200 %>% 
    pivot_longer(
      cols = c(AJ, NAE, EB1, EB2), 
      names_to = "Estimator",      
      values_to = "X100"           
    ) %>% 
    rename(Scenario = scenario) %>% 
    mutate(Source = "Old")
  
  df <- rbind(df, erd_pois_1_200)
  
  
  load("Erdmann/results/Boxplot_Scenario3_N200.rda")
  erd_pois_3_200 <- boxplotData %>% 
    mutate(scenario = "pois_3")
  erd_pois_3_200 <- erd_pois_3_200 %>% 
    pivot_longer(
      cols = c(AJ, NAE, EB1, EB2), 
      names_to = "Estimator",      
      values_to = "X100"           
    ) %>% 
    rename(Scenario = scenario) %>% 
    mutate(Source = "Old")
  
  df <- rbind(df, erd_pois_3_200)
  
  load("Erdmann/results/Boxplot_Scenario5_N200.rda")
  
  erd_markov_1_200 <- boxplotData %>% 
    mutate(scenario = "markov_1")
  erd_markov_1_200 <- erd_markov_1_200 %>% 
    pivot_longer(
      cols = c(AJ, NAE, EB1, EB2), 
      names_to = "Estimator",      
      values_to = "X100"           
    ) %>% 
    rename(Scenario = scenario) %>% 
    mutate(Source = "Old")
  
  df <- rbind(df, erd_markov_1_200)
  
  load("Erdmann/results/Boxplot_Scenario7_N200.rda")
  
  erd_markov_3_200 <- boxplotData %>% 
    mutate(scenario = "markov_3")
  erd_markov_3_200 <- erd_markov_3_200 %>% 
    pivot_longer(
      cols = c(AJ, NAE, EB1, EB2),
      names_to = "Estimator",     
      values_to = "X100"          
    ) %>% 
    rename(Scenario = scenario) %>% 
    mutate(Source = "Old")
  
  df <- rbind(df, erd_markov_3_200)
  
  load("Erdmann/results/Boxplot_Scenario9_N200.rda")
  
  erd_non_markov_1_200 <- boxplotData %>% 
    mutate(scenario = "non_markov_1")
  erd_non_markov_1_200 <- erd_non_markov_1_200 %>% 
    pivot_longer(
      cols = c(AJ, NAE, EB1, EB2),
      names_to = "Estimator",     
      values_to = "X100"          
    ) %>% 
    rename(Scenario = scenario) %>% 
    mutate(Source = "Old")
  
  df <- rbind(df, erd_non_markov_1_200)
  
  load("Erdmann/results/Boxplot_Scenario11_N200.rda")
  
  erd_non_markov_3_200 <- boxplotData %>% 
    mutate(scenario = "non_markov_3")
  erd_non_markov_3_200 <- erd_non_markov_3_200 %>% 
    pivot_longer(
      cols = c(AJ, NAE, EB1, EB2),
      names_to = "Estimator",     
      values_to = "X100"          
    ) %>% 
    rename(Scenario = scenario) %>% 
    mutate(Source = "Old")
  
  df <- rbind(df, erd_non_markov_3_200)
  
  df$Scenario <- factor(df$Scenario, levels = c("pois_1", "pois_3",
                                                "markov_1", "markov_3",
                                                "non_markov_1", "non_markov_3"))
  df$Estimator[df$Estimator=="NAE"]<-"NA"
  save(df, file = "Analysis/final_res_scripts/final_results/evaluated/boxplot_200_final.rda")
  }

df_200_boxplot_all <- df

# with terminating event
{
  load("Analysis/final_res_scripts/final_results/results/poisson200_final_results.RData")

  pois_2_200_res <- rbind(
    data.frame("100" = normal2[seq(3, length(normal2), by = 4)],
               Estimator = "s(tend)+transition"),
    data.frame("100" = normal2_notrans[seq(3, length(normal2_notrans), by = 4)],
               Estimator = "s(tend)"),
    data.frame("100" = (cutoff2 %>% clean_results())[seq(3, length(cutoff2 %>% clean_results()), by = 4)],
               Estimator = "s(tend, by=transition)+transition")
  )

  pois_2_200_res <- cbind(pois_2_200_res, Scenario = "pois_2", Source = "New")

  pois_4_200_res <- rbind(
    data.frame("100" = normal4[seq(3, length(normal4), by = 4)],
               Estimator = "s(tend)+transition"),
    data.frame("100" = normal4_notrans[seq(3, length(normal4_notrans), by = 4)],
               Estimator = "s(tend)"),
    data.frame("100" = (cutoff4 %>% clean_results())[seq(3, length(cutoff4 %>% clean_results()), by = 4)],
               Estimator = "s(tend, by=transition)+transition")
  )

  pois_4_200_res <- cbind(pois_4_200_res, Scenario = "pois_4", Source = "New")

  load("Analysis/final_res_scripts/final_results/results/mark200_final_results.RData")

  mark_2_200_res <- rbind(
    data.frame("100" = normal2[seq(3, length(normal2), by = 4)],
               Estimator = "s(tend)+transition"),
    data.frame("100" = (cutoff2 %>% clean_results())[seq(3, length(cutoff2 %>% clean_results()), by = 4)],
               Estimator = "s(tend, by=transition)+transition")
  )

  mark_2_200_res <- cbind(mark_2_200_res, Scenario = "markov_2", Source = "New")

  mark_4_200_res <- rbind(
    data.frame("100" = normal4[seq(3, length(normal4), by = 4)],
               Estimator = "s(tend)+transition"),
    data.frame("100" = (cutoff4 %>% clean_results())[seq(3, length(cutoff4 %>% clean_results()), by = 4)],
               Estimator = "s(tend, by=transition)+transition")
  )

  mark_4_200_res <- cbind(mark_4_200_res, Scenario = "markov_4", Source = "New")

  load("Analysis/final_res_scripts/final_results/results/non_mark200_final_results.RData")

  non_mark_2_200_res <- rbind(
    data.frame("100" = normal2[seq(3, length(normal2), by = 4)],
               Estimator = "s(tend)+transition"),
    data.frame("100" = (cutoff2 %>% clean_results())[seq(3, length(cutoff2 %>% clean_results()), by = 4)],
               Estimator = "s(tend, by=transition)+transition")
  )


  non_mark_2_200_res <- cbind(non_mark_2_200_res, Scenario = "non_markov_2", Source = "New")

  non_mark_4_200_res <- rbind(
    data.frame("100" = (normal4 %>% clean_results())[seq(3, length(normal4 %>% clean_results()), by = 4)],
               Estimator = "s(tend)+transition"),
    data.frame("100" = (cutoff4 %>% clean_results())[seq(3, length(cutoff4 %>% clean_results()), by = 4)],
               Estimator = "s(tend, by=transition)+transition")
  )

  non_mark_4_200_res <- cbind(non_mark_4_200_res, Scenario = "non_markov_4", Source = "New")

  df <- rbind(pois_2_200_res, pois_4_200_res,
              mark_2_200_res, mark_4_200_res
              , non_mark_2_200_res, non_mark_4_200_res
  )

  
  load("Erdmann/results/Boxplot_Scenario2_N200.rda")
  erd_pois_2_200 <- boxplotData %>% 
    mutate(scenario = "pois_2")
  erd_pois_2_200 <- erd_pois_2_200 %>% 
    pivot_longer(
      cols = c(AJ, NAE, EB1, EB2),
      names_to = "Estimator",     
      values_to = "X100"          
    ) %>% 
    rename(Scenario = scenario) %>% 
    mutate(Source = "Old")
  
  df <- rbind(df, erd_pois_2_200)
  
  
  load("Erdmann/results/Boxplot_Scenario4_N200.rda")
  erd_pois_4_200 <- boxplotData %>% 
    mutate(scenario = "pois_4")
  erd_pois_4_200 <- erd_pois_4_200 %>% 
    pivot_longer(
      cols = c(AJ, NAE, EB1, EB2),
      names_to = "Estimator",     
      values_to = "X100"          
    ) %>% 
    rename(Scenario = scenario) %>% 
    mutate(Source = "Old")
  
  df <- rbind(df, erd_pois_4_200)
  
  load("Erdmann/results/Boxplot_Scenario6_N200.rda")
  
  erd_markov_2_200 <- boxplotData %>% 
    mutate(scenario = "markov_2")
  erd_markov_2_200 <- erd_markov_2_200 %>% 
    pivot_longer(
      cols = c(AJ, NAE, EB1, EB2),
      names_to = "Estimator",     
      values_to = "X100"          
    ) %>% 
    rename(Scenario = scenario) %>% 
    mutate(Source = "Old")
  
  df <- rbind(df, erd_markov_2_200)
  
  load("Erdmann/results/Boxplot_Scenario8_N200.rda")
  
  erd_markov_4_200 <- boxplotData %>% 
    mutate(scenario = "markov_4")
  erd_markov_4_200 <- erd_markov_4_200 %>% 
    pivot_longer(
      cols = c(AJ, NAE, EB1, EB2), 
      names_to = "Estimator",      
      values_to = "X100"           
    ) %>% 
    rename(Scenario = scenario) %>% 
    mutate(Source = "Old")
  
  df <- rbind(df, erd_markov_4_200)
  
  load("Erdmann/results/Boxplot_Scenario10_N200.rda")
  
  erd_non_markov_2_200 <- boxplotData %>% 
    mutate(scenario = "non_markov_2")
  erd_non_markov_2_200 <- erd_non_markov_2_200 %>% 
    pivot_longer(
      cols = c(AJ, NAE, EB1, EB2),
      names_to = "Estimator",     
      values_to = "X100"          
    ) %>% 
    rename(Scenario = scenario) %>% 
    mutate(Source = "Old")
  
  df <- rbind(df, erd_non_markov_2_200)
  
  load("Erdmann/results/Boxplot_Scenario12_N200.rda")
  
  erd_non_markov_4_200 <- boxplotData %>% 
    mutate(scenario = "non_markov_4")
  erd_non_markov_4_200 <- erd_non_markov_4_200 %>% 
    pivot_longer(
      cols = c(AJ, NAE, EB1, EB2), 
      names_to = "Estimator",      
      values_to = "X100"           
    ) %>% 
    rename(Scenario = scenario) %>% 
    mutate(Source = "Old")
  
  df <- rbind(df, erd_non_markov_4_200)
  
  df$Scenario <- factor(df$Scenario, levels = c("pois_2", "pois_4",
                                                "markov_2", "markov_4",
                                                "non_markov_2", "non_markov_4"))
  df$Estimator[df$Estimator=="NAE"]<-"NA"
  save(df, file = "Analysis/final_res_scripts/final_results/evaluated/boxplot_term_200_final.rda")
  }

df_200_boxplot_all <- rbind(df_200_boxplot_all, df) 


save(df_200_boxplot_all, file = "Analysis/final_res_scripts/final_results/evaluated/boxplot_all_200_final.rda")



