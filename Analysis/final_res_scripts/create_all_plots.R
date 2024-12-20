

# define used colors per estimator

color_erd <- c("NA"="#FF6666", "EB1"="#FF0000", "EB2"="#990000", "AJ"="#660000")
# color_mine <- c("Smooth Effect on Time"="#66B2FF", "Smooth Effect on Time + Intercept Change on Transition"="#3399FF", "Smooth Effect on Time dependent on Transition"="#0066FF", "s(tend, by=transition)+wait"="#003399", "s(tend, by=transition)+wait(nonmark)"="#001F5B")
color_mine <- c("Smooth Effect on Time"="#66B2FF", "Smooth Effect on Time + Intercept Change on Transition"="#0066FF", "Smooth Effect on Time dependent on Transition"="#003399") #, "s(tend, by=transition)+wait"="#003399", "s(tend, by=transition)+wait(nonmark)"="#001F5B")
colors <- c(color_erd, color_mine)
cols <- scale_color_manual(values = colors)
cols_fill <- scale_fill_manual(values = colors)


# poisson N=100 Bias and RMSE

load("Analysis/final_res_scripts/final_results/pois_100_plot.rda")

pois_100_plot <- pois_100_plot %>% 
  mutate(Estimator = factor(Estimator, levels = c(names(color_erd), c("s(tend)", "s(tend)+transition", "s(tend, by=transition)+transition")), labels = c(names(color_erd), names(color_mine))))

pois100_plot <- ggplot(pois_100_plot,
                       aes(x = t,
                           y = bias,
                           color = Estimator,
                           shape = Source)) +
  geom_jitter(size=3, alpha=1, width=1, height = 0) +
  # geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  # theme_bw() +
  ylim(c(-2, 2)) +
  cols +
  facet_wrap(~scenario,
             labeller = labeller(scenario = c("rand_noterm"="Random Censoring / No Terminating Event",
                                              "rand_term"="Random Censoring / Terminating Event",
                                              "state_noterm"="State Dependent Censoring / No Terminating Event",
                                              "state_term"="State Dependent Censoring / Terminating Event"))) +
  theme_bw() +
  labs(x="Time", y="relative Bias (%)") +
  theme(
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(color = "black", size = 14),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 15),
    axis.title = element_text(size = 15),
    legend.title = element_text(size = 15)
  ) +
  theme(legend.position = "bottom") +
  guides(
    color = guide_legend(ncol = 2),     
    shape = guide_legend(ncol = 1)      
  ) 
pois100_plot_rmse <- ggplot(pois_100_plot,
                            aes(x = t,
                                y = rmse,
                                color = Estimator,
                                shape = Source)) +
  geom_jitter(size=3, alpha=1, width=1, height = 0) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw() +
  ylim(c(0, 1)) +
  cols +
  facet_wrap(~scenario,
             labeller = labeller(scenario = c("rand_noterm"="Random Censoring / No Terminating Event",
                                              "rand_term"="Random Censoring / Terminating Event",
                                              "state_noterm"="State Dependent Censoring / No Terminating Event",
                                              "state_term"="State Dependent Censoring / Terminating Event"))) +
  theme_bw() +
  labs(x="Time", y="RMSE") +
  theme(
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(color = "black", size = 14),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 15),
    axis.title = element_text(size = 15),
    legend.title = element_text(size = 15)
  ) +
  theme(legend.position = "bottom") +
  guides(
    color = guide_legend(ncol = 2),     
    shape = guide_legend(ncol = 1)      
  ) 

ggsave("Analysis/final_res_scripts/final_results/plots/pois100_plot_rmse.png", pois100_plot_rmse, width = 11, height = 11)
ggsave("Analysis/final_res_scripts/final_results/plots/pois100_plot_bias.png", pois100_plot, width = 11, height = 11)

# markov N=100 Bias and RMSE

load("Analysis/final_res_scripts/final_results/plot_markov100.rda")
plot_markov100 <- plot_markov100 %>% 
  filter(!(Estimator %in% c("s(tend, by=transition)+wait(nonmark)",
                            "s(tend, by=transition)+wait"))) %>% 
  mutate(Estimator = factor(Estimator, levels = c(names(color_erd), c("s(tend)", "s(tend)+transition", "s(tend, by=transition)+transition")), labels = c(names(color_erd), names(color_mine))))

mark100_plot <- ggplot(plot_markov100,
                       aes(x = t,
                           y = bias,
                           color = Estimator,
                           shape = Source)) +
  geom_jitter(size=3, alpha=1, width=1, height = 0) +
  # geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw() +
  # ylim(c(-5, 5)) +
  cols +
  facet_wrap(~scenario,
             labeller = labeller(scenario = c("rand_noterm"="Random Censoring / No Terminating Event",
                                              "rand_term"="Random Censoring / Terminating Event",
                                              "state_noterm"="State Dependent Censoring / No Terminating Event",
                                              "state_term"="State Dependent Censoring / Terminating Event")),
             scales = "free_y") +
  theme_bw() +
  labs(x="Time", y="relative Bias (%)") +
  theme(
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(color = "black", size = 14),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 15),
    axis.title = element_text(size = 15),
    legend.title = element_text(size = 15)
  ) +
  theme(legend.position = "bottom") +
  guides(
    color = guide_legend(ncol = 2, nrow = 4),     
    shape = guide_legend(ncol = 1)      
  ) 



mark100_plot_rmse <- ggplot(plot_markov100,
                            aes(x = t,
                                y = rmse,
                                color = Estimator,
                                shape = Source)) +
  geom_jitter(size=3, alpha=1, width=1, height = 0) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw() +
  # ylim(c(0, 1)) +
  cols +
  facet_wrap(~scenario,
             labeller = labeller(scenario = c("rand_noterm"="Random Censoring / No Terminating Event",
                                              "rand_term"="Random Censoring / Terminating Event",
                                              "state_noterm"="State Dependent Censoring / No Terminating Event",
                                              "state_term"="State Dependent Censoring / Terminating Event")),
             scales = "free_y") +
  theme_bw() +
  labs(x="Time", y="RMSE") +
  theme(
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(color = "black", size = 14),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 15),
    axis.title = element_text(size = 15),
    legend.title = element_text(size = 15)
  ) +
  theme(legend.position = "bottom") +
  guides(
    color = guide_legend(ncol = 2, nrow = 4),     
    shape = guide_legend(ncol = 1)      
  ) 

ggsave("Analysis/final_res_scripts/final_results/plots/mark100_plot_bias.png", mark100_plot, width = 11, height = 11)

ggsave("Analysis/final_res_scripts/final_results/plots/mark100_plot_rmse.png", mark100_plot_rmse, width = 11, height = 11)

# non markov N=100 Bias and RMSE

load("Analysis/final_res_scripts/final_results/plot_non_markov100.rda")

plot_non_markov100 <- plot_non_markov100 %>% 
  filter(!(Estimator %in% c("te(tend, wait)+transition",
                            "s(tend, by=transition)+wait(nonmark)",
                            "s(tend, by=transition)+wait"))) %>% 
  mutate(Estimator = factor(Estimator, levels = c(names(color_erd), c("s(tend)", "s(tend)+transition", "s(tend, by=transition)+transition")), labels = c(names(color_erd), names(color_mine))))


non_mark100_plot <- ggplot(plot_non_markov100,
                           aes(x = t,
                               y = bias,
                               color = Estimator,
                               shape = Source)) +
  geom_jitter(size=3, alpha=1, width=1, height = 0) +
  # geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw() +
  # ylim(c(-2, 2)) +
  cols +
  facet_wrap(~scenario,
             labeller = labeller(scenario = c("rand_noterm"="Random Censoring / No Terminating Event",
                                              "rand_term"="Random Censoring / Terminating Event",
                                              "state_noterm"="State Dependent Censoring / No Terminating Event",
                                              "state_term"="State Dependent Censoring / Terminating Event")),
             scales = "free_y") +
  theme_bw() +
  labs(x="Time", y="relative Bias (%)") +
  theme(
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(color = "black", size = 14),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 15),
    axis.title = element_text(size = 15),
    legend.title = element_text(size = 15)
  ) +
  theme(legend.position = "bottom") +
  guides(
    color = guide_legend(ncol = 2, nrow = 4),     
    shape = guide_legend(ncol = 1)      
  ) 

non_mark100_plot_rmse <- ggplot(plot_non_markov100,
                                aes(x = t,
                                    y = rmse,
                                    color = Estimator,
                                    shape = Source)) +
  geom_jitter(size=3, alpha=1, width=1, height = 0) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw() +
  # ylim(c(0, 1)) +
  cols +
  facet_wrap(~scenario,
             labeller = labeller(scenario = c("rand_noterm"="Random Censoring / No Terminating Event",
                                              "rand_term"="Random Censoring / Terminating Event",
                                              "state_noterm"="State Dependent Censoring / No Terminating Event",
                                              "state_term"="State Dependent Censoring / Terminating Event")),
             scales = "free_y") +
  theme_bw() +
  labs(x="Time", y="RMSE") +
  theme(
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(color = "black", size = 14),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 15),
    axis.title = element_text(size = 15),
    legend.title = element_text(size = 15)
  ) +
  theme(legend.position = "bottom") +
  guides(
    color = guide_legend(ncol = 2, nrow = 4),     
    shape = guide_legend(ncol = 1)      
  ) 

ggsave("Analysis/final_res_scripts/final_results/plots/nonmark100_plot_bias.png", non_mark100_plot, width = 11, height = 11)

ggsave("Analysis/final_res_scripts/final_results/plots/nonmark100_plot_rmse.png", non_mark100_plot_rmse, width = 11, height = 11)


# boxplots for all scenarios N=100

load("Analysis/final_res_scripts/final_results/boxplot_all_data_100_final.rda")

boxplot_all_data$Scenario <- factor(boxplot_all_data$Scenario, levels = c("pois_1", "pois_2", "pois_3", "pois_4",
                                                                          "markov_1", "markov_2", "markov_3", "markov_4",
                                                                          "non_markov_1", "non_markov_2", "non_markov_3", "non_markov_4"))

hlines <- data.frame(Scenario = c("pois_1", "pois_2", "pois_3", "pois_4",
                                  "markov_1", "markov_2", "markov_3", "markov_4",
                                  "non_markov_1", "non_markov_2", "non_markov_3", "non_markov_4"),
                     yintercept = c(trueMeanMaster[[1]][3], trueMeanMaster[[2]][3], trueMeanMaster[[3]][3], trueMeanMaster[[4]][3],
                                    trueMeanMaster[[5]][3], trueMeanMaster[[6]][3], trueMeanMaster[[7]][3], trueMeanMaster[[8]][3],
                                    trueMeanMaster[[9]][3], trueMeanMaster[[10]][3], trueMeanMaster[[11]][3], trueMeanMaster[[12]][3]))

boxplot_all_data <- boxplot_all_data %>% 
  filter(Estimator != "s(tend, by=transition)+wait(nonmark)" & Estimator != "s(tend, by=transition)+wait") %>% 
  mutate(Estimator = factor(Estimator, levels = c(names(color_erd), c("s(tend)", "s(tend)+transition", "s(tend, by=transition)+transition")), labels = c(names(color_erd), names(color_mine))))



# pois 100



# Filter the dataset for specific scenarios
filtered_data <- boxplot_all_data %>%
  filter(grepl("pois", Scenario)) %>% 
  mutate(Scenario = factor(Scenario,
                           levels =  c("pois_1",
                                       "pois_2",
                                       "pois_3",
                                       "pois_4"),
                           labels = c("Random Censoring / No Terminating Event",
                                      "Random Censoring / Terminating Event",
                                      "State Dependent Censoring / No Terminating Event",
                                      "State Dependent Censoring / Terminating Event"))) #%>% 
  # mutate(Estimator = factor(Estimator, levels = sort(unique(Estimator)), labels = c(names(color_erd), names(color_mine))))

hlines_pois100 <- hlines %>%
  filter(grepl("pois", Scenario)) %>% 
  mutate(Scenario = factor(Scenario,
                           levels =  c("pois_1",
                                       "pois_2",
                                       "pois_3",
                                       "pois_4"),
                           labels = c("Random Censoring / No Terminating Event",
                                      "Random Censoring / Terminating Event",
                                      "State Dependent Censoring / No Terminating Event",
                                      "State Dependent Censoring / Terminating Event")))

# Create the boxplot
boxplot_pois <- ggplot(filtered_data, aes(y = X100, fill = Estimator)) +
  geom_boxplot() +
  theme_bw() +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(size = 12, face = "bold")
  ) +
  labs(
    x = "Estimators",
    y = "Number of Expected Recurrent Events at timepoint t=80"
  ) +
  cols_fill +
  facet_wrap(~Scenario) +
  theme(
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(color = "black", size = 12.5),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 15),
    axis.title = element_text(size = 15),
    legend.title = element_text(size = 15),
    axis.text.x = element_blank(),  
    #axis.ticks.x = element_line(size = 0.5, color="black")
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
  ) +
  #scale_x_discrete(expand = c(0, 0), labels = NULL) +
  theme(legend.position = "bottom") +
  guides(
    fill = guide_legend(ncol = 2)    
  ) +
  # geom_segment(y = trueMeanMaster[[1]][3], yend = trueMeanMaster[[1]][3], x = 1, xend = 7, color="black")
  geom_hline(data=hlines_pois100, aes(yintercept = yintercept), color = "black", linetype = "dashed", size = 0.6, alpha=0.7) +
  ylim(c(2, 5.5))

ggsave("Analysis/final_res_scripts/final_results/plots/boxplot_pois100.png", boxplot_pois, width = 11, height = 11)

# mark 100

# Filter the dataset for specific scenarios
filtered_data <- boxplot_all_data %>%
  filter(!grepl("pois", Scenario), !grepl("_markov", Scenario)) %>% 
  mutate(Scenario = factor(Scenario,
                           levels =  c("markov_1",
                                       "markov_2",
                                       "markov_3",
                                       "markov_4"),
                           labels = c("Random Censoring / No Terminating Event",
                                      "Random Censoring / Terminating Event",
                                      "State Dependent Censoring / No Terminating Event",
                                      "State Dependent Censoring / Terminating Event"))) #%>% 
  # mutate(Estimator = factor(Estimator, levels = sort(unique(Estimator)), labels = c(names(color_erd), names(color_mine)[-1])))

filtered_data <- filtered_data %>% 
  filter(!(Estimator %in% c("s(tend, by=transition)+wait(nonmark)",
                            "s(tend, by=transition)+wait")))

hlines_mark100 <- hlines %>%
  filter(!grepl("pois", Scenario), !grepl("_markov", Scenario)) %>% 
  mutate(Scenario = factor(Scenario,
                           levels =  c("markov_1",
                                       "markov_2",
                                       "markov_3",
                                       "markov_4"),
                           labels = c("Random Censoring / No Terminating Event",
                                      "Random Censoring / Terminating Event",
                                      "State Dependent Censoring / No Terminating Event",
                                      "State Dependent Censoring / Terminating Event")))
# Create the boxplot
boxplot_mark <- ggplot(filtered_data, aes(y = X100, fill = Estimator)) +
  geom_boxplot() +
  theme_bw() +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(size = 12, face = "bold")
  ) +
  labs(
    x = "Estimators",
    y = "Number of Expected Recurrent Events at timepoint t=80"
  ) +
  cols_fill +
  facet_wrap(~Scenario) +
  theme(
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(color = "black", size = 12.5),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 15),
    axis.title = element_text(size = 15),
    legend.title = element_text(size = 15),
    axis.text.x = element_blank(),  
    #axis.ticks.x = element_line(size = 0.5, color="black")
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
  ) +
  #scale_x_discrete(expand = c(0, 0), labels = NULL) +
  theme(legend.position = "bottom") +
  guides(
    fill = guide_legend(ncol = 2, nrow = 4)    
  ) +
  # geom_segment(y = trueMeanMaster[[1]][3], yend = trueMeanMaster[[1]][3], x = 1, xend = 7, color="black")
  geom_hline(data=hlines_mark100, aes(yintercept = yintercept), color = "black", linetype = "dashed", size = 0.6, alpha=0.7) +
  ylim(c(1, 3.5))

ggsave("Analysis/final_res_scripts/final_results/plots/boxplot_mark100.png", boxplot_mark, width = 11, height = 11) 

# non mark 100

# Filter the dataset for specific scenarios
filtered_data <- boxplot_all_data %>%
  filter(grepl("non_markov", Scenario)) %>% 
  mutate(Scenario = factor(Scenario,
                           levels =  c("non_markov_1",
                                       "non_markov_2",
                                       "non_markov_3",
                                       "non_markov_4"),
                           labels = c("Random Censoring / No Terminating Event",
                                      "Random Censoring / Terminating Event",
                                      "State Dependent Censoring / No Terminating Event",
                                      "State Dependent Censoring / Terminating Event"))) #%>% 
  # mutate(Estimator = factor(Estimator, levels = sort(unique(Estimator)), labels = c(names(color_erd), names(color_mine)[-1])))

filtered_data <- filtered_data %>% 
  filter(!(Estimator %in% c("s(tend, by=transition)+wait(nonmark)",
                            "s(tend, by=transition)+wait")))

hlines_non_mark100 <- hlines %>%
  filter(grepl("non_markov", Scenario)) %>% 
  mutate(Scenario = factor(Scenario,
                           levels =  c("non_markov_1",
                                       "non_markov_2",
                                       "non_markov_3",
                                       "non_markov_4"),
                           labels = c("Random Censoring / No Terminating Event",
                                      "Random Censoring / Terminating Event",
                                      "State Dependent Censoring / No Terminating Event",
                                      "State Dependent Censoring / Terminating Event")))



# Create the boxplot
boxplot_non_mark <- ggplot(filtered_data, aes(y = X100, fill = Estimator)) +
  geom_boxplot() +
  theme_bw() +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(size = 12, face = "bold")
  ) +
  labs(
    x = "Estimators",
    y = "Number of Expected Recurrent Events at timepoint t=80"
  ) +
  cols_fill +
  facet_wrap(~Scenario) +
  theme(
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(color = "black", size = 12.5),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 15),
    axis.title = element_text(size = 15),
    legend.title = element_text(size = 15),
    axis.text.x = element_blank(),  
    #axis.ticks.x = element_line(size = 0.5, color="black")
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
  ) +
  #scale_x_discrete(expand = c(0, 0), labels = NULL) +
  theme(legend.position = "bottom") +
  guides(
    fill = guide_legend(ncol = 2, nrow = 4)    
  ) +
  # geom_segment(y = trueMeanMaster[[1]][3], yend = trueMeanMaster[[1]][3], x = 1, xend = 7, color="black")
  geom_hline(data=hlines_non_mark100, aes(yintercept = yintercept), color = "black", linetype = "dashed", size = 0.6, alpha=0.7)

ggsave("Analysis/final_res_scripts/final_results/plots/boxplot_non_mark100.png", boxplot_non_mark, width = 11, height = 11)

# N=200 no term all scenarios

load("Analysis/final_res_scripts/final_results/poisson200_plot_df.rda")
load("Analysis/final_res_scripts/final_results/plot_markov200.rda")
load("Analysis/final_res_scripts/final_results/plot_non_markov200.rda")

pois_200_plot <- pois_200_plot %>% 
  mutate(scenario = paste0("pois_", scenario)) %>% 
  rename(Estimator = method,
         Source = from) %>% 
  mutate(Source = ifelse(Source == "new", "PAMM", "non-parametric")) %>% 
  mutate(Estimator = ifelse(Estimator == "spline_by=transition_wait",
                            "s(tend, by=transition)+wait",
                            ifelse(Estimator == "spline_by=transition_wait_nonmarkov",
                                   "s(tend, by=transition)+wait(nonmark)",
                                   ifelse(Estimator == "spline_by=transition",
                                          "s(tend, by=transition)+transition",
                                          ifelse(Estimator == "normal_spline",
                                                 "s(tend)+transition",
                                                 ifelse(Estimator == "normal_spline_notrans",
                                                        "s(tend)",
                                                        Estimator)))))) %>% 
  filter(!(Estimator %in% c("te(tend, wait)+transition",
                            "tensor_spline",
                            "s(tend, by=transition)+wait",
                            "s(tend, by=transition)+wait(nonmark)"))) #%>% 
  # mutate(Estimator = factor(Estimator, levels = c(names(color_erd), c("s(tend)", "s(tend)+transition", "s(tend, by=transition)+transition")), labels = c(names(color_erd), names(color_mine))))

plot_markov200 <- plot_markov200 %>%
  mutate(scenario = paste0("markov_", scenario)) %>% 
  filter(!(Estimator %in% c("te(tend, wait)+transition",
                            "tensor_spline",
                            "s(tend, by=transition)+wait",
                            "s(tend, by=transition)+wait(nonmark)")))# %>%
  # mutate(Estimator = factor(Estimator, levels = c(names(color_erd), c("s(tend)", "s(tend)+transition", "s(tend, by=transition)+transition")), labels = c(names(color_erd), names(color_mine))))

plot_non_markov200 <- plot_non_markov200 %>%
  mutate(scenario = paste0("non_markov_", scenario)) %>% 
  # rename(Estimator = method,
  #        Source = from) %>% 
  # mutate(Source = ifelse(Source == "new", "PAMM", "non-parametric"))%>% 
  mutate(Estimator = ifelse(Estimator == "spline_by=transition_wait",
                            "s(tend, by=transition)+wait",
                            ifelse(Estimator == "spline_by=transition_wait_nonmarkov",
                                   "s(tend, by=transition)+wait(nonmark)",
                                   ifelse(Estimator == "spline_by=transition",
                                          "s(tend, by=transition)+transition",
                                          ifelse(Estimator == "normal_spline",
                                                 "s(tend)+transition",
                                                 ifelse(Estimator == "normal_spline_notrans",
                                                        "s(tend)",
                                                        Estimator)))))) %>% 
  filter(!(Estimator %in% c("te(tend, wait)+transition",
                            "tensor_spline",
                            "s(tend, by=transition)+wait",
                            "s(tend, by=transition)+wait(nonmark)")))# %>%
  # mutate(Estimator = factor(Estimator, levels = c(names(color_erd), c("s(tend)", "s(tend)+transition", "s(tend, by=transition)+transition")), labels = c(names(color_erd), names(color_mine))))

all_bias_in_on <- rbind(pois_200_plot, plot_markov200, plot_non_markov200)

all_bias_in_on <- all_bias_in_on %>% 
  mutate(Estimator = factor(Estimator, levels = c(names(color_erd), c("s(tend)", "s(tend)+transition", "s(tend, by=transition)+transition")), labels = c(names(color_erd), names(color_mine)))) %>% 
  mutate(names = as.factor(ifelse(grepl("pois", scenario),
                                  "Poisson Setting",
                                  ifelse(grepl("non_markov", scenario),
                                         "Non-Markov Setting",
                                         "Markov Setting")))) %>% 
  mutate(scenario = ifelse(grepl("rand_noterm", scenario),
                           "Random Censoring \n No Terminating Event",
                           ifelse(grepl("rand_term", scenario),
                                  "Random Censoring \n Terminating Event",
                                  ifelse(grepl("state_noterm", scenario),
                                         "State Dependent Censoring \n No Terminating Event",
                                         "State Dependent Censoring \n Terminating Event")))) %>% 
  filter(!(Estimator == "Smooth Effect on Time" & scenario %in% c("Random Censoring \n Terminating Event", "State Dependent Censoring \n Terminating Event")))

all_bias_in_on$names <- factor(all_bias_in_on$names, levels = c("Poisson Setting", "Markov Setting", "Non-Markov Setting"))

bias_200_noterm_plot <- ggplot(all_bias_in_on %>% filter(scenario %in% c("Random Censoring \n No Terminating Event",
                                                                         "State Dependent Censoring \n No Terminating Event"))
                               ,
                               aes(x = t,
                                   y = bias,
                                   color = Estimator,
                                   shape = Source)) +
  geom_jitter(size=2, alpha=2, width=1, height = 0) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw() +
  facet_grid(~names+scenario,
             switch = "x",
             scales = "free_y"
  ) +
  #ylim(c(-30,  20)) +
  theme(legend.position = "bottom") +
  # theme(axis.title.x = element_blank()) +
  cols +
  theme(
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(color = "black", size = 9),
    axis.text = element_text(size = 9),
    legend.text = element_text(size = 12.5),
    axis.title = element_text(size = 12.5),
    legend.title = element_text(size = 12.5)
  ) +
  labs(y = "relative Bias (%)",
       x = "Timepoints t") +
  guides(
    color = guide_legend(ncol = 2, nrow=4),     
    shape = guide_legend(ncol = 1)
  )

ggsave("Analysis/final_res_scripts/final_results/plots/bias_200_noterm_plot.png", bias_200_noterm_plot, width = 11, height = 7)

bias_200_term_plot <- ggplot(all_bias_in_on %>% filter(scenario %in% c("Random Censoring \n Terminating Event",
                                                                       "State Dependent Censoring \n Terminating Event"))
                             ,
                             aes(x = t,
                                 y = bias,
                                 color = Estimator,
                                 shape = Source)) +
  geom_jitter(size=2, alpha=2, width=1, height = 0) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw() +
  facet_grid(~names+scenario,
             switch = "x",
             scales = "free_y"
  ) +
  #ylim(c(-30,  20)) +
  theme(legend.position = "bottom") +
  # theme(axis.title.x = element_blank()) +
  cols +
  theme(
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(color = "black", size = 9),
    axis.text = element_text(size = 9),
    legend.text = element_text(size = 12.5),
    axis.title = element_text(size = 12.5),
    legend.title = element_text(size = 12.5)
  ) +
  labs(y = "relative Bias (%)",
       x = "Timepoints t") +
  guides(
    color = guide_legend(ncol = 2, nrow=4),     
    shape = guide_legend(ncol = 1)
  )

ggsave("Analysis/final_res_scripts/final_results/plots/bias_200_term_plot.png", bias_200_term_plot, width = 11, height = 7)

# boxplot N=200 PAMM estimators over time

load("Analysis/final_res_scripts/final_results/boxplot_200_all_t.rda")

df_200_boxplot_all_t <- df

load("Analysis/final_res_scripts/final_results/boxplot_200_all_term_t.rda")

df_200_boxplot_all_t <- rbind(df_200_boxplot_all_t, df)

# pivot longer df_200_boxplot_all_t so that all coloumns with x are one
df_200_boxplot_all_t <- df_200_boxplot_all_t %>%
  pivot_longer(cols = c("X40", "X60", "X80", "X100"),
               names_to = "t",
               values_to = "value") 

df_200_boxplot_all_tt <- df_200_boxplot_all_t %>% 
  mutate(t = factor(t,
                    levels = c("X40", "X60", "X80", "X100"),
                    labels = c("40", "60", "80", "100"))) %>% 
  rename(scenario=Scenario) %>% 
  mutate(names = factor(ifelse(grepl("pois", scenario),
                               "Poisson Setting",
                               ifelse(grepl("non_markov", scenario),
                                      "Non-Markov Setting",
                                      "Markov Setting")))) %>% 
  mutate(scenario = ifelse(grepl("1", scenario),
                           "Random Censoring \n No Terminating Event",
                           ifelse(grepl("2", scenario),
                                  "Random Censoring \n Terminating Event",
                                  ifelse(grepl("3", scenario),
                                         "State Dependent Censoring \n No Terminating Event",
                                         "State Dependent Censoring \n Terminating Event")))) %>% 
  mutate(names = factor(names, levels = c("Poisson Setting", "Markov Setting", "Non-Markov Setting"))) %>% 
  mutate(Estimator = factor(Estimator, levels = c(names(color_erd), c("s(tend)", "s(tend)+transition", "s(tend, by=transition)+transition")), labels = c(names(color_erd), names(color_mine)))) %>% 
  filter(!(Estimator == "Smooth Effect on Time" & scenario %in% c("Random Censoring \n Terminating Event", "State Dependent Censoring \n Terminating Event")))

hlines2 <- data.frame(names = as.factor(rep(c("Poisson Setting", "Markov Setting", "Non-Markov Setting"), each = 16)),
                      scenario = rep(c("Random Censoring \n No Terminating Event", "Random Censoring \n Terminating Event",
                                       "State Dependent Censoring \n No Terminating Event", "State Dependent Censoring \n Terminating Event"), each = 4),
                      yintercept = as.numeric(unlist(trueMeanMaster)))

boxplot_over_time_200 <- ggplot(df_200_boxplot_all_tt,
                                aes(fill = Estimator,
                                    x = t,
                                    y = value)) +
  geom_boxplot() +
  facet_wrap(~names+scenario, scales = "free") +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(color = "black", size = 9),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 15),
    axis.title = element_text(size = 15),
    legend.title = element_text(size = 15),
    #axis.text.x = element_blank(),  
    #axis.ticks.x = element_line(size = 0.5, color="black")
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
  ) +
  #scale_x_discrete(expand = c(0, 0), labels = NULL) +
  theme(legend.position = "bottom") +
  guides(
    fill = guide_legend(ncol=1)    
  ) +
  cols_fill +
  geom_hline(data=hlines2, aes(yintercept = yintercept), color = "black", linetype = "dashed", size = 0.6, alpha=0.7) +
  labs(x = "Time", y = "Number of expected recurrent Events") 

ggsave("Analysis/final_res_scripts/final_results/plots/boxplot_over_time_200.png", plot = boxplot_over_time_200, width = 11, height = 15)



# bias plots for N=200

pois200_plot <- ggplot(all_bias_in_on %>% filter(names == "Poisson Setting"), #pois_200_plot,
                       aes(x = t,
                           y = bias,
                           color = Estimator,
                           shape = Source)) +
  geom_jitter(size=3, alpha=1, width=1, height = 0) +
  # geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  # theme_bw() +
  ylim(c(-2, 2)) +
  cols +
  facet_wrap(~scenario,
             labeller = labeller(scenario = c("pois_rand_noterm"="Random Censoring / No Terminating Event",
                                              "pois_rand_term"="Random Censoring / Terminating Event",
                                              "pois_state_noterm"="State Dependent Censoring / No Terminating Event",
                                              "pois_state_term"="State Dependent Censoring / Terminating Event"))) +
  theme_bw() +
  labs(x="Time", y="relative Bias (%)") +
  theme(
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(color = "black", size = 14),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 15),
    axis.title = element_text(size = 15),
    legend.title = element_text(size = 15)
  ) +
  theme(legend.position = "bottom") +
  guides(
    color = guide_legend(ncol = 2, nrow=4),     
    shape = guide_legend(ncol = 1)      
  ) 

mark200_plot <- ggplot(all_bias_in_on %>% filter(names == "Markov Setting"),#plot_markov200,
                       aes(x = t,
                           y = bias,
                           color = Estimator,
                           shape = Source)) +
  geom_jitter(size=3, alpha=1, width=1, height = 0) +
  # geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  # theme_bw() +
  # ylim(c(-2, 2)) +
  cols +
  facet_wrap(~scenario,
             labeller = labeller(scenario = c("markov_rand_noterm"="Random Censoring / No Terminating Event",
                                              "markov_rand_term"="Random Censoring / Terminating Event",
                                              "markov_state_noterm"="State Dependent Censoring / No Terminating Event",
                                              "markov_state_term"="State Dependent Censoring / Terminating Event")),
             scales = "free_y") +
  theme_bw() +
  labs(x="Time", y="relative Bias (%)") +
  theme(
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(color = "black", size = 14),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 15),
    axis.title = element_text(size = 15),
    legend.title = element_text(size = 15)
  ) +
  theme(legend.position = "bottom") +
  guides(
    color = guide_legend(ncol = 2, nrow=4),     
    shape = guide_legend(ncol = 1)      
  ) 

nonmark200_plot <- ggplot(all_bias_in_on %>% filter(names == "Non-Markov Setting"),#plot_non_markov200,
                          aes(x = t,
                              y = bias,
                              color = Estimator,
                              shape = Source)) +
  geom_jitter(size=3, alpha=1, width=1, height = 0) +
  # geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  # theme_bw() +
  # ylim(c(-2, 2)) +
  cols +
  facet_wrap(~scenario,
             labeller = labeller(scenario = c("non_markov_rand_noterm"="Random Censoring / No Terminating Event",
                                              "non_markov_rand_term"="Random Censoring / Terminating Event",
                                              "non_markov_state_noterm"="State Dependent Censoring / No Terminating Event",
                                              "non_markov_state_term"="State Dependent Censoring / Terminating Event")),
             scales = "free_y") +
  theme_bw() +
  labs(x="Time", y="relative Bias (%)") +
  theme(
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(color = "black", size = 14),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 15),
    axis.title = element_text(size = 15),
    legend.title = element_text(size = 15)
  ) +
  theme(legend.position = "bottom") +
  guides(
    color = guide_legend(ncol = 2, nrow=4),     
    shape = guide_legend(ncol = 1)      
  ) 


ggsave("Analysis/final_res_scripts/final_results/plots/pois200_plot_bias.png", pois200_plot, width = 11, height = 11)
ggsave("Analysis/final_res_scripts/final_results/plots/mark200_plot_bias.png", mark200_plot, width = 11, height = 11)
ggsave("Analysis/final_res_scripts/final_results/plots/nonmark200_plot_bias.png", nonmark200_plot, width = 11, height = 11)

# rmse for n=200

pois200_plot_rmse <- ggplot(all_bias_in_on %>% filter(names == "Poisson Setting"), #pois_200_plot,
                            aes(x = t,
                                y = rmse,
                                color = Estimator,
                                shape = Source)) +
  geom_jitter(size=3, alpha=1, width=1, height = 0) +
  # geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  # theme_bw() +
  ylim(c(0, 1)) +
  cols +
  facet_wrap(~scenario,
             labeller = labeller(scenario = c("pois_rand_noterm"="Random Censoring / No Terminating Event",
                                              "pois_rand_term"="Random Censoring / Terminating Event",
                                              "pois_state_noterm"="State Dependent Censoring / No Terminating Event",
                                              "pois_state_term"="State Dependent Censoring / Terminating Event"))) +
  theme_bw() +
  labs(x="Time", y="RMSE") +
  theme(
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(color = "black", size = 14),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 15),
    axis.title = element_text(size = 15),
    legend.title = element_text(size = 15)
  ) +
  theme(legend.position = "bottom") +
  guides(
    color = guide_legend(ncol = 2, nrow=4),     
    shape = guide_legend(ncol = 1)      
  ) 

mark200_plot_rmse <- ggplot(all_bias_in_on %>% filter(names == "Markov Setting"),#plot_markov200,
                            aes(x = t,
                                y = rmse,
                                color = Estimator,
                                shape = Source)) +
  geom_jitter(size=3, alpha=1, width=1, height = 0) +
  # geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  # theme_bw() +
  # ylim(c(-2, 2)) +
  cols +
  facet_wrap(~scenario,
             labeller = labeller(scenario = c("markov_rand_noterm"="Random Censoring / No Terminating Event",
                                              "markov_rand_term"="Random Censoring / Terminating Event",
                                              "markov_state_noterm"="State Dependent Censoring / No Terminating Event",
                                              "markov_state_term"="State Dependent Censoring / Terminating Event")),
             scales = "free_y") +
  theme_bw() +
  labs(x="Time", y="RMSE") +
  theme(
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(color = "black", size = 14),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 15),
    axis.title = element_text(size = 15),
    legend.title = element_text(size = 15)
  ) +
  theme(legend.position = "bottom") +
  guides(
    color = guide_legend(ncol = 2, nrow=4),     
    shape = guide_legend(ncol = 1)      
  ) 

nonmark200_plot_rmse <- ggplot(all_bias_in_on %>% filter(names == "Non-Markov Setting"),#plot_non_markov200,
                               aes(x = t,
                                   y = rmse,
                                   color = Estimator,
                                   shape = Source)) +
  geom_jitter(size=3, alpha=1, width=1, height = 0) +
  # geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  # theme_bw() +
  # ylim(c(-2, 2)) +
  cols +
  facet_wrap(~scenario,
             labeller = labeller(scenario = c("non_markov_rand_noterm"="Random Censoring / No Terminating Event",
                                              "non_markov_rand_term"="Random Censoring / Terminating Event",
                                              "non_markov_state_noterm"="State Dependent Censoring / No Terminating Event",
                                              "non_markov_state_term"="State Dependent Censoring / Terminating Event")),
             scales = "free_y") +
  theme_bw() +
  labs(x="Time", y="RMSE") +
  theme(
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(color = "black", size = 14),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 15),
    axis.title = element_text(size = 15),
    legend.title = element_text(size = 15)
  ) +
  theme(legend.position = "bottom") +
  guides(
    color = guide_legend(ncol = 2, nrow=4),     
    shape = guide_legend(ncol = 1)      
  ) 


ggsave("Analysis/final_res_scripts/final_results/plots/pois200_plot_rmse.png", pois200_plot_rmse, width = 11, height = 11)
ggsave("Analysis/final_res_scripts/final_results/plots/mark200_plot_rmse.png", mark200_plot_rmse, width = 11, height = 11)
ggsave("Analysis/final_res_scripts/final_results/plots/nonmark200_plot_rmse.png", nonmark200_plot_rmse, width = 11, height = 11)



# boxplot vgl N=100 vs N=200

load("Analysis/final_res_scripts/final_results/boxplot_all_data_100_final.rda")
load("Analysis/final_res_scripts/final_results/boxplot_all_200_final.rda")

vgl_boxplot_N <- rbind(boxplot_all_data %>% # filter(Source=="New") %>%
                         filter(!(Estimator %in% c("s(tend, by=transition)+wait", "s(tend, by=transition)+wait(nonmark)"))) %>% 
                         mutate(N="100"),
                       df_200_boxplot_all %>% mutate(N="200")) %>% 
  mutate("Size of the Dataset" = as.factor(N)) %>%
  rename(scenario = Scenario) %>% 
  mutate(names = factor(ifelse(grepl("pois", scenario),
                               "Poisson Setting",
                               ifelse(grepl("non_markov", scenario),
                                      "Non-Markov Setting",
                                      "Markov Setting")))) %>% 
  mutate(scenario = ifelse(grepl("1", scenario),
                           "Random Censoring \n No Terminating Event",
                           ifelse(grepl("2", scenario),
                                  "Random Censoring \n Terminating Event",
                                  ifelse(grepl("3", scenario),
                                         "State Dependent Censoring \n No Terminating Event",
                                         "State Dependent Censoring \n Terminating Event")))) %>% 
  mutate(Estimator = factor(Estimator, levels = sort(unique(Estimator)), labels = c(names(color_erd), names(color_mine))))

# levels(vgl_boxplot_N$names) <- c("Poisson Setting", "Markov Setting", "Non-Markov Setting")
vgl_boxplot_N <- vgl_boxplot_N %>% 
  mutate(names = factor(names, levels = c("Poisson Setting", "Markov Setting", "Non-Markov Setting")))

hlines <- data.frame(Scenario = c("pois_1", "pois_2", "pois_3", "pois_4",
                                  "markov_1", "markov_2", "markov_3", "markov_4",
                                  "non_markov_1", "non_markov_2", "non_markov_3", "non_markov_4"),
                     yintercept = c(trueMeanMaster[[1]][3], trueMeanMaster[[2]][3], trueMeanMaster[[3]][3], trueMeanMaster[[4]][3],
                                    trueMeanMaster[[5]][3], trueMeanMaster[[6]][3], trueMeanMaster[[7]][3], trueMeanMaster[[8]][3],
                                    trueMeanMaster[[9]][3], trueMeanMaster[[10]][3], trueMeanMaster[[11]][3], trueMeanMaster[[12]][3]))


hlines2 <- data.frame(names = as.factor(rep(c("Poisson Setting", "Markov Setting", "Non-Markov Setting"), each = 4)),
                      scenario = c("Random Censoring \n No Terminating Event", "Random Censoring \n Terminating Event",
                                   "State Dependent Censoring \n No Terminating Event", "State Dependent Censoring \n Terminating Event"),
                      yintercept = as.numeric(hlines$yintercept))


box_vgl_Ns <- ggplot(vgl_boxplot_N, 
                     aes(y = X100,
                         fill = Estimator,
                         color = `Size of the Dataset`,
                         x = Estimator)) +
  geom_boxplot() +
  facet_wrap(~ names + scenario,
             scales = "free_y",
             nrow = 4,
             dir = "v") +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(color = "black", size = 12),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 14),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 14),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    #axis.ticks.x = element_line(size = 0.5, color="black")
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
  ) +
  #scale_x_discrete(expand = c(0, 0), labels = NULL) +
  theme(legend.position = "bottom") +
  guides(
    fill = guide_legend(ncol=2), 
    color = guide_legend(ncol=1)
  ) +
  cols_fill +
  labs(y = "Number of Expected Recurrent Events at t=80") +
  geom_hline(data=hlines2, aes(yintercept = yintercept), color = "black", linetype = "dashed", size = 0.6, alpha=0.7) +
  scale_color_manual(values = c("100" = "black", "200" = "red"))


ggsave("Analysis/final_res_scripts/final_results/plots/boxplot_vgl_Ns.png", box_vgl_Ns, width = 11, height = 15) 


