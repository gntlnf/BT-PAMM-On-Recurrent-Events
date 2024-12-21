# load pammtools to create ped tables
setwd("pammtools-multi-state")
devtools::load_all()
setwd("..")

data <- data_pois_1_100[[1]]

data <- data %>% 
  mutate(entry = round(entry, 3),
         exit = round(exit, 3))


library(xtable)

data <- data %>%
  filter(id %in% c(3, 5, 10))

data$id <- c(1,2,2,3,3,3)

print(xtable(data, caption = "Your Table Caption Here"), 
      include.rownames = FALSE, 
      tabular.environment = "tabular", 
      booktabs = TRUE)


for_ped <- data %>%
  mutate(status = ifelse(to != "cens", 1, 0),     
         to = ifelse(to == "cens", from + 1, to),
         transition = as.factor(paste0(from, "->", to))
  ) 

ped <- as_ped(data = for_ped,
              formula = Surv(entry, exit, status) ~ 1,
              id = "id",
              transition = "transition",
              timescale = "calendar",
)

ped$offset <- round(ped$offset, 3)

print(xtable(ped, caption = "Your Table Caption Here"), 
      include.rownames = FALSE, 
      tabular.environment = "tabular", 
      booktabs = TRUE)


data <- data_pois_2_100[[1]]

data <- data %>% # take 1, 3
  filter(id %in% c(1, 3))

data$id <- rep(1:2, each = 4)

data$entry <- round(data$entry, 3)
data$exit <- round(data$exit, 3)

print(xtable(data, caption = "Your Table Caption Here"), 
      include.rownames = FALSE, 
      tabular.environment = "tabular", 
      booktabs = TRUE)


data <- data %>%
  mutate(status = ifelse(to != "cens", 1, 0),     
         to = ifelse(to == "cens", from + 1, to),
         to = ifelse(!is.na(as.numeric(to)), 
                     as.numeric(gsub("death_", "", to))*2, 
                     ifelse(to == "cens",
                            to,
                            as.numeric(gsub("death_", "", to)) * 2 + 1)),
         from = from * 2,
         transition = as.factor(paste0(from, "->", to)))


listi <- sapply(seq(0, max(as.numeric(data$to) - 2, na.rm = TRUE), by = 2), function(x) {
  list(c(x + 1, x + 2))
})

if(max(as.numeric(data$to), na.rm = TRUE)%%2!=0) listi <- append(listi, list(c(max(as.numeric(data$to)+1))))

for_ped <- add_counterfactual_transitions(data %>%
                                            rename(tstart=entry,
                                                   tstop=exit
                                            ), 
                                          from_col = "from",
                                          to_col = "to",
                                          transition_col = "transition",
                                          from_to_pairs = listi
) %>%
  rename(entry = tstart,
         exit = tstop
  )


ped <- as_ped(data = for_ped,
              formula = Surv(entry, exit, status) ~ .,
              id = "id",
              transition = "transition",
              timescale = "calendar"
              , cut = c(unique(for_ped$exit), c(40, 60, 80, 100))
)

ped$offset <- round(ped$offset, 3)


print(xtable(ped, caption = "Your Table Caption Here"), 
      include.rownames = FALSE, 
      tabular.environment = "tabular", 
      booktabs = TRUE)

# create result tables here

load("Analysis/final_res_scripts/final_results/pois_100_plot.rda")




  color_erd <- c("NA"="#FF6666", "EB1"="#FF0000", "EB2"="#990000", "AJ"="#660000")
# color_mine <- c("Smooth Effect on Time"="#66B2FF", "Smooth Effect on Time + Intercept Change on Transition"="#3399FF", "Smooth Effect on Time dependent on Transition"="#0066FF", "s(tend, by=transition)+wait"="#003399", "s(tend, by=transition)+wait(nonmark)"="#001F5B")
color_mine <- c("Smooth Effect on Time"="#66B2FF", "Smooth Effect on Time + Intercept Change on Transition"="#0066FF", "Smooth Effect on Time dependent on Transition"="#003399") #, "s(tend, by=transition)+wait"="#003399", "s(tend, by=transition)+wait(nonmark)"="#001F5B")
colors <- c(color_erd, color_mine)
cols <- scale_color_manual(values = colors)
cols_fill <- scale_fill_manual(values = colors)
load("Analysis/final_res_scripts/final_results/pois_100_plot.rda")
load("Analysis/final_res_scripts/final_results/plot_markov100.rda")
load("Analysis/final_res_scripts/final_results/plot_non_markov100.rda")

pois_100_plot <- pois_100_plot %>% 
  mutate(scenario = paste0("pois_", scenario)) %>% 
  # rename(Estimator = method,
  #        Source = from) %>% 
  # mutate(Source = ifelse(Source == "new", "PAMM", "non-parametric")) %>% 
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

plot_markov100 <- plot_markov100 %>%
  mutate(scenario = paste0("markov_", scenario)) %>% 
  filter(!(Estimator %in% c("te(tend, wait)+transition",
                            "tensor_spline",
                            "s(tend, by=transition)+wait",
                            "s(tend, by=transition)+wait(nonmark)")))# %>%
# mutate(Estimator = factor(Estimator, levels = c(names(color_erd), c("s(tend)", "s(tend)+transition", "s(tend, by=transition)+transition")), labels = c(names(color_erd), names(color_mine))))

plot_non_markov100 <- plot_non_markov100 %>%
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
                            "s(tend, by=transition)+wait(nonmark)"))) #%>%
# mutate(Estimator = factor(Estimator, levels = c(names(color_erd), c("s(tend)", "s(tend)+transition", "s(tend, by=transition)+transition")), labels = c(names(color_erd), names(color_mine))))

all_bias_in_on <- rbind(pois_100_plot, plot_markov100, plot_non_markov100)

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

for_table <- all_bias_in_on %>% 
  select(bias, rmse, t, scenario, Estimator, Source, names) %>% 
  pivot_wider(names_from = t, values_from = c(bias, rmse)) 




library(xtable)




pois <- for_table %>% 
  filter(names == "Poisson Setting") %>% 
  select(Estimator, scenario, bias_40, bias_60, bias_80, bias_100, rmse_40, rmse_60, rmse_80, rmse_100) %>% 
  mutate(Estimator = ifelse(Estimator == "Smooth Effect on Time",
                            "Simple",
                            ifelse(Estimator == "Smooth Effect on Time + Intercept Change on Transition",
                                   "Intercept",
                                   ifelse(Estimator == "Smooth Effect on Time dependent on Transition",
                                          "Complex",
                                          Estimator))))



for(i in unique(pois$scenario)) {
print(xtable(pois %>% filter(scenario==i) %>% select(-scenario), caption = cat("Poisson Setting: ", i), 
      include.rownames = FALSE, 
      tabular.environment = "tabular", 
      booktabs = TRUE))
}


# mark 

mark <- for_table %>% 
  filter(names == "Markov Setting") %>% 
  select(Estimator, scenario, bias_40, bias_60, bias_80, bias_100, rmse_40, rmse_60, rmse_80, rmse_100) %>% 
  mutate(Estimator = ifelse(Estimator == "Smooth Effect on Time",
                            "Simple",
                            ifelse(Estimator == "Smooth Effect on Time + Intercept Change on Transition",
                                   "Intercept",
                                   ifelse(Estimator == "Smooth Effect on Time dependent on Transition",
                                          "Complex",
                                          Estimator))))



for(i in unique(mark$scenario)) {
  print(xtable(mark %>% filter(scenario==i) %>% select(-scenario), caption = cat("Markov Setting: ", i), 
               include.rownames = FALSE, 
               tabular.environment = "tabular", 
               booktabs = TRUE))
}

# non mark 

nonmark <- for_table %>% 
  filter(names == "Non-Markov Setting") %>% 
  select(Estimator, scenario, bias_40, bias_60, bias_80, bias_100, rmse_40, rmse_60, rmse_80, rmse_100) %>% 
  mutate(Estimator = ifelse(Estimator == "Smooth Effect on Time",
                            "Simple",
                            ifelse(Estimator == "Smooth Effect on Time + Intercept Change on Transition",
                                   "Intercept",
                                   ifelse(Estimator == "Smooth Effect on Time dependent on Transition",
                                          "Complex",
                                          Estimator))))



for(i in unique(nonmark$scenario)) {
  print(xtable(nonmark %>% filter(scenario==i) %>% select(-scenario), caption = cat("Non-Markov Setting: ", i), 
               include.rownames = FALSE, 
               tabular.environment = "tabular", 
               booktabs = TRUE))
}






# N=200

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


for_table <- all_bias_in_on %>% 
  select(bias, rmse, t, scenario, Estimator, Source, names) %>% 
  pivot_wider(names_from = t, values_from = c(bias, rmse)) 




library(xtable)




pois <- for_table %>% 
  filter(names == "Poisson Setting") %>% 
  select(Estimator, scenario, bias_40, bias_60, bias_80, bias_100, rmse_40, rmse_60, rmse_80, rmse_100) %>% 
  mutate(Estimator = ifelse(Estimator == "Smooth Effect on Time",
                            "Simple",
                            ifelse(Estimator == "Smooth Effect on Time + Intercept Change on Transition",
                                   "Intercept",
                                   ifelse(Estimator == "Smooth Effect on Time dependent on Transition",
                                          "Complex",
                                          Estimator))))



for(i in unique(pois$scenario)) {
  print(xtable(pois %>% filter(scenario==i) %>% select(-scenario), caption = cat("Poisson Setting: ", i), 
               include.rownames = FALSE, 
               tabular.environment = "tabular", 
               booktabs = TRUE))
}


# mark 

mark <- for_table %>% 
  filter(names == "Markov Setting") %>% 
  select(Estimator, scenario, bias_40, bias_60, bias_80, bias_100, rmse_40, rmse_60, rmse_80, rmse_100) %>% 
  mutate(Estimator = ifelse(Estimator == "Smooth Effect on Time",
                            "Simple",
                            ifelse(Estimator == "Smooth Effect on Time + Intercept Change on Transition",
                                   "Intercept",
                                   ifelse(Estimator == "Smooth Effect on Time dependent on Transition",
                                          "Complex",
                                          Estimator))))



for(i in unique(mark$scenario)) {
  print(xtable(mark %>% filter(scenario==i) %>% select(-scenario), caption = cat("Markov Setting: ", i), 
               include.rownames = FALSE, 
               tabular.environment = "tabular", 
               booktabs = TRUE))
}

# non mark 

nonmark <- for_table %>% 
  filter(names == "Non-Markov Setting") %>% 
  select(Estimator, scenario, bias_40, bias_60, bias_80, bias_100, rmse_40, rmse_60, rmse_80, rmse_100) %>% 
  mutate(Estimator = ifelse(Estimator == "Smooth Effect on Time",
                            "Simple",
                            ifelse(Estimator == "Smooth Effect on Time + Intercept Change on Transition",
                                   "Intercept",
                                   ifelse(Estimator == "Smooth Effect on Time dependent on Transition",
                                          "Complex",
                                          Estimator))))



for(i in unique(nonmark$scenario)) {
  print(xtable(nonmark %>% filter(scenario==i) %>% select(-scenario), caption = cat("Non-Markov Setting: ", i), 
               include.rownames = FALSE, 
               tabular.environment = "tabular", 
               booktabs = TRUE))
}


