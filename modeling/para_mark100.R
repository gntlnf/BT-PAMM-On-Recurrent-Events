library(doParallel)
library(foreach)

num_cores <- detectCores() - 1 
cl <- makeCluster(num_cores)
registerDoParallel(cl)

clusterEvalQ(cl, {
  source("initial_run.R") 
  source("skip_data_generation.R")
  setwd("pammtools-multi-state")
  devtools::load_all()
  setwd("..")
})

run_parallel <- function(data, formula = NULL, competing = FALSE, cutoff = FALSE, wait = FALSE, non_markov = FALSE) {
  foreach(i = seq_along(data), .combine = 'c') %dopar% {
    mu(data[[i]], form = formula, competing = competing, cutoff = cutoff, wait = wait, non_markov = non_markov)
  }
}

# Scenario 1: rand_noterm
data_mark_1_100 <- data_mark_1_100
normal1 <- run_parallel(data_mark_1_100, "ped_status ~ s(tend) + transition")
cutoff1 <- run_parallel(data_mark_1_100, "ped_status ~ s(tend, by = transition) + transition", cutoff = TRUE)

# Scenario 2: rand_term
data_mark_2_100 <- data_mark_2_100[-c(21)]
normal2 <- run_parallel(data_mark_2_100, "ped_status ~ s(tend) + transition", competing = TRUE)
cutoff2 <- run_parallel(data_mark_2_100, "ped_status ~ s(tend, by = transition) + transition", cutoff = TRUE, competing = TRUE)

# Scenario 3: state_noterm
data_mark_3_100 <- data_mark_3_100
normal3 <- run_parallel(data_mark_3_100, "ped_status ~ s(tend) + transition")
cutoff3 <- run_parallel(data_mark_3_100, "ped_status ~ s(tend, by = transition) + transition", cutoff = TRUE)

# Scenario 4: state_term
data_mark_4_100 <- data_mark_4_100[-c(6, 10, 65, 94)]
normal4 <- run_parallel(data_mark_4_100, "ped_status ~ s(tend) + transition", competing = TRUE)
cutoff4 <- run_parallel(data_mark_4_100, "ped_status ~ s(tend, by = transition) + transition", cutoff = TRUE, competing = TRUE)

save(normal1, cutoff1,   
     normal2, cutoff2,  
     normal3, cutoff3,  
     normal4, cutoff4,  
     file = "Analysis/final_res_scripts/final_results/mark_final_results.RData")
print("Markov 100 done")

stopCluster(cl)
