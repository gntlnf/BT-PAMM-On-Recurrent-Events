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

run_parallel <- function(data, formula = NULL, competing = FALSE, cutoff = FALSE, wait = FALSE) {
  foreach(i = seq_along(data), .combine = 'c') %dopar% {
    mu(data[[i]], form = formula, competing = competing, cutoff = cutoff, wait = wait)
  }
}

# Scenario 1
data <- data_mark_1_200[-c(18, 76)]
normal1 <- run_parallel(data, "ped_status ~ s(tend) + transition")
cutoff1 <- run_parallel(data, "ped_status ~ s(tend, by = transition) + transition", cutoff = TRUE)

# Scenario 2
data <- data_mark_2_200[-c(32)]
normal2 <- run_parallel(data, "ped_status ~ s(tend) + transition", competing = TRUE)
cutoff2 <- run_parallel(data, "ped_status ~ s(tend, by = transition) + transition", cutoff = TRUE, competing = TRUE)

# Scenario 3
data <- data_mark_3_200[-c(30)]
normal3 <- run_parallel(data, "ped_status ~ s(tend) + transition", competing = TRUE)
cutoff3 <- run_parallel(data, "ped_status ~ s(tend, by = transition) + transition", cutoff = TRUE, competing = TRUE)

# Scenario 4
data <- data_mark_4_200[-c(99)]
normal4 <- run_parallel(data, "ped_status ~ s(tend) + transition", competing = TRUE)
cutoff4 <- run_parallel(data, "ped_status ~ s(tend, by = transition) + transition", cutoff = TRUE, competing = TRUE)

save(
  normal1, cutoff1, 
  normal2, cutoff2, 
  normal3, cutoff3, 
  normal4, cutoff4,
  file = "Analysis/final_res_scripts/final_results/results/mark200_final_results.RData"
)
print("Markov 200 done")

stopCluster(cl)
