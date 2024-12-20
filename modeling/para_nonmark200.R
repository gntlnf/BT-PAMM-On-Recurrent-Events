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

# Scenario 1
data <- data_non_mark_1_200
normal1 <- run_parallel(data, "ped_status ~ s(tend) + transition")
# normal1_notrans <- run_parallel(data, "ped_status ~ s(tend)")
# spline1 <- run_parallel(data, "ped_status ~ s(tend, by = transition) + transition")
cutoff1 <- run_parallel(data, "ped_status ~ s(tend, by = transition) + transition", cutoff = TRUE)
# wait1 <- run_parallel(data, "ped_status ~ s(tend, by = transition) + transition", cutoff = TRUE, wait = TRUE)
# wait1_nonmarkov <- run_parallel(data, "ped_status ~ s(tend, by = transition) + transition", cutoff = TRUE, non_markov = TRUE)
# tspline1 <- run_parallel(data, "ped_status ~ te(tend, wait) + transition", cutoff = TRUE, non_markov = TRUE)

# Scenario 2
data <- data_non_mark_2_200
normal2 <- run_parallel(data, "ped_status ~ s(tend) + transition", competing = TRUE)
# normal2_notrans <- run_parallel(data, "ped_status ~ s(tend)", competing = TRUE)
# spline2 <- run_parallel(data, "ped_status ~ s(tend, by = transition) + transition", competing = TRUE)
cutoff2 <- run_parallel(data, "ped_status ~ s(tend, by = transition) + transition", cutoff = TRUE, competing = TRUE)
# wait2 <- run_parallel(data, "ped_status ~ s(tend, by = transition) + transition", cutoff = TRUE, competing = TRUE, wait = TRUE)
# wait2_nonmarkov <- run_parallel(data, "ped_status ~ s(tend, by = transition) + transition", cutoff = TRUE, competing = TRUE, non_markov = TRUE)
# tspline2 <- run_parallel(data, "ped_status ~ te(tend, wait) + transition", cutoff = TRUE, non_markov = TRUE, competing = TRUE)

# Scenario 3
data <- data_non_mark_3_200
normal3 <- run_parallel(data, "ped_status ~ s(tend) + transition")
# normal3_notrans <- run_parallel(data, "ped_status ~ s(tend)")
# spline3 <- run_parallel(data, "ped_status ~ s(tend, by = transition) + transition")
cutoff3 <- run_parallel(data, "ped_status ~ s(tend, by = transition) + transition", cutoff = TRUE)
# wait3 <- run_parallel(data, "ped_status ~ s(tend, by = transition) + transition", cutoff = TRUE, wait = TRUE)
# wait3_nonmarkov <- run_parallel(data, "ped_status ~ s(tend, by = transition) + transition", cutoff = TRUE, non_markov = TRUE)
# tspline3 <- run_parallel(data, "ped_status ~ te(tend, wait) + transition", cutoff = TRUE, non_markov = TRUE)

# Scenario 4
data <- data_non_mark_4_200
normal4 <- run_parallel(data, "ped_status ~ s(tend) + transition", competing = TRUE)
# normal4_notrans <- run_parallel(data, "ped_status ~ s(tend)", competing = TRUE)
# spline4 <- run_parallel(data, "ped_status ~ s(tend, by = transition) + transition", competing = TRUE)
cutoff4 <- run_parallel(data, "ped_status ~ s(tend, by = transition) + transition", cutoff = TRUE, competing = TRUE)
# wait4 <- run_parallel(data, "ped_status ~ s(tend, by = transition) + transition", cutoff = TRUE, competing = TRUE, wait = TRUE)
# wait4_nonmarkov <- run_parallel(data, "ped_status ~ s(tend, by = transition) + transition", cutoff = TRUE, competing = TRUE, non_markov = TRUE)
# tspline4 <- run_parallel(data, "ped_status ~ te(tend, wait) + transition", cutoff = TRUE, non_markov = TRUE, competing = TRUE)

save(normal1, cutoff1, 
     normal2, cutoff2, 
     normal3, cutoff3, 
     normal4, cutoff4, 
     file = "Analysis/final_res_scripts/final_results/non_mark200_final_results_N200.RData")
print("Non-Markov 200 done")

stopCluster(cl)
