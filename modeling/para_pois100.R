library(doParallel)
library(foreach)

num_cores <- detectCores() - 1 
cl <- makeCluster(num_cores)
registerDoParallel(cl)

clusterEvalQ(cl, {
  source("initial_run.R")
  source("skip_data_generation.R")
})

run_parallel <- function(data, formula, competing = FALSE, cutoff = FALSE, wait = FALSE) {
  foreach(i = seq_along(data), .combine = 'c') %dopar% {
    mu(data[[i]], form = formula, competing = competing, cutoff = cutoff, wait = wait)
  }
}

# Scenario 1: rand_noterm
data <- data_pois_1_100
normal1 <- run_parallel(data, "ped_status ~ s(tend) + transition")
normal1_notrans <- run_parallel(data, "ped_status ~ s(tend)")
spline1 <- run_parallel(data, "ped_status ~ s(tend, by = transition) + transition")
cutoff1 <- run_parallel(data, "ped_status ~ s(tend, by = transition) + transition", cutoff = TRUE)
wait1 <- run_parallel(data, "ped_status ~ s(tend, by = transition) + transition", cutoff = TRUE, wait = TRUE)

# Scenario 2: rand_term
data <- data_pois_2_100[-c(21, 74)]
normal2 <- run_parallel(data, "ped_status ~ s(tend) + transition", competing = TRUE)
normal2_notrans <- run_parallel(data, "ped_status ~ s(tend)", competing = TRUE)
spline2 <- run_parallel(data, "ped_status ~ s(tend, by = transition) + transition", competing = TRUE)
cutoff2 <- run_parallel(data, "ped_status ~ s(tend, by = transition) + transition", cutoff = TRUE, competing = TRUE)
wait2 <- run_parallel(data, "ped_status ~ s(tend, by = transition) + transition", cutoff = TRUE, wait = TRUE, competing = TRUE)

# Scenario 3: state_noterm
data <- data_pois_3_100[-c(17, 20, 21, 44, 62, 90)]
normal3 <- run_parallel(data, "ped_status ~ s(tend) + transition")
normal3_notrans <- run_parallel(data, "ped_status ~ s(tend)")
spline3 <- run_parallel(data, "ped_status ~ s(tend, by = transition) + transition")
cutoff3 <- run_parallel(data, "ped_status ~ s(tend, by = transition) + transition", cutoff = TRUE)
wait3 <- run_parallel(data, "ped_status ~ s(tend, by = transition) + transition", cutoff = TRUE, wait = TRUE)

# Scenario 4: state_term
data <- data_pois_4_100[-c(1, 8, 21, 28, 70, 83)]
normal4 <- run_parallel(data, "ped_status ~ s(tend) + transition", competing = TRUE)
normal4_notrans <- run_parallel(data, "ped_status ~ s(tend)", competing = TRUE)
spline4 <- run_parallel(data, "ped_status ~ s(tend, by = transition) + transition", competing = TRUE)
cutoff4 <- run_parallel(data, "ped_status ~ s(tend, by = transition) + transition", cutoff = TRUE, competing = TRUE)
wait4 <- run_parallel(data, "ped_status ~ s(tend, by = transition) + transition", cutoff = TRUE, wait = TRUE, competing = TRUE)

save(normal1, spline1, cutoff1, normal1_notrans, wait1,
     normal2, spline2, cutoff2, normal2_notrans, wait2,
     normal3, spline3, cutoff3, normal3_notrans, wait3,
     normal4, spline4, cutoff4, normal4_notrans, wait4,
     file = "Analysis/final_res_scripts/final_results/poisson_final_results.RData")
print("Poisson 100 done")

stopCluster(cl)
