
# here a function provided by Alexandra Erdmann is sourced:
source("Erdmann/fct/simulation_recurrent_events.R")

# and modified for my use
sim <- \(repN, scenario) {
  data <- replicate(repN, simu_recurr(scenario$N, scenario$alpha,
                                      cens = scenario$cens, gamma = scenario$gamma,
                                      type2 = scenario$type2, m = scenario$m,
                                      stat.depend = scenario$stat.depend,
                                      cens.haz = scenario$cens.haz, death.haz = scenario$death.haz,
                                      staggered = scenario$staggered, nondeg = scenario$nondeg,
                                      stopcrit = scenario$stopcrit, EOS = scenario$EOS, 
                                      censRate = scenario$censRate), simplify = FALSE)
  return(data)
}

# source("initial_run.R")
setwd("Analysis")
# pois no term event random cens n=100
# set.seed(45678)
set.seed(1234)

data_pois_1_100 <- sim(repN=100, scenario=scenarioMaster[[1]])


data_pois_1_25 <- sim(repN=100, scenario=scenarioMaster[[2]])


data_pois_1_50 <- sim(repN=100, scenario=scenarioMaster[[3]])


data_pois_1_200 <- sim(repN=100, scenario=scenarioMaster[[4]])





# pois term event random cens n=100
# set.seed(45678)
set.seed(1235)

data_pois_2_100 <- sim(repN=100, scenario=scenarioMaster[[5]])


data_pois_2_25 <- sim(repN=100, scenario=scenarioMaster[[6]])


data_pois_2_50 <- sim(repN=100, scenario=scenarioMaster[[7]])


data_pois_2_200 <- sim(repN=100, scenario=scenarioMaster[[8]])



# pois no term event state depend cens n=100
# set.seed(45678)
set.seed(1236)

data_pois_3_100 <- sim(repN=100, scenario=scenarioMaster[[9]])


data_pois_3_25 <- sim(repN=100, scenario=scenarioMaster[[10]])


data_pois_3_50 <- sim(repN=100, scenario=scenarioMaster[[11]])


data_pois_3_200 <- sim(repN=100, scenario=scenarioMaster[[12]])



# pois term event state depend cens n=100
# set.seed(45678)
set.seed(1237)

data_pois_4_100 <- sim(repN=100, scenario=scenarioMaster[[13]])


data_pois_4_25 <- sim(repN=100, scenario=scenarioMaster[[14]])


data_pois_4_50 <- sim(repN=100, scenario=scenarioMaster[[15]])


data_pois_4_200 <- sim(repN=100, scenario=scenarioMaster[[16]])




# markov no term event random cens n=100

# set.seed(45678)
set.seed(1238)

data_mark_1_100 <- sim(repN=100, scenario=scenarioMaster[[17]])


data_mark_1_25 <- sim(repN=100, scenario=scenarioMaster[[18]])


data_mark_1_50 <- sim(repN=100, scenario=scenarioMaster[[19]])


data_mark_1_200 <- sim(repN=100, scenario=scenarioMaster[[20]])


# markov term event random cens n=100

# set.seed(45678)
set.seed(1239)

data_mark_2_100 <- sim(repN=100, scenario=scenarioMaster[[21]])


data_mark_2_25 <- sim(repN=100, scenario=scenarioMaster[[22]])


data_mark_2_50 <- sim(repN=100, scenario=scenarioMaster[[23]])


data_mark_2_200 <- sim(repN=100, scenario=scenarioMaster[[24]])


# markov no term event state depend cens n=100

# set.seed(45678)
set.seed(1240)

data_mark_3_100 <- sim(repN=100, scenario=scenarioMaster[[25]])


data_mark_3_25 <- sim(repN=100, scenario=scenarioMaster[[26]])


data_mark_3_50 <- sim(repN=100, scenario=scenarioMaster[[27]])


data_mark_3_200 <- sim(repN=100, scenario=scenarioMaster[[28]])


# markov term event state depend cens n=100

# set.seed(45678)
set.seed(1241)

data_mark_4_100 <- sim(repN=100, scenario=scenarioMaster[[29]])


data_mark_4_25 <- sim(repN=100, scenario=scenarioMaster[[30]])


data_mark_4_50 <- sim(repN=100, scenario=scenarioMaster[[31]])


data_mark_4_200 <- sim(repN=100, scenario=scenarioMaster[[32]])







# non markov no term event random cens n=100

# set.seed(45678)
set.seed(1243)

data_non_mark_1_100 <- sim(repN=100, true_mean=trueMeanMaster[[9]], scenario=scenarioMaster[[33]])


data_non_mark_1_25 <- sim(repN=100, true_mean=trueMeanMaster[[9]], scenario=scenarioMaster[[34]])


data_non_mark_1_50 <- sim(repN=100, true_mean=trueMeanMaster[[9]], scenario=scenarioMaster[[35]])


data_non_mark_1_200 <- sim(repN=100, true_mean=trueMeanMaster[[9]], scenario=scenarioMaster[[36]])




# non markov term event random cens n=100

# set.seed(45678)
set.seed(1244)

data_non_mark_2_100 <- sim(repN=100, true_mean=trueMeanMaster[[10]], scenario=scenarioMaster[[37]])


data_non_mark_2_25 <- sim(repN=100, true_mean=trueMeanMaster[[10]], scenario=scenarioMaster[[38]])


data_non_mark_2_50 <- sim(repN=100, true_mean=trueMeanMaster[[10]], scenario=scenarioMaster[[39]])


data_non_mark_2_200 <- sim(repN=100, true_mean=trueMeanMaster[[10]], scenario=scenarioMaster[[40]])



# non markov no term event state depend cens n=100

# set.seed(45678)
set.seed(1245)

data_non_mark_3_100 <- sim(repN=100, true_mean=trueMeanMaster[[11]], scenario=scenarioMaster[[41]])


data_non_mark_3_25 <- sim(repN=100, true_mean=trueMeanMaster[[11]], scenario=scenarioMaster[[42]])


data_non_mark_3_50 <- sim(repN=100, true_mean=trueMeanMaster[[11]], scenario=scenarioMaster[[43]])


data_non_mark_3_200 <- sim(repN=100, true_mean=trueMeanMaster[[11]], scenario=scenarioMaster[[44]])



# non markov term event state depend cens n=100

# set.seed(45678)
set.seed(1246)

data_non_mark_4_100 <- sim(repN=100, true_mean=trueMeanMaster[[12]], scenario=scenarioMaster[[45]])


data_non_mark_4_25 <- sim(repN=100, true_mean=trueMeanMaster[[12]], scenario=scenarioMaster[[46]])


data_non_mark_4_50 <- sim(repN=100, true_mean=trueMeanMaster[[12]], scenario=scenarioMaster[[47]])


data_non_mark_4_200 <- sim(repN=100, true_mean=trueMeanMaster[[12]], scenario=scenarioMaster[[48]])




# pois no term event random cens n=1000

# set.seed(45678)
set.seed(1234)

data_pois_1_1000 <- sim(repN=1000, true_mean=trueMeanMaster[[1]], scenario=scenarioMaster[[1]])



# mark no term event random cens n=1000

# set.seed(45678)
set.seed(1243)

data_mark_1_1000 <- sim(repN=1000, true_mean=trueMeanMaster[[5]], scenario=scenarioMaster[[17]])




# save all as rda in one go

save(data_pois_1_100, data_pois_1_25, data_pois_1_50, data_pois_1_200,
     data_pois_2_100, data_pois_2_25, data_pois_2_50, data_pois_2_200,
     data_pois_3_100, data_pois_3_25, data_pois_3_50, data_pois_3_200,
     data_pois_4_100, data_pois_4_25, data_pois_4_50, data_pois_4_200,
     data_mark_1_100, data_mark_1_25, data_mark_1_50, data_mark_1_200,
     data_mark_2_100, data_mark_2_25, data_mark_2_50, data_mark_2_200,
     data_mark_3_100, data_mark_3_25, data_mark_3_50, data_mark_3_200,
     data_mark_4_100, data_mark_4_25, data_mark_4_50, data_mark_4_200,
     data_non_mark_1_100, data_non_mark_1_25, data_non_mark_1_50, data_non_mark_1_200,
     data_non_mark_2_100, data_non_mark_2_25, data_non_mark_2_50, data_non_mark_2_200,
     data_non_mark_3_100, data_non_mark_3_25, data_non_mark_3_50, data_non_mark_3_200,
     data_non_mark_4_100, data_non_mark_4_25, data_non_mark_4_50, data_non_mark_4_200,
     data_pois_1_1000, data_mark_1_1000,
     file = "Data_generation/data_all.rda")


set.wd("..")

