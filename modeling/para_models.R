
# load necessary function
# 1. processes data to fitting format depending on Competing Mortality (Yes/No)
# 2. transforms data to PED format
# 3. fits the PAMM model
# 4. estimates Transition Probability Matrix
source("Analysis/mu_functions.R")

# here the pammtools version including multi-state is loaded
# the "add_trans_prob" function was slightly modified to result in 
# the transition probability matrix or dataframe, default was only dataframe
setwd("pammtools-multi-state")
devtools::load_all()
setwd("..")


# estimate the mean number of recurrent events using PAMM based estimators
# each file parallelized 
# optimized for windows
source("modeling/para_pois100.R")
source("modeling/para_pois200.R")
source("modeling/para_mark100.R")
source("modeling/para_mark200.R")
source("modeling/para_nonmark100.R")
source("modeling/para_nonmark200.R")
