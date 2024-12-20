

source("Analysis/mu_functions.R")

# here the pammtools version including multi-state is loaded
# the "add_trans_prob" function was slightly modified to result in 
# the transition probability matrix or dataframe, default was only dataframe
setwd("pammtools-multi-state")
devtools::load_all()
setwd("..")

source("modeling/para_pois100.R")
source("modeling/para_pois200.R")
source("modeling/para_mark100.R")
source("modeling/para_mark200.R")
source("modeling/para_nonmark100.R")
source("modeling/para_nonmark200.R")
