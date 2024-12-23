# Comparison of different estimators for the expected number of events in recurrent events with competing terminal event

This Repository contains the code used for the thesis *"Comparison of different estimators for the expected number of events in recurrent events with competing terminal event"*.
It contains the data generation in form of a simulation study, estimation and visualization of the results.
Some code was not created for this thesis. 
The code for the simulation study (provided by Erdmann and Beyersmann) is in the folder `simulation_study`. 
Code that is not live yet of the R package `pammtools` for multistate settings is in the folder `pammtools-multi-state`.   


## Structure

  - folder `Erdmann`: contains the code provided by Alexandra Erdmann for the simulation study and nonparametric estimation.
  - folder `Data_generation`: contains the code for the data generation which utilizes code from the above.
  - `initial_run.R` and `skip_data_generation.R`: are the files to load all necessary packages and data if the data generation is skipped.
  - folder `modeling`: contains the code for PAMM modeling.
  - folder `Analysis`: contains the code for the analysis of the results and the creation of the visualization in graphs and tables.

## How to reproduce the entire thesis

Reproducing this entire thesis will take some time and includes parallelized code optimized for Windows. 

### Replication of Alexandra Erdmann's results 

This file was entirely created for the paper *"Comparison of nonparametric estimators of the expected number of recurrent events"*, I only adjusted absolut paths and added
a seed when estimating the true mean values to ensure reproducibility. Running this file takes quite some time. 
```
source("Erdmann/Main_Revision")
``` 
The estimations of the true mean value of recurrent events will be saved in `Erdmann/saves`.
All estimation results will be saved in `Erdmann/results` it includes:
  - all results in `tex` files
  - all results as `rda`
  - the results for the data sized $N \in \\{100, 200\\}$

### To reproduce this paper's results:

First all dependencies will be loaded. 
This includes the packages:

  - `survival`
  - `etm`
  - `mvna`
  - `data.table`
  - `dplyr`
  - `MASS`
  - `xtable`
  - `doParallel`
  - `foreach`

```
source("initial_run.R")
```

#### To create the datasets for my analysis run (optional)

This file uses parts of Erdmann's simulation study code. 
This step is optional as all datasets are alreay saved in this Repo.
```
source("Data_generation/generates_data.R")
```
All datasets will be saved as `Data_generation/data_all.rda`.
    

#### Load Datasets (optional if you run the previous step)
This file loads all datasets  
```
load("Data_generation/data_all.rda")
```

#### To reproduce all the PAMM modeling (optional as all results are saved in this Repo)

#### To reproduce all the PAMM modeling (optional as all results are saved in this Repo)

These files are parallelized and optimized for Windows. 
The computational time is pretty long.
```
source("modeling/para_models.R")
```
The results for each Scenario are saved in `Analysis/final_res_scripts/final_results/results`

To then evaluate the results run
```
source("Analysis/final_res_scripts/evaluate_all.R")
```
The results will be saved in `Analysis/final_res_scripts/final_results/evaluated`.

#### Reproduce graphs and tables (this is possible even when skipping all above)


To then create all plots run 
```
source("Analysis/final_res_scripts/create_all_plots.R")
```

The PED example and result tables are created 
```
source("Analysis/final_res_scripts/create_ped_tables.R")
```
The tables will be saved in `Analysis/final_res_scripts/final_results/tables` and the plots in `Analysis/final_res_scripts/final_results/plots`.
