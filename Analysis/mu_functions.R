
# model:
# add formula as wished
# ped_status ~ s(tend) + transition
# ped_status ~ s(tend, by = transition) + transition
# clarify if competing risk or not
# add times you want to check default: c(40, 60, 80, 100)
# clarify if you want to cut transition on 75 quantile
# eq_haz macht, dass bei 75 quantile immer der selbe hazard angenommen wird
# meth method "fREML" or "REMl
# specify if wait should be included
# non_markov waittime not only for the last event
# qwait wheter to use seq(quantile(0.4), quantile(0.6), by = 0.5) or unique(wait) ############### not well implemented yet
# clusterwait, wether to clusterwaittime
# poisson wether or not the model should assume the data is from a poisson process (poisson only works with competing=T)
mu <- \(data, 
          form = "ped_status ~ s(tend) + transition",
          competing   = FALSE,
          times       = c(40, 60, 80, 100),
          cutoff      = FALSE,
          eq_haz      = FALSE,
          meth        = "fREML",
          wait        = FALSE,
          non_markov  = FALSE,
          # qwait       = FALSE,
          clusterwait = FALSE,
          poisson     = FALSE,
          print_mod   = FALSE) {
  # browser()
  if(poisson & !competing) stop("poisson only works with competing=T")
  if(non_markov) wait <- TRUE
  if(wait) {
    if(grepl("wait", form)) form <- formula(form)
    else form <- formula(paste0(form," + wait"))
    data <- data %>%
      group_by(id) %>%
      arrange(exit) %>%  
      mutate(
        wait = exit - entry  
      ) %>% 
      mutate(wait = lag(wait, default = 0)) %>% 
      ungroup()
    if(non_markov) {
      data <- data %>% 
        group_by(id) %>% 
        arrange(exit) %>% 
        mutate(wait = cummean(wait)) %>% 
        # mutate(wait = cumsum(wait)) %>% # macht das sinn? das ist doch dasselbe wie tend
        ungroup()
    }
    if(clusterwait) {
      data <- data %>% 
        mutate(wait = as.factor(ifelse(wait > quantile(data$wait, 0.6),
                                       "long",
                                       "short")))
    }
  }
  else form <- formula(form)
  
  if(eq_haz | cutoff) {
    # cutt <- ceiling(unname((quantile(data$from)[4])))
    cutt <- data %>% 
      group_by(id) %>% 
      mutate(endstate = max(from)) %>%
      filter(from==endstate) %>% 
      mutate(endstate = ifelse(grepl("[a-z]", to),
                               endstate,
                               endstate + 1)
      ) %>% 
      pull(endstate) %>% 
      quantile(0.9) %>% 
      unname() %>% 
      ceiling()
    }
  if(cutoff) {
    dat <- data
    data <- data %>% 
      mutate(from = ifelse(from >= cutt,
                         cutt,
                         from),
           to = ifelse(from >= cutt,
                       ifelse(grepl("[a-z]", to),
                              ifelse(to == "cens",
                                     to,
                                     paste0("death_", cutt)),
                              cutt+1),
                       to))
  }
  if(competing) {
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
    if(poisson & competing) {
    for_ped <- for_ped %>% 
      mutate(wait = (ifelse(to%%2 == 0,
                            "transition",
                            "death")))
    }
  }
  else{
  for_ped <- data %>%
    mutate(status = ifelse(to != "cens", 1, 0),     
           to = ifelse(to == "cens", from + 1, to),
           transition = as.factor(paste0(from, "->", to)),
           enum = from
    ) 
  }
  ped <- as_ped(data = for_ped,
                formula = Surv(entry, exit, status) ~ .,
                id = "id",
                transition = "transition",
                timescale = "calendar"
                , cut = c(unique(for_ped$exit), times)
  )
  
  mod <- pamm(form
              ,
              data=ped,
              engine   = "bam", 
              method   = meth,
              offset = offset,
              discrete = TRUE,
              family = poisson())
  if(print_mod) return(summary(mod))
  if(wait | poisson) {
    wait <- TRUE
    if(clusterwait | poisson) s <- ped %>% pull(wait) %>% unique()
    else {
    # if(qwait) w <- c(seq(quantile(ped$wait, 0.4), quantile(ped$wait, 0.6), by = 0.5))
    # else      
    # w <- c(unique(ped$wait))
    qs <- for_ped$wait %>% 
      # unique() %>% 
      quantile(c(0.4,0.6)) 
    
    s <- for_ped %>% 
      filter(wait >= qs[1] & wait <= qs[2] & status == 1) %>%
      pull(wait) %>% 
      unique()
    }
    newdata <- make_newdata(ped,
                            tend = unique(tend),
                            transition = unique(transition)
                            , wait = s
                            ) %>%
      group_by(transition, wait) %>% 
      add_cumu_hazard(mod)
  }
  else {
  newdata <- make_newdata(ped,
                          tend = unique(tend),
                          transition = unique(transition)) %>%
    group_by(transition) %>% 
    add_cumu_hazard(mod)
  }
  
  max_enum <- max(as.numeric(for_ped %>% filter(status==1) %>% pull(to), na.rm=T))# max(as.numeric(for_ped$to), na.rm=T)


  if(eq_haz) trans_mat <- add_trans_prob(newdata = newdata , object = mod, ci=F, same_hazard = cutt)

  else trans_mat <- add_trans_prob(newdata = newdata , object = mod, ci=F)

  if(wait) {
    # if(!clusterwait) {
    pwait <- table(sort(ped %>% filter(ped_status==1) %>% pull(wait)))/nrow(ped %>% filter(ped_status==1))
    
    
    ps <- which(names(pwait) %in% s)
    
    
    pwait <- pwait[ps] / sum(pwait[ps]) 
    
    x <- trans_mat
    
    for (i in seq_along(x)) {
      x[[i]] <- x[[i]] * pwait[i]

    }

    trans_mat <- array(0, dim = dim(x[[1]]))


    for (i in 1:(dim(trans_mat)[3])) {
      trans_mat[, , i] <- Reduce("+", lapply(x, function(d) d[, , i]))
    }
    # }
    # else
  }

  else trans_mat <- trans_mat[[1]]

  
  ####
  
  # if(cutoff & !competing) {
  # 
  #   event_typesn <- c(1:cutt, mean(as.numeric(dat %>%
  #                                                   filter(as.numeric(to) > cutt) %>%
  #                                                   pull(to)), na.rm=TRUE))
  # } # nochmal überprüfen ob das so stimmt#################################################################################
  # else 
    event_typesn <- c(1:max_enum)
  
  ####
  
  
  
  if(competing) {
    highest_trans_to_death <- FALSE
    if(max_enum%%2!=0) highest_trans_to_death <- TRUE
    event_typesn <- (c(rep(0:((max_enum/2)-1), each = 2)
    # , ifelse(cutoff,
    # mean(as.numeric(dat %>%
    #                   filter(as.numeric(to) > cutt) %>%
    #                   pull(to)), na.rm=TRUE)
    , floor(max_enum/2)
    # )
    ))
    if(highest_trans_to_death) event_typesn <- c(event_typesn, ceiling(max_enum/2))
  }

  sapply(times, function(t) {
    
    last_time <-  max(which(sort(unique(ped 
                                        %>% pull(tend))) <= t))

    if(competing) {
      # sum(sapply(event_typesn,
      #            \(i)
      #            {
      #              trans_mat[1, i + 1, last_time]
      #            }
      # )
      # * (c(0, rep(1:((max_enum/2)-1), each = 2),
      #      ifelse(cutoff,
      #             mean(as.numeric(dat %>%
      #                               filter(as.numeric(to) > cutt) %>%
      #                               pull(to)), na.rm=TRUE),
      #             max_enum/2))))

      trans_mat[1, , last_time] %*% event_typesn
        # (c(rep(0:((max_enum/2)-1), each = 2), (max_enum/2)))
        # (c(0, rep(1:((max_enum/2)#-1 # eventuell wieder reinmachen
        #                                          ), each = 2)
        #                                # , ifelse(cutoff,
        #                                       # mean(as.numeric(dat %>%
        #                                       #                   filter(as.numeric(to) > cutt) %>%
        #                                       #                   pull(to)), na.rm=TRUE),
        #                                       # max_enum/2
        #                                       # )
        #                                ))
    }
    
    else sum(trans_mat[1, -1, last_time] * (event_typesn))
    
  }
  )
  
}












# mean, sd, bias, rmse
m_s_b_r <- \(results, meannumber) {
  vapply(seq_along(c(40, 60, 80, 100)), 
       \(x) {
         vec <- c("mean"=mean(unlist(results)[seq(x, length(results), by = 4)]),
                "sd"=sd(unlist(results)[seq(x, length(results), by = 4)]),
                "bias"=bias_rmse(unlist(results)[seq(x, length(results), by = 4)], trueMeanMaster[[meannumber]][x])[2],
                "sd_b"=bias_rmse(unlist(results)[seq(x, length(results), by = 4)], trueMeanMaster[[meannumber]][x])[4],
                "rmse"=bias_rmse(unlist(results)[seq(x, length(results), by = 4)], trueMeanMaster[[meannumber]][x])[3],
                "True"=trueMeanMaster[[meannumber]][x]
                # ,"median"=median(unlist(results)[seq(x, length(results), by = 4)])
                )
         # colnames(vec) <- c(40, 60, 80, 100)
         vec
       },
       numeric(6)
)
}

# clean_results

clean_results <- \(results) {
  if(!is.array(results)) results <- results %>% array(dim = c(4, length(.)/4))
  results[, apply(results, 2, function(col) all(!is.na(col) & col >= 0 & col <= 15))]
  }

