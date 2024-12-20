
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

