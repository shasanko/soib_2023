occu0 = foreach(i = chunks[[1]], .combine = 'rbind', .errorhandling = 'remove',
                .packages = c("tictoc", "glue", "dplyr", "data.table",
                              "dtplyr")) %dopar%
   {
     #i = 1 
     data = data
     i = i
     speciesforocc = speciesforocc
     queen_neighbours = g1_nb_q
     species = speciesforocc$eBird.English.Name.2022[i]
    status = speciesforocc$status[i]
    
    tic(glue("Modelled occupancy of {species}"))
    
    # filter data only within the known spatial and temporal range of the species
    data = data %>%
      filter(COMMON.NAME == species) %>%
      distinct(gridg3, month) %>% 
      left_join(data)
    
    data_filt_mig <- data %>% 
      # filter (or not) the data based on migratory status
      filt_data_for_mig(species, status)
    
    # expanding data for absences also
    data_exp = expand_dt(data_filt_mig, species) %>% 
      filter(!is.na(gridg1)) %>%
      # converting months to seasons
      mutate(month = as.numeric(month)) %>%
      mutate(month = case_when(month %in% c(12, 1, 2) ~ "Win",
                               month %in% c(3, 4, 5) ~ "Sum",
                               month %in% c(6, 7, 8) ~ "Mon",
                               month %in% c(9, 10, 11) ~ "Aut")) %>% 
      mutate(month = as.factor(month))
    
    # reordering the checklists within each grid to minimise bias
    data_exp = data_exp[sample(x = 1:nrow(data_exp)),] 
    
    # number of checklists per grid cell
    lists_per_grid = data_exp %>%
      group_by(gridg1) %>% 
      reframe(lpg = n())
    
    # deciding a cutoff threshold based on 95% quantile---ignore all lists after that
    listcutoff = quantile(lists_per_grid$lpg, 0.95, na.rm = TRUE)
    
    data_exp = data_exp %>%
      arrange(gridg1) %>%
      group_by(gridg1) %>% 
      mutate(group.id = 1:n()) %>% 
      ungroup()
    
    # presences and absences
    occdata_full = data_exp %>%
      group_by(gridg1) %>% 
      # does the grid have the species?
      summarize(presence = sum(OBSERVATION.COUNT)) %>%
      mutate(presence = replace(presence, presence > 1, 1),
             # initialising this column which will later have info on 
             # proportion of neighbouring cells that have the species
             prop_nb = 0,
             gridg1 = as.character(gridg1))
    
    occdata_cell_nb <- occdata_full %>% 
      # numeric vector of neighbours of each cell being iterated over
      mutate(nb_list = map(gridg1, ~ as.numeric(queen_neighbours[[as.numeric(.)]]))) %>% 
      # this gives fewer neighbours than above, because not all neighbours are in data
      # (and even fewer with complete lists, etc.)
      mutate(occdata_nb = map(nb_list, ~ occdata_full %>% 
                                filter(gridg1 %in% .x) %>% 
                                pull(presence))) %>% 
      # numerator: total number of neighbour cells that have the species
      # denominator should be all neighbour cells
      mutate(prop_nb = map2_dbl(occdata_nb, nb_list, ~ sum(.x)/length(.y))) %>%
      dplyr::select(-nb_list, -occdata_nb)
    
    # absences
    occdata_abs = occdata_cell_nb %>% filter(presence != 1)
    occdata_cell_nb = occdata_cell_nb %>% dplyr::select(-presence)
    
    
    # creating matrices for occupancy
    # need data.table because very large objects
    setDT(data_exp)
    
    # matrices of detection/non-, season, and median list length (matrix needed for occ)
    det = dcast(data_exp, gridg1 ~ group.id, value.var = "OBSERVATION.COUNT")
    cov.month = dcast(data_exp, gridg1 ~ group.id, value.var = "month")
    cov.nosp = dcast(data_exp, gridg1 ~ group.id, value.var = "no.sp")
    
    # back to dataframe
    det = setDF(det)
    cov.month = setDF(cov.month)
    cov.nosp = setDF(cov.nosp)
    
    # for every grid cell, selecting only N lists; this removes outlier grid cells 
    # like Bangalore
    det = det[, 1:listcutoff]
    cov.month = cov.month[, 1:listcutoff]
    cov.nosp = cov.nosp[, 1:listcutoff]
    
    
    # calculate the number of checklists per grid going into the analysis
    samples.grid = det %>% 
      mutate(samples = rowSums(!is.na(dplyr::select(., -gridg1)))) %>%
      dplyr::select(gridg1,samples)
    n.samplespergrid = ncol(det) - 1
    
    
    
    # input to occupancy modelling
    occdata_UFO = unmarkedFrameOccu(
      y = det[, -1], # response (only 1s and 0s)
      siteCovs = data.frame(prop_nb = occdata_cell_nb$prop_nb),
      obsCovs = list(cov1 = cov.nosp[, -1],
                     cov2 = cov.month[, -1])
    )
    
    # create newdata
    cov.nosp.long = cov.nosp %>%
      pivot_longer(!gridg1, names_to = "sample", values_to = "cov1") %>%
      filter(!is.na(cov1))
    cov.month.long = cov.month %>%
      pivot_longer(!gridg1, names_to = "sample", values_to = "cov2") %>%
      filter(!is.na(cov2))
    newdata.det = cov.month.long %>%
      left_join(cov.nosp.long) %>% dplyr::select(-sample)
    
    newdata.occ = occdata_cell_nb
    
    
    # if not resident, we don't use seasonality as a covariate because data 
    # already filtered to those months
    if (status == "R") {
      
      occ_det = tryCatch({occu(~ log(cov1) * cov2 ~ prop_nb,
                               data = occdata_UFO,
                               starts = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                               engine = "C")},
                         error = function(cond){"skip"})
      
    } else {
      
      occ_det = tryCatch({occu(~ log(cov1) ~ prop_nb, 
                               data = occdata_UFO, 
                               starts = c(0, 0, 0, 0),
                               engine = "C")},
                         error = function(cond){"skip"})
      
    }
    
    # output of above will be character when errored ("skip")
    if (!is.character(occ_det)) {
      
      # detection probability
      occpred_detection = unmarked::predict(occ_det, 
                                            newdata = newdata.det, type = "det")
      occpred_detection = occpred_detection %>% dplyr::select(-lower, -upper) %>%
        bind_cols(newdata.det) %>%
        filter(!is.na(Predicted)) %>% mutate(pred.inv = 1-Predicted) %>%
        group_by(gridg1) %>% mutate(mean.prod = prod(pred.inv)) %>%
        mutate(se.ratio.inv = SE/pred.inv) %>% ungroup() %>%
        group_by(gridg1) %>% reframe(det = 1-max(mean.prod),
                                     inv.det = max(mean.prod),
                                     det.se = max(mean.prod)*sum(se.ratio.inv))
      
      
      # occupancy
      occpred_occupancy = unmarked::predict(occ_det, 
                                            newdata = newdata.occ, type = "state")
      occpred_occupancy = occpred_occupancy %>% dplyr::select(-lower, -upper) %>%
        bind_cols(newdata.occ) %>%
        rename(occ = Predicted, occ.se = SE) %>%
        dplyr::select(gridg1, prop_nb, occ, occ.se) %>%
        left_join(occpred_detection) %>%
        left_join(samples.grid) %>%
        left_join(occdata_abs) %>% 
        mutate(presence = case_when(is.na(presence)~1,TRUE~presence)) %>%
        mutate(COMMON.NAME = species, status = status,
               occupancy = occ*inv.det,
               se = occ*inv.det*((det.se/inv.det) + (occ.se/occ)))
      
    }
    
    toc()
    
    # to combine
    tocomb = occpred_occupancy
    return(tocomb)
    
   }

#----

errors <- list()

occu0 = foreach(i = chunks[[1]], .combine = 'rbind', .errorhandling = 'remove',
                .packages = c("tictoc", "glue", "dplyr", "data.table",
                              "dtplyr")) %dopar%
  {
    #i = 1 
    tryCatch(
    data = data
    i = i
    speciesforocc = speciesforocc
    queen_neighbours = g1_nb_q
    species = speciesforocc$eBird.English.Name.2022[i]
    status = speciesforocc$status[i]
    
    tic(glue("Modelled occupancy of {species}"))
    
    # filter data only within the known spatial and temporal range of the species
    data = data %>%
      filter(COMMON.NAME == species) %>%
      distinct(gridg3, month) %>% 
      left_join(data)
    
    data_filt_mig <- data %>% 
      # filter (or not) the data based on migratory status
      filt_data_for_mig(species, status)
    
    # expanding data for absences also
    data_exp = expand_dt(data_filt_mig, species) %>% 
      filter(!is.na(gridg1)) %>%
      # converting months to seasons
      mutate(month = as.numeric(month)) %>%
      mutate(month = case_when(month %in% c(12, 1, 2) ~ "Win",
                               month %in% c(3, 4, 5) ~ "Sum",
                               month %in% c(6, 7, 8) ~ "Mon",
                               month %in% c(9, 10, 11) ~ "Aut")) %>% 
      mutate(month = as.factor(month))
    
    # reordering the checklists within each grid to minimise bias
    data_exp = data_exp[sample(x = 1:nrow(data_exp)),] 
    
    # number of checklists per grid cell
    lists_per_grid = data_exp %>%
      group_by(gridg1) %>% 
      reframe(lpg = n())
    
    # deciding a cutoff threshold based on 95% quantile---ignore all lists after that
    listcutoff = quantile(lists_per_grid$lpg, 0.95, na.rm = TRUE)
    
    data_exp = data_exp %>%
      arrange(gridg1) %>%
      group_by(gridg1) %>% 
      mutate(group.id = 1:n()) %>% 
      ungroup()
    
    # presences and absences
    occdata_full = data_exp %>%
      group_by(gridg1) %>% 
      # does the grid have the species?
      summarize(presence = sum(OBSERVATION.COUNT)) %>%
      mutate(presence = replace(presence, presence > 1, 1),
             # initialising this column which will later have info on 
             # proportion of neighbouring cells that have the species
             prop_nb = 0,
             gridg1 = as.character(gridg1))
    
    occdata_cell_nb <- occdata_full %>% 
      # numeric vector of neighbours of each cell being iterated over
      mutate(nb_list = map(gridg1, ~ as.numeric(queen_neighbours[[as.numeric(.)]]))) %>% 
      # this gives fewer neighbours than above, because not all neighbours are in data
      # (and even fewer with complete lists, etc.)
      mutate(occdata_nb = map(nb_list, ~ occdata_full %>% 
                                filter(gridg1 %in% .x) %>% 
                                pull(presence))) %>% 
      # numerator: total number of neighbour cells that have the species
      # denominator should be all neighbour cells
      mutate(prop_nb = map2_dbl(occdata_nb, nb_list, ~ sum(.x)/length(.y))) %>%
      dplyr::select(-nb_list, -occdata_nb)
    
    # absences
    occdata_abs = occdata_cell_nb %>% filter(presence != 1)
    occdata_cell_nb = occdata_cell_nb %>% dplyr::select(-presence)
    
    
    # creating matrices for occupancy
    # need data.table because very large objects
    setDT(data_exp)
    
    # matrices of detection/non-, season, and median list length (matrix needed for occ)
    det = dcast(data_exp, gridg1 ~ group.id, value.var = "OBSERVATION.COUNT")
    cov.month = dcast(data_exp, gridg1 ~ group.id, value.var = "month")
    cov.nosp = dcast(data_exp, gridg1 ~ group.id, value.var = "no.sp")
    
    # back to dataframe
    det = setDF(det)
    cov.month = setDF(cov.month)
    cov.nosp = setDF(cov.nosp)
    
    # for every grid cell, selecting only N lists; this removes outlier grid cells 
    # like Bangalore
    det = det[, 1:listcutoff]
    cov.month = cov.month[, 1:listcutoff]
    cov.nosp = cov.nosp[, 1:listcutoff]
    
    
    # calculate the number of checklists per grid going into the analysis
    samples.grid = det %>% 
      mutate(samples = rowSums(!is.na(dplyr::select(., -gridg1)))) %>%
      dplyr::select(gridg1,samples)
    n.samplespergrid = ncol(det) - 1
    
    
    
    # input to occupancy modelling
    occdata_UFO = unmarkedFrameOccu(
      y = det[, -1], # response (only 1s and 0s)
      siteCovs = data.frame(prop_nb = occdata_cell_nb$prop_nb),
      obsCovs = list(cov1 = cov.nosp[, -1],
                     cov2 = cov.month[, -1])
    )
    
    # create newdata
    cov.nosp.long = cov.nosp %>%
      pivot_longer(!gridg1, names_to = "sample", values_to = "cov1") %>%
      filter(!is.na(cov1))
    cov.month.long = cov.month %>%
      pivot_longer(!gridg1, names_to = "sample", values_to = "cov2") %>%
      filter(!is.na(cov2))
    newdata.det = cov.month.long %>%
      left_join(cov.nosp.long) %>% dplyr::select(-sample)
    
    newdata.occ = occdata_cell_nb
    
    
    # if not resident, we don't use seasonality as a covariate because data 
    # already filtered to those months
    if (status == "R") {
      
      occ_det = tryCatch({occu(~ log(cov1) * cov2 ~ prop_nb,
                               data = occdata_UFO,
                               starts = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                               engine = "C")},
                         error = function(cond){"skip"})
      
    } else {
      
      occ_det = tryCatch({occu(~ log(cov1) ~ prop_nb, 
                               data = occdata_UFO, 
                               starts = c(0, 0, 0, 0),
                               engine = "C")},
                         error = function(cond){"skip"})
      
    }
    
    # output of above will be character when errored ("skip")
    if (!is.character(occ_det)) {
      
      # detection probability
      occpred_detection = unmarked::predict(occ_det, 
                                            newdata = newdata.det, type = "det")
      occpred_detection = occpred_detection %>% dplyr::select(-lower, -upper) %>%
        bind_cols(newdata.det) %>%
        filter(!is.na(Predicted)) %>% mutate(pred.inv = 1-Predicted) %>%
        group_by(gridg1) %>% mutate(mean.prod = prod(pred.inv)) %>%
        mutate(se.ratio.inv = SE/pred.inv) %>% ungroup() %>%
        group_by(gridg1) %>% reframe(det = 1-max(mean.prod),
                                     inv.det = max(mean.prod),
                                     det.se = max(mean.prod)*sum(se.ratio.inv))
      
      
      # occupancy
      occpred_occupancy = unmarked::predict(occ_det, 
                                            newdata = newdata.occ, type = "state")
      occpred_occupancy = occpred_occupancy %>% dplyr::select(-lower, -upper) %>%
        bind_cols(newdata.occ) %>%
        rename(occ = Predicted, occ.se = SE) %>%
        dplyr::select(gridg1, prop_nb, occ, occ.se) %>%
        left_join(occpred_detection) %>%
        left_join(samples.grid) %>%
        left_join(occdata_abs) %>% 
        mutate(presence = case_when(is.na(presence)~1,TRUE~presence)) %>%
        mutate(COMMON.NAME = species, status = status,
               occupancy = occ*inv.det,
               se = occ*inv.det*((det.se/inv.det) + (occ.se/occ)))
      
    }
    
    toc()
    
    # to combine
    tocomb = occpred_occupancy
    return(tocomb),
    error = function(e) {
      list(error = TRUE, message = e$message, call = deparse(e$call))
    }
  )  
}

errors <- Filter(function(x) is.list(x) && isTRUE(x$error), occu0)




