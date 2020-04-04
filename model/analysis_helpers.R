## Analysis helpers for the SaniPath Analysis tool
## these have been derived from the original server.R
## file.
library(rlist)
library(plyr)
library(dplyr)
library(purrr)
#library(reshape2)
library(magrittr)
library(doParallel)
library(rjags)


# CONCENTRATIONS ---------------------------------------------
####### CASEY EDITED #####
compute_concentrations <- function(collection_data, lab_data,
                                   configure = configure,
                                   pathway_codes,
                                   pathway_labels,
                                   neighborhood_mapping = list(),
                                   lab_MF= T) {
  if (is.null(configure)) stop('Missing configure object!')
  if (length(pathway_codes) < 1 | length(pathway_labels) < 1) stop('Pathway data missing!')
  if (length(neighborhood_mapping) < 1) stop('Neighborhood data map missing!')

  # lab_analysis_method <- unique(lab_data$lab_analysis)
  if (!lab_MF) {
    reading = configure$idexx_reading
    value = configure$idexx_value
    MF = F
  } else {
    reading = configure$membrane_reading
    value = configure$membrane_value
    MF = T
  }
  denoms = configure$denoms
  # lab_data %<>% filter(lab_id %in% collection_data$col_id[1:500])
  collection_data %<>% filter(!is.na(col_id))
  lab_data %<>% filter(!is.na(lab_id))
  # Calculate the e coli combined dataframe
  ec_data <- create_ecData(collection_data = collection_data,
                           lab_data = lab_data,
                           mpn_tbl = configure$mpn_tbl,
                           reading = reading,
                           value = value,
                           denoms = denoms,
                           MF = MF)
  #############~~~~~ CASEY ADDED THE LINE BELOW #####
  ec_data <- filter(ec_data, !is.na(col_neighborhood))
  # now build our output of concentration values
  pathway_selected_vector <- suppressWarnings(as.integer(pathway_codes) %>% .[!is.na(.)])

  found_pathways <- unique(ec_data$sample_type)
  conc<-list()
  for (i in unique(factor_to_numeric(ec_data$neighbor))) {
    print(i)
    # sample type 1=drain water, 2=produce, 3=piped water, 4=ocean water, 5=surface water, 6=flood water, 7=Public Latrine Surfaces, 8=particulate, 9=bathing
    for (j in 1:length(pathway_selected_vector)) {
      if (pathway_selected_vector[j] %in% found_pathways) {
        s = names(pathway_codes[j])
        x <-list(s = s,
                 neighb = i,
                 sample = pathway_labels[[s]],
                 neighborhood = lookup_neighborhood(neighborhood_mapping, i),# The neighborhood information should change based on the configuration before deployment.
                 data = ec_data$ec_conc[which(ec_data$neighbor == i
                                              & ec_data$sample_type == pathway_selected_vector[j])])

        x$plot_name <- paste0(x$neighborhood,", ", x$sample, '\n(N=',length(x$data),")")
        x$fn <- sprintf('%s_%s.png', i, x$s)
        conc <- append(conc, list(x))
      }
    }
  }
  return(conc)
}


############### CASEY CHANGED THIS #####################
compute_concentrationsES <- function(collection_data, lab_data,
                                   configure = configure,
                                   pathway_codes,
                                   pathway_labels,
                                   neighborhood_mapping = list(),
                                   lab_MF= T) {
  if (is.null(configure)) stop('Missing configure object!')
  if (length(pathway_codes) < 1 | length(pathway_labels) < 1) stop('Pathway data missing!')
  if (length(neighborhood_mapping) < 1) stop('Neighborhood data map missing!')
  
  # lab_analysis_method <- unique(lab_data$lab_analysis)
  if (!lab_MF) {
    reading = configure$idexx_reading
    value = configure$idexx_value
    MF = F
  } else {
    reading = configure$membrane_reading
    value = configure$membrane_value
    MF = T
  }
  denoms = configure$denoms
  # lab_data %<>% filter(lab_id %in% collection_data$col_id[1:500])
  collection_data %<>% filter(!is.na(col_id))
  lab_data %<>% filter(!is.na(lab_id))
  # Calculate the e coli combined dataframe
  ec_data <- create_ecDataES(collection_data = collection_data,
                           lab_data = lab_data,
                           mpn_tbl = configure$mpn_tbl,
                           reading = reading,
                           value = value,
                           denoms = denoms,
                           MF = MF)
  ######~~~~~ Added this to show results for all surveillance by one neighborhood
  #ec_data$neighbor<-999
  # now build our output of concentration values
  pathway_selected_vector <- suppressWarnings(as.integer(pathway_codes) %>% .[!is.na(.)])
  
  found_pathways <- unique(ec_data$sample_type)
  conc<-list()
  for (i in unique(factor_to_numeric(ec_data$neighbor))) {
    # sample type 1=drain water, 2=produce, 3=piped water, 4=ocean water, 5=surface water, 6=flood water, 7=Public Latrine Surfaces, 8=particulate, 9=bathing
    for (j in 1:length(pathway_selected_vector)) {
      if (pathway_selected_vector[j] %in% found_pathways) {
        s = names(pathway_codes[j])
        x <-list(s = s,
                 neighb = i,
                 sample = pathway_labels[[s]],
                 neighborhood = lookup_neighborhood(neighborhood_mapping, i),# The neighborhood information should change based on the configuration before deployment.
                 data = ec_data$ec_conc[which(ec_data$neighbor == i
                                              & ec_data$sample_type == pathway_selected_vector[j])])
        
        x$plot_name <- paste0(x$neighborhood,", ", x$sample, '\n(N=',length(x$data),")")
        x$fn <- sprintf('%s_%s.png', i, x$s)
        conc <- append(conc, list(x))
      }
    }
  }
  return(conc)
}


# master create_ecData
create_ecData <- function(collection_data, lab_data, mpn_tbl,
                          reading, value,
                          denoms,
                          MF = F # defaults to IDEXX method
                          ) {
  #logic to decide whether the function recieves IDEXX data or MF data;
  #This is assuming all the samples will be tested in one of the method: either IDEXX or MF.
  #This field will be filled based on configuration of the project.

  if (!MF) {
    # idexx specific value manipulation
    lab_data %<>% ec_prepare_idexx(reading, mpn_tbl)
  } else {
    lab_data %<>% ec_prepare_mf(reading, value)
  }

  # These steps are the same for both methods
  ec_data <- ec_merge(collection_data, lab_data)

  # add denominators
  ec_data %<>% ec_add_denoms(denoms)

  # calculate the swaps
  ec_data %<>% ec_calc_swaps()

  # calculate the conditions
  cond_func <- if (MF) ec_mf_conditions else ec_idexx_conditions

  ec_data %<>% cond_func(value)

  #FOR SPT
  # ec_data$neighbor <- as.factor(ec_data$col_ward)
  #FOR SP
  ec_data$neighbor <- as.factor(ec_data$col_neighborhood)
  return(ec_data)


}

create_ecDataES <- function(collection_data, lab_data, mpn_tbl,
                          reading = configure$idexx_reading, value = configure$idexx_value,
                          denoms = configure$denoms,
                          MF = F # defaults to IDEXX method
) {
  #logic to decide whether the function recieves IDEXX data or MF data;
  #This is assuming all the samples will be tested in one of the method: either IDEXX or MF.
  #This field will be filled based on configuration of the project.
  
  if (!MF) {
    # idexx specific value manipulation
    lab_data %<>% ec_prepare_idexx(reading, mpn_tbl)
  } else {
    lab_data %<>% ec_prepare_mf(reading, value)
  }
  
  # These steps are the same for both methods
  ec_data <- ec_merge(collection_data, lab_data)
  
  # add denominators
  ec_data %<>% ec_add_denoms(denoms)
  
  # calculate the swaps
  ec_data %<>% ec_calc_swaps()
  
  # calculate the conditions
  cond_func <- if (MF) ec_mf_conditions else ec_idexx_conditions
  
  ec_data %<>% cond_func(value)
  
  ec_data$neighbor <- as.factor(ec_data$col_ward)
  
  return(ec_data)
  
  
}

# FREQUENCIES ----------------------------------------------------------------
compute_frequencies <- function(..., type='pie',
                                analysis_type=NULL,
                                configure=NULL,
                                pathway_labels = configure$pathway_labels,
                                pathway_codes = configure$pathway_codes,
                                neighborhood_mapping = list()
                                ) {
  # calculate the appropriate factors for plotting pie charts
  # and people plots.  This can handle all of the different survey types
  # household, community, and school.  The function returns a long list.
  # Each object in the list contains 4 elements:
  # sample, neighborhood, age, and data
  # The first three offer the identifying information of the path
  # in question.  The final, data, has the frequency counts. Specify
  # the type of freqencies necessary by specifying type = 'ppl plot'
  # or 'pie chart'.  Default is pie chart.
  #
  # Ex.
  # > calculate_freq(household_data, type= 'pie chart')
  #  $path
  #  $path$sample
  #  [1] "Public Latrine"
  #  $path$age
  #  [1] "Children"
  #  $path$neighborhood
  #  [1] "Neighborhood 2"
  #  $path$data
  #  [1] 1 1 1 1 1 1 2 2 3 3 3 3 4 4 5 5
  #
  # Or Ex.
  # > calculate_freq(hh, sch, comm)

  if (is.null(configure)) stop('Missing configure object!')
  if (length(pathway_codes) < 1 | length(pathway_labels) < 1) stop('Pathway data missing!')
  if (length(neighborhood_mapping) < 1) stop('Neighborhood data map missing!')
  # this allows us to pass multiple data objects without having to explictly
  #say what they are. since the surveys always follow a pattern for the question
  # headers, we can figure out what data we have using that.
  dat <- list(...)
  # this should be based on the columns within each export
  # the ^ is a special regex command meaning starts with
  data_map <- c('household_data' = '^h_', 'community_data' = '^c_', 'school_data' = '^s_')

  # let's figure out what we have
  surveys_matched <- character()
  for (x in dat) { # look at each object that we passed in
    # check if any of the column headers match what we expect
    match <- sapply(data_map, function(dn) any(grepl(dn, names(x))))
    if (any(match)) {
      # if it matches, make an object with that name
      assign(names(data_map[match]), x)
      surveys_matched <- c(surveys_matched, names(data_map[match]))
    }
  }

  # some error handling
#   if (!(length(surveys_matched) == 1 | length(surveys_matched) == 3)) {
#     stop(paste0('Something is wrong with the data. Either pass 1 or 3 data objects.\n',
#                 'Matched objects: ', paste(surveys_matched, collapse=', ')))
#   }
  if (!any(surveys_matched %in% names(data_map))) {
    stop(paste('Unable to determine survey type. Do the column headers have h, s, or c in the names?\n',
               'Matched objects:', paste(surveys_matched, collapse=', ')))

  }

  if (is.null(analysis_type)) {
    # update the survey type
    analysis_type <- ifelse(length(surveys_matched) == 3, 'combined', gsub('_data', '', surveys_matched))
  }

  if (analysis_type == 'combined') {
    df_for_analysis <- bind_rows(household_data, community_data, school_data)
  } else {
    df_for_analysis <- eval(parse(text=paste0(analysis_type, '_data')))
  }

  freq <- find_pathways(df_for_analysis,
                        analysis_type,
                        pathway_labels = pathway_labels,
                        neighborhood_mapping = neighborhood_mapping)

  # lastly, make sure it's the right numbers.
  if (type == 'pie') {
    return(freq)
  }
  # frequencies for pie charts
  else if (type == 'ppl') {

    # if we want data for a people plot, calculate 4 - the value per vector object in the list
    freq <- lapply(freq, function(x) {
      x$data %<>% subtract(4, .)
      x
    })
    return(freq)
  } else {
    warning('Unknown type.  Options are "pie" or "ppl"\n')
  }
}

find_pathways <- function(df, analysis_type,
                          pathway_labels=configure$pathway_labels,
                          neighborhood_mapping) {
  neighborhoods <- unique(unlist(df[,grep('neighborhood$', names(df))])) %>% .[!is.na(.)]
  # this is ugly, but it works
  # pattern match columns, split by underscore, convert to rows,
  # look at the first three columns and find unique combinations,
  # these are our full pathways.
  pathways <- grep('[a-z]{1}_[a-z]{1,2}_[a-z]{1}', names(df), value=T, perl = T) %>%
    strsplit("_") %>%
    lapply(function(j) as.data.frame(t(j), stringsAsFactors=F)) %>%
    bind_rows() %>%
    .[,c(2:3)] %>%
    .[!duplicated(.),] %>%
    .[!apply(. == 'metadata' | . == 'neighborhood', 1, any),] # neighborhood is making it through for some reason

  pathways <- pathways[grepl('^a$|^c$', pathways$V3) &
                         pathways$V2 %in% names(pathway_labels),]

  # iterate through neighborhoods and find pathways for each
  freq <- lapply(neighborhoods, function(n) {
    apply(pathways, 1, function(pathway_combo) {
      x <- find_pathway(df,
                        neighb = n,
                        analysis_type,
                        pathway_combo[1],
                        pathway_combo[2],
                        neighborhood_mapping)
      x$sample <- pathway_labels[[pathway_combo[1]]]
      x$plot_name <- sprintf("%s, %s\n%s (N= %s)", x$neighborhood, x$sample, x$age, length(x$data))
      x$s <- unname(pathway_combo[1])
      x$neighb <- factor_to_numeric(n)
      x$pop <- unname(pathway_combo[2])
      x$analysis_type <- analysis_type
      x$fn <- sprintf("%s_%s_%s_%s.png", x$neighb, x$s, x$pop, x$analysis_type)
      list(x)
      }) %>% unname() %>% unlist(recursive=F)
  }) %>% unlist(recursive=F)

  freq <- freq[sapply(freq, function(x) !is.null(x$data) & !is.null(x$sample))]

  return(freq)
}

find_pathway <- function(df, neighb,
                         analysis_type,
                         pathway_type,
                         population_type,
                         neighborhood_mapping) {
  neighb_subset <- df[,grepl('(h|c|s)_neighborhood$', names(df)), drop=F]
  neighb_subset <- apply(neighb_subset, 1, function(x) any(x == neighb))

  return(list(sample = unname(pathway_type),
              age = switch(population_type, 'a' = 'Adults', 'c' = 'Children'),
              neighborhood = lookup_neighborhood(neighborhood_mapping, neighb),
              data = find_freq(df[neighb_subset,], pathway_type, population_type)
        )
  )
}

find_freq <- function(df, pathway_type, population_type) {
  # For a given survey dataframe, find the answers for that pathway and population type
  # assumes the pattern survey_pathway_population naming convention and expects
  # _3, _2, etc for school and community surveys, household only pulls one column.
  value_map = list('_3' = 1,
                   '_2' = 2,
                   '_1' = 3,
                   '_0' = 4,
                   '_na' = 5)
  # find the column names that match the pattern
  col <- paste0(c(pathway_type, population_type), collapse='_')
  cols <- paste0("_", col, names(value_map), '$')
  cols <- c(cols, sprintf("_%s$", col))
  relevant <- grepl(paste0(cols, collapse="|"), names(df))
  if (any(relevant)) {
    subset_df <- df[,relevant, drop=F]
    results <- lapply(names(subset_df), function(x) {
      sub_sub <- df[,x] %>% factor_to_numeric() %>% .[!is.na(.)]
      if (grepl("_(\\d|na)$", x) & !is_empty(sub_sub)) {
        if (sum(sub_sub, na.rm=T) > 0) {
          pos <- strsplit(x, '_') %>% unlist() %>% .[length(.)] %>% sprintf("_%s", .)
          sub_sub <- rep(unlist(value_map[pos]), sum(sub_sub))
        }
      }
      else {
        sub_sub
      }
    }) %>% unlist()

    if (!is_empty(results)) {
      results <- results[!is.na(results)] %>% unname()

    }
    else {
      results <- NULL
    }


  } else {
    results <- NULL
  }


  return(results)

}




# EXPOSURE -----------------------------------------------------
compute_exposure <- function(freq,
                             conc,
                             configure = configure,
                             pathway_codes = pathway_codes,
                             pathway_labels = pathway_labels,
                             neighborhood_mapping = list(),
                             parallel=T,
                             nc=detectCores(),
                             lab_MF = F) {
  # function to caclulate the percent of population
  # exposed for all pathways given.  performs Bayesian
  # analysis on behavior and environmental data first
  # then calculates the final statistics for plotting
  # nburn=1000, niter=10000, thin=1, cutpoint=c(0, 5, 10)
  # run the Bayesian analyses
  if (is.null(configure)) stop('Missing configure object!')
  if (length(pathway_codes) < 1 | length(pathway_labels) < 1) stop('Pathway data missing!')
  if (length(neighborhood_mapping) < 1) stop('Neighborhood data map missing!')

  attach(configure)
  pathway_codes = pathway_codes

  freq <- bayesian_behavior_estimates(freq,
                                      nburn = jags_par_freq$nburn,
                                      niter = jags_par_freq$niter,
                                      thin = jags_par_freq$thin,
                                      cut_point = cut_point,
                                      init_freq = init_freq,
                                      parallel=parallel, nc=nc)
  conc <- bayesian_environmental_estimates(conc,
                                           nburn = jags_par_env$nburn,
                                           niter = jags_par_env$niter,
                                           thin = jags_par_env$thin,
                                           parallel=parallel, nc=nc)

  
  # based on the original ps_plot section of the shiny server,
  # it seems they are based on the behavoir data
  # need to find the number of neighborhoods
  # and samples
  #
  # freq.x <- lapply(freq, function(x) as.data.frame.list(x[!grepl('data', names(x))], stringsAsFactors = F)) %>% bind_rows()
  # conc.x <- lapply(conc, function(x) as.data.frame.list(x[!grepl('data', names(x))], stringsAsFactors = F)) %>% bind_rows()
  #
  # test <- left_join(freq.x, conc.x, by=c('s' = 's', 'neighb' = 'neighb'))
  # test %<>% filter(!is.na(sample.y))

  neighborhoods <- c(unique(names(list.names(conc, neighborhood))), unique(names(list.names(freq, neighborhood)))) # unique neighborhood values
  neighborhoods <- unique(neighborhoods[duplicated(neighborhoods)]) # if it's duplicated, then it will show up in both freq and conc

  # Bathing water can be assumed as 30, all others should be present to calculate
  samples <- c(unique(names(list.names(conc, sample))), unique(names(list.names(freq, sample)))) # unique sample values
  samples <- unique(samples[duplicated(samples)])

  age <- unique(names(list.names(freq, age)))
  # This assumes that we have both samples and survey data for each neighborhood, pathway, age group.  What if we don't?
  ps.freq <- list()
  for (smp in samples) {
    for (nb in neighborhoods) {
      sconc <- list.which(conc, neighborhood == nb && sample == smp)
      if (length(sconc)> 0 & !is.null(sconc)) {
        sub.conc <- conc[[sconc]]
        for (a in age) {
          # filter frequency to just the age we want
          sfreq <- list.which(freq, sample == smp && neighborhood == nb && age == a)
          if (length(sfreq) > 0 & !is.null(sfreq)){
            sub.freq <- freq[[sfreq]]
            # calculate the exposure. Requires concentration, freq is optional
            exposed <- calculate_exposure(sub.freq, sub.conc, smp, nsim = nsim, intake = intake, pathway_codes = pathway_codes)
            # update the object at this position
            ps.freq <- append(ps.freq, list(exposed))
          }
        }
      }
    }
  }

  # give back the updated behavior data object
  return(ps.freq)
}

bayesian_environmental_estimates <- function(conc, nburn=1000, niter=10000, thin=1, parallel = T, nc=detectCores()) {
  # Run bayesian model on the environmental data collected.  this will be run for each
  # neighborhood, age, and sample combination.  Warning: Could take quite a while.
  # Future development: Way of backgrounding this?
  calcul <- paste(nburn,niter,thin,sep="|")

  # environmental samples
  tomonitor <- c("mu","sigma")
  if (parallel) {
    func <- partial(mclapply, mc.cores=nc)
  } else {
    func <- lapply
  }

  conc <- func(1:length(conc), function(k) {
    log_ec<-log10(as.numeric(conc[[k]]$data))
    env_data<-list(lnconc=log_ec,N=length(log_ec))

    modelpos <- jags.model(file="model/env_model.jags.orig",data=env_data,n.chains=3);
    update(modelpos,n.burn=nburn);
    env_mcmcpos <- coda.samples(modelpos,tomonitor,n.iter=niter,thin=thin);
    #Bayesian estimators of mu and sigma
    mu <-summary(env_mcmcpos)$statistics[1,1]
    sigma <-summary(env_mcmcpos)$statistics[2,1]

    append(conc[[k]], list('mu' = mu, 'sigma' = sigma))

  })

  return(conc)
}

bayesian_behavior_estimates <- function(freq, nburn=1000, niter=10000, thin=1,
                                        cut_point = list('times' = c(0, 5, 10), 'days' = c(0, 3, 6)),
                                        init_freq = list('times' = c(NA, 2, 7, 12), "days" = c(NA, 2, 5, 7), 'r' = 1, 'p' = 0.2),
                                        parallel=T,
                                        nc= detectCores()) {
  # Run bayesian model on the behavior data collected.  this will be run for each
  # neighborhood, age, and sample combination.  Warning: Could take quite a while.
  # Future development: Way of backgrounding this?

  bemonitor <- c("p","r")
  calcul <- paste(nburn,niter,thin,sep="|")

  if (parallel) {func <- partial(mclapply, mc.cores=nc)} else {func <- lapply}

  freq <- func(1:length(freq), function(k) {
    print(k)
    # set up the data for analysis to be passed to jags
    freq_be0=freq[[k]]$data
    freq_be<-freq_be0[which(freq_be0>=0)]
    #initial values
    init_be<-as.numeric(rep(0,length(freq_be)))
    init_be[which(freq_be==1)]<-1
    init_be[which(freq_be==2)]<-2
    init_be[which(freq_be==3)]<-3

    if (freq[[k]]$s=='dw'){
      cutpoint<- cut_point$days
      init_freq_be<-as.numeric(rep( init_freq$days[1],length(freq_be)))
      init_freq_be[which(freq_be==1)]<- init_freq$days[2]
      init_freq_be[which(freq_be==2)]<- init_freq$days[3]
      init_freq_be[which(freq_be==3)]<- init_freq$days[4]
    } else {
      cutpoint<- cut_point$times
      init_freq_be<-as.numeric(rep( init_freq$times[1],length(freq_be)))
      init_freq_be[which(freq_be==1)]<- init_freq$times[2]
      init_freq_be[which(freq_be==2)]<- init_freq$times[3]
      init_freq_be[which(freq_be==3)]<- init_freq$times[4]
    }

    be_data<-list(select=freq_be,N=length(freq_be),cut=cutpoint)
    init<-list(freq=init_freq_be,r= init_freq$r,p= init_freq$p)

    # Jags model runs
    modelpos <- jags.model(file="model/be_model.jags.orig",data=be_data,n.chains=3,inits=init)
    update(modelpos,n.burn=nburn)
    cat('Coda Samples\n\n')
    be_mcmcpos <- coda.samples(modelpos, bemonitor, n.iter=niter, thin=thin)

    # Extract results
    # Bayesian estimators of p and r
    if (all(freq_be %in% c(0,1))){
      p<-summary(be_mcmcpos)$quantiles[1,3]
      r<-summary(be_mcmcpos)$quantiles[2,3]
    } else {
      p <-summary(be_mcmcpos)$statistics[1,1]
      r <-summary(be_mcmcpos)$statistics[2,1]
    }

    append(freq[[k]], list('p' = p, 'r' = r))

  })
  return(freq)
}

calculate_exposure <- function(behavior_data, concentration_data, smp, nsim = 1000, intake = configure$intake, pathway_codes = configure$pathway_codes) {
  # used for people plot generation.  this is for a single pathway
  # and assumes the data are already subset to the appropriate level
  intake %<>% as.matrix()
  e <- rep(NA, nsim)
  f <- rep(NA, nsim)
  risk <- rep(NA, nsim)

  # values applied based on sample and age
  #intake<-array(c(0.0006,1,10.43,0.0154,0.037,0.0006,0.034,NA,0.0499,1,
  #                0.01,0.5,4.14,0.2042,0.2042,0.01,0.034,NA,0.09975,0.5),c(10,2)) #need to input this information!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  rownames(intake) <- c('d', 'p', 'dw', 'o', 's',
                        'f', 'l', 'pa', 'bw','sf')
  colnames(intake) <- c("a", "c")

  # simulate some numbers
  for (m in 1:nsim){
    # is it necessary for this to be in a loop?
    e[m] <- rnorm(1, concentration_data$mu, concentration_data$sigma)
    if (smp %in% unlist(pathway_codes[c('p','f','l','bw','sf')])) {
      f[m] <- round(rnbinom(1, size= behavior_data$r, prob= behavior_data$p)/7*30)
    } else if (smp==pathway_codes['dw']) {
      f[m] <- min(round(rnbinom(1, size= behavior_data$r, prob= behavior_data$p)/7*30),30)
    } else {
      f[m] <- rnbinom(1, size= behavior_data$r, prob= behavior_data$p)
    }
    risk[m] <- f[m]* (10^e[m]) * intake[behavior_data$s, behavior_data$pop]
  }

  non0 <- function(mc){
    tmp <- mc; tmp[!(tmp>0)] <- NA
    return(tmp)
  }

  n <-(1-length(which(f==0))/nsim)*100;
  dose <-log10(mean(non0(risk),na.rm=TRUE))
  # add the percent exposure and dose information to the behavior data
  behavior_data <- append(behavior_data, list('n' = n, 'dose' = dose))

  # give the updated object back
  return(behavior_data)

}


make_spt_sample <- function(sample, es, mf, mst, enrichment, dna, pcr){

  sample <- filter(sample, col_neighborhood=="1", col_id!="DWa1001(UF)", col_id!="DWa1002(UF)")
  sample <- subset(sample, select=-c(subscriberid, simserial, deviceid))
  
  sample$col_id <- with(sample, make.unique(as.character(col_id)))
  sample$col_id_val <- with(sample, make.unique(as.character(col_id_val)))
  sample$col_sample_type <- as.numeric(sample$col_sample_type)

  es$col_id <- with(es, make.unique(as.character(col_id)))

  mf$lab_id_val <- with(mf, make.unique(as.character(lab_id_val)))
  mf$lab_id <- with(mf, make.unique(as.character(lab_id)))
  
  pathway_codes = list('d' = 1, 'p' = 2, 'dw' = 3, 'o' = 4, 's' = 5, 'f' = 6, 'l' = 7,
                       'pa' = 8, 'bw' = 9, 'sf' = 10, 'pl'=11, 'ms'=12, 'fb'=99, 'nc'=98, 'pc'=97)
  pathway_labels =list('d' = 'Drain Water', 'p' = 'Produce', 'dw' = 'Municipal and Piped Water', 'o' = 'Ocean Water',
                       's' = 'Surface Water', 'f' = 'Flood Water', 'l' = 'Public Latrine',
                       'pa' = 'Particulate', 'bw' = 'Bathing Water', 'sf' = 'Street Food',
                       'pl' = 'Pooled Latrine', 'ms' = 'Moore Swab', 'fb' = 'Field Blank',
                       'nc' = 'Negative Control', 'pc' = 'Positive Control')
  neighbs = list("Neighborhood 1" = 1)
  
  
  ##### importing mst data #####
  mst$lab_id <- with(mst, make.unique(as.character(lab_id)))
  
  ##### importing enrichment data #####
  enrichment$lab_id <- with(enrichment, make.unique(as.character(lab_id)))
  
  ###### importing dna data #####
  dna$lab_id <- with(dna, make.unique(as.character(lab_id)))
  
  ##### importing pcr data #####
  pcr$lab_id <- with(pcr, make.unique(as.character(lab_id)))
  
  
  ##### cleaning all id's of random spaces #####
  sample$col_id <- sub(' ', '', sample$col_id)
  es$col_id <- sub(' ', '', es$col_id)
  mf$lab_id <- sub(' ', '', mf$lab_id)
  mst$lab_id <- sub(' ', '', mst$lab_id)
  enrichment$lab_id <- sub(' ', '', enrichment$lab_id)
  dna$lab_id <- sub(' ', '', dna$lab_id)
  pcr$lab_id <- sub(' ', '', pcr$lab_id)
  
  
  
  
  #this is renaming the drinking water from hood 2 that was mislabeled
  dwrow <- which(grepl("DW2002", sample$col_id))
  sample$col_id[dwrow] <- "DWa2005"
  dwrow <- which(grepl("DW2002", mf$lab_id))
  mf$lab_id[dwrow] <- "DWa2005"
  dwrow <- which(grepl("DW2002", mst$lab_id))
  mst$lab_id[dwrow] <- "DWa2005"
  dwrow <- which(grepl("DW2002", dna$lab_id))
  dna$lab_id[dwrow] <- "DWa2005"
  dwrow <- which(grepl("DW2002", pcr$lab_id))
  pcr$lab_id[dwrow] <- "DWa2005"
  dwrow <- which(grepl("DW2002", enrichment$lab_id))
  enrichment$lab_id[dwrow] <- "DWa2005"
  
  
  # merging SPT samples
  
  #NOTE FOR CASEY: NEED TO FIND WAY TO ONLY KEEP ONE ENTRY WHEN THERE ARE DUPLICATE SAMPLE IDS
  
  
  ##### Merging SPT Sample Files
  spt_sample5 <- merge(sample, mf, by.x="col_id", by.y="lab_id", all.x=TRUE)
  spt_sample4 <- merge (spt_sample5, mst, by.x="col_id", by.y="lab_id", all.x=TRUE)
  spt_sample3 <- merge (spt_sample4, dna, by.x="col_id", by.y="lab_id", all.x=TRUE)
  spt_sample2 <- merge (spt_sample3, enrichment, by.x="col_id", by.y="lab_id", all.x=TRUE)
  spt_sample <- merge (spt_sample2, pcr, by.x="col_id", by.y="lab_id", all.x=TRUE)
  
  
  spt_sample$MF_done <- 0
  for(i in 1:nrow(spt_sample)){
    if(is.na(spt_sample$lab_1_dil_tested[i])){
      spt_sample$MF_done[i]=0
    }
    else{
      spt_sample$MF_done[i]=1
    }
  }
  
  spt_sample$pcr_done <- 0
  for(h in 1:nrow(spt_sample)){
    if(is.na(spt_sample$lab_pcr_date.y[h])){
      spt_sample$pcr_done[h]=0
    }
    else{
      spt_sample$pcr_done[h]=1
    }
  }
  
  spt_sample$pcr_valid <- 0
  for(a in 1:nrow(spt_sample)){
    if(is.na(spt_sample$lab_pcr[a])){
      spt_sample$pcr_valid[a]=NA
    }
    else if(spt_sample$lab_pcr[a]=="0"){
      spt_sample$pcr_valid[a]=0
    }
    else{
      spt_sample$pcr_valid[a]=1
    }
  }
  
  spt_sample$dna_extracted <- 0
  for(a in 1:nrow(spt_sample)){
    if(is.na(spt_sample$lab_dna_yn[a])){
      spt_sample$dna_extracted[a]=0
    }
    else if(spt_sample$lab_dna_yn[a]=="0"){
      spt_sample$dna_extracted[a]=0
    }
    else{
      spt_sample$dna_extracted[a]=1
    }
  }
  
return(spt_sample)
}



make_es_sample <- function(es, mf, mst, enrichment, dna, pcr){


  es$col_id <- with(es, make.unique(as.character(col_id)))

  mf$lab_id_val <- with(mf, make.unique(as.character(lab_id_val)))
  mf$lab_id <- with(mf, make.unique(as.character(lab_id)))
  
  pathway_codes = list('d' = 1, 'p' = 2, 'dw' = 3, 'o' = 4, 's' = 5, 'f' = 6, 'l' = 7,
                       'pa' = 8, 'bw' = 9, 'sf' = 10, 'pl'=11, 'ms'=12, 'fb'=99, 'nc'=98, 'pc'=97)
  pathway_labels =list('d' = 'Drain Water', 'p' = 'Produce', 'dw' = 'Municipal and Piped Water', 'o' = 'Ocean Water',
                       's' = 'Surface Water', 'f' = 'Flood Water', 'l' = 'Public Latrine',
                       'pa' = 'Particulate', 'bw' = 'Bathing Water', 'sf' = 'Street Food',
                       'pl' = 'Pooled Latrine', 'ms' = 'Moore Swab', 'fb' = 'Field Blank',
                       'nc' = 'Negative Control', 'pc' = 'Positive Control')
  neighbs = list("Neighborhood 1" = 1)
  
  
  ##### importing mst data #####

  mst$lab_id <- with(mst, make.unique(as.character(lab_id)))
  
  ##### importing enrichment data #####

  enrichment$lab_id <- with(enrichment, make.unique(as.character(lab_id)))
  
  ###### importing dna data #####

  dna$lab_id <- with(dna, make.unique(as.character(lab_id)))
  
  ##### importing pcr data #####

  pcr$lab_id <- with(pcr, make.unique(as.character(lab_id)))
  
  
  ##### cleaning all id's of random spaces #####
  sample$col_id <- sub(' ', '', sample$col_id)
  es$col_id <- sub(' ', '', es$col_id)
  mf$lab_id <- sub(' ', '', mf$lab_id)
  mst$lab_id <- sub(' ', '', mst$lab_id)
  enrichment$lab_id <- sub(' ', '', enrichment$lab_id)
  dna$lab_id <- sub(' ', '', dna$lab_id)
  pcr$lab_id <- sub(' ', '', pcr$lab_id)
  
  
  
  
  #this is renaming the drinking water from hood 2 that was mislabeled
  dwrow <- which(grepl("DW2002", sample$col_id))
  sample$col_id[dwrow] <- "DWa2005"
  dwrow <- which(grepl("DW2002", mf$lab_id))
  mf$lab_id[dwrow] <- "DWa2005"
  dwrow <- which(grepl("DW2002", mst$lab_id))
  mst$lab_id[dwrow] <- "DWa2005"
  dwrow <- which(grepl("DW2002", dna$lab_id))
  dna$lab_id[dwrow] <- "DWa2005"
  dwrow <- which(grepl("DW2002", pcr$lab_id))
  pcr$lab_id[dwrow] <- "DWa2005"
  dwrow <- which(grepl("DW2002", enrichment$lab_id))
  enrichment$lab_id[dwrow] <- "DWa2005"
  
  
  # merging SPT samples
  
  #NOTE FOR CASEY: NEED TO FIND WAY TO ONLY KEEP ONE ENTRY WHEN THERE ARE DUPLICATE SAMPLE IDS
  
  
  ##### Merging SPT Sample Files
  es_sample5 <- merge(es, mf, by.x="col_id", by.y="lab_id", all.x=TRUE)
  es_sample4 <- merge (es_sample5, mst, by.x="col_id", by.y="lab_id", all.x=TRUE)
  es_sample3 <- merge (es_sample4, dna, by.x="col_id", by.y="lab_id", all.x=TRUE)
  es_sample2 <- merge (es_sample3, enrichment, by.x="col_id", by.y="lab_id", all.x=TRUE)
  es_sample <- merge (es_sample2, pcr, by.x="col_id", by.y="lab_id", all.x=TRUE)
  
  
  es_sample$MF_done <- 0
  for(i in 1:nrow(es_sample)){
    if(is.na(es_sample$lab_1_dil_tested[i])){
      es_sample$MF_done[i]=0
    }
    else{
      es_sample$MF_done[i]=1
    }
  }
  
  es_sample$pcr_done <- 0
  for(h in 1:nrow(es_sample)){
    if(is.na(es_sample$lab_pcr_date.y[h])){
      es_sample$pcr_done[h]=0
    }
    else{
      es_sample$pcr_done[h]=1
    }
  }
  
  es_sample$pcr_valid <- 0
  for(a in 1:nrow(es_sample)){
    if(is.na(es_sample$lab_pcr[a])){
      es_sample$pcr_valid[a]=NA
    }
    else if(es_sample$lab_pcr[a]=="0"){
      es_sample$pcr_valid[a]=0
    }
    else{
      es_sample$pcr_valid[a]=1
    }
  }
  
  es_sample$dna_extracted <- 0
  for(a in 1:nrow(es_sample)){
    if(is.na(es_sample$lab_dna_yn[a])){
      es_sample$dna_extracted[a]=0
    }
    else if(es_sample$lab_dna_yn[a]=="0"){
      es_sample$dna_extracted[a]=0
    }
    else{
      es_sample$dna_extracted[a]=1
    }
  }
  
  return(es_sample)
}
