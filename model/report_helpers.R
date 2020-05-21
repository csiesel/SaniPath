# Report Helpers
library(rlist)
library(plyr)
library(tidyr)
library(tidyselect)
library(dplyr)
# REPORT ============================================================

compute_report <- function(params = list(city_name = 'Atlanta, GA',
                                         lab_name = 'Bill Nye, Inc',
                                         survey_team_name = 'SaniRath, Inc',
                                         lab_MF = F,
                                         language = "English",
                                         household_data = data.frame(),
                                         school_data = data.frame(),
                                         community_data = data.frame(),
                                         sample_data = data.frame(),
                                         lab_data = data.frame(),
                                         ps_freq = list(),
                                         pathway_codes = list(),
                                         pathway_labels= list(),
                                         neighborhood_mapping = list(),
                                         populations = list(adults=T, children=T),
                                         interventions = data.frame(),
                                         freq_thresh=50),
                           out_dir = './',
                           out_file='report.docx',
                           output_format = 'word_document',
                           temp=T) {
  # Compute all of the necessary parameters for the report
  # generation
  # ___________________________________
  # city_name => city where the tool was deployed
  # lab_name => lab processing the samples
  # start_date => the day the tool was deployed
  # lab_MF => boolean of whether Membrane Filtration was used for lab
  # pathways => a character vector of the pathways used
  # language => language of the surveys (from configuration of tool)
  # household_data => data collected at households
  # school_data => data collected at schools
  # community_data => data collected at community
  # sample_data => data collected for sample collection
  # ps_freq => results from exposure calcluations
  # neighborhood_mapping => named list of Neighborhood text = coded value
  # pathway_codes => named list of pathway codes = coded value
  # pathway_labels => named list of pathway codes = pathway label name
  # freq_thresh => the threshold for determining if something is dominant
  # ___________________________________
  # returns a list with named attributes related to the report
  # document params
  if (temp) {
    out_dir = tempdir()
  }
  # Sys.setenv(HOME=tempdir())
  gc()
  options <- c('word_document', 'pdf_document')
  if (!(output_format %in% options)) stop(sprintf("output_format invalid. options: %s", paste(options, collapse=', ')))

  attach(params)
  # Set up the pathways param
  pathways = pathway_labels %>% unlist()
  params$pathways = make_sentence_list(pathways)
  rm(pathways)

  # make a dataframe of results for use throughout the report
  params$rpt_results <- report_results(ps_freq, freq_thresh)

  params$pop_rpt_results <- params$rpt_results %>% filter(age == ifelse(populations$adults==TRUE,'Adults','Children'))
  # make the summary pathway information for executive summary
  params$top_pathways <- summary.top_pathways(params$rpt_results, top_n=3)

  # discussion
  params$discussion_chunks <- determine_discussion(params$pop_rpt_results, n = 3)

  # make the report specific discussion paragraphs
  params$discussion_blocks <- discussion.blocks(params$pop_rpt_results, params)

  # build the end date based on last submission of any form
  dt <- . %>% as.character() %>% as.Date()
  get_col <- . %>% .[,grep('submission_time', names(.))] %>% as.character()
  dates <- c(get_col(household_data),
             get_col(school_data),
             get_col(community_data),
             get_col(lab_data),
             get_col(sample_data))
  params$end_date <- max(as.Date(as.character(dates), origin='1970-01-01'), na.rm=T)
  params$start_date <- min(as.Date(as.character(dates), origin='1970-01-01'), na.rm=T)
  rmarkdown::render('report.Rmd',output_file=out_file, output_dir = out_dir, params= params, output_format=output_format)
  return(file.path(out_dir, out_file))

}

# Summary/Methodology Info -----

behavior_table <- function(household_data=NULL, school_data=NULL, community_data=NULL, neighborhood_mapping) {
  # Make a tabluation of the results by neighborhood
  count_data <- function(df, form_name) {
    if (!is.null(df) & nrow(df) > 0) {
      x <- as.data.frame(table(df[,grep('_neighborhood$', names(df))]), stringsAsFactors=F)
      x$data <- form_name
    } else {
      x <- data.frame(stringsAsFactors = F)
    }
    return(x)
  }

  results <- bind_rows(
    count_data(household_data, 'Household'),
    count_data(community_data, 'Community'),
    count_data(school_data, 'School')
  )

  n_map <- names(neighborhood_mapping)
  names(n_map) <- neighborhood_mapping %>% unlist()
  results$Var1 %<>% as.character() %>% revalue(n_map)

  results %<>% spread(data, Freq)
  names(results)[1] <- 'Neighborhood'
  results[is.na(results)] <- 0
  totals <- results %>% select(-Neighborhood) %>%
    summarise_all(sum, na.rm=T) %>%
    mutate(Neighborhood="Total")
  results %<>% bind_rows(totals)

  return(results)
}

sample_table <- function(sample_data, neighborhood_mapping, pathway_codes, pathway_labels) {
  # Make a table of the different samples collected
  x <- as.data.frame(table('Neighborhood' = as.character(sample_data$col_neighborhood),
                           'Sample' = as.character(sample_data$col_sample_type)), stringsAsFactors = F)
  n_map <- names(neighborhood_mapping)
  names(n_map) <- neighborhood_mapping %>% unlist()
  x$Neighborhood %<>% revalue(n_map, warn_missing = F)

  p_map <- sapply(names(pathway_codes), function(x) pathway_labels[[x]])
  names(p_map) <- unlist(pathway_codes)
  x$Sample %<>% revalue(p_map, warn_missing = F)

  x %<>% spread(Sample, Freq)
  totals <- x %>% select(-Neighborhood) %>%
    summarise_all(sum, na.rm=T) %>%
    mutate(Neighborhood="Total")
  x %<>% bind_rows(totals)


  # add missing pathways
  labels <- unlist(pathway_labels)

  missing_pathways <- labels[!(labels %in% names(x))]
  if (length(missing_pathways) > 0) {
    for (col in missing_pathways) {
      x[[col]] <- 0
    }
  }

  return(x)
}


summary.top_pathways <- function(rpt_results, top_n=3) {
  # find the top pathways for population groups
  # used for sentence creation in the report
  # ____________________________
  # rpt_results => the df of results by neighborhood, pathway, and population
  # top_n => the number of top pathways to find
  # ____________________________
  # returns a named list with 'adults' and 'children' attributes
  #   which contain pathway names

  r <- rpt_results %>% group_by(age, pathway) %>%
    summarise(n_dom = sum(dominant),
              nonlogE = sum(nonlogE)) %>%
    arrange(desc(age), desc(n_dom), desc(nonlogE))

  top_pathways <- list()
  for (p in unique(rpt_results$age)) {
    top_pathways[[tolower(p)]] <- r %>% filter(age == p) %>% pull(pathway) %>% head(top_n)
  }

  return(top_pathways)

}

# Report result preparation ----

report_results <- function(ps.freq, freq_thresh = 50) {
  # summarize results by neighborhood an age group
  # freq_thresh is used to determine the high/low frequency
  # pathways.  default is 50%
  # this will return a nested list of neighborhoods by age
  # with the dominant pathways identified and a summary table

  neighborhoods <- unique(names(list.names(ps.freq, neighborhood)))
  age <- sapply(ps.freq, function(x) x$age) %>% unlist() %>% unique()

  pathway_results <- data.frame()
  for (i in neighborhoods) {
    # look at each neighborhood
    # then at each age
    # I don't remember why we had to store the data in lists
    # changing that to one dataframe now for ease of use. if
    # this were to all be refactored, it could be much more
    # efficient
    for (a in age) {
      subset <- ps.freq[list.which(ps.freq, neighborhood == i && age == a)]
      subset_result <- report_pathwayResult(subset, freq_thresh)
      pathway_results %<>% bind_rows(subset_result$pathway_table)
    }
  }

  return(pathway_results)

}


# Discussion ----
# Uses the results from report_results(ps_freq)

discussion.dominant_pathway_table <- function(rpt_results) {
  # Make the table with dominant pathways identified
  # ________________________
  # rpt_results => the results from report_results()
  # ________________________
  # returns a dataframe formatted for printing the dominant pathways
  rslt <- rpt_results %>%
    group_by(neighborhood, age) %>%
    filter(dominant == 1) %>%
    summarize(dominant = paste0(pathway, collapse=', \n')) %>%
    spread(age, dominant) %>%
    rename('Neighborhood' = 'neighborhood') %>%
    as.data.frame()

  return(rslt)
}

discussion.mark_exposure<- function(rpt_results) {
  # based on a ranking of total exposure to be determined by summing exposure
  # for all pathways within a neighborhood, E = sum(non-log dose*percent exposed)
  # for all pathways within one neighborhood= sum(Population Dose for all
  # pathways within one neighborhood)
  # this marks max and min exposure based on non-log10 dose * percent exposed (nonlogE)
  return(rpt_results %>%
    group_by(neighborhood, age) %>%
    summarise(exp = sum(nonlogE)) %>%
    ungroup() %>%
    mutate(maxE = exp == max(exp),
           minE = exp == min(exp)))
}

discussion.high_exposure_neighborhood <- function(rpt_results) {
  # Utility to show only high_exposure_neighborhood
  # filters on max column created by discussion.mark_exposure()
  # ___________________________
  # rpt_results => dataframe of neighborhood,pathway, population results
  # ___________________________
  # returns a filtered dataframe for just pathways and populations where
  # neighborhood and population is the max
  exposure <- discussion.mark_exposure(rpt_results) %>%
    filter(maxE)

  hdn <- rpt_results %>%
    filter(neighborhood == exposure$neighborhood & age == exposure$age)

  return(hdn)
}

discussion.high_exposure_neighborhood_table_data <- function(rpt_results) {
  # Utility to show only high_exposure_neighborhood
  # filters on max column created by discussion.mark_exposure()
  # ___________________________
  # rpt_results => dataframe of neighborhood,pathway, population results
  # ___________________________
  # returns a filtered dataframe for just pathways and populations where
  # neighborhood and population is the max
  exposure <- discussion.mark_exposure(rpt_results) %>%
    filter(maxE)

  hdn2 <- rpt_results%>%
    filter(neighborhood == exposure$neighborhood)

  return(hdn2)
}


discussion.high_exposure_neighborhood_max <- function(hdn) {
  # Filter high exposure neighborhood even further to just the
  # max pathway and population
  # ___________________________
  # hdn => high_exposure_neighborhood dataframe
  # ___________________________
  # returns a one row dataframe where exposure is max
  return(hdn %>% filter(nonlogE == max(nonlogE)))
}

discussion.low_exposure_neighborhood <- function(rpt_results) {
  # Utility to show only low_exposure_neighborhood
  # filters on max column created by discussion.mark_exposure()
  # ___________________________
  # rpt_results => dataframe of neighborhood,pathway, population results
  # ___________________________
  # returns a filtered dataframe for just pathways and populations where
  # neighborhood and population is the min
  exposure <- discussion.mark_exposure(rpt_results) %>%
    filter(minE)

  ldn <- rpt_results %>%
    filter(neighborhood == exposure$neighborhood & age == exposure$age)

  return(ldn)
}
discussion.low_exposure_neighborhood_min <- function(ldn) {
  # Filter low exposure neighborhood even further to just the
  # max pathway and population
  # ___________________________
  # ldn => low_exposure_neighborhood dataframe
  # ___________________________
  # returns a one row dataframe where exposure is min
  return(ldn %>% filter(nonlogE == max(nonlogE)))
}

discussion.exposure_plot <- function(exposure_data, lab_MF=F) {
  # Make a set of people plots based on filtered information
  # used for high and low exposure neighborhood plots
  # __________________________
  # exposure_data => data from discussion.low_exposure_neighborhood
  #     or discussion.high_exposure_neighborhood
  # lab_MF => boolean if lab method was membrane filtration or not
  # __________________________
  # returns a list of ggplot objects
  exposure_data %<>%
    rename('sample' = 'pathway')
  plots <- lapply(1:nrow(exposure_data),  function(x) {
    x <- as.list(exposure_data[x,])
    make_pplplot(x,
                 title= x$sample,
                 subtitle = sprintf('%s exposed\n%s %s',
                                    scales::percent(round(x$n/100, 3)),
                                    paste('Log10',round(if_else(is_null(x$dose), 0, x$dose), 1)),
                                    lab_label(lab_MF)),
                 caption = '',
                 lab_MF = lab_MF) +
      theme(plot.title = element_text(size=8),
            plot.subtitle = element_text(size=6))
  })


  # determine size of plots and grid arrangement
  # trying to be as square as possible
  n_plots <- length(plots)
  if (n_plots > 5) {
    ncols <- ceiling(n_plots / 2 )
    nrows <- 2
  } else {
    ncols = n_plots
    nrows=1
  }
  return(arrangeGrob(grobs=plots,
               nrow=nrows,
               ncol=ncols,
               left=sprintf("%s, %s",
                            unique(exposure_data$neighborhood),
                            unique(exposure_data$age)),
               bottom =  grid::textGrob("Note - Dose values are displayed in log scale", gp=grid::gpar(fontsize=6))
               ))
}

discussion.high_exposure_neighborhood_plot <- function(hdn, lab_MF=F) {
  # a wrapper around discussion.exposure_plot
  discussion.exposure_plot(hdn, lab_MF)
}

discussion.low_exposure_neighborhood_plot <- function(ldn, lab_MF=F) {
  # a wrapper around discussion.exposure_plot
  discussion.exposure_plot(ldn, lab_MF)
}

discussion.high_exposure_neighborhood_table <- function(hdn2, lab_MF) {
  # make the high exposure neighborhood table (Table 2)
  # uses output from discussion.high_exposure_neighborhood()
  # _________________________
  # hdn2 => high_exposure_neighborhood() output
  # lab_MF => whether Membrane filtration was used for labs
  # _________________________
  # returns a formatted table ready for printing

  first_columns <- c('Age Group', 'Metric')

  neighborhood_table <- bind_rows(hdn2 %>% group_by(age) %>%
                                    # make pathway columns with values of E
                                    spread(pathway, E) %>%
                                    # We're looking at numbers, so collapse those columns down
                                    summarise_if(is_double, sum, na.rm=T) %>%
                                    mutate(Metric=lab_label(lab_MF)),
                                  hdn2 %>% group_by(age) %>%
                                    spread(pathway, n) %>%
                                    summarise_if(is_double, sum, na.rm=T) %>%
                                    mutate(Metric="Percent Exposed")
  ) %>% # two numbers are joined here
    select(-freq, -n, -high_dose, -dose, -dominant, -max_dominant, -E) %>%
    # Fix the column ordering now
    select(age, Metric, names(.) %>% .[!(.%in% first_columns)]) %>%
    # Arrange it in ascending order by age
    arrange(age) %>%
    # blank out the Age label for Percent Exposed
    mutate(age= ifelse(Metric == "Percent Exposed", "", age)) %>%
    rename('Age Group' = 'age')

  return(neighborhood_table)
}

determine_discussion <- function(pop_rpt_results, neighborhood, n=3) {
  # take a df of report results from report_results()
  # and find up to n high dose pathways, combining
  # children and adults
  r <- pop_rpt_results %>% group_by(pathway, p_code) %>%
    summarise(n_dom = sum(dominant),
              nonlogE = sum(nonlogE),
              n_neighborhoods = n_distinct(neighborhood),
              high_freq = n_distinct(neighborhood[freq == 1]),
              low_freq = n_distinct(neighborhood[freq == 0]),
              high_dose = sum(dominant == 1),
              low_dose = sum(dominant == 0)) %>%
    arrange(desc(n_dom), desc(nonlogE))

  pop_rpt_results <- r
  if(nrow(pop_rpt_results)>n){
    pop_rpt_results %<>% .[1:n,]
  }

  return(pop_rpt_results)

}



discussion.blocks <- function(pop_rpt_results, params, n=3, disc_dir='report_sections/discussion_paragraphs/') {
  # Generate the discussion paragraphs
  # will pull file based on pathway code in the file name
  # ie files named discussion_X.Rmd where X is a valid pathway code already found in the data
  # ______________________________
  # rpt_results => the report results in dataframe form
  # params => report parameters passed down from compute_report()
  #     containing information about lab_MF etc.  These are then
  #     passed to each child block while rendering
  # n => the number of discussion paragraphs to make
  # disc_dir => the directory location of the discussion child paragraphs
  # ______________________________
  # returns a list of rendered markdown text
  #   use cat() in a code chunk with results ='asis' to render appropriately

  discussion_chunks <- determine_discussion(pop_rpt_results, n=n)
  discussion_chunks$discussion_para <- paste0(disc_dir, "discussion_", discussion_chunks$p_code, '.Rmd')
  disc_blocks <- apply(discussion_chunks, 1, function(x) {
    params$block_vals <- as.list(x)
    child <- paste0(readLines(as.character(x['discussion_para'])), collapse='\n')
    knitr::knit(text = child)
  })
  return(disc_blocks)
}


# Interventions ----
interventions.make_table <- function(discussion_chunks, interventions, pathway_labels) {
  # make the interventions table for the implications section
  # _______________________________
  # discussion_chunks => the filtered report data from determine_discussion()
  # interventions => a dataframe of interventions
  # pathway_labels => these overwrite whatever labels are in the interventions
  #   dataframe to ensure they are correct and up to date based on current settings
  # _______________________________
  # returns a table ready for printing


  specific_interventions <- interventions %>%
    # filter interventions based on pathway codes found in discussion_chunks
    filter(pathway_code %in% discussion_chunks$p_code) %>%
    group_by(pathway_code, intervention_domain) %>%
    mutate(id=1:n()) %>% # spread wants unique columns so this gets around that
    # data are in long format, so spread to wide
    spread(intervention_domain, intervention) %>%
    select(-id, -pathway_label) %>% ungroup()# we don't need that id column any more

  specific_interventions %<>% left_join(data.frame(pathway_code = names(pathway_labels),
                                                   pathway_label = unlist(pathway_labels),
                                                   stringsAsFactors=F), by='pathway_code')

  # format and relabel for proper printing
  specific_interventions %<>% select(pathway_label, private_domain, public_domain) %>%
    #  drop any duplicated row labels on the pathway column so it appears to be grouped
    mutate(pathway_label = ifelse(duplicated(pathway_label), ' ', pathway_label)) %>%
    rename(`Pathway` = pathway_label,
           `Private Domain` = `private_domain`,
           `Public Domain` = `public_domain`)


  return(specific_interventions)
}


interventions.short_list <- function(discussion_chunks, interventions, pathway_labels) {
  # Take a data frame of results and join with the interventions
  # data frame.  Make a bulleted markdown list as an output.
  # __________________________
  # discussion_chunks => filtered report results from determine_discussion()
  # interventions => a dataframe of interventions
  # pathway_labels => these overwrite whatever labels are in the interventions
  #   dataframe to ensure they are correct and up to date based on current settings
  # _______________________________
  # returns a character vector of lines ready for printing with cat()

  # make a utility to add bulleting markdown formatting to the text
  bullets <- . %>% .[!is.na(.)] %>% paste0("* ", ., '\n')


  specific_interventions <- interventions %>%
    filter(pathway_code %in% discussion_chunks$p_code) %>%
    group_by(pathway_code, intervention_domain) %>%
    mutate(id=1:n()) %>% # spread wants unique columns so this gets around that
    spread(intervention_domain, intervention) %>%
    select(-id) %>% ungroup()# we don't need that id column any more

  # get the final column that has the combined interventions and add bullets to them
  #
  specific_interventions %<>% select(pathway_label, combined) %>%
    pull(combined) %>% .[!is.na(.)] %>% bullets()


  return(specific_interventions)
}


# Utilities -----
sp_table <- function(tbl, width=20, label_columns=1, max_width=3) {
  # split a table into multiple columns for printing in markdown
  # ___________________________
  # tbl => a dataframe-like object
  # width => the width in cm(?)
  # label_columns => an integer or vector of ints to repeat if the
  #   table is split
  # max_width => the max number of pathways to display before
  #   splitting the table
  # __________________________
  # returns a list of tables ready for printing
  #   and cats the results to the console (which renders during knitting)

  n_col <- ncol(tbl)

  n_lbl <- length(label_columns) # the number of label columns, assumes they're all at the left side
  n_split = ceiling((n_col-n_lbl) / max_width) # the total number of splits we need to do
  n_cols = ceiling((n_col-n_lbl)/n_split) # the number of columns we can afford in each split not including labels
  indices = data.frame(
                        starts = seq(n_lbl+1, n_col, by=n_cols), # the start indices of values
                        ends = seq(n_lbl+n_cols, n_lbl+ceiling((n_col-n_lbl)/n_cols)*n_cols, by=n_cols) # the end indices of values
  )
  indices$ends[which(indices$ends>n_col)]<-n_col
  tbls <- apply(indices, 1, function(j) cat(pander::pander(tbl[c(label_columns, j['starts']:j['ends'])],keep.line.breaks=T, split.cells=width, missing='')))


  return(tbls)
}

# A utility for making the proper lab label based on lab_MF
lab_label <- . %>% ifelse(., "CFU/Month E. coli", "MPN/Month E. coli")

behavioral_surveys_collected <- function(params) {
  labels <- c('household', 'school', 'community')
  test <- function(x) {nrow(x) > 0}
  test_result <- sapply(labels, function(x) test(params[[sprintf('%s_data', x)]]))
  return(make_sentence_list(labels[test_result]))
}

adults_and_children <- function(populations=list(adults=T, children=T)) {
  # a utility to insert 'adults', 'children' or 'adults and children' based on
  # report results and selected population
  k.pop <- which(names(populations)=='adults')
  populations <- suppressWarnings(as.logical(populations))
  if (all(populations)) {
    x <- "adults and children"
  } else {
    x <- ifelse(populations[k.pop], 'adults', 'children')
  }
  return(x)
}

adults_and_children_study <- function(params){
  # a utility to insert 'adults', 'children' or 'adults and children' based on
  # what populations were included in the overall study
  if (!is.null(params$top_pathways$adults)) {
    y <- "adults"
  }
  if (!is.null(params$top_pathways$children)) {
    y <- "children"
  }
  if (!is.null(params$top_pathways$adults) & !is.null(params$top_pathways$children)) {
    y <- "adults and children"
  }
  return(y)
}

household_community_meetings <- function(household_data=NULL, community_data=NULL) {
  # a utility to make phrases related to household and community meetings if
  # data are available
  test <- c(!is_empty(household_data),!is_empty(community_data))
  if (all(test)) {
    x <- 'households and community meetings'
  } else {
    x <- ifelse(test[1], 'households', 'community meetings')
  }
  return(x)
}

make_sentence_list <- function(x) {
  # take a vector of strings and collapse into one
  # adding and and commas where needed
  if (length(x) > 1) x[length(x)] %<>% paste("and ",. )
  collapse_sent <- ifelse(length(x) > 2, ', ', ' ')
  x_sent <- paste(x, collapse=collapse_sent)
  return(x_sent)
}


percentage_answered <- function(n_vector) {
  # a utility to convert a yes/no answer column into a percent answered
  n_vector %<>% as.numeric()
  val <- round(sum(n_vector[n_vector == 1], na.rm=T) / length(n_vector[!is.na(n_vector)]), 3)
  return(scales::percent(val))
}

proper_number <- function(number) {
  # If a number is less than 10, we can
  # insert the proper word, otherwise
  # return the number unchanged
  if (number < 10) {
    number = c('1' = 'one',
               '2' = 'two',
               '3' = 'three',
               '4' = 'four',
               '5' = 'five',
               '6' = 'six',
               '7' = 'seven',
               '8' = 'eight',
               '9' = 'nine')[as.character(number)]
  }
  return(number)
}

conditional_paragraph <- function(blockvals,
                                  intro_sentence = '',
                                 high_dose_high_freq = '',
                                 high_dose_mixed_freq = '',
                                 high_dose_low_freq = '',
                                 low_dose_high_freq = '',
                                 mixed_dose_mixed_freq = '',
                                 mixed_dose_high_freq = '',
                                 concluding_sentence = '') {
  # Wrapper around a case_when statement to make the discussion blocks easier
  # ____________________________________________________
  # blockvals => a list with the following attributes:
  #   - high_dose => a number
  #   - low_dose => a number
  #   - high_freq => a number
  #   - low_freq => a number
  # high_dose_high_freq => the sentence to use
  # high_dose_mixed_freq => the sentence to use
  # high_dose_low_freq => the sentence to use
  # low_dose_high_freq => the sentence to use
  # mixed_dose_mixed_freq => the sentence to use
  # mixed_dose_high_freq => the sentence to use
  # ____________________________________________________
  # returns a sentence
  dose_dom <- test_dominance(blockvals$high_dose, blockvals$low_dose)
  freq_dom <- test_dominance(blockvals$high_freq, blockvals$low_freq)

  mid_sent <- case_when(
    dose_dom == 'high' & freq_dom == 'high' ~ high_dose_high_freq,
    dose_dom == 'high' & freq_dom == 'mixed' ~ high_dose_mixed_freq,
    dose_dom == 'high' & freq_dom == 'low' ~ high_dose_low_freq,
    dose_dom == 'low' & freq_dom == 'high' ~ low_dose_high_freq,
    dose_dom == 'mixed' & freq_dom == 'mixed' ~ mixed_dose_mixed_freq,
    dose_dom == 'mixed' & freq_dom == 'high' ~ mixed_dose_high_freq
  )
  sentence <- paste(intro_sentence, mid_sent, concluding_sentence)
  return(sentence)
}

test_dominance <- function(high, low) {
  return(case_when(high > 0 & low == 0 ~ 'high',
                   high > 0 & low > 0 ~ 'mixed',
                   high == 0 & low > 0 ~ 'low',
                   TRUE ~ 'low')) # the default case if all else fails
}

pluralize <- function(word, n, add_text = '') {
  # Literally just adds an s at the end of a word if
  # n is greater than 1.  useful if the n_neighborhoods
  # or number of respondents can change.  add_text
  # sticks any addtional text at the end.  useful for
  # punctuation.
  return(paste0(word, ifelse(n > 1, "s", ""), add_text))
}



# Legacy -----
# Don't delete.  Newer code uses.
report_pathwayResult <- function(ps.freq, freq_thresh = 50) {
  # identify the dominant pathway(s) and
  # create a 2x2 table arranging pathways by dose and frequency
  # for each neighborhood and age group
  # intented use to take a list object of samples for a given neighborhood
  # and age
  # freq_thresh is used to determine the high/low frequency
  # pathways.  default is 50%

  # calculate an exposure score
  # E = log10( (10 ^ dose) * n/100)
  calc_exposure_score <- function(list_obj, freq_thresh ) {
    if (list_obj$dose == Inf || is.null(list_obj$dose)) list_obj$dose = 0
    E = log10( (10 ^ list_obj$dose) * list_obj$n/100)
    f = (list_obj$n >= freq_thresh)
    return(data.frame(t(c('E' = E,
                          'nonlogE' = (10 ^ list_obj$dose) * list_obj$n/100, # TODO: Is this correct?
                          'freq' = f,
                          'n' = list_obj$n,
                          'dose' = list_obj$dose))))
  }

  # calculate the dose and frequency score for each pathway
  pathway_results <- data.frame()
  for (i in ps.freq) {
    pathway_results <- rbind.fill(pathway_results, calc_exposure_score(i, freq_thresh))
  }

  # Counting Inf as Low Dose
  pathway_results$E[pathway_results$E == Inf] <- NA
  dose_threshold <- .5*(min(pathway_results$E, na.rm=T)+ max(pathway_results, na.rm=T))
  # create a data frame with binary high dose/high frequency variables for table arrangement
  pathway_results$high_dose <- as.numeric(pathway_results$E > 10 |
                                            pathway_results$E >= dose_threshold)
  # pathway_results$high_dose[is.na(pathway_results$high_dose)] <- 0

  # add the pathway names
  pathway_results$pathway <- unlist(lapply(ps.freq, function(a) a$sample))
  pathway_results$p_code <- unlist(lapply(ps.freq, function(a) a$s))

  pathway_results <- pathway_results[order(pathway_results$E, decreasing=T),]

  # reshape the data
  result_matrix <- matrix(NA, nrow=2, ncol=2)
  rownames(result_matrix) <- c("High Dose", "Low Dose")
  colnames(result_matrix) <- c('High Frequency', 'Low Frequency')
  # identify the dominant pathways

  pathway_results$dominant <- as.numeric(pathway_results$E >= (max(pathway_results$E, na.rm=T) - 1) | pathway_results$E >= 10)

  for (i in 1:nrow(pathway_results)) {
    # false (0) now needs to be 2 to indicate the row or column
    score <- pathway_results[i, c('high_dose', 'freq')]
    score[score == 0] <- 2

    score <- unlist(score)
    if (pathway_results$dominant[i] == 1 & !is.na(pathway_results$dominant[i])) {
      dom <- as.character("\\*")
    } else {
      dom <- ""
    }

    # now insert the pathway into that spot
    result_matrix[score[1], score[2]] <- paste0(c(result_matrix[score[1], score[2]], paste0(pathway_results[i, 'pathway'],dom)), collapse="\n")
  }
  result_matrix <- gsub('NA', '', result_matrix)
  result_matrix[is.na(result_matrix)] <- ""

  # Identify the most dominant pathway
  pathway_results$max_dominant <- as.numeric(pathway_results$E == max(pathway_results$E, na.rm=T))
  pathway_results$age <- sapply(ps.freq, function(x) x$age) %>% unlist() %>% unique()
  pathway_results$neighborhood <- sapply(ps.freq, function(x) x$neighborhood) %>% unlist() %>% unique()

  # max_dominant <- max_dominant[!is.na(max_dominant)]

  # max_dom_info <- ps.freq[[list.which(ps.freq, sample == max_dominant)]]



  return(list(#'max_dominant' = max_dom_info, # the highest dominant pathway
              'dominant' = pathway_results[pathway_results$dominant == 1 & !is.na(pathway_results$dominant), 'pathway'],
              'matrix' = result_matrix,
              'pathway_table' = pathway_results))

}

identify <- function(ps.freq, attribute='dose', FUN=max) {
  # Run a function on the exposure data
  # ______________________________________
  # ps.freq => calculated results from calculate_exposure
  # attribute => the list attribute to examine
  # FUN => function to apply on the attribute selected
  # ______________________________________
  # returns an index value(s) where attribute %in% result of FUN

  attr_values <- sapply(ps.freq, function(x) x[attribute]) %>% unlist()
  result <- which(attr_values %in% FUN(attr_values))
  return(result)
}
