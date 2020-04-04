# Upload fake data to an account
library(httr)
# library(tidyverse)
library(dplyr)
library(magrittr)
library(purrr)


# make some helper api functions so we don't have to type it every time
api <- . %>% sprintf("%sapi/v1/%s", url, .)
get_req <- partial(GET, config=authenticate(usr, pwd)) 
get <- . %>% api() %>% get_req() %>% content()



get_form_schema <- . %>% paste0("forms/", ., "/form.json") %>% 
   get()

get_form_names <- . %>% .$children %>% sapply(function(x) x$name)

pattern <- '([c,s,h]_._a$)|([c,s,h]_._a_\\d+)'

form_pathways <- . %>% grep(pattern, ., value=T, perl=T) %>% substr(3, 3) %>% unique()


# default values 
get_pathways <- . %>% names() %>% form_pathways()
pick_pathway <- . %>% .[sample(1:length(.), 1)]

weights <- . %>% runif() %>% divide_by(sum(.))
get_neighborhoods <- . %>% .$children %>% .[[list.which(., grepl('[h,s,c,col]_neighborhood$', name))]] %>% .$children %>% sapply(function(x) x$name)
get_sample_types <- . %>% .$children %>% .[[list.which(., grepl('[lab,col]_sample_type$', name))]]  %>% .$children %>% sapply(function(x) x$name)

fake_data <- function(row, example_data, nsub=500) {
  row %<>% as.list()
  cat("_________", row$example_data, "________________\n")
  
  form_info <- get_form_schema(row$formid)
  fields <- form_info %>% get_form_names()
  ex_dat <- example_data#[[row$example_data]] %>% .[names(.) %in% fields]

  if (grepl('(?i)community|household|school', row$example_data)) {
    pways <- get_pathways(ex_dat)
    existing_paths <- form_pathways(fields)
    missing_p <- !(existing_paths %in% pways)
    if (any(missing_p)) {
      cat('Adding missing pathways...\n')
      for (f in existing_paths[]) {
        pick <- pick_pathway(pways)
        faked <- grep(sprintf('[h,s,c]_%s_[a,c]$|[h,s,c]_%s_[a,c]_\\d+$', pick, pick), fields)
        ex_dat %<>% bind_cols(ex_dat[,faked])
      }
    }
  }
  
  
  cat('Shuffling the data around...\n')
  ex_dat <- ex_dat[sample(1:nrow(ex_dat), nsub, replace=T),]
  ex_dat <- apply(ex_dat, 2, function(x) sample(unique(x), size=nrow(ex_dat), replace=T, prob=weights(length(unique(x))))) 
  ex_dat[grepl('n/a', ex_dat)] <- NA 
  ex_dat %<>% as_tibble()
  
  smp_type <- grepl('[lab,col]_sample_type', fields, ignore.case = T)
  if (any(smp_type)) {
    cat('Generating new sample types...\n')
    samps <- get_sample_types(form_info) 
    ex_dat[,smp_type] <- sample(samps, nsub, replace=T, prob = weights(length(samps)))
  }
  
  n <- grepl('[h,c,s, col]_neighborhood$', names(ex_dat))
  if (any(n)) {
    cat('Generating new neighborhood values...\n')
    neighborhoods <- get_neighborhoods(form_info)
    ex_dat[,n] <- sample(neighborhoods, nsub, replace=T, prob=weights(length(neighborhoods)))
  }
  
  
  
  # posting fake data
  cat("Posting data to kobo...\n")
  tf <- file.path(paste0(tempfile(), '.csv'))
  write_csv(ex_dat, tf)
  
  r <- POST(sprintf('%s/api/v1/forms/%s/csv_import', url, row$formid),
            # add_headers("Authorization" = paste('Token', TOKEN)),
            authenticate(usr, pwd),
            progress(),
            #'Content-Disposition' = 'form-data'),
            body=list(csv_file=upload_file(tf)), encode = 'multipart')
  cat('Status Code: ', r$status_code, "\n")

  cat('Data Records found:', nrow(kt_getData(url, usr, pwd, row$formid)), "\n")

  return(list('response' = r, 'data' = ex_dat))
}


