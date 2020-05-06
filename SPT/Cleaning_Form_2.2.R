library(tidyverse)
library(data.table)
library(googlesheets4)


#reading in file
url <- "https://docs.google.com/spreadsheets/d/13f1QVlxypEKZxy4PVRBZFaHnub3SAYYtupkhtnVW5pQ/edit#gid=1601030900"
form_2.2 <- read_sheet(url, sheet = "2.2.raw")
#removing the training/pilot data
form_2.2 <- form_2.2[-c(1:12),]

#renaming to match further code
so <- form_2.2

#creating variables for fourth COb sample (1 observation had 4 COb samples)
so$col_cob_col4 <- as.character(NA)
so$col_cob_ids4 <- as.integer(NA)
so$col_cob4 <- as.character(NA)
so$col_cob_other4 <- as.integer(NA)
so$col_cob14 <- as.character(NA)
so$col_cob1_other4 <- as.integer(NA)

#splitting into food prep and child obs samples
fp <- so %>% filter(col_sample_type==14)
co <- so %>% filter(col_sample_type==15)

##### Food Prep ####
#combining food prep samples into one row
# function below is so I can use dplyr to combine data (replaces overlapping data with last entry)
# should be ok since the only overlap is gps, time, and other meta data
coalesce_by_column <- function(df) {
  return(dplyr::coalesce(!!! as.list(df)))
}
#group by HH code and then collapse to one row
foodprep <- fp %>% 
  group_by(col_id) %>% 
  summarise_all(coalesce_by_column)


#### Child Obs ####

#get unique codes for HH's
ids <- unique(co$col_id)

#setting up temp2 dataframe
childobs <-data.frame()

for(p in 1:length(ids)){
  #obs references that have the same hh id
  refs <- which(co$col_id==ids[p])
  #temporary dataframe for all data with same hh id
  temp <- co[c(refs),]
  #getting unique COb sample names - used in {for} loop below
  cob1 <- unique(na.omit(str_extract(c(temp$col_cob_ids, temp$col_cob_ids2, temp$col_cob_ids3), "^COb....")))
  #getting obs number that has COa data - all COb's will be added to this row
  v <- which(!is.na(temp$col_coa_id))
  for(i in 1:length(cob1)){
    print(paste0("num cob: ", i))
    #Getting which column has the COb sample id
    z <- which(apply(temp, 2, function(x) any(grepl(cob1[i],x))))
    #gets which obs has the COb id
    w <- which(temp[z]==cob1[i])
    
    #all of the below if/else loops set the COb id's from the separate observations
    # to the same columns with the observation containing the COa sample
    # essentially collapsing all CObs into the same COa row
    if(i==1){
      temp[v,103:108] <- temp[w,z:(z+5)]
    }
    else if(i==2){
      temp[v,109:114] <- temp[w,z:(z+5)]
    }
    else if(i==3){
      temp[v,115:120] <- temp[w,z:(z+5)]
    }
    else if(i==4){
      temp[v,144:149] <- temp[w,z:(z+5)]
    }
  }
  #building the final dataset
  childobs <- rbind(childobs, temp)
}

#filtering to only the rows with full data (i.e. those with COa id's)
childobs <- childobs %>% filter(!is.na(col_coa_id))

#combining child and foodprep again
so_full <- rbind(childobs, foodprep)

#writing csv
write_csv(so_full, paste0(path,"2.2_cleaned_22Apr2020_cs.csv"))
