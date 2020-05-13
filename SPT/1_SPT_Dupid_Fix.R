library(tidyverse)
library(googlesheets4)

#url of google spreadsheet
url <- "https://docs.google.com/spreadsheets/d/13f1QVlxypEKZxy4PVRBZFaHnub3SAYYtupkhtnVW5pQ/edit#gid=1601030900"

#### Sample function definition ####
sample <- function(fix,clean){
  #renaming id's and editing date for matching
  fix <- fix %>%
    rename(id_val = col_id_val, id = col_id) %>%
    mutate(date = as.character(as.Date(col_start_dt)))
  #renaming id's, removing spaces, and editiing date
  clean <- clean %>%
    rename(id_fix = col_id_fix, id = col_id) %>%
    mutate(id = str_replace_all(id, " ", ""),
           date = as.character(as.Date(date)))
  #for all duplicate id's, match the fixed ID (from clean google doc spreadsheet)
  #based on the date and the original id and assign to the id_fix
  for(i in 1:nrow(fix)){
    print(fix$id[i])
    fix$id_fix[i] = clean$id_fix[which(clean$date==fix$date[i] & clean$id==fix$id[i])]
  }
  return(fix)
}

#### Lab function definition ####
lab <- function(fix,clean,form){
  # Same overall process as the sample form except these forms are tricky
  # each form has different variable names and date references
  
  #split into doing the process based on form fed into the function
  if(form %in% c(3.1,3.2,3.5)){
    fix <- fix %>%
      rename(id_val = lab_id_val, id = lab_id) %>%
      mutate(date= as.character(as.Date(lab_processing)))
    
    clean <- clean %>%
      rename(id_fix = lab_id_fix, id = lab_id) %>%
      # filter(id != id_fix) %>%
      mutate(id = str_replace_all(id, " ", ""),
             date = as.character(as.Date(date)))
    
    for(i in 1:nrow(fix)){
      print(fix$id[i])
      fix$id_fix[i] = clean$id_fix[min(which(clean$date==fix$date[i] & clean$id==fix$id[i]))]
      #For some samples there are multiple indices that will match the below criteria.
      # This will assign the original value to the minimum index, replace the clean$ID
      # with "", and the next match will go to the next clean$id_fix that matches the
      # clean$id.
      clean$id[min(which(clean$date==fix$date[i] & clean$id==fix$id[i]))] <- ""
      
      print(fix$id_fix[i])
    }
  }
  else if(form==3.3){
    fix <- fix %>%
      rename(id_val = lab_id_val, id = lab_id) %>%
      mutate(date= as.character(as.Date(lab_dna_date)))
    
    clean <- clean %>%
      rename(id_fix = lab_id_fix, id = lab_id) %>%
      mutate(id = str_replace_all(id, " ", ""),
             date = as.character(as.Date(date)))
    
    for(i in 1:nrow(fix)){
      print(fix$id[i])
      fix$id_fix[i] = clean$id_fix[min(which(clean$date==fix$date[i] & clean$id==fix$id[i]))]
      
      clean$id[min(which(clean$date==fix$date[i] & clean$id==fix$id[i]))] <- ""
      
      print(fix$id_fix[i])
    }
  }
  else if(form==3.4){
    fix <- fix %>%
      rename(id_val = lab_id_val, id = lab_id) %>%
      mutate(date= as.character(as.Date(lab_pcr_ty_date)))
    
    clean <- clean %>%
      rename(id_fix = lab_id_fix, id = lab_id) %>%
      # filter(id != id_fix) %>%
      mutate(id = str_replace_all(id, " ", ""),
             date = as.character(as.Date(date)))
    
    for(i in 1:nrow(fix)){
      print(fix$id[i])
      fix$id_fix[i] = clean$id_fix[min(which(clean$date==fix$date[i] & clean$id==fix$id[i]))]
      
      clean$id[min(which(clean$date==fix$date[i] & clean$id==fix$id[i]))] <- ""
      
      print(fix$id_fix[i])
    }
  }
  
  return(fix)
  
}


#### General process for all forms ####
# 1. read in raw and clean sheet from google doc
# 2. remove all 'NA' and '#VALUE' entries with complete.cases
# 3. get a list of all duplicate ID's in raw data (dup_#.#)
# 4. set up a dataset of only duplicate ID's from raw data (fix_#.#)
# 5. run through sample/lab function to get ID fixes

# *note: clean files are not cleaned data - these are the tabs from Wolfgang's doc
#  that show the id fixes required




#### Form 2.1 ####
raw_2.1 <- read_sheet(url, sheet = "2.1.raw")
clean_2.1 <- read_sheet(url, sheet = "2.1.clean")
clean_2.1 <- clean_2.1[complete.cases(clean_2.1$col_id),]
#special date cleanup from some of Wolfgang's manual edits
clean_2.1$date[which(clean_2.1$col_id_fix=="FB1002a")] <-  "2019-06-03 UTC"
clean_2.1$date[which(clean_2.1$col_id_fix=="FB1002")] <-  "2019-06-25 UTC"
clean_2.1$date[which(clean_2.1$col_id_fix=="FB1076")] <-"2019-10-17 UTC"
dup_2.1 <- raw_2.1$col_id[raw_2.1$col_id %in% raw_2.1$col_id[duplicated(raw_2.1$col_id)]]
fix_2.1 <- raw_2.1 %>%
  filter(col_id %in% dup_2.1) %>%
  mutate(id_fix="")

fix_2.1 <- sample(fix_2.1, clean_2.1) %>%
  select(id, id_fix, date, `_index`) %>%
  mutate(form=2.1)




#### Form 2.2 ####
# This one has some special cleaning going on...
raw_2.2 <- read_sheet(url, sheet = "2.2.raw")

form_2.2 <- raw_2.2[-c(1:12),]

#renaming to match further code
so <- form_2.2

#creating variables for fourth COb sample (1 observation had 4 COb samples)
so$col_cob_col4 <- as.character(NA)
so$col_cob_ids4 <- as.character(NA)
so$col_cob4 <- as.character(NA)
so$col_cob_other4 <- as.character(NA)
so$col_cob14 <- as.character(NA)
so$col_cob1_other4 <- as.character(NA)

#splitting into food prep and child obs samples
fp <- so %>% filter(col_sample_type==14)
co <- so %>% filter(col_sample_type==15)

## Food Prep ##
#creating col_id_so variable to match with MF lab_id
foodprep <- fp %>%
  mutate(col_id_so = ifelse(!is.na(col_fpa_id),
                            toupper(col_fpa_id),
                            ifelse(!is.na(col_pfb_id),
                                   toupper(col_pfb_id),
                                   ifelse(!is.na(col_fpc_id),
                                          toupper(col_fpc_id),
                                          ifelse(!is.na(col_fpd_id),
                                                 toupper(col_fpd_id),
                                                 toupper(col_fpe_id))))))

## Child Obs ##
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
    if(i==1){temp[v,103:108] <- temp[w,z:(z+5)]}
    else if(i==2){temp[v,109:114] <- temp[w,z:(z+5)]}
    else if(i==3){temp[v,115:120] <- temp[w,z:(z+5)]}
    else if(i==4){temp[v,145:149] <- temp[w,z:(z+4)]}
  }
  #building the final dataset
  childobs <- rbind(childobs, temp)
}

#filtering to only the rows with full data (i.e. those with COa id's)
childobs <- childobs %>% filter(!is.na(col_coa_id)) %>%
  mutate(col_coa_id=toupper(col_coa_id),
         col_cob_ids=toupper(col_cob_ids),
         col_cob_ids2=toupper(col_cob_ids2),
         col_cob_ids3=toupper(col_cob_ids3),
         col_cob_ids4=toupper(col_cob_ids4),
         col_id_so=NA)

#combining child and foodprep again
so_full <- rbind(childobs, foodprep)

raw_2.2 <- so_full


#### Form 2.4 ####
raw_2.4 <- read_sheet(url, sheet = "2.4.raw")
clean_2.4 <- read_sheet(url, sheet = "2.4.clean")
clean_2.4 <- clean_2.4[complete.cases(clean_2.4$col_id),]
dup_2.4 <- raw_2.4$col_id[raw_2.4$col_id %in% raw_2.4$col_id[duplicated(raw_2.4$col_id)]]
fix_2.4 <- raw_2.4 %>%
  filter(col_id %in% dup_2.4) %>%
  mutate(id_fix="")

fix_2.4 <- sample(fix_2.4, clean_2.4) %>%
  select(id, id_fix, date, `_index`) %>%
  mutate(form=2.4)




#### Form 3.1 ####
raw_3.1 <- read_sheet(url, sheet = "3.1.raw")
clean_3.1 <- read_sheet(url, sheet = "3.1.clean")
clean_3.1 <- clean_3.1[complete.cases(clean_3.1$lab_id),]
dup_3.1 <- raw_3.1$lab_id[raw_3.1$lab_id %in% raw_3.1$lab_id[duplicated(raw_3.1$lab_id)]]
fix_3.1 <- raw_3.1 %>%
  filter(lab_id %in% dup_3.1) %>%
  mutate(lab_id=str_replace_all(lab_id, " ", ""),
         lab_id_val=str_replace_all(lab_id_val, " ", ""),
         id_fix="")

fix_3.1 <- lab(fix_3.1, clean_3.1, form=3.1) %>%
  select(id, id_fix, date, `_index`) %>%
  mutate(form=3.1)




#### Form 3.2 ####
raw_3.2 <- read_sheet(url, sheet = "3.2.raw")
clean_3.2 <- read_sheet(url, sheet = "3.2.clean")
clean_3.2 <- clean_3.2[complete.cases(clean_3.2$lab_id),]
dup_3.2 <- raw_3.2$lab_id[raw_3.2$lab_id %in% raw_3.2$lab_id[duplicated(raw_3.2$lab_id)]]
fix_3.2 <- raw_3.2 %>%
  filter(lab_id %in% dup_3.2) %>%
  mutate(lab_id=str_replace_all(lab_id, " ", ""),
         lab_id_val=str_replace_all(lab_id_val, " ", ""),
         id_fix="")

fix_3.2 <- lab(fix_3.2, clean_3.2, form=3.2) %>%
  select(id, id_fix, date, `_index`) %>%
  mutate(form=3.2)




#### Form 3.3 ####
raw_3.3 <- read_sheet(url, sheet = "3.3.raw")
clean_3.3 <- read_sheet(url, sheet = "3.3.clean")
clean_3.3 <- clean_3.3[complete.cases(clean_3.3$lab_id),]
dup_3.3 <- raw_3.3$lab_id[raw_3.3$lab_id %in% raw_3.3$lab_id[duplicated(raw_3.3$lab_id)]]
fix_3.3 <- raw_3.3 %>%
  filter(lab_id %in% dup_3.3) %>%
  mutate(lab_id=str_replace_all(lab_id, " ", ""),
         lab_id_val=str_replace_all(lab_id_val, " ", ""),
         id_fix="")

fix_3.3 <- lab(fix_3.3, clean_3.3, form=3.3) %>%
  select(id, id_fix, date, `_index`) %>%
  mutate(form=3.3)




#### Form 3.4 ####
raw_3.4 <- read_sheet(url, sheet = "3.4.raw")
clean_3.4 <- read_sheet(url, sheet = "3.4.clean")
clean_3.4 <- clean_3.4[complete.cases(clean_3.4$lab_id),]
dup_3.4 <- raw_3.4$lab_id[raw_3.4$lab_id %in% raw_3.4$lab_id[duplicated(raw_3.4$lab_id)]]
fix_3.4 <- raw_3.4 %>%
  filter(lab_id %in% dup_3.4) %>%
  mutate(lab_id=str_replace_all(lab_id, " ", ""),
         lab_id_val=str_replace_all(lab_id_val, " ", ""),
         id_fix="")

fix_3.4 <- lab(fix_3.4, clean_3.4, form=3.4) %>%
  select(id, id_fix, date, `_index`) %>%
  mutate(form=3.4)




#### Form 3.5 ####
raw_3.5 <- read_sheet(url, sheet = "3.5.raw")
clean_3.5 <- read_sheet(url, sheet = "3.5.clean")
clean_3.5 <- clean_3.5[complete.cases(clean_3.5$lab_id),]
dup_3.5 <- raw_3.5$lab_id[raw_3.5$lab_id %in% raw_3.5$lab_id[duplicated(raw_3.5$lab_id)]]
fix_3.5 <- raw_3.5 %>%
  filter(lab_id %in% dup_3.5) %>%
  mutate(lab_id=str_replace_all(lab_id, " ", ""),
         lab_id_val=str_replace_all(lab_id_val, " ", ""),
         id_fix="")

fix_3.5 <- lab(fix_3.5, clean_3.5, form=3.5) %>%
  select(id, id_fix, date, `_index`) %>%
  mutate(form=3.5)




#### Final File ####
dup_id_fix <- rbind(fix_2.1, fix_2.4, fix_3.1, fix_3.2, fix_3.3, fix_3.4, fix_3.5)
#removing the id's that don't need changed (ID == ID_FIX)
dup_id_fix <- dup_id_fix %>% 
  filter(id != id_fix)

write_csv(dup_id_fix, "dup_id_fix.csv")





#### Changing ID's in raw data ####
#Replacing the col_id and lab_id values with the fixed values
# *uncomment the id_val if interested
for(i in 1:nrow(dup_id_fix)){
  #pulling raw data based on form
  temp <- get(paste0("raw_",dup_id_fix$form[i]))
  #below for sample forms
  if(dup_id_fix$form[i] %in% c(2.1, 2.2, 2.4)){
    temp$col_id[which(dup_id_fix$`_index`[i]==temp$`_index`)] <- dup_id_fix$id_fix[i]
    print(temp$col_id[which(dup_id_fix$`_index`[i]==temp$`_index`)])
    # temp$col_id_val[which(dup_id_fix$`_index`[i]==temp$`_index`)] <- dup_id_fix$id_fix[i]
  }
  #below for lab forms
  else{
    temp$lab_id[which(dup_id_fix$`_index`[i]==temp$`_index`)] <- dup_id_fix$id_fix[i]
    print(temp$lab_id[which(dup_id_fix$`_index`[i]==temp$`_index`)])
    # temp$lab_id_val[which(dup_id_fix$`_index`[i]==temp$`_index`)] <- dup_id_fix$id_fix[i]
  }
  #assigning updated data to the raw form
  assign(paste0("raw_",dup_id_fix$form[i]), temp)
}

raw_3.1$lab_enum <- as.character(raw_3.1$lab_enum)
raw_3.2$lab_enum <- as.character(raw_3.2$lab_enum)
raw_3.3$lab_enum <- as.character(raw_3.3$lab_enum)
raw_3.4$lab_enum <- as.character(raw_3.4$lab_enum)
raw_3.4$lab_pcr_type <- as.character(raw_3.4$lab_pcr_type)
raw_3.5$lab_enum <- as.character(raw_3.5$lab_enum)
raw_3.5$lab_ward <- as.numeric(raw_3.5$lab_ward)
raw_3.5$lab_neighborhood <- as.numeric(raw_3.5$lab_neighborhood)
raw_3.5$lab_sample_type <- as.numeric(raw_3.5$lab_sample_type)
raw_3.5$lab_dna_done <- as.numeric(raw_3.5$lab_dna_done)
raw_3.5$lab_dna_date <- as.character(raw_3.5$lab_dna_date)


any(duplicated(raw_2.1$col_id))
any(duplicated(raw_2.4$col_id))
any(duplicated(raw_3.1$lab_id))
any(duplicated(raw_3.2$lab_id))
any(duplicated(raw_3.3$lab_id))
any(duplicated(raw_3.4$lab_id))
any(duplicated(raw_3.5$lab_id))






write_excel_csv(raw_2.1, paste0("SPT/data/raw_2.1_", Sys.Date(), ".csv"))
write_excel_csv(raw_2.2, paste0("SPT/data/raw_2.2_", Sys.Date(), ".csv"))
write_excel_csv(raw_2.4, paste0("SPT/data/raw_2.4_", Sys.Date(), ".csv"))
write_excel_csv(raw_3.1, paste0("SPT/data/raw_3.1_", Sys.Date(), ".csv"))
write_excel_csv(raw_3.2, paste0("SPT/data/raw_3.2_", Sys.Date(), ".csv"))
write_excel_csv(raw_3.3, paste0("SPT/data/raw_3.3_", Sys.Date(), ".csv"))
write_excel_csv(raw_3.4, paste0("SPT/data/raw_3.4_", Sys.Date(), ".csv"))
write_excel_csv(raw_3.5, paste0("SPT/data/raw_3.5_", Sys.Date(), ".csv"))


rm(dup_2.1, dup_2.4, dup_3.1, dup_3.2, dup_3.3, dup_3.4, dup_3.5,
   clean_2.1, clean_2.4, clean_3.1, clean_3.2, clean_3.3, clean_3.4, clean_3.5,
   fix_2.1, fix_2.4, fix_3.1, fix_3.2, fix_3.3, fix_3.4, fix_3.5,
   temp)
