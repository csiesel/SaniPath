library(ggplot2)
library(readxl)
library(dplyr)
library(bindr)
library(plotly)
library(expss)
library(knitr)

setwd(paste0(getwd(),"/kolkata"))

#importing SPT sample data
sample <- read_excel("spt_sample.xlsx", col_names=TRUE, na="NA")
#cleaning SPT sample data
# This one only counts hood 1: sample <- filter(sample, col_neighborhood=="1", col_id!="DWa1001(UF)", col_id!="DWa1002(UF)")
sample <- filter(sample, col_id!="DWa1001(UF)", col_id!="DWa1002(UF)")
sample$col_id <- unique(sample$col_id)
sample$col_sample_type <- as.numeric(sample$col_sample_type)
samplecol <- as.data.frame(table(sample$col_sample_type))
sum(samplecol$Freq)
samplecol

cro(sample$col_sample_type, sample$col_neighborhood)
#importing ES sample data
es <- read_excel("es_sample.xlsx", col_names=TRUE, na="NA")
#importing mf data
mf <- read_excel("mf_results.xlsx", col_names=TRUE, na="NA")
#importing mst data
mst <- read_excel("mst_results.xlsx", col_names=TRUE, na="NA")
#importing dna data
dna <- read_excel("dna_extraction.xlsx", col_names=TRUE, na="NA")
#importing pcr data
pcr <- read_excel("pcr_results.xlsx", col_names=TRUE, na="NA")

#cleaning all id's
sample$col_id <- sub(' ', '', sample$col_id)
es$col_id <- sub(' ', '', es$col_id)
mf$lab_id <- sub(' ', '', mf$lab_id)
mst$lab_id <- sub(' ', '', mst$lab_id)
dna$lab_id <- sub(' ', '', dna$lab_id)
pcr$lab_id <- sub(' ', '', pcr$lab_id)


# testing the merge things
test <- merge(sample, mf, by.x="col_id", by.y="lab_id")
test$MF_done <- 0
for(i in 1:nrow(test)){
  if(is.na(test$lab_1_dil_tested[i])){
    test$MF_done=0
  }
  else{
    test$MF_done=1
  }
}

