###example code for the assignment for June 24th###
#The task is to summary the number of PCR testing and number of positive 
#and persumtive positive for S. Typhi and Paratyphi A.

###direction
setwd("./SPT/student_work/")

###packages
  #helpful for plotting
library(ggplot2)
  #helpful data manipulation packages
library(tidyverse)
  #helpful for more data manipluation
library(reshape2)

###load data
load("./data/es1_final.rda")
load("./data/spt1_final.rda")

### data manipulation;
#combine SPT and ES datasets;
#check whether the columns are matching in those two datasets;
all.equal(names(spt1_final),names(es1_final)) 
#returns TRUE, so good to combine.
dat.micro <- rbind(spt1_final,es1_final)
#Variables that we need to work on is col_id, sample_type, pcr_date, 
#typhi_positive, typhi_presumptive, paratyphi_positive, paratyphi_presumptive.
col.sel <- which(names(dat.micro) %in% c("col_id", "sample_type", "pcr_date", 
                                        "typhi_positive", "typhi_presumptive", 
                                        "paratyphi_positive", "paratyphi_presumptive"))
#remove those rows without any PCR testing (identified using pcr_date).
row.sel <- which(!is.na(dat.micro$pcr_date))
#subset the micro dataset
dat.micro <- dat.micro[row.sel,col.sel]

###explore the dataset;
#First remember your goals to summarize few columns by date;

#date column;
#few things to check on when you work on a R object;
class(dat.micro$pcr_date) 
#returns "character" means that your date is currently a character object;
#need to reformat this to date;
dat.micro$pcr_date <- as.Date(dat.micro$pcr_date,format="%Y-%m-%d")
class(dat.micro$pcr_date) #new it changes to a date object;

#result columns;
class(dat.micro$typhi_positive)
class(dat.micro$typhi_presumptive)
class(dat.micro$paratyphi_positive)
class(dat.micro$paratyphi_presumptive)
#all numeric variables;

###summary sample size and positive counts;

#I left this chuck of codes to show you all each columns were calculated;
##############################################################################
#sample size
aggregate(dat.micro$col_id, by = list(date=dat.micro$pcr_date), length)
#positive counts, please remember to include the na.rm=T remove NA;
aggregate(dat.micro$typhi_positive, by = list(date=dat.micro$pcr_date), sum, na.rm=T)
aggregate(dat.micro$typhi_presumptive, by = list(date=dat.micro$pcr_date), sum, na.rm=T)
aggregate(dat.micro$paratyphi_positive, by = list(date=dat.micro$pcr_date), sum, na.rm=T)
aggregate(dat.micro$paratyphi_presumptive, by = list(date=dat.micro$pcr_date), sum, na.rm=T)
##############################################################################

#create the final results for output;
output <- merge(aggregate(dat.micro$col_id, by = list(date=dat.micro$pcr_date), length),
                aggregate(dat.micro[,c("typhi_positive", "typhi_presumptive","paratyphi_positive", "paratyphi_presumptive")], 
                          by = list(date=dat.micro$pcr_date), sum, na.rm=T),
                by="date")
#fix the names for your output;
names(output) <- c("date","n","num pos typhi","num prepos typhi","num pos para","num prepos para")

#output;
#save as R object;
save(output,file="./res/summary_by_PCR_date.rda")
#save as csv file;
write.csv(output,file="./res/summary_by_PCR_date.csv")
#go to your direction of res top check the outputs you generated;


# Visualizations ----------------------------------------------------------

#Thinking about what might be good to show as a visual....
#  1. count of positive and presumptive samples over time for typhi and paratyphi
#  2. how many samples of the total number came back as positive or presumptive for typhi and paratyphi


#1. count of typhi/paratyphi positives over time (each a separate plot)
typhi_pos <- ggplot(data=output, aes(x=date, y=`num pos typhi`), stat="identity") +
  geom_col(color="darkblue") +
  labs(title="Typhi Positive")
typhi_pos

typhi_prepos <- ggplot(data=output, aes(x=date, y=`num prepos typhi`), stat="identity") +
  geom_col(color="lightblue") +
  labs(title="Typhi Presumptive")
typhi_prepos

paratyphi_pos <- ggplot(data=output, aes(x=date, y=`num pos para`), stat="identity") +
  geom_col(color="darkgreen") +
  labs(title="Paratyphi Positive")
paratyphi_pos

paratyphi_prepos <- ggplot(data=output, aes(x=date, y=`num prepos para`), stat="identity") +
  geom_col(color="lightgreen") +
  labs(title="Paratyphi Presumptive")
paratyphi_prepos

##### 2. Below is a little more advanced visualizations with data manipulation steps ####

# How can we group and show more than 1 subset on the same plot.... MELT!

#The melt function from reshape2 is useful for plotting groups
#   - this essentially going from a 'wide' dataframe to a 'long' dataframe
#     with 3 variables: date, variable, and value

#The variable that is used to identify the observations is "date"
output2 <- melt(output, id="date")

#Viewing the dataframe you can see that each date now has an observation
#with an entry in 'variable' and 'value' equal to the previous variables
#of `num pos typhi`, `num prepos typhi`, etc.
View(output2)

#I don't want to show the 'n' values in the same way as positive/presumptive values
#so I will create a new dataframe with just 'n' by date
output3 <- output2 %>% filter(variable =="n")


#Typhi plots
#   - starting with the melted dataframe (output2), filtering it to only include values
#     for typhi (`num pos typhi` and `num prepos typhi`), plotting that data as stacked bars,
#     then adding in the total number of samples per day as a grey column
#   - other aspects of plot are changing the title, legend title, etc.
typhi_plot <- output2 %>% filter(variable %in% c("num prepos typhi", "num pos typhi")) %>%
  ggplot(data=., aes(x=date, y=value, fill=variable, color=variable)) +
  geom_col(data=output3, aes(x=date, y=value), fill="grey", color="grey") +
  geom_col(position="stack") +
  guides(color=FALSE) +
  labs(title="S. Typhi +/presumptive by date", fill="")

typhi_plot


#Paratyphi plots
#   - starting with the melted dataframe (output2), filtering it to only include values
#     for typhi (`num pos para` and `num prepos para`), plotting that as stacked bars,
#     then adding in the total number of samples per day as a grey column
#   - other aspects of plot are changing the title, legend title, etc.
paratyphi_plot <- output2 %>% filter(variable %in% c("num prepos para", "num pos para")) %>%
  ggplot(data=., aes(x=date, y=value, fill=variable, color=variable)) +
  geom_col(data=output3, aes(x=date, y=value), fill="grey", color="grey") +
  geom_col(position="stack") +
  guides(color=FALSE) +
  labs(title="S. Paratyphi +/presumptive by date", fill="")

paratyphi_plot
