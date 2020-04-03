


# read ghana kumasi form
df.col.7 <- read.csv(paste0(getwd(), "/data/deployments/07_ghana-kumasi/", "ksisanipath-sample-1617", ".csv"), 
                     sep=",", stringsAsFactors=F, header=T, 
                     na.strings = c("n/a", "NA", "N/A", "---"))

# read ghana kumasi form
df.lab.7 <- read.csv(paste0(getwd(), "/data/deployments/07_ghana-kumasi/", "ksisanipath-lab-1616", ".csv"), 
                     sep=",", stringsAsFactors=F, header=T, na.strings = c("n/a", "NA", "N/A", "---"))



table(df.col.7$col_sample_type)
table(df.lab.7$lab_sample_type)


df.col.7$col_id[duplicated(df.col.7$col_id)]
df.lab.7$lab_id[duplicated(df.lab.7$lab_id)]


# need to delete - col id 4111 -> no lab data
#                  col id 5010 -> no lab data
#                  lab id 5105 -> double
#                  lab id 4110 -> double

df.col.7 <- df.col.7[!(df.col.7$col_id == 4111), ]
df.col.7 <- df.col.7[!(df.col.7$col_id == 5010), ]
df.lab.7 <- df.lab.7[!(df.lab.7$lab_start_dt == "2018-10-22T13:51:59.064Z"), ]   #5105
df.lab.7 <- df.lab.7[!(df.lab.7$lab_id == 4110), ]



write.csv(df.col.7, paste0(getwd(), "/data/deployments/07_ghana-kumasi/", "col_clean", ".csv"), row.names = F, na="")
write.csv(df.lab.7, paste0(getwd(), "/data/deployments/07_ghana-kumasi/", "lab_clean", ".csv"), row.names = F, na="")
