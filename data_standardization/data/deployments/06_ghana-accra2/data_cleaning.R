# data cleaning ghana accra

# there are multiple samples collected with the same sample_id. this is also true in the lab form. 
# i.e.: there are multiple samples with the same id and the exposure calculation will not be correct.


df.lab.6 <- read.csv(paste0(getwd(), "/data/deployments/06_ghana-accra2/", "accrametro-lab-1497", ".csv"), 
                     sep=",", stringsAsFactors=F, header=T, na.strings = c("n/a", "NA", "N/A", "---"))


df.col.6 <- read.csv(paste0(getwd(), "/data/deployments/06_ghana-accra2/", "accrametro-sample-1498(1)", ".csv"), 
                     sep=",", stringsAsFactors=F, header=T, 
                     na.strings = c("n/a", "NA", "N/A", "---"))

#1 done
#2 done
#3 done
#4 only 1 neighb.
#5 not avail
#6 done
#7 done
#8 done
#9 done
#10 done

# ********************************************************************************
colnames(df.col.6)[colnames(df.col.6) == 'col_start'] <- 'col_start_dt'
df.col.6$col_date <- substr(df.col.6$col_start_dt, 1, 10)
df.col.6$col_date <- as.Date(df.col.6$col_date, format = "%Y-%m-%d")

# Create date variable
df.lab.6$lab_date <- NA
df.lab.6$lab_date <- substr(df.lab.6$lab_processing, 1, 10)
df.lab.6$lab_date <- as.Date(df.lab.6$lab_date, format = "%Y-%m-%d")
# ********************************************************************************

table(df.col.6$col_sample_type)
table(df.lab.6$lab_sample_type)

df1 <- df.col.6 %>% filter(col_sample_type == 1)
df2 <- df.lab.6 %>% filter(lab_sample_type == 1)

df1$col_id[duplicated(df1$col_id)]
df2$lab_id[duplicated(df2$lab_id)]

df1 <- df1[order(df1$col_start),]
table(df1$col_neighborhood)
df1$col_id[df1$col_neighborhood == 269] <- df1$col_id[df1$col_neighborhood == 269] + 100

df2$lab_id[df2$lab_date >= '2018-10-10'] <- df2$lab_id[df2$lab_date >= '2018-10-10'] + 100

df.col.6[df.col.6$col_sample_type == 1, ] <- df1
df.lab.6[df.lab.6$lab_sample_type == 1, ] <- df2
# ********************************************************************************

table(df.col.6$col_sample_type)
table(df.lab.6$lab_sample_type)

df1 <- df.col.6 %>% filter(col_sample_type == 2)
df2 <- df.lab.6 %>% filter(lab_sample_type == 2)

df1$col_id[df1$col_id == 1001] <- 2001

df1$col_id[duplicated(df1$col_id)]
df2$lab_id[duplicated(df2$lab_id)]

df1 <- df1[order(df1$col_start),]
table(df1$col_neighborhood)
table(df1$col_date)
df1$col_id[df1$col_neighborhood == 269] <- df1$col_id[df1$col_neighborhood == 269] + 100

table(df2$lab_date)
df2$lab_id[df2$lab_date >= '2018-10-08'] <- df2$lab_id[df2$lab_date >= '2018-10-08'] + 100

df.col.6[df.col.6$col_sample_type == 2, ] <- df1
df.lab.6[df.lab.6$lab_sample_type == 2, ] <- df2

# ********************************************************************************

table(df.col.6$col_sample_type)
table(df.lab.6$lab_sample_type)

df1 <- df.col.6 %>% filter(col_sample_type == 3)
df2 <- df.lab.6 %>% filter(lab_sample_type == 3)

df1$col_id[duplicated(df1$col_id)]
df2$lab_id[duplicated(df2$lab_id)]

df1 <- df1[order(df1$col_start),]
table(df1$col_neighborhood)
df1$col_id[df1$col_neighborhood == 269] <- df1$col_id[df1$col_neighborhood == 269] + 100

df2$lab_id[df2$lab_date >= '2018-10-08'] <- df2$lab_id[df2$lab_date >= '2018-10-08'] + 100
df1$col_id[df1$col_start_dt == "2018-10-09T08:30:00.000Z"] <- 1108


df.col.6[df.col.6$col_sample_type == 3, ] <- df1
df.lab.6[df.lab.6$lab_sample_type == 3, ] <- df2
# ********************************************************************************

table(df.col.6$col_sample_type)
table(df.lab.6$lab_sample_type)

df1 <- df.col.6 %>% filter(col_sample_type == 6)
df2 <- df.lab.6 %>% filter(lab_sample_type == 6)

df1$col_id[duplicated(df1$col_id)]
df2$lab_id[duplicated(df2$lab_id)]

df1 <- df1[order(df1$col_start),]
table(df1$col_neighborhood)
df1$col_id[df1$col_neighborhood == 269] <- df1$col_id[df1$col_neighborhood == 269] + 100

df2$lab_id[df2$lab_date >= '2018-10-09'] <- df2$lab_id[df2$lab_date >= '2018-10-09'] + 100

df.col.6[df.col.6$col_sample_type == 6, ] <- df1
df.lab.6[df.lab.6$lab_sample_type == 6, ] <- df2
# ********************************************************************************

table(df.col.6$col_sample_type)
table(df.lab.6$lab_sample_type)

df1 <- df.col.6 %>% filter(col_sample_type == 7)
df2 <- df.lab.6 %>% filter(lab_sample_type == 7)

df1$col_id[duplicated(df1$col_id)]
df2$lab_id[duplicated(df2$lab_id)]

df1 <- df1[order(df1$col_start),]
table(df1$col_neighborhood)
df1$col_id[df1$col_neighborhood == 269] <- df1$col_id[df1$col_neighborhood == 269] + 100

df1$col_id[df1$col_start_dt == "2018-10-09T08:41:00.000Z"] <- 3109

df2$lab_id[df2$lab_date >= '2018-10-08'] <- df2$lab_id[df2$lab_date >= '2018-10-08'] + 100

df.col.6[df.col.6$col_sample_type == 7, ] <- df1
df.lab.6[df.lab.6$lab_sample_type == 7, ] <- df2
# ********************************************************************************

table(df.col.6$col_sample_type)
table(df.lab.6$lab_sample_type)

df1 <- df.col.6 %>% filter(col_sample_type == 8)
df2 <- df.lab.6 %>% filter(lab_sample_type == 8)

df1$col_id[duplicated(df1$col_id)]
df2$lab_id[duplicated(df2$lab_id)]

df1 <- df1[order(df1$col_start),]
table(df1$col_neighborhood)
df1$col_id[df1$col_neighborhood == 269] <- df1$col_id[df1$col_neighborhood == 269] + 100

df2$lab_id[df2$lab_date >= '2018-10-10'] <- df2$lab_id[df2$lab_date >= '2018-10-10'] + 100
df2$lab_id[df2$lab_processing == "2018-10-03T12:30:00.000Z"] <- 6004

df.col.6[df.col.6$col_sample_type == 8, ] <- df1
df.lab.6[df.lab.6$lab_sample_type == 8, ] <- df2
# ********************************************************************************

table(df.col.6$col_sample_type)
table(df.lab.6$lab_sample_type)

df1 <- df.col.6 %>% filter(col_sample_type == 9)
df2 <- df.lab.6 %>% filter(lab_sample_type == 9)

df1$col_id[duplicated(df1$col_id)]
df2$lab_id[duplicated(df2$lab_id)]

df1 <- df1[order(df1$col_start),]
table(df1$col_neighborhood)
df1$col_id[df1$col_neighborhood == 269] <- df1$col_id[df1$col_neighborhood == 269] + 100

df2$lab_id[df2$lab_date >= '2018-10-10'] <- df2$lab_id[df2$lab_date >= '2018-10-10'] + 100

df.col.6[df.col.6$col_sample_type == 9, ] <- df1
df.lab.6[df.lab.6$lab_sample_type == 9, ] <- df2
# ********************************************************************************

table(df.col.6$col_sample_type)
table(df.lab.6$lab_sample_type)

df1 <- df.col.6 %>% filter(col_sample_type == 10)
df2 <- df.lab.6 %>% filter(lab_sample_type == 10)

df1$col_id[duplicated(df1$col_id)]
df2$lab_id[duplicated(df2$lab_id)]

df1 <- df1[order(df1$col_start),]
table(df1$col_neighborhood)
df1$col_id[df1$col_neighborhood == 269] <- df1$col_id[df1$col_neighborhood == 269] + 100

df2$lab_id[df2$lab_date >= '2018-10-10'] <- df2$lab_id[df2$lab_date >= '2018-10-10'] + 100

df.col.6[df.col.6$col_sample_type == 10, ] <- df1
df.lab.6[df.lab.6$lab_sample_type == 10, ] <- df2
# ********************************************************************************


table(df.col.6$col_sample_type)
df.col.6$col_id[duplicated(df.col.6$col_id)]
df.lab.6$lab_id[duplicated(df.lab.6$lab_id)]

df.col.6$col_date <- NULL
df.lab.6$lab_date <- NULL


df.col.6$col_id_val <- df.col.6$col_id
df.lab.6$lab_id_val <- df.lab.6$lab_id


write.csv(df.col.6, paste0(getwd(), "/data/deployments/06_ghana-accra2/", "col_clean", ".csv"), row.names = F, na="")
write.csv(df.lab.6, paste0(getwd(), "/data/deployments/06_ghana-accra2/", "lab_clean", ".csv"), row.names = F, na="")

