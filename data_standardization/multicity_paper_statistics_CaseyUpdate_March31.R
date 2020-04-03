# multicity paper code
# data exporation for multicity paper

#### Casey added this chunk since the loading of data was missing in Wolfgang's file ####
library(tidyverse)
library(knitr)
library(kableExtra)

# Import data:
source("helper_dataload_all.R")
source("helper_graph.R")
source("helper_graph_combined.R")


# data setup (adding country and city names etc.)
df.ecdata <- df.ecdata %>% 
        left_join(., meta_sampleID, by = c("sample_type" = "id")) %>%
        left_join(., meta_dply[, c("id", "country", "city", "citylabel")], by = c("dply_num" = "id"))

df.col <- df.col %>% left_join(., meta_dply[c("id", "citylabel")], by = c("dply_num" = "id"))
df.lab <- df.lab %>% left_join(., meta_dply[c("id", "citylabel")], by = c("dply_num" = "id"))
df.h <- df.h %>% left_join(., meta_dply[c("id", "citylabel")], by = c("dply_num" = "id"))
df.c <- df.c %>% left_join(., meta_dply[c("id", "citylabel")], by = c("dply_num" = "id"))
df.s <- df.s %>% left_join(., meta_dply[c("id", "citylabel")], by = c("dply_num" = "id"))



################ Start of Wolfgang's code

# line 94 ----
df.ecdata %>% group_by(sample_type_name, city) %>% 
        summarise(min = min(log10(ec_conc), na.rm = T),
                  max = max(log10(ec_conc), na.rm = T),
                  mean = mean(log10(ec_conc), na.rm = T),
                  median = median(log10(ec_conc), na.rm = T)) -> df

df.ecdata %>% group_by(col_sample_type_alt, city) %>% 
        summarise(min = min(log10(ec_conc), na.rm = T),
                  max = max(log10(ec_conc), na.rm = T),
                  mean = mean(log10(ec_conc), na.rm = T),
                  median = median(log10(ec_conc), na.rm = T)) -> df2

# range of serving size of produce
df.lab %>% filter(lab_sample_type == 2) %>%
        summarise(min = min(lab_p_weight, na.rm = T),
                  max = max(lab_p_weight, na.rm = T))

df.lab$lab_p_weight




# line 110 ----

df.ecdata %>%  filter(sample_type == 3) %>% 
        group_by(city) %>% summarise(min = min(log10(ec_conc), na.rm = T),
                                     max = max(log10(ec_conc), na.rm = T),
                                     mean = mean(log10(ec_conc), na.rm = T),
                                     median = median(log10(ec_conc), na.rm = T))

df.ecdata %>%  filter(sample_type == 3) %>% 
        group_by(city) %>% summarise(min = min(ec_conc),
                                     max = max(ec_conc),
                                     mean = mean(ec_conc),
                                     median = median(ec_conc))

df.water <- df.ecdata %>%  
        filter(sample_type == 3) 
sum(df.water$ec_conc < 10, na.rm=TRUE)
nrow(df.water)
(242/345)*100

df.water1 <- df.ecdata %>%  
        filter(sample_type == 33) 

# line 118 ----
# range of serving size
df.lab %>% filter(lab_sample_type == 2) %>%
        summarise("n" = n(),
                  "min" = min(lab_p_weight, na.rm = T),
                  "max" = max(lab_p_weight, na.rm = T),
                  "mean" = mean(lab_p_weight, na.rm = T))

df.lab %>% filter(lab_sample_type == 10) %>%
        summarise("n" = n(),
                  "min" = min(lab_sf_weight, na.rm = T),
                  "max" = max(lab_sf_weight, na.rm = T),
                  "mean" = mean(lab_sf_weight, na.rm = T))


# produce
df2 <- df.col %>% filter(col_sample_type == 2) %>% select(col_UID, col_p_type) %>% 
        left_join(., df.ecdata, by = c("col_UID" = "UID")) %>% 
        left_join(., meta_produce, by = c("col_p_type" = "produce_id")) %>% 
        select("UID" = col_UID, dply_num, country, city, citylabel, neighb_UID, col_p_type, produce, ec_conc)

df2 %>% filter(dply_num == 2) %>% group_by(produce) %>% summarise("n" = n(), 
                                        "min ecoli" = round(min(log10(ec_conc), na.rm = T), 2),
                                        "max ecoli" = round(max(log10(ec_conc), na.rm = T), 2),
                                        "mean ecoli" = round(mean(log10(ec_conc), na.rm = T), 2),
                                        "median ecoli" = round(median(log10(ec_conc), na.rm = T), 2),
                                        "standard deviation" = round(sd(log10(ec_conc), na.rm = T), 2),
                                        "variance" = round(var(log10(ec_conc), na.rm = T), 2)
)


# streetfood
df <- df.col %>% filter(col_sample_type == 10) %>% select(col_UID, col_sf_type) %>%
        left_join(., df.ecdata, by = c("col_UID" = "UID")) %>%
        left_join(., meta_streetfood, by = c("col_sf_type" = "streetfood_id")) %>%
        select("UID" = col_UID, dply_num, country, city, citylabel, neighb_UID, col_sf_type, streetfood, ec_conc)

df %>% filter(dply_num == 2) %>% group_by(streetfood) %>% summarise("n" = n(), 
                                                                  "min ecoli" = round(min(log10(ec_conc), na.rm = T), 2),
                                                                  "max ecoli" = round(max(log10(ec_conc), na.rm = T), 2),
                                                                  "mean ecoli" = round(mean(log10(ec_conc), na.rm = T), 2),
                                                                  "median ecoli" = round(median(log10(ec_conc), na.rm = T), 2),
                                                                  "standard deviation" = round(sd(log10(ec_conc), na.rm = T), 2),
                                                                  "variance" = round(var(log10(ec_conc), na.rm = T), 2)
)


# line 191 ----

df.ecdata %>%  group_by(citylabel) %>% summarise(min = min(lab_date),
                                            max = max(lab_date)) %>% write.csv("dates_lab.csv")


df.h$h_date <- as.Date(df.h$h_date, format = "%Y-%m-%d")
df.c$c_date <- as.Date(df.c$c_date, format = "%Y-%m-%d")
df.s$s_date <- as.Date(df.s$s_date, format = "%Y-%m-%d")



df.h %>%  group_by(citylabel) %>% summarise(min = min(h_date),
                                                 max = max(h_date)) %>% write.csv("dates_h.csv")

df.c %>%  group_by(citylabel) %>% summarise(min = min(c_date),
                                            max = max(c_date)) %>% write.csv("dates_c.csv")

df.s %>%  group_by(citylabel) %>% summarise(min = min(s_date),
                                            max = max(s_date)) %>% write.csv("dates_s.csv")


class(df.c$c_date)


# line 246 ----
# mean and range of levels of fecal contamination across cities by sample type
df.ecdata %>%  
        group_by(city, sample_type_name) %>% 
        summarise(min = min(log10(ec_conc), na.rm = T),
                  max = max(log10(ec_conc), na.rm = T),
                  mean = mean(log10(ec_conc), na.rm = T),
                  median = median(log10(ec_conc), na.rm = T)) %>%
        write.csv("df_range_sample.csv")


# line 353 ----
table(df.h$h_pl_a_th)
df.h %>% select(city, h_pl_a_th ) %>% group_by(city) %>% summarize(total = n(),
                                                                   yes = sum(h_pl_a_th == 1, na.rm = T),
                                                                   no = sum(h_pl_a_th == 2, na.rm = T),
                                                                   dnk = sum(h_pl_a_th == 3, na.rm = T),
                                                                   na = sum(h_pl_a_th == 4, na.rm = T),
                                                                   na1 = sum(is.na(h_pl_a_th)),
                                                                   total1 = total - dnk - na - na1,
                                                                   yes1 = yes/total1,
                                                                   no1 = no/ total1,
                                                                   total2 = total - dnk - na ,
                                                                   yes2 = yes/total2,
                                                                   no2 = no/ total2)

#
table(df.h$h_pl_a_tu)
df.h %>% select(city, h_pl_a_tu ) %>% group_by(city) %>% summarize(total = n(),
                                                                   yes = sum(h_pl_a_tu == 1, na.rm = T),
                                                                   no = sum(h_pl_a_tu == 2, na.rm = T),
                                                                   na1 = sum(is.na(h_pl_a_tu)),
                                                                   yes1 = yes/total,
                                                                   no1 = no/ total,
                                                                   total2 = total - na1,
                                                                   yes2 = yes/total2,
                                                                   no2 = no/ total2)
#
table(df.h$h_pl_a_tw)
df.h %>% select(city, h_pl_a_tw ) %>% group_by(city) %>% summarize(total = n(),
                                                                   yes = sum(h_pl_a_tw == 1, na.rm = T),
                                                                   no = sum(h_pl_a_tw == 2, na.rm = T),
                                                                   na = sum(h_pl_a_tw == 4, na.rm = T),
                                                                   na1 = sum(is.na(h_pl_a_tw)),
                                                                   total1 = total - na - na1,
                                                                   yes1 = yes/total1,
                                                                   no1 = no/ total1,
                                                                   total2 = total - na ,
                                                                   yes2 = yes/total2,
                                                                   no2 = no/ total2)
#
table(df.h$h_pl_a_tf)
df.h %>% select(city, h_pl_a_tf ) %>% group_by(city) %>% summarize(total = n(),
                                                                   yes = sum(h_pl_a_tf == 1, na.rm = T,)
                                                                   no = sum(h_pl_a_tf == 2, na.rm = T),
                                                                   dnk = sum(h_pl_a_tf == 3, na.rm = T),
                                                                   na1 = sum(is.na(h_pl_a_tf)),
                                                                   total1 = total - dnk - na1,
                                                                   yes1 = yes/total1,
                                                                   no1 = no/ total1,
                                                                   total2 = total - dnk,
                                                                   yes2 = yes/total2,
                                                                   no2 = no/ total2)

#
table(df.h$h_pl_a_ts)
df.h %>% select(city, h_pl_a_ts ) %>% group_by(city) %>% summarize(total = n(),
                                                                   yes11 = sum(h_pl_a_ts == 1, na.rm = T),
                                                                   yes22 = sum(h_pl_a_ts == 2, na.rm = T),
                                                                   yes = yes11 + yes22,
                                                                   no = sum(h_pl_a_ts == 3, na.rm = T),
                                                                   dnk = sum(h_pl_a_ts == 4, na.rm = T),
                                                                   na = sum(h_pl_a_ts == 5, na.rm = T),
                                                                   na1 = sum(is.na(h_pl_a_ts)),
                                                                   total1 = total - dnk - na - na1,
                                                                   yes1 = yes/total1,
                                                                   no1 = no/ total1,
                                                                   total2 = total - dnk - na,
                                                                   yes2 = yes/total2,
                                                                   no2 = no/ total2)


