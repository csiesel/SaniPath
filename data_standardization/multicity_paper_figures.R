# ***********************************************************************************************
# Multicity Paper
# figures and tables 
# ***********************************************************************************************
# last edit: 02/19/2020

library(tidyverse)

# Import data:
source("helper_dataload_all.R")

# data setup (adding country and city names etc.)
df.ecdata <- df.ecdata %>% 
    left_join(., meta_sampleID, by = c("sample_type" = "id")) %>%
    left_join(., meta_dply[, c("id", "country", "city", "citylabel")], by = c("dply_num" = "id"))

df.col <- df.col %>% left_join(., meta_dply[c("id", "citylabel")], by = c("dply_num" = "id"))
df.lab <- df.lab %>% left_join(., meta_dply[c("id", "citylabel")], by = c("dply_num" = "id"))
df.h <- df.h %>% left_join(., meta_dply[c("id", "citylabel")], by = c("dply_num" = "id"))
df.c <- df.c %>% left_join(., meta_dply[c("id", "citylabel")], by = c("dply_num" = "id"))
df.s <- df.s %>% left_join(., meta_dply[c("id", "citylabel")], by = c("dply_num" = "id"))


# graphs will be by city

table(df.ecdata$dply_num, df.ecdata$sample_type)

# df.ecdata$sample_type[df.ecdata$sample_type == 3 & df.ecdata$col_sample_type_alt != "Drinking Water" ] <- 33
#recode other drinking water
# df.ecdata$sample_type[df.ecdata$sample_type == 3 & df.ecdata$col_sample_type_alt != "Drinking Water" 
#                       & df.ecdata$col_sample_type_alt != "" ] <- 33

table(df.ecdata$col_sample_type_alt)
table(df.ecdata$sample_type)
table(df.ecdata$sample_type_name)

# df.ecdata$col_sample_type_alt <- case_when(df.ecdata$sample_type == 1 & df.ecdata$col_sample_type_alt == "" ~ "Open Drains",
#                                            df.ecdata$sample_type == 2 & df.ecdata$col_sample_type_alt == "" ~ "Produce",
#                                            # df.ecdata$sample_type == 3 & df.ecdata$col_sample_type_alt == "" ~ "Drinking Water",
#                                            df.ecdata$sample_type == 4 & df.ecdata$col_sample_type_alt == "" ~ "Ocean",
#                                            df.ecdata$sample_type == 5 & df.ecdata$col_sample_type_alt == "" ~ "Surface Water",
#                                            df.ecdata$sample_type == 6 & df.ecdata$col_sample_type_alt == "" ~ "Floodwater",
#                                            df.ecdata$sample_type == 7 & df.ecdata$col_sample_type_alt == "" ~ "Public Latrine",
#                                            df.ecdata$sample_type == 8 & df.ecdata$col_sample_type_alt == "" ~ "Particulate",
#                                            df.ecdata$sample_type == 9 & df.ecdata$col_sample_type_alt == "" ~ "Bathing Water",
#                                            df.ecdata$sample_type == 10 & df.ecdata$col_sample_type_alt == "" ~ "Street Food",
#                                            TRUE ~ as.character(df.ecdata$col_sample_type_alt))
df.ecdata$sample_type_name <- as.character(df.ecdata$sample_type_name)

# df.ecdata$sample_type_name[df.ecdata$sample_type == 33] <- "Other Drinking Water"
# df.ecdata$sample_type_name <- as.factor(df.ecdata$sample_type_name)

df.ecdata$col_sample_type_alt <- as.character(df.ecdata$col_sample_type_alt)
df.ecdata$col_sample_type_alt[df.ecdata$col_sample_type_alt == "Drinking Water"] <- ""
# figure 1 -----
# pathway * city
# levels(df.ecdata$col_sample_type_alt) <- gsub(" ", "\n", levels(df.ecdata$col_sample_type_alt))
df.ecdata$col_sample_type_alt[df.ecdata$col_sample_type_alt == "Other Drinking Water"] <- "Other DW"

# df.ecdata$col_sample_type_alt <- as.factor(df.ecdata$col_sample_type_alt)
# levels.list <- c("Open Drains",
#                  "Raw Produce",
#                  "Drinking Water", "Borehole", "Bottled Water", "Ice", "Other DW", "Shallow Well", "Well Water",
#                  "Oceans",
#                  "Surface Water",
#                  "Floodwater",
#                  "Public Latrine",
#                  "Particulate",
#                  "Bathing Water",
#                  "Street Food",
#                  "Other Drinking Water")
# 
# df.ecdata$col_sample_type_alt <- factor(df.ecdata$col_sample_type_alt, levels(df.ecdata$col_sample_type_alt)[factor(levels.list)])

# df.ecdata$sample_type_name[df.ecdata$sample_type == 3] <- "Municipal Drinking Water"

df.ecdata$sample_type_name <- as.factor(df.ecdata$sample_type_name)
levels.list <- c("Open Drain Water",
                 "Raw Produce",
                 "Municipal Drinking Water",
                 "Ocean",
                 "Surface Water",
                 "Floodwater",
                 "Public Latrine",
                 "Soil",
                 "Bathing Water",
                 "Street Food",
                 "Other Drinking Water")

df.ecdata$sample_type_name <- factor(df.ecdata$sample_type_name, levels(df.ecdata$sample_type_name)[factor(levels.list)])




# df.ecdata %>% #filter(col_sample_type_alt == "Open Drains") %>%
#         ggplot(., aes(x=factor(col_sample_type_alt), y=log10(ec_conc), fill=factor(city))) +
#         stat_boxplot(geom = "errorbar", width = 0.35) +
#         geom_boxplot() +
#         # facet_wrap(~ sample_type_name, nrow = 2, scales = "free_x") +
#         labs(fill = "City",
#              x = "",
#              y = "E.coli (Log10)") +
#         theme_bw() +
#         theme(legend.position="bottom",
#               axis.text=element_text(size=8),
#               strip.background = element_rect(fill="white")) +
#         scale_fill_brewer(palette="Set1")
        # scale_fill_manual(values = c('#1f78b4','#33a02c','#a6cee3','#bf812d','#6baed6',
                                     # '#4292c6','#e31a1c','#fdbf6f','#08519c','#cab2d6'))


df.ecdata %>% 
        ggplot(., aes(x=factor(col_sample_type_alt), y=log10(ec_conc), fill=city)) +
        # stat_boxplot(geom = "errorbar", width = 0.35) +
        # geom_boxplot(position = position_dodge2(preserve = "single") ) +
        geom_boxplot( ) +
        # facet_wrap(~ sample_type_name, scales = "free", ncol=5) +
        # facet_wrap(~ sample_type_name, scales = "free_x", ncol=5) +
        facet_wrap( ~ sample_type_name, scales = "free_x") + 
        labs(fill = "City",
             x = "",
             y = "E.coli (Log10)") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2),
              # legend.position = c(0.6, 0.1),
              # legend.direction = "horizontal",
              axis.text=element_text(size=8),
              strip.background = element_rect(fill="white")) +
        scale_fill_brewer(palette="Set1") 
        # scale_fill_discrete(name = '')
        # coord_flip()
    
df.ecdata %>% filter(sample_type == 33) %>%
        ggplot(., aes(x=factor(col_sample_type_alt), y=log10(ec_conc), fill=city)) +
        # stat_boxplot(geom = "errorbar", width = 0.35) +
        geom_boxplot(position = position_dodge2(preserve = "single") )
    
#sample size
df.ecdata %>% group_by(sample_type_name, city) %>% summarize(n = n(),
                                                             na = sum(is.na(ec_conc)),
                                                             n_actual = n-na) %>% 
write.csv(., paste0(getwd(), "/images/multicity_environment_sample-size.csv"), row.names = F, na="")





##### figure 1 ----
# add each one in powerpoint to compile entire graphics
# in ppt: select all, then save as jpeg, then convert to pdf

table(df.ecdata$sample_type_name)
myColors <- brewer.pal(10, "Set3")   #ifelse(prop == TRUE, "Set2", "Set1")
names(myColors) <- unique(meta_dply$city)
colScale <- scale_fill_manual(values = myColors)

odwdf <- data.frame(odw=c("Borehole", "Bottled Water", "Ice", "Other DW", "Shallow Well", "Well Water"), loc = c(1:6), 
                    y=rep(5,6))

for (i in c(1:10)) {
        pathwaynr <- i
        
        df.ecdata %>% filter(sample_type == pathwaynr) %>%
                # df.ecdata %>% filter(sample_type != 33) %>%
                ggplot(., aes(x=factor(city), y=log10(ec_conc), fill=city), group=city) +
                # ggplot(., aes(x=factor(col_sample_type_alt), y=log10(ec_conc), fill=city), group=city) +
                geom_boxplot() +
                facet_grid( ~ sample_type_name, scales = "free_x") +
                labs(fill = "City",
                     x = "",
                     y = "") +
                theme_bw() +
                ylim(c(-1,10)) +
                theme(#axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2),
                        # legend.position = c(0.6, 0.1),
                        # legend.direction = "horizontal",
                        legend.position = "none",
                        axis.text=element_text(size=10),
                        strip.background = element_rect(fill="white"),
                        axis.title.x=element_blank(),
                        axis.text.x=element_blank()) +
                colScale -> p 

        ggsave(filename=paste0(getwd(), "/images/", "figure1_v19_", pathwaynr, ".png"), plot = p, width = 2.5, height = 3.3, units="in", dpi = 500)
        
}
        
pathwaynr <- 4
df.ecdata %>% filter(sample_type == pathwaynr) %>%
        # ggplot(., aes(x=factor(sample_type_name), y=log10(ec_conc), fill=city), group=city) +
    ggplot(., aes(x=factor(city), y=log10(ec_conc), fill=city), group=city) +
    # ggplot(., aes(x=factor(col_sample_type_alt), y=log10(ec_conc), fill=city), group=city) +
        geom_boxplot() +
        facet_grid( ~ sample_type_name, scales = "free_x") +
        labs(fill = "City",
             x = "",
             y = "") +
        theme_bw() +
        ylim(c(-1,10)) +
        theme(#axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2),
                # legend.position = c(0.6, 0.1),
                # legend.direction = "horizontal",
                legend.position = "none",
                axis.text=element_text(size=8),
                strip.background = element_rect(fill="white"),
                # strip.text.x = element_text(size = 12),
                axis.title.x=element_blank(),
                axis.text.x=element_blank()) +
        colScale -> p 
# labeling for 33
p <- p + geom_text(aes(x = col_sample_type_alt, y = 4.5, label = col_sample_type_alt), size = 3, angle = 90,  hjust = 0)

ggsave(filename=paste0(getwd(), "/images/", "figure1_v19_", pathwaynr, ".png"), plot = p, width = 1, height = 3.3, units="in", dpi = 500)

#width for ocean (4) 1
#width for surface water (5) 1.6
#width for all others 2.5

aggregate(log10(ec_conc) ~  city, df.ecdata[df.ecdata$sample_type == 3, ], mean)
aggregate(log10(ec_conc) ~  city, df.ecdata[df.ecdata$sample_type == 3, ], median)

# table 3 ----
df.ecdata %>% group_by(sample_type, city) %>% summarise(mean = round(log10(mean(ec_conc, na.rm = T)),2),
                                                        min = round(log10(min(ec_conc, na.rm = T)),2),
                                                        max = round(log10(max(ec_conc, na.rm = T)),2),
                                                        median = round(log10(median(ec_conc, na.rm = T)),2)) -> df.summary

df.ecdata %>% filter(sample_type == 33) %>%
    group_by(col_sample_type_alt, city) %>% summarise(min = min(log10(ec_conc), na.rm = T),
                                                        max = max(log10(ec_conc), na.rm = T),
                                                        mean = mean(log10(ec_conc), na.rm = T),
                                                        median = median(log10(ec_conc), na.rm = T))

# ggsave(filename=paste0(getwd(), "/images/", "figure1_v5.png"),plot = last_plot(), width = 14.23, height = 8, units="in", dpi = 500)
# ggsave(filename=paste0(getwd(), "/images/", "figure1_v5.pdf"),plot = last_plot(), width = 14.23, height = 8, units="in", dpi = 500)




# figure 2 -----
# see other file for graph


# combnded frequency
# stacked bar chart

# frequ combined - answer options by city
# adults -----
{
    df.all <- data.frame()
    
    adults = T
    # adults = F
    pathways <- c("d", "p", "dw", "odw", "o", "s", "f", "l", "bw", "sf")
    # pathways for y/n and y/no questions
    # pathways.yn <- c("h_dw_e_wt", "h_p_e", "h_pl_a_th", "h_pl_a_tu", "h_pl_a_tw", "h_pl_a_tf") 
    
    for (w in pathways){
            xx.var <- w
            
            # data 
            pop <- if (adults == T) "_a" else "_c"
            s.var <- xx.var
            xx.var <- paste0(xx.var, pop)
            # HH
            var <-  paste0("h_", xx.var)
            df.h %>% select(city, "coding" = var) %>%
                    group_by(city) %>%
                    summarise(a3 = sum(coding==1, na.rm = T),
                              a2 = sum(coding==2, na.rm = T),
                              a1 = sum(coding==3, na.rm = T),
                              a0 = sum(coding==4, na.rm = T),
                              some1 = sum(sum(coding==1, na.rm = T), sum(coding==2, na.rm = T), sum(coding==3, na.rm = T) ) ,
                              never1 = sum(coding==4, na.rm = T),
                              exclude = sum(sum(coding==5, na.rm = T), sum(coding==6, na.rm = T)),
                              na = sum(is.na(coding)),
                              sum1 = n(),
                              sum=(some1+never1) ) %>%
                    select(city, a3, a2, a1, a0, some1, never1, sum) %>% 
                    data.frame() -> df1
            
            
            # SS
            var <-  paste0("s_", xx.var)
            vars <- c(paste0(var,"_3"), paste0(var,"_2"), paste0(var,"_1"), paste0(var,"_0"), paste0(var,"_na")) 
            varspaste <- paste(vars, collapse = "|")
            df <- select(df.s, city, matches(varspaste)) 
            
            df %>% group_by(city) %>% summarise(a3 = sum(eval(parse(text = vars[1])), na.rm = T),
                                                a2 = sum(eval(parse(text = vars[2])), na.rm = T),
                                                a1 = sum(eval(parse(text = vars[3])), na.rm = T),
                                                a0 = sum(eval(parse(text = vars[4])), na.rm = T),
                                                some1 = (a3+a2+a1),
                                                never1 = a0,
                                                sum = sum(a3, a2, a1, a0) ) %>%
                    # select(citylabel, some1, never1, sum) %>% 
                    data.frame() -> df2
            

            # CC
            var <-  paste0("c_", xx.var)
            vars <- c(paste0(var,"_3"), paste0(var,"_2"), paste0(var,"_1"), paste0(var,"_0"), paste0(var,"_na")) 
            varspaste <- paste(vars, collapse = "|")
            df <- select(df.c, city, matches(varspaste)) 
            
            df %>% group_by(city) %>% summarise(a3 = sum(eval(parse(text = vars[1])), na.rm = T),
                                                a2 = sum(eval(parse(text = vars[2])), na.rm = T),
                                                a1 = sum(eval(parse(text = vars[3])), na.rm = T),
                                                a0 = sum(eval(parse(text = vars[4])), na.rm = T),
                                                some1 = (a3+a2+a1),
                                                never1 = a0,
                                                sum = sum(a3, a2, a1, a0)) %>%
                    # select(citylabel, some1, never1, sum) %>% 
                    data.frame()  -> df3
            

            df4 <- bind_rows(df1, df2)
            df4 <- bind_rows(df4, df3)
            df.comb <- df4 %>% group_by(city) %>% summarise(a3 = sum(a3),
                                                                  a2 = sum(a2),
                                                                  a1 = sum(a1),
                                                                  a0 = sum(a0),
                                                                  some1 = sum(some1),
                                                                  never1 = sum(never1),
                                                                  sum = sum(sum))
            
            
            df.comb <- select(df.comb, city, a3, a2, a1, a0)
            df.comb$sample_type <- w
            
            df.all <- rbind(df.all, df.comb)
    }
    
    
    df.all.a <- df.all
    df.all.a$pop <- "Adults"
    
    # df.all.c <- df.all
    # df.all.c$pop <- "Children"
    
    }

# frequ combined - answer options by city
# children -----
# adults -----
{
    df.all <- data.frame()
    
    # adults = T
    adults = F
    pathways <- c("d", "p", "dw", "odw", "o", "s", "f", "l", "bw", "sf")
    # pathways for y/n and y/no questions
    # pathways.yn <- c("h_dw_e_wt", "h_p_e", "h_pl_a_th", "h_pl_a_tu", "h_pl_a_tw", "h_pl_a_tf") 
    
    for (w in pathways){
        xx.var <- w
        
        # data 
        pop <- if (adults == T) "_a" else "_c"
        s.var <- xx.var
        xx.var <- paste0(xx.var, pop)
        # HH
        var <-  paste0("h_", xx.var)
        df.h %>% select(city, "coding" = var) %>%
            group_by(city) %>%
            summarise(a3 = sum(coding==1, na.rm = T),
                      a2 = sum(coding==2, na.rm = T),
                      a1 = sum(coding==3, na.rm = T),
                      a0 = sum(coding==4, na.rm = T),
                      some1 = sum(sum(coding==1, na.rm = T), sum(coding==2, na.rm = T), sum(coding==3, na.rm = T) ) ,
                      never1 = sum(coding==4, na.rm = T),
                      exclude = sum(sum(coding==5, na.rm = T), sum(coding==6, na.rm = T)),
                      na = sum(is.na(coding)),
                      sum1 = n(),
                      sum=(some1+never1) ) %>%
            select(city, a3, a2, a1, a0, some1, never1, sum) %>% 
            data.frame() -> df1
        
        
        # SS
        var <-  paste0("s_", xx.var)
        vars <- c(paste0(var,"_3"), paste0(var,"_2"), paste0(var,"_1"), paste0(var,"_0"), paste0(var,"_na")) 
        varspaste <- paste(vars, collapse = "|")
        df <- select(df.s, city, matches(varspaste)) 
        
        df %>% group_by(city) %>% summarise(a3 = sum(eval(parse(text = vars[1])), na.rm = T),
                                            a2 = sum(eval(parse(text = vars[2])), na.rm = T),
                                            a1 = sum(eval(parse(text = vars[3])), na.rm = T),
                                            a0 = sum(eval(parse(text = vars[4])), na.rm = T),
                                            some1 = (a3+a2+a1),
                                            never1 = a0,
                                            sum = sum(a3, a2, a1, a0) ) %>%
            # select(citylabel, some1, never1, sum) %>% 
            data.frame() -> df2
        
        
        # CC
        var <-  paste0("c_", xx.var)
        vars <- c(paste0(var,"_3"), paste0(var,"_2"), paste0(var,"_1"), paste0(var,"_0"), paste0(var,"_na")) 
        varspaste <- paste(vars, collapse = "|")
        df <- select(df.c, city, matches(varspaste)) 
        
        df %>% group_by(city) %>% summarise(a3 = sum(eval(parse(text = vars[1])), na.rm = T),
                                            a2 = sum(eval(parse(text = vars[2])), na.rm = T),
                                            a1 = sum(eval(parse(text = vars[3])), na.rm = T),
                                            a0 = sum(eval(parse(text = vars[4])), na.rm = T),
                                            some1 = (a3+a2+a1),
                                            never1 = a0,
                                            sum = sum(a3, a2, a1, a0)) %>%
            # select(citylabel, some1, never1, sum) %>% 
            data.frame()  -> df3
        
        
        df4 <- bind_rows(df1, df2)
        df4 <- bind_rows(df4, df3)
        df.comb <- df4 %>% group_by(city) %>% summarise(a3 = sum(a3),
                                                        a2 = sum(a2),
                                                        a1 = sum(a1),
                                                        a0 = sum(a0),
                                                        some1 = sum(some1),
                                                        never1 = sum(never1),
                                                        sum = sum(sum))
        
        
        df.comb <- select(df.comb, city, a3, a2, a1, a0)
        df.comb$sample_type <- w
        
        df.all <- rbind(df.all, df.comb)
    }

    
    df.all.c <- df.all
    df.all.c$pop <- "Children"
    
}


df.alll <- rbind(df.all.a, df.all.c)

table(df.alll$sample_type)
df.alll$sample_type[df.alll$sample_type == "bw"] <- "Bathing Water"
df.alll$sample_type[df.alll$sample_type == "d"] <- "Open Drain Water"
df.alll$sample_type[df.alll$sample_type == "dw"] <- "Municipal Drinking Water"
df.alll$sample_type[df.alll$sample_type == "odw"] <- "Other DW"
df.alll$sample_type[df.alll$sample_type == "f"] <- "Floodwater"
df.alll$sample_type[df.alll$sample_type == "l"] <- "Public Latrine"
df.alll$sample_type[df.alll$sample_type == "o"] <- "Oc." #ocean
df.alll$sample_type[df.alll$sample_type == "p"] <- "Raw Produce"
df.alll$sample_type[df.alll$sample_type == "s"] <- "Surface Water"
df.alll$sample_type[df.alll$sample_type == "sf"] <- "Street Food"


df.perc <- df.alll %>% group_by(city, sample_type, pop) %>%
        mutate(., sum = sum(a3,a2,a1,a0),
                  a3 = a3/sum,
                  a2 = a2/sum,
                  a1 = a1/sum,
                  a0 = a0/sum) %>%
        data.frame() %>%
        select(city, sample_type, pop, sum, a3, a2, a1, a0 )

# write data for dashboard
# write.csv(df.perc, paste0(getwd(), "/data/behavior_all_city_percent_02122020.csv"), row.names = F, na="")

colnames(df.perc) <- c("city", "sample_type", "pop", "sum", "10+", "6-10", "<5", "Never")

df.perc %>% group_by()

# remove kolkata
df.perc <- df.perc %>% filter(!city == "Kolkata")



df.perc$sum <- NULL
df.perc$pop <- factor(df.perc$pop)

#manually adding never for siem reap.
df.perc$Never[df.perc$city == "Siem Reap" & df.perc$sample_type == "Municipal Drinking Water"] <- 1


df.perc[ is.na(df.perc) ] <- NA

# rearrange facets with levels
# df.perc <- df.perc %>% mutate(df.perc = factor(.$sample_type, levels = factor(c("Ocn"))))


#rearrange sample types
unique(df.perc$sample_type)
df.perc$sample_type <- as.factor(df.perc$sample_type)
levels.list <- c(
                 "Open Drain Water",
                                  "Oc.",
                 "Surface Water",
                 "Other DW",
                 "Municipal Drinking Water",
                                  "Bathing Water",
                 "Floodwater",
                 "Public Latrine",
                 "Raw Produce",
                 "Street Food"
                 )

df.perc$sample_type <- factor(df.perc$sample_type, levels(df.perc$sample_type)[factor(levels.list)])


df.perc %>% #filter(pop == "Adults") %>%
        melt(., id.vars = c("city", "sample_type", "pop")) %>%
        na.omit(value) %>%
        ggplot(aes(x = city, y = value, fill = variable)) +
        # ggplot(aes(x = city, y = value, fill = variable, label = round(value*100,0))) + #add value labels
        geom_bar(stat = "identity") +
        # geom_text(size = 3, position = position_stack(vjust = 0.5)) +
        facet_grid(pop ~ sample_type, scales = "free_x", space = "free_x") + #scales = "free_x"
        # facet_grid(sample_type ~ pop) +
        theme_bw() +
        labs(fill = "Frequency",
             x = "City",
             y = "Percent") +
        # theme(strip.text.y = element_text(size = 7)) +
        scale_fill_brewer(palette="Set2") +
        theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2),
              strip.text.x = element_text(size = 8),
              strip.text.y = element_text(size = 10),
              strip.background = element_rect(fill="white"),
              legend.position="bottom") + 
        scale_y_continuous(labels = scales::percent) -> p
p

# ggsave(filename=paste0(getwd(), "/images/", "figure2_v4.png"),plot = last_plot(), width = 15, height = 8, units="in", dpi = 300)
# ggsave(filename=paste0(getwd(), "/images/", "figure2_v7.png"),plot = last_plot(), width = 13, height = 6.9, units="in", dpi = 300)
# ggsave(filename=paste0(getwd(), "/images/figure2/", "figure2_all_v9.png"),plot = last_plot(), width = 16.67, height = 8.69, units="in", dpi = 300)
# ggsave(filename=paste0(getwd(), "/images/figure2/", "figure2_all_v9.pdf"),plot = last_plot(), width = 16.67, height = 8.69, units="in", dpi = 300)

ggsave(filename=paste0(getwd(), "/images/figure2/", "figure2_all_v10.png"),plot = last_plot(), width = 12, height = 6, units="in", dpi = 300)
ggsave(filename=paste0(getwd(), "/images/figure2/", "figure2_all_v10.pdf"),plot = last_plot(), width = 12, height = 6, units="in", dpi = 300)

# ggsave(filename=paste0(getwd(), "/images/figure2/", "figure2_all_v2.png"),plot = p, width = 12, height = 6, units="in", dpi = 300)
# ggsave(filename=paste0(getwd(), "/images/figure2/", "figure2_all_v8_adults.png"),plot = p, width = 12, height = 6, units="in", dpi = 300)
# ggsave(filename=paste0(getwd(), "/images/figure2/", "figure2_all_v6_kolkata2.png"),plot = p, width = 12, height = 6, units="in", dpi = 300)


# alternative plot to see frequency by neighborhood in city



df.perc %>% filter(sample_type == "Bathing Water") %>% 
    melt(., id.vars = c("city", "sample_type", "pop")) %>%
    na.omit(value) %>%
    ggplot(aes(x = city, y = value, fill = variable)) +
    # ggplot(aes(x = city, y = value, fill = variable, label = round(value*100,0))) + #add value labels
    geom_bar(stat = "identity") +
    # geom_text(size = 3, position = position_stack(vjust = 0.5)) +
    facet_grid(pop ~ sample_type, scales = "free_x", space = "free_x") + #scales = "free_x"
    # facet_grid(sample_type ~ pop) +
    theme_bw() +
    labs(fill = "Frequency",
         x = "City",
         y = "Percent") +
    # theme(strip.text.y = element_text(size = 7)) +
    scale_fill_brewer(palette="Set2") +
    theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2),
          strip.text.x = element_text(size = 6),
          strip.text.y = element_text(size = 8),
          strip.background = element_rect(fill="white"),
          legend.position="bottom") + 
    scale_y_continuous(labels = scales::percent)




# plot 3 exposure -----

# library(readxl)
# 
# multi <- read_excel(paste0(getwd(), "/data/cross_country_data_2019-07-02.xlsx"), col_names = TRUE, na = "NA")
# multi <- left_join(multi, meta_dply[c("citylabel", "date")])
# 
# multi$unlogdose <- 10^(multi$dose)
# multi$unlogexp <- multi$unlogdose * (multi$percent / 100)
# 
# # this graph is for one city, but future analysis will calculate 1 exposure per city - 
# #        then use city instead of neighborhood variable and plot all cityies next to each other
# multi %>%
#         filter(city == "Dhaka" & age == "a") %>%
#         ggplot(., aes(x=neighborhood, y=(unlogexp), fill=pathway )) +
#         geom_bar(stat = "identity") +
#         scale_fill_brewer(palette="Set1") +
#         theme_bw() +
#         labs(title = "Dhaka, Adults",
#              fill = "Pathway",
#              x = "Neighborhood",
#              y = "Exposure") 
# 
# 
# 

# table 5 -----
df.all <- data.frame()

pathways.yn <- c("pl_a_th", "pl_a_tu", "pl_a_tw", "pl_a_tf", "pl_a_ts")
for (w in pathways.yn){
    xx.var <- w
    
    # data 
    s.var <- xx.var
    
    # HH
    var <-  paste0("h_", xx.var)
    df.h %>% select(city, "coding" = var) %>%
        group_by(city) %>%
        summarise(yes = sum(coding==1, na.rm = T),
                  no = sum(coding==2, na.rm = T),
                  dnk = sum(coding==3, na.rm = T),
                  nappl = sum(coding==4, na.rm = T),
                  sum1 = sum(yes, no, dnk, nappl) ) %>%
        data.frame() -> df1
    
    df1$sample_type <- w
    
    df.all <- rbind(df.all, df1)
}

df.all$percyes <- round(df.all$yes/df.all$sum1,3)
df.all

df.all <- data.frame()

# pathways for y/n and y/no questions
# pathways.yn <- c("dw_e_wt", "p_e", "pl_a_th", "pl_a_tu", "pl_a_tw", "pl_a_tf")
# "p_e" has 2 and 1 coding 
for (w in pathways.yn){
        xx.var <- w
        
        # data 
        s.var <- xx.var
        
        # HH
        var <-  paste0("h_", xx.var)
        df.h %>% select(city, "coding" = var) %>%
                group_by(city) %>%
                summarise(yes = sum(coding==1, na.rm = T),
                          no = sum(coding==2, na.rm = T),
                          dnk = sum(coding==3, na.rm = T),
                          nappl = sum(coding==4, na.rm = T),
                          sum1 = sum(yes, no, dnk, nappl) ) %>%
                data.frame() -> df1
        # df1$summ <- df1$yes + df1$no + df1$dnk + df1$nappl + df1$na
        df1$city <- as.character(df1$city)
        
        # SS
        var <-  paste0("s_", xx.var)
        vars <- if(w == "p_e") {
                c(paste0(var,"_2"), paste0(var,"_1"), paste0(var,"_na")) 
                } else {
                c(paste0(var,"_1"), paste0(var,"_0"), paste0(var,"_na"))
                }
        varspaste <- paste(vars, collapse = "|")
        df <- select(df.s, city, matches(varspaste)) 
        
        df %>% group_by(city) %>% 
                summarise(yes = sum(eval(parse(text = vars[1])), na.rm = T),
                          no = sum(eval(parse(text = vars[2])), na.rm = T),
                          dnk = sum(eval(parse(text = vars[3])), na.rm = T),
                          sum1 = sum(yes, no, dnk) ) %>%
                data.frame() -> df2
        df2$nappl <- 0

        df2 <- df2 %>% select(city, yes, no, dnk, nappl, sum1)
        df2$city <- as.character(df2$city)
        
        # add missing deployments manually
        suppressWarnings(df2 <- rbind(df2, c(999, rep(0,8))))
        suppressWarnings(df2 <- rbind(df2, c(998, rep(0,8))))
        suppressWarnings(df2 <- rbind(df2, c(998, rep(0,8))))
        df2$city[nrow(df2)-2] <- "Atlanta"
        df2$city[nrow(df2)-1] <- "Siem Reap"
        df2$city[nrow(df2)  ] <- "Maputo"
        
        # sort
        df2 <- df2[match(df1$city, df2$city),]
        
        
        # CC
        var <-  paste0("c_", xx.var)
        vars <- if(w == "p_e") {
                c(paste0(var,"_2"), paste0(var,"_1"), paste0(var,"_na")) 
        } else {
                c(paste0(var,"_1"), paste0(var,"_0"), paste0(var,"_na"))
        }
        varspaste <- paste(vars, collapse = "|")
        df <- select(df.c, city, matches(varspaste)) 
        
        df %>% group_by(city) %>% 
                summarise(yes = sum(eval(parse(text = vars[1])), na.rm = T),
                          no = sum(eval(parse(text = vars[2])), na.rm = T),
                          dnk = sum(eval(parse(text = vars[3])), na.rm = T),
                          sum1 = sum(yes, no, dnk) ) %>%
                data.frame() -> df3
        df3$nappl <- 0

        df3 <- df3 %>% select(city, yes, no, dnk, nappl, sum1)
        df3$city <- as.character(df3$city)
        
        
        # add missing deployments manually
        suppressWarnings(df3 <- rbind(df3, c(999, rep(0,8))))
        suppressWarnings(df3 <- rbind(df3, c(998, rep(0,8))))
        suppressWarnings(df3 <- rbind(df3, c(998, rep(0,8))))
        suppressWarnings(df3 <- rbind(df3, c(998, rep(0,8))))
        df3$city[nrow(df3)-3] <- "Atlanta"
        df3$city[nrow(df3)-2] <- "Vellore"
        df3$city[nrow(df3)-1] <- "Siem Reap"
        df3$city[nrow(df3)  ] <- "Maputo"
        
        # sort
        df3 <- df3[match(df1$city, df3$city),]
        
        # adding all 3 df together
        df.comb <- data.frame()
        
        for (i in df1$city) {
                df <- cbind("city" = i, df1[df1$city == i, ][,-1] + df2[df2$city == i, ][,-1] + df2[df2$city == i, ][,-1])
                df.comb <- rbind(df.comb, df)
        }
        
        df.comb$sample_type <- w
        
        df.all <- rbind(df.all, df.comb)
}




df.all.yn <- df.all
# df.all.yn <- df.all.yn[-c(2,7,11,16,17,18,20,29,34,38,43,47,52), ]

# check math:
# df.all.yn$summ <- df.all.yn$yes + df.all.yn$no + df.all.yn$dnk + df.all.yn$nappl

table(df.all.yn$sample_type)

# write.csv(df.all.yn, paste0(getwd(), "/images/", "multicity_table.csv"), row.names = F, na="")


# 
