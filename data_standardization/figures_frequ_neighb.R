# **********************************************************************************
# **********************************************************************************
# plots to generate behavior data 
# frequency by neighborhood
# **********************************************************************************
library(tidyverse)
library(reshape2)
library(ggrepel)

path <- "/Users/caseysiesel/Desktop/SaniPath/data_standardization/"

source(paste0(path,"helper_dataload_all.R"))

# **********************************************************************************
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
                df.h %>% select(neighb_UID, "coding" = var) %>%
                        group_by(neighb_UID) %>%
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
                        select(neighb_UID, a3, a2, a1, a0, some1, never1, sum) %>% 
                        data.frame() -> df1
                
                
                # SS
                var <-  paste0("s_", xx.var)
                vars <- c(paste0(var,"_3"), paste0(var,"_2"), paste0(var,"_1"), paste0(var,"_0"), paste0(var,"_na")) 
                varspaste <- paste(vars, collapse = "|")
                df <- select(df.s, neighb_UID, matches(varspaste)) 
                
                df %>% group_by(neighb_UID) %>% summarise(a3 = sum(eval(parse(text = vars[1])), na.rm = T),
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
                df <- select(df.c, neighb_UID, matches(varspaste)) 
                
                df %>% group_by(neighb_UID) %>% summarise(a3 = sum(eval(parse(text = vars[1])), na.rm = T),
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
                df.comb <- df4 %>% group_by(neighb_UID) %>% summarise(a3 = sum(a3),
                                                            a2 = sum(a2),
                                                            a1 = sum(a1),
                                                            a0 = sum(a0),
                                                            some1 = sum(some1),
                                                            never1 = sum(never1),
                                                            sum = sum(sum))
                
                
                df.comb <- select(df.comb, neighb_UID, a3, a2, a1, a0)
                df.comb$sample_type <- w
                
                df.all <- rbind(df.all, df.comb)
        }
        
        
        df.all.a <- df.all
        df.all.a$pop <- "Adults"
        
        # df.all.c <- df.all
        # df.all.c$pop <- "Children"
        
}

# children -----
{
        df.all <- data.frame()
        
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
                df.h %>% select(neighb_UID, "coding" = var) %>%
                        group_by(neighb_UID) %>%
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
                        select(neighb_UID, a3, a2, a1, a0, some1, never1, sum) %>% 
                        data.frame() -> df1
                
                
                # SS
                var <-  paste0("s_", xx.var)
                vars <- c(paste0(var,"_3"), paste0(var,"_2"), paste0(var,"_1"), paste0(var,"_0"), paste0(var,"_na")) 
                varspaste <- paste(vars, collapse = "|")
                df <- select(df.s, neighb_UID, matches(varspaste)) 
                
                df %>% group_by(neighb_UID) %>% summarise(a3 = sum(eval(parse(text = vars[1])), na.rm = T),
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
                df <- select(df.c, neighb_UID, matches(varspaste)) 
                
                df %>% group_by(neighb_UID) %>% summarise(a3 = sum(eval(parse(text = vars[1])), na.rm = T),
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
                df.comb <- df4 %>% group_by(neighb_UID) %>% summarise(a3 = sum(a3),
                                                                      a2 = sum(a2),
                                                                      a1 = sum(a1),
                                                                      a0 = sum(a0),
                                                                      some1 = sum(some1),
                                                                      never1 = sum(never1),
                                                                      sum = sum(sum))
                
                
                df.comb <- select(df.comb, neighb_UID, a3, a2, a1, a0)
                df.comb$sample_type <- w
                
                df.all <- rbind(df.all, df.comb)
        }
        
        
        df.all.c <- df.all
        df.all.c$pop <- "Children"
        
}

df.alll <- rbind(df.all.a, df.all.c)

df.alll$sample_type[df.alll$sample_type == "bw"] <- "Bathing Water"
df.alll$sample_type[df.alll$sample_type == "d"] <- "Open Drain Water"
df.alll$sample_type[df.alll$sample_type == "dw"] <- "Municipal Drinking Water"
df.alll$sample_type[df.alll$sample_type == "odw"] <- "Other Drinking Water"
df.alll$sample_type[df.alll$sample_type == "f"] <- "Floodwater"
df.alll$sample_type[df.alll$sample_type == "l"] <- "Public Latrine"
df.alll$sample_type[df.alll$sample_type == "o"] <- "Ocean"
df.alll$sample_type[df.alll$sample_type == "p"] <- "Raw Produce"
df.alll$sample_type[df.alll$sample_type == "s"] <- "Surface Water"
df.alll$sample_type[df.alll$sample_type == "sf"] <- "Street Food"


df.perc <- df.alll %>% group_by(neighb_UID, sample_type, pop) %>%
        mutate(., sum = sum(a3,a2,a1,a0),
               a3 = a3/sum,
               a2 = a2/sum,
               a1 = a1/sum,
               a0 = a0/sum) %>%
        data.frame() %>%
        select(neighb_UID, sample_type, pop, sum, a3, a2, a1, a0 )

# write data for dashboard
# write.csv(df.perc, paste0(getwd(), "/data/behavior_all_city_percent_09162019.csv"), row.names = F, na="")

colnames(df.perc) <- c("neighb_UID", "sample_type", "pop", "sum", "10+", "6-10", "<5", "Never")

df.perc %>% group_by()



df.perc$sum <- NULL
df.perc$pop <- factor(df.perc$pop)


df.perc[ is.na(df.perc) ] <- NA

# add city 
df.perc <- df.perc %>% 
        left_join(., meta_neighb[, c("neighb_UID", "deployment_id")], by = c("neighb_UID" = "neighb_UID")) %>%
        left_join(., meta_dply[, c("id", "country", "city", "citylabel")], by = c("deployment_id" = "id"))









df.perc <- df.perc %>% left_join(meta_neighb[c("neighb_UID", "neighborhood")])
df.perc <- df.perc %>% filter(neighb_UID != 501 & neighb_UID != 502)

#### Casey Added ####
write.csv(df.perc, paste0(getwd(), "/data/behavior_all_city_neighborhood_03242020.csv"), row.names = F, na="")

df.perc$unit <- factor(df.perc$sample_type,
                             levels=c("Open Drain Water", "Ocean", "Surface Water",
                                      "Bathing Water", "Floodwater", "Public Latrine", "Raw Produce", "Street Food",
                                      "Municipal Drinking Water", "Other Drinking Water"),
                             labels=c("Open Drain Water"="Times/Month", "Ocean"="Month",
                                      "Surface Water"="Times/Month", "Bathing Water"="Times/Week",
                                      "Floodwater"="Times/Week", "Public Latrine"="Times/Week",
                                      "Raw Produce"="Times/Week", "Street Food"="Times/Week",
                                      "Municipal Drinking Water"="Days/Week", "Other Drinking Water"="Days/Week"))

#GGPlot version
df.perc %>%
        melt(., id.vars = c("neighb_UID", "sample_type", "pop", "city", "country", "citylabel", "deployment_id", "neighborhood", "unit")) %>%
        na.omit(value) %>%
        # ggplot(aes(x = factor(neighb_UID), y = value, fill = variable)) +
        ggplot(aes(y = neighborhood, x = value, fill = variable)) + #add value labels
        geom_bar(stat = "identity") +
        facet_nested(pop + city ~ sample_type + unit, scales = "free", space = "free", labeller = labeller(pop=label_wrap_gen(5), city=label_wrap_gen(5))) + #scales = "free_x"
        theme_bw() +
        labs(title = "Distribution of Behaviors",
             fill = "Frequency",
             x = "Percent",
             y = "Neighborhood") +
        # theme(strip.text.y = element_text(size = 7)) +
        scale_fill_brewer(palette="Set2") +
        theme(axis.text.x = element_text(angle = 45, hjust = 0.95, size = 7),
              axis.text.y = element_text(size=7),
              # theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2),
              strip.text.x = element_text(size = 8),
              strip.text.y = element_text(size = 6, angle=0, margin=margin(t= 50, r= 0, b=500 , l= 0, unit="pt")),
              strip.background = element_rect(fill="grey"),
              strip.placement = "outside",
              legend.position="bottom",
              panel.spacing=unit(0.1,"lines")) + 
        scale_x_continuous(breaks=seq(0,1,by=.5), labels = scales::percent)

ggplotly(x)


df.perc %>%
        melt(., id.vars = c("neighb_UID", "sample_type", "pop", "city", "country", "citylabel", "deployment_id", "neighborhood", "unit")) %>%
        na.omit(value) %>%
        # ggplot(aes(x = factor(neighb_UID), y = value, fill = variable)) +
        ggplot(aes(x = neighborhood, y = value, fill = variable)) + #add value labels
        geom_bar(stat = "identity") +
        geom_text(aes(label=neighborhood), size=2) +
        coord_flip() +
        facet_nested(pop + city ~ sample_type + unit, scales = "free", space = "free") + #scales = "free_x"
        theme_bw() +
        labs(title = "Distribution of Behaviors",
             fill = "Frequency",
             x = "City",
             y = "Percent") +
        # theme(strip.text.y = element_text(size = 7)) +
        scale_fill_brewer(palette="Set2") +
        

        theme(axis.text.x = element_text(angle = 45, hjust = 0.95, size = 7),
              # theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2),
              strip.text.x = element_text(size = 10),
              strip.text.y = element_text(size = 10),
              strip.background = element_rect(fill="grey"),
              legend.position="bottom",
              panel.spacing=unit(0.5,"lines")) + 
        scale_y_continuous(labels = scales::percent)









df.perc <- df.behav

df.perc <- df.perc %>% filter(neighb_UID != 501 & neighb_UID != 502)
# write.csv(df.perc, paste0(getwd(), "/data/behavior_all_city_percent_09162019.csv"), row.names = F, na="")

for (i in 1:10){
        sampletype <- unique(df.perc$sample_type)[i]
        
        df.perc %>% filter(sample_type == sampletype) %>% 
                melt(., id.vars = c("neighb_UID", "sample_type", "pop", "city", "country", "citylabel", "deployment_id", "neighborhood")) %>%
                na.omit(value) %>%
                # ggplot(aes(x = factor(neighb_UID), y = value, fill = variable)) +
                ggplot(aes(x = neighborhood, y = value, fill = variable, label = round(value*100,0))) + #add value labels
                geom_bar(stat = "identity") +
                geom_text(size = 3, position = position_stack(vjust = 0.5)) +
                facet_grid(pop ~ sample_type + city, scales = "free", space = "free") + #scales = "free_x"
                theme_bw() +
                labs(title = sampletype,
                     fill = "Frequency",
                     x = "City",
                     y = "Percent") +
                # theme(strip.text.y = element_text(size = 7)) +
                scale_fill_brewer(palette="Set2") +
                theme(axis.text.x = element_text(angle = 45, hjust = 0.95, size = 7),
                      # theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2),
                            strip.text.x = element_text(size = 6),
                      strip.text.y = element_text(size = 8),
                      strip.background = element_rect(fill="white"),
                      legend.position="bottom") + 
                scale_y_continuous(labels = scales::percent) -> p
        
        # ggsave(filename=paste0(getwd(), "/images/figure2_neighb/", "fig2_neighb_", i, ".png"), plot = p, width = 12, height = 6, units="in", dpi = 300)
        p
}


# filter sampletype for each
sampletype <- unique(df.perc$sample_type)[1]

df.perc %>% filter(sample_type == sampletype) %>% 
        melt(., id.vars = c("neighb_UID", "sample_type", "pop", "city", "country", "citylabel", "deployment_id")) %>%
        na.omit(value) %>%
        ggplot(aes(x = factor(neighb_UID), y = value, fill = variable)) +
        # ggplot(aes(x = city, y = value, fill = variable, label = round(value*100,0))) + #add value labels
        geom_bar(stat = "identity") +
        # geom_text(size = 3, position = position_stack(vjust = 0.5)) +
        facet_grid(pop ~ city, scales = "free_x", space = "free_x") + #scales = "free_x"
        theme_bw() +
        labs(title = sampletype,
             fill = "Frequency",
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
