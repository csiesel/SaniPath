library(tidyverse)
library(reshape2)
library(ggh4x)
library(ggplot2)


df.h <- read.csv("~/Downloads/spdurban-household-2192.csv", header=TRUE)


{
  df.all <- data.frame()
  
  adults = T
  # adults = F
  pathways <- c("d", "p", "dw", "o", "s", "f", "l", "bw", "sf")
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
    df.h %>% select(h_neighborhood, "coding" = var) %>%
      group_by(h_neighborhood) %>%
      summarise(a3 = sum(coding==1, na.rm = T),
                a2 = sum(coding==2, na.rm = T),
                a1 = sum(coding==3, na.rm = T),
                a0 = sum(coding==4, na.rm = T),
                some1 = sum(sum(coding==1, na.rm = T), sum(coding==2, na.rm = T), sum(coding==3, na.rm = T) ) ,
                never1 = sum(coding==4, na.rm = T),
                exclude = sum(sum(coding==5, na.rm = T), sum(coding==6, na.rm = T)),
                na = sum(is.na(coding)),
                sum1 = n(),
                sum=(some1+never1)) %>%
      select(h_neighborhood, a3, a2, a1, a0, some1, never1, sum) %>% 
      data.frame() -> df1
    
    # 
    # # SS
    # var <-  paste0("s_", xx.var)
    # vars <- c(paste0(var,"_3"), paste0(var,"_2"), paste0(var,"_1"), paste0(var,"_0"), paste0(var,"_na")) 
    # varspaste <- paste(vars, collapse = "|")
    # df <- select(df.s, h_neighborhood, matches(varspaste)) 
    # 
    # df %>% group_by(h_neighborhood) %>% summarise(a3 = sum(eval(parse(text = vars[1])), na.rm = T),
    #                                           a2 = sum(eval(parse(text = vars[2])), na.rm = T),
    #                                           a1 = sum(eval(parse(text = vars[3])), na.rm = T),
    #                                           a0 = sum(eval(parse(text = vars[4])), na.rm = T),
    #                                           some1 = (a3+a2+a1),
    #                                           never1 = a0,
    #                                           sum = sum(a3, a2, a1, a0) ) %>%
    #   # select(citylabel, some1, never1, sum) %>% 
    #   data.frame() -> df2
    # 
    # # CC
    # var <-  paste0("c_", xx.var)
    # vars <- c(paste0(var,"_3"), paste0(var,"_2"), paste0(var,"_1"), paste0(var,"_0"), paste0(var,"_na")) 
    # varspaste <- paste(vars, collapse = "|")
    # df <- select(df.c, h_neighborhood, matches(varspaste)) 
    # 
    # df %>% group_by(h_neighborhood) %>% summarise(a3 = sum(eval(parse(text = vars[1])), na.rm = T),
    #                                           a2 = sum(eval(parse(text = vars[2])), na.rm = T),
    #                                           a1 = sum(eval(parse(text = vars[3])), na.rm = T),
    #                                           a0 = sum(eval(parse(text = vars[4])), na.rm = T),
    #                                           some1 = (a3+a2+a1),
    #                                           never1 = a0,
    #                                           sum = sum(a3, a2, a1, a0)) %>%
    #   # select(citylabel, some1, never1, sum) %>% 
    #   data.frame()  -> df3
    # # 
    # df4 <- bind_rows(df1, df2)
    # df4 <- bind_rows(df4, df3)
    df.comb <- df1 %>% group_by(h_neighborhood) %>% summarise(a3 = sum(a3),
                                                          a2 = sum(a2),
                                                          a1 = sum(a1),
                                                          a0 = sum(a0),
                                                          some1 = sum(some1),
                                                          never1 = sum(never1),
                                                          sum = sum(sum))
    
    
    df.comb <- select(df.comb, h_neighborhood, a3, a2, a1, a0)
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
  pathways <- c("d", "p", "dw", "o", "s", "f", "l", "bw", "sf")
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
    df.h %>% select(h_neighborhood, "coding" = var) %>%
      group_by(h_neighborhood) %>%
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
      select(h_neighborhood, a3, a2, a1, a0, some1, never1, sum) %>% 
      data.frame() -> df1
    
    # 
    # # SS
    # var <-  paste0("s_", xx.var)
    # vars <- c(paste0(var,"_3"), paste0(var,"_2"), paste0(var,"_1"), paste0(var,"_0"), paste0(var,"_na")) 
    # varspaste <- paste(vars, collapse = "|")
    # df <- select(df.s, neighb_UID, matches(varspaste)) 
    # 
    # df %>% group_by(neighb_UID) %>% summarise(a3 = sum(eval(parse(text = vars[1])), na.rm = T),
    #                                           a2 = sum(eval(parse(text = vars[2])), na.rm = T),
    #                                           a1 = sum(eval(parse(text = vars[3])), na.rm = T),
    #                                           a0 = sum(eval(parse(text = vars[4])), na.rm = T),
    #                                           some1 = (a3+a2+a1),
    #                                           never1 = a0,
    #                                           sum = sum(a3, a2, a1, a0) ) %>%
    #   # select(citylabel, some1, never1, sum) %>% 
    #   data.frame() -> df2
    # 
    # # CC
    # var <-  paste0("c_", xx.var)
    # vars <- c(paste0(var,"_3"), paste0(var,"_2"), paste0(var,"_1"), paste0(var,"_0"), paste0(var,"_na")) 
    # varspaste <- paste(vars, collapse = "|")
    # df <- select(df.c, neighb_UID, matches(varspaste)) 
    # 
    # df %>% group_by(neighb_UID) %>% summarise(a3 = sum(eval(parse(text = vars[1])), na.rm = T),
    #                                           a2 = sum(eval(parse(text = vars[2])), na.rm = T),
    #                                           a1 = sum(eval(parse(text = vars[3])), na.rm = T),
    #                                           a0 = sum(eval(parse(text = vars[4])), na.rm = T),
    #                                           some1 = (a3+a2+a1),
    #                                           never1 = a0,
    #                                           sum = sum(a3, a2, a1, a0)) %>%
    #   # select(citylabel, some1, never1, sum) %>% 
    #   data.frame()  -> df3
    # 
    # df4 <- bind_rows(df1, df2)
    # df4 <- bind_rows(df4, df3)
    df.comb <- df1 %>% group_by(h_neighborhood) %>% summarise(a3 = sum(a3),
                                                          a2 = sum(a2),
                                                          a1 = sum(a1),
                                                          a0 = sum(a0),
                                                          some1 = sum(some1),
                                                          never1 = sum(never1),
                                                          sum = sum(sum))
    
    
    df.comb <- select(df.comb, h_neighborhood, a3, a2, a1, a0)
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


df.perc <- df.alll %>% group_by(h_neighborhood, sample_type, pop) %>%
  mutate(., sum = sum(a3,a2,a1,a0),
         a3 = a3/sum,
         a2 = a2/sum,
         a1 = a1/sum,
         a0 = a0/sum) %>%
  data.frame() %>%
  select(h_neighborhood, sample_type, pop, sum, a3, a2, a1, a0 )

# write data for dashboard
# write.csv(df.perc, paste0(getwd(), "/data/behavior_all_city_percent_09162019.csv"), row.names = F, na="")

colnames(df.perc) <- c("neighborhood", "sample_type", "pop", "sum", "10+", "6-10", "<5", "Never")

df.perc %>% group_by()



df.perc$sum <- NULL
df.perc$pop <- factor(df.perc$pop)


df.perc[ is.na(df.perc) ] <- NA
df.perc$neighborhood <- factor(df.perc$neighborhood, labels=c("421"="Quarry Road", "422"="Point Precinct",
                                                              "423"="Hammonds Farm", "424"= "Mzinyathi"))

df.perc$unit <- factor(df.perc$sample_type, levels=c("Open Drain Water", "Ocean", "Surface Water",
                                                     "Bathing Water", "Floodwater", "Public Latrine", "Raw Produce", "Street Food",
                                                     "Municipal Drinking Water"), labels=c("Open Drain Water"="Times/Month", "Ocean"="Month", "Surface Water"="Times/Month",
                                                     "Bathing Water"="Times/Week", "Floodwater"="Times/Week", "Public Latrine"="Times/Week", "Raw Produce"="Times/Week", "Street Food"="Times/Week",
                                                     "Municipal Drinking Water"="Days/Week"))

#------------------------------------------
  bx_plot <- df.perc %>%
    melt(., id.vars = c("neighborhood", "sample_type", "pop", "unit")) %>%
    na.omit(value) %>%
    # ggplot(aes(x = factor(neighb_UID), y = value, fill = variable)) +
    ggplot(aes(x = neighborhood, y = value, fill = variable)) + #add value labels
    geom_bar(stat = "identity") +
    facet_nested(pop ~ sample_type + unit, scales = "free", space = "free") + #scales = "free_x"
    theme_bw() +
    labs(title = "Distribution of Behaviors from Household Surveys",
         fill = "Frequency",
         x = "City",
         y = "Percent") +
    # theme(strip.text.y = element_text(size = 7)) +
    scale_fill_brewer(palette="Set2") +
    theme(axis.text.x = element_text(angle = 45, hjust = 0.95, size = 9),
          # theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2),
          strip.text.x = element_text(size = 10),
          strip.text.y = element_text(size = 10),
          strip.background = element_rect(fill="grey"),
          legend.position="bottom",
          panel.spacing=unit(1,"lines")) + 
    scale_y_continuous(labels = scales::percent)


bx_plot  
png("durban_bx.png", width=1440, height=855, bg="transparent")
ggsave(filename="durban_bx.png", plot = bx_plot, width = 16, height = 8.5, units="in", dpi = 500)
bx_plot  
dev.off()
