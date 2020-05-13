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
  pathways <- c("p", "sf")
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
      select( a3, a2, a1, a0, some1, never1, sum) %>% 
      data.frame() -> df1
    
    
    # SS
    var <-  paste0("s_", xx.var)
    vars <- c(paste0(var,"_3"), paste0(var,"_2"), paste0(var,"_1"), paste0(var,"_0"), paste0(var,"_na")) 
    varspaste <- paste(vars, collapse = "|")
    df <- select(df.s, neighb_UID, matches(varspaste)) 
    
    df %>% summarise(a3 = sum(eval(parse(text = vars[1])), na.rm = T),
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
    
    df %>%  summarise(a3 = sum(eval(parse(text = vars[1])), na.rm = T),
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
    df.comb <- df4 %>% summarise(a3 = sum(a3),
                                                          a2 = sum(a2),
                                                          a1 = sum(a1),
                                                          a0 = sum(a0),
                                                          some1 = sum(some1),
                                                          never1 = sum(never1),
                                                          sum = sum(sum))
    
    
    df.comb <- select(df.comb, a3, a2, a1, a0)
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
  pathways <- c("p", "sf")
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
      select( a3, a2, a1, a0, some1, never1, sum) %>% 
      data.frame() -> df1
    
    
    # SS
    var <-  paste0("s_", xx.var)
    vars <- c(paste0(var,"_3"), paste0(var,"_2"), paste0(var,"_1"), paste0(var,"_0"), paste0(var,"_na")) 
    varspaste <- paste(vars, collapse = "|")
    df <- select(df.s, neighb_UID, matches(varspaste)) 
    
    df %>% summarise(a3 = sum(eval(parse(text = vars[1])), na.rm = T),
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
    
    df %>%  summarise(a3 = sum(eval(parse(text = vars[1])), na.rm = T),
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
    df.comb <- df4 %>% summarise(a3 = sum(a3),
                                 a2 = sum(a2),
                                 a1 = sum(a1),
                                 a0 = sum(a0),
                                 some1 = sum(some1),
                                 never1 = sum(never1),
                                 sum = sum(sum))
    
    
    df.comb <- select(df.comb, a3, a2, a1, a0)
    df.comb$sample_type <- w
    
    df.all <- rbind(df.all, df.comb)
  }
  
  df.all.c <- df.all
  df.all.c$pop <- "Children"
  
}

df.alll <- rbind(df.all.a, df.all.c)


df.alll$sample_type[df.alll$sample_type == "p"] <- "Raw Produce"

df.alll$sample_type[df.alll$sample_type == "sf"] <- "Street Food"


df.perc <- df.alll %>% group_by(sample_type, pop) %>%
  mutate(., sum = sum(a3,a2,a1,a0),
         a3 = a3/sum,
         a2 = a2/sum,
         a1 = a1/sum,
         a0 = a0/sum) %>%
  data.frame() %>%
  select(sample_type, pop, sum, a3, a2, a1, a0 )


colnames(df.perc) <- c("sample_type", "pop", "sum", "10+", "6-10", "<5", "Never")

df.perc %>% group_by()



df.perc$sum <- NULL
df.perc$pop <- factor(df.perc$pop)


df.perc[ is.na(df.perc) ] <- NA


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
  melt(., id.vars = c("sample_type", "pop", "unit")) %>%
  na.omit(value) %>%
  # ggplot(aes(x = factor(neighb_UID), y = value, fill = variable)) +
  ggplot(aes(y = pop, x = value, fill = variable)) + #add value labels
  geom_bar(stat = "identity") +
  facet_nested(~sample_type + unit, scales = "free", space = "free", labeller = labeller(pop=label_wrap_gen(5), city=label_wrap_gen(5))) + #scales = "free_x"
  theme_bw() +
  labs(title = "Distribution of Behaviors",
       fill = "Frequency",
       x = "Percent",
       y = "Population") +
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
