# ***********************************************************************************************
# Multicity Paper
# separate graphs by survey (hh, sc, co)
# ***********************************************************************************************
# last edit: 02/01/2020

# ***********************************************************************************************
# HH ----
# ***********************************************************************************************
df.all <- data.frame()
adults = T
pathways <- c("d", "p", "dw", "odw", "o", "s", "f", "l", "bw", "sf")

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
        
        
        
        # adding all 3 df together
        df.comb <- df1
        
        df.comb <- select(df.comb, city, a3, a2, a1, a0)
        df.comb$sample_type <- w
        
        df.all <- rbind(df.all, df.comb)
}
df.all.a <- df.all
df.all.a$pop <- "Adults"

# ***********************************************************************************************
df.all <- data.frame()
adults = F
pathways <- c("d", "p", "dw", "odw", "o", "s", "f", "l", "bw", "sf")

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
        
        
        # adding all 3 df together
        df.comb <- df1
        
        df.comb <- select(df.comb, city, a3, a2, a1, a0)
        df.comb$sample_type <- w
        
        df.all <- rbind(df.all, df.comb)
}
df.all.c <- df.all
df.all.c$pop <- "Children"

df.alll <- rbind(df.all.a, df.all.c)

table(df.alll$sample_type)
df.alll$sample_type[df.alll$sample_type == "bw"] <- "Bathing Water"
df.alll$sample_type[df.alll$sample_type == "d"] <- "Open Drains"
df.alll$sample_type[df.alll$sample_type == "dw"] <- "Drinking Water (DW)"
df.alll$sample_type[df.alll$sample_type == "odw"] <- "DW, other"
df.alll$sample_type[df.alll$sample_type == "f"] <- "Floodwater"
df.alll$sample_type[df.alll$sample_type == "l"] <- "Public Latrine"
df.alll$sample_type[df.alll$sample_type == "o"] <- "Ocean"
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

colnames(df.perc) <- c("city", "sample_type", "pop", "sum", "10+", "6-10", "<5", "Never")

df.perc$sum <- NULL
df.perc$pop <- factor(df.perc$pop)

df.perc[ is.na(df.perc) ] <- NA



df.perc %>% 
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

ggsave(filename=paste0(getwd(), "/images/figure2/", "figure2_hh_v1.png"),plot = last_plot(), width = 16.67, height = 8.69, units="in", dpi = 300)
ggsave(filename=paste0(getwd(), "/images/figure2/", "figure2_hh_v1.pdf"),plot = last_plot(), width = 16.67, height = 8.69, units="in", dpi = 300)

## with labels
df.perc %>% 
        melt(., id.vars = c("city", "sample_type", "pop")) %>%
        na.omit(value) %>%
        # ggplot(aes(x = city, y = value, fill = variable)) +
        ggplot(aes(x = city, y = value, fill = variable, label = round(value*100,0))) + #add value labels
        geom_bar(stat = "identity") +
        geom_text(size = 3, position = position_stack(vjust = 0.5)) +
        facet_grid(pop ~ sample_type, scales = "free_x", space = "free_x") + #scales = "free_x"
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

ggsave(filename=paste0(getwd(), "/images/figure2/", "figure2_hh_v2.png"),plot = last_plot(), width = 16.67, height = 8.69, units="in", dpi = 300)
ggsave(filename=paste0(getwd(), "/images/figure2/", "figure2_hh_v2.pdf"),plot = last_plot(), width = 16.67, height = 8.69, units="in", dpi = 300)

# ***********************************************************************************************
# SC ----
# ***********************************************************************************************
df.all <- data.frame()
adults = T
pathways <- c("d", "p", "dw", "odw", "o", "s", "f", "l", "bw", "sf")

for (w in pathways){
        xx.var <- w
        
        # data 
        pop <- if (adults == T) "_a" else "_c"
        s.var <- xx.var
        xx.var <- paste0(xx.var, pop)
        
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
        
        
        # adding all 3 df together
        df.comb <- df2
        
        df.comb <- select(df.comb, city, a3, a2, a1, a0)
        df.comb$sample_type <- w
        
        df.all <- rbind(df.all, df.comb)
}
df.all.a <- df.all
df.all.a$pop <- "Adults"

# ***********************************************************************************************
df.all <- data.frame()
adults = F
pathways <- c("d", "p", "dw", "odw", "o", "s", "f", "l", "bw", "sf")

for (w in pathways){
        xx.var <- w
        
        # data 
        pop <- if (adults == T) "_a" else "_c"
        s.var <- xx.var
        xx.var <- paste0(xx.var, pop)
        
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
        
        
        # adding all 3 df together
        df.comb <- df2
        
        df.comb <- select(df.comb, city, a3, a2, a1, a0)
        df.comb$sample_type <- w
        
        df.all <- rbind(df.all, df.comb)
}
df.all.c <- df.all
df.all.c$pop <- "Children"

df.alll <- rbind(df.all.a, df.all.c)

table(df.alll$sample_type)
df.alll$sample_type[df.alll$sample_type == "bw"] <- "Bathing Water"
df.alll$sample_type[df.alll$sample_type == "d"] <- "Open Drains"
df.alll$sample_type[df.alll$sample_type == "dw"] <- "Drinking Water (DW)"
df.alll$sample_type[df.alll$sample_type == "odw"] <- "DW, other"
df.alll$sample_type[df.alll$sample_type == "f"] <- "Floodwater"
df.alll$sample_type[df.alll$sample_type == "l"] <- "Public Latrine"
df.alll$sample_type[df.alll$sample_type == "o"] <- "Ocean"
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

colnames(df.perc) <- c("city", "sample_type", "pop", "sum", "10+", "6-10", "<5", "Never")

df.perc$sum <- NULL
df.perc$pop <- factor(df.perc$pop)

df.perc[ is.na(df.perc) ] <- NA



df.perc %>% 
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

ggsave(filename=paste0(getwd(), "/images/figure2/", "figure2_sc_v1.png"),plot = last_plot(), width = 16.67, height = 8.69, units="in", dpi = 300)
ggsave(filename=paste0(getwd(), "/images/figure2/", "figure2_sc_v1.pdf"),plot = last_plot(), width = 16.67, height = 8.69, units="in", dpi = 300)


df.perc %>% 
        melt(., id.vars = c("city", "sample_type", "pop")) %>%
        na.omit(value) %>%
        # ggplot(aes(x = city, y = value, fill = variable)) +
        ggplot(aes(x = city, y = value, fill = variable, label = round(value*100,0))) + #add value labels
        geom_bar(stat = "identity") +
        geom_text(size = 3, position = position_stack(vjust = 0.5)) +
        facet_grid(pop ~ sample_type, scales = "free_x", space = "free_x") + #scales = "free_x"
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

ggsave(filename=paste0(getwd(), "/images/figure2/", "figure2_sc_v2.png"),plot = last_plot(), width = 16.67, height = 8.69, units="in", dpi = 300)
ggsave(filename=paste0(getwd(), "/images/figure2/", "figure2_sc_v2.pdf"),plot = last_plot(), width = 16.67, height = 8.69, units="in", dpi = 300)


# ***********************************************************************************************
# CO ----
# ***********************************************************************************************
df.all <- data.frame()
adults = T
pathways <- c("d", "p", "dw", "odw", "o", "s", "f", "l", "bw", "sf")

for (w in pathways){
        xx.var <- w
        
        # data 
        pop <- if (adults == T) "_a" else "_c"
        s.var <- xx.var
        xx.var <- paste0(xx.var, pop)
        
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
        
        df.comb <- df3
        
        
        df.comb <- select(df.comb, city, a3, a2, a1, a0)
        df.comb$sample_type <- w
        
        df.all <- rbind(df.all, df.comb)
}

df.all.a <- df.all
df.all.a$pop <- "Adults"

# ***********************************************************************************************
df.all <- data.frame()
adults = F
pathways <- c("d", "p", "dw", "odw", "o", "s", "f", "l", "bw", "sf")

for (w in pathways){
        xx.var <- w
        
        # data 
        pop <- if (adults == T) "_a" else "_c"
        s.var <- xx.var
        xx.var <- paste0(xx.var, pop)
       
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
        
        df.comb <- df3
        
        df.comb <- select(df.comb, city, a3, a2, a1, a0)
        df.comb$sample_type <- w
        
        df.all <- rbind(df.all, df.comb)
}
df.all.c <- df.all
df.all.c$pop <- "Children"

df.alll <- rbind(df.all.a, df.all.c)

table(df.alll$sample_type)
df.alll$sample_type[df.alll$sample_type == "bw"] <- "Bathing Water"
df.alll$sample_type[df.alll$sample_type == "d"] <- "Open Drains"
df.alll$sample_type[df.alll$sample_type == "dw"] <- "Drinking Water (DW)"
df.alll$sample_type[df.alll$sample_type == "odw"] <- "DW, other"
df.alll$sample_type[df.alll$sample_type == "f"] <- "Floodwater"
df.alll$sample_type[df.alll$sample_type == "l"] <- "Public Latrine"
df.alll$sample_type[df.alll$sample_type == "o"] <- "Ocean"
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

colnames(df.perc) <- c("city", "sample_type", "pop", "sum", "10+", "6-10", "<5", "Never")

df.perc$sum <- NULL
df.perc$pop <- factor(df.perc$pop)

df.perc[ is.na(df.perc) ] <- NA



df.perc %>% 
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

ggsave(filename=paste0(getwd(), "/images/figure2/", "figure2_co_v1.png"),plot = last_plot(), width = 16.67, height = 8.69, units="in", dpi = 300)
ggsave(filename=paste0(getwd(), "/images/figure2/", "figure2_co_v1.pdf"),plot = last_plot(), width = 16.67, height = 8.69, units="in", dpi = 300)

df.perc %>% 
        melt(., id.vars = c("city", "sample_type", "pop")) %>%
        na.omit(value) %>%
        # ggplot(aes(x = city, y = value, fill = variable)) +
        ggplot(aes(x = city, y = value, fill = variable, label = round(value*100,0))) + #add value labels
        geom_bar(stat = "identity") +
        geom_text(size = 3, position = position_stack(vjust = 0.5)) +
        facet_grid(pop ~ sample_type, scales = "free_x", space = "free_x") + #scales = "free_x"
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

ggsave(filename=paste0(getwd(), "/images/figure2/", "figure2_co_v2.png"),plot = last_plot(), width = 16.67, height = 8.69, units="in", dpi = 300)
ggsave(filename=paste0(getwd(), "/images/figure2/", "figure2_co_v2.pdf"),plot = last_plot(), width = 16.67, height = 8.69, units="in", dpi = 300)
