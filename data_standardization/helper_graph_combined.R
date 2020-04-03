# functions for graph generation - for markdown file

# function to graph cumulative frequency
# 1. standard questions
# 2. y/n questions


# ************************************************************************************************************
# standard questions
# ************************************************************************************************************

# freq.comb("d", adults = T)
# freq.comb("d", adults = F)

##
# xx.var <- "d"

freq.comb <- function(xx.var, adults = T) {
        # data ----
        pop <- if (adults == T) "_a" else "_c"
        s.var <- xx.var
        xx.var <- paste0(xx.var, pop)
        # HH
        var <-  paste0("h_", xx.var)
        df.h %>% select(citylabel, "coding" = var) %>%
                group_by(citylabel) %>%
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
                select(citylabel, a3, a2, a1, a0, some1, never1, sum) %>% 
                data.frame() -> df1
        
        
        # SS
        var <-  paste0("s_", xx.var)
        vars <- c(paste0(var,"_3"), paste0(var,"_2"), paste0(var,"_1"), paste0(var,"_0"), paste0(var,"_na")) 
        varspaste <- paste(vars, collapse = "|")
        df <- select(df.s, citylabel, matches(varspaste)) 
        
        df %>% group_by(citylabel) %>% summarise(a3 = sum(eval(parse(text = vars[1])), na.rm = T),
                                                a2 = sum(eval(parse(text = vars[2])), na.rm = T),
                                                a1 = sum(eval(parse(text = vars[3])), na.rm = T),
                                                a0 = sum(eval(parse(text = vars[4])), na.rm = T),
                                                some1 = (a3+a2+a1),
                                                never1 = a0,
                                                sum = sum(a3, a2, a1, a0) ) %>%
                # select(citylabel, some1, never1, sum) %>% 
                data.frame() -> df2
        
        # add missing deployments manually
        suppressWarnings(df2 <- rbind(df2, c(999, rep(0,8))))
        suppressWarnings(df2 <- rbind(df2, c(998, rep(0,8))))
        suppressWarnings(df2 <- rbind(df2, c(998, rep(0,8))))
        suppressWarnings(df2 <- rbind(df2, c(998, rep(0,8))))
        df2$citylabel[nrow(df2)-3] <- "Atlanta 16"
        df2$citylabel[nrow(df2)-2] <- "Siem Reap 16"
        df2$citylabel[nrow(df2)-1] <- "Maputo 15"
        df2$citylabel[nrow(df2)  ] <- "Maputo 16"
        
        # sort
        df2 <- df2[match(meta_dply$citylabel, df2$citylabel),]
        
        
        # CC
        var <-  paste0("c_", xx.var)
        vars <- c(paste0(var,"_3"), paste0(var,"_2"), paste0(var,"_1"), paste0(var,"_0"), paste0(var,"_na")) 
        varspaste <- paste(vars, collapse = "|")
        df <- select(df.c, citylabel, matches(varspaste)) 
        
        df %>% group_by(citylabel) %>% summarise(a3 = sum(eval(parse(text = vars[1])), na.rm = T),
                                                a2 = sum(eval(parse(text = vars[2])), na.rm = T),
                                                a1 = sum(eval(parse(text = vars[3])), na.rm = T),
                                                a0 = sum(eval(parse(text = vars[4])), na.rm = T),
                                                some1 = (a3+a2+a1),
                                                never1 = a0,
                                                sum = sum(a3, a2, a1, a0)) %>%
                # select(citylabel, some1, never1, sum) %>% 
                data.frame()  -> df3
        
        # add missing deployments manually
        suppressWarnings(df3 <- rbind(df3, c(999, rep(0,8))))
        suppressWarnings(df3 <- rbind(df3, c(998, rep(0,8))))
        suppressWarnings(df3 <- rbind(df3, c(998, rep(0,8))))
        suppressWarnings(df3 <- rbind(df3, c(998, rep(0,8))))
        suppressWarnings(df3 <- rbind(df3, c(998, rep(0,8))))
        df3$citylabel[nrow(df3)-4] <- "Atlanta 16"
        df3$citylabel[nrow(df3)-3] <- "Vellore 14"
        df3$citylabel[nrow(df3)-2] <- "Siem Reap 16"
        df3$citylabel[nrow(df3)-1] <- "Maputo 15"
        df3$citylabel[nrow(df3)  ] <- "Maputo 16"
        
        # sort
        df3 <- df3[match(meta_dply$citylabel, df3$citylabel),]
        
        # adding all 3 df together
        df.comb <- data.frame()
        for (i in meta_dply$citylabel) {
                df <- cbind("citylabel" = i, df1[df1$citylabel == i, ][,-1] + df2[df2$citylabel == i, ][,-1] + df2[df2$citylabel == i, ][,-1])
                df.comb <- rbind(df.comb, df)
        }
        
        df.comb$exposed <- df.comb$some1 / df.comb$sum
        df.comb$not_exposed <- df.comb$never1 / df.comb$sum
        
        
        # graph ----
        labelpop <- if (adults == T) "Adults" else "Children"   #label for population
        labeltitle <- meta_sampleID$sample_type_name[meta_sampleID$sample_type_var == s.var]   #label for question
        
        myColors <- c(brewer.pal(nrow(meta_dply)-1, "Set3"), "coral")
        names(myColors) <- unique(meta_dply$citylabel)
        colScale <- scale_fill_manual(values = myColors)
        
        df.comb %>% select(citylabel, exposed, not_exposed) %>%
                melt(., id.vars= "citylabel") %>%
                filter(!is.na(value)) %>%
                ggplot(., aes(x = variable, y = value, 
                              group = citylabel,
                              fill = factor(citylabel))) + 
                scale_y_continuous(limits = c(0, 1), labels = scales::percent_format()) +
                geom_hline(aes(yintercept = .25), color = "grey", linetype = "dashed") +
                geom_hline(aes(yintercept = .50), color = "grey", linetype = "dashed") +
                geom_hline(aes(yintercept = .75), color = "grey", linetype = "dashed") +
                geom_bar(stat = "identity",  position = position_dodge2(preserve = "single"), width = .7) + #position = "dodge"
                geom_text(aes(label=round(value*100,0), group = citylabel), vjust=-.2,
                          position = position_dodge(.7), size = 3 ) +
                theme_bw() +
                facet_grid(~variable, scales = "free_x") +
                colScale +
                labs(fill = "Deployment",
                     x = "Frequency",
                     y = "Proportion",
                     title = paste0(labeltitle, ": ", "Combinded Frequency"),
                     subtitle = paste0(toupper(labelpop), " - Percent by Deployment") ) 
}



# ************************************************************************************************************
# y/n questions
# ************************************************************************************************************

# freq.comb.yn("dw_e_wt")
# freq.comb.yn("p_e")
# freq.comb.yn("pl_a_th")
# freq.comb.yn("pl_a_tu")
# freq.comb.yn("pl_a_tw")
# freq.comb.yn("pl_a_tf")

# xx.var <- "dw_e_wt"

freq.comb.yn <- function(xx.var) {
        # data ----
        # HH
        var <-  paste0("h_", xx.var)
        df.h %>% select(citylabel, "coding" = var) %>%
                group_by(citylabel) %>%
                summarise(a1 = sum(coding==1, na.rm = T),
                          a0 = sum(coding==2, na.rm = T),
                          sum=(a1+a0) ) %>%
                data.frame() -> df1
        
        
        # SS
        var <-  paste0("s_", xx.var)
        vars <- if(xx.var == "p_e") c(paste0(var,"_2"), paste0(var,"_1")) else c(paste0(var,"_1"), paste0(var,"_0"))
        varspaste <- paste(vars, collapse = "|")
        df <- select(df.s, citylabel, matches(varspaste)) 
        
        df %>% group_by(citylabel) %>% summarise(a1 = sum(eval(parse(text = vars[1])), na.rm = T),
                                                a0 = sum(eval(parse(text = vars[2])), na.rm = T),
                                                sum = sum(a1, a0) ) %>%
                data.frame() -> df2
        
        suppressWarnings(df2 <- rbind(df2, c(999, 0,0,0)))
        suppressWarnings(df2 <- rbind(df2, c(998, 0,0,0)))
        suppressWarnings(df2 <- rbind(df2, c(998, 0,0,0)))
        suppressWarnings(df2 <- rbind(df2, c(998, 0,0,0)))
        suppressWarnings(df2 <- rbind(df2, c(998, 0,0,0)))
        df2$citylabel[nrow(df2)-4] <- "Vellore 14"
        df2$citylabel[nrow(df2)-3] <- "Atlanta 16"
        df2$citylabel[nrow(df2)-2] <- "Siem Reap 16"
        df2$citylabel[nrow(df2)-1] <- "Maputo 15"
        df2$citylabel[nrow(df2)  ] <- "Maputo 16"
        
        # sort
        df2 <- df2[match(meta_dply$citylabel, df2$citylabel),]
        
        
        # CC
        var <-  paste0("c_", xx.var)
        vars <- if(xx.var == "p_e") c(paste0(var,"_2"), paste0(var,"_1")) else c(paste0(var,"_1"), paste0(var,"_0"))
        varspaste <- paste(vars, collapse = "|")
        df <- select(df.c, citylabel, matches(varspaste)) 
        
        df %>% group_by(citylabel) %>% summarise(a1 = sum(eval(parse(text = vars[1])), na.rm = T),
                                                a0 = sum(eval(parse(text = vars[2])), na.rm = T),
                                                sum = sum(a1, a0)) %>%
                data.frame()  -> df3
        
        suppressWarnings(df3 <- rbind(df3, c(999, 0,0,0)))
        suppressWarnings(df3 <- rbind(df3, c(998, 0,0,0)))
        suppressWarnings(df3 <- rbind(df3, c(998, 0,0,0)))
        suppressWarnings(df3 <- rbind(df3, c(998, 0,0,0)))
        suppressWarnings(df3 <- rbind(df3, c(998, 0,0,0)))
        df3$citylabel[nrow(df3)-4] <- "Atlanta 16"
        df3$citylabel[nrow(df3)-3] <- "Vellore 14"
        df3$citylabel[nrow(df3)-2] <- "Siem Reap 16"
        df3$citylabel[nrow(df3)-1] <- "Maputo 15"
        df3$citylabel[nrow(df3)  ] <- "Maputo 16"
        
        # sort
        df3 <- df3[match(meta_dply$citylabel, df3$citylabel),]

        # adding all 3 df together
        df.comb <- data.frame()
        for (i in meta_dply$citylabel) {
                df <- cbind("citylabel" = i, df1[df1$citylabel == i, ][,-1] + df2[df2$citylabel == i, ][,-1] + df2[df2$citylabel == i, ][,-1])
                df.comb <- rbind(df.comb, df)
        }
        
        df.comb$yes <- df.comb$a1 / df.comb$sum
        df.comb$no <- df.comb$a0 / df.comb$sum
        
        
        # graph ----
        labeltitle <- case_when(xx.var == "dw_e_wt" ~ "Water Treatment",
                                xx.var == "p_e" ~ "Do you wash Produce before eating?",
                                xx.var == "pl_a_th" ~ "Do you have any Toilet in your HH?",
                                xx.var == "pl_a_tu" ~ "Do you use the Toilet in your HH?",
                                xx.var == "pl_a_tw" ~ "Do you flush the Toilet with Water?",
                                xx.var == "pl_a_tf" ~ "Does the Toilet in HH ever flood?")  #label for question
        
        myColors <- c(brewer.pal(nrow(meta_dply)-1, "Set3"), "coral")
        names(myColors) <- unique(meta_dply$citylabel)
        colScale <- scale_fill_manual(values = myColors)
        
        df.comb %>% select(citylabel, yes, no) %>%
                melt(., id.vars= "citylabel") %>%
                filter(!is.na(value)) %>%
                ggplot(., aes(x = variable, y = value, 
                              group = citylabel,
                              fill = factor(citylabel))) + 
                scale_y_continuous(limits = c(0, 1), labels = scales::percent_format()) +
                geom_hline(aes(yintercept = .25), color = "grey", linetype = "dashed") +
                geom_hline(aes(yintercept = .50), color = "grey", linetype = "dashed") +
                geom_hline(aes(yintercept = .75), color = "grey", linetype = "dashed") +
                geom_bar(stat = "identity",  position = position_dodge2(preserve = "single"), width = .7) + #position = "dodge"
                geom_text(aes(label=round(value*100,0), group = citylabel), vjust=-.2,
                          position = position_dodge(.7), size = 3 ) +
                theme_bw() +
                facet_grid(~variable, scales = "free_x") +
                colScale +
                labs(fill = "Deployment",
                     x = "Frequency",
                     y = "Proportion",
                     title = paste0(labeltitle, " - ", "Combinded Frequency"),
                     subtitle = "Percent by Deployment") 
}




