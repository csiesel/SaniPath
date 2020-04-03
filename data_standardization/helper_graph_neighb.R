# functions for graph generation - for markdown file
# behavioral graphs 
# by neighborhood

# create function to graph community and school survey questions

library(ggplot2)
library(dplyr)
library(RColorBrewer)

answers <- read.csv(paste0(getwd(), "/data/meta_data/", "meta__answers", ".csv"), 
                    sep=",", stringsAsFactors=F, header=T)
questions <- read.csv(paste0(getwd(), "/data/meta_data/", "meta__questions", ".csv"), 
                      stringsAsFactors=F, header=T)

answers.cs <- read.csv(paste0(getwd(), "/data/meta_data/", "meta__answers_cs", ".csv"), 
                       sep=",", stringsAsFactors=F, header=T)
questions.cs <- read.csv(paste0(getwd(), "/data/meta_data/", "meta__questions_cs", ".csv"), 
                         stringsAsFactors=F, header=T)


# creat color scheme
gg_color_hue <- function(n) {
        hues = seq(15, 375, length = n + 1)
        hcl(h = hues, l = 65, c = 100)[1:n]
}

myColors <- gg_color_hue(nrow(meta_neighb))
names(myColors) <- unique(meta_neighb$neighb_UID)
colScale <- scale_fill_manual(values = myColors)

# create ggplot graphs for household surveys - by deployment
# dff <- df.h
# x.var <- "h_d_a"

freq.graph.h.neighb <- function(dff, x.var){
        # x.var <- rlang::sym(x.var)
        label1 <- questions$answer[questions$variable == x.var]         #answer frequency
        labeling <- answers[answers$answer == label1, ][,-1]
        labelpop <- questions$population[questions$variable == x.var]   #label for population
        labeltitle <- questions$question[questions$variable == x.var]   #label for question
        
        # myColors <- brewer.pal(nrow(meta_dply), "Set1")   #ifelse(prop == TRUE, "Set2", "Set1")
        # names(myColors) <- unique(meta_dply$city)
        # colScale <- scale_fill_manual(values = myColors)
        
        dff %>% select(neighb_num, dply_num, "coding" = x.var) %>%
                left_join(., labeling, by="coding") %>%
                mutate(label = factor(.$label, levels = factor(labeling$label))) %>%
                filter(!is.na(label)) %>%
                filter(!label %in% c("Do not know", "N/A")) %>%  #remove two answer options
                
                ggplot(., aes(x = label, 
                              y = ..prop..,
                              group = neighb_num,
                              fill = factor(neighb_num) )) +
                scale_y_continuous(limits = c(0, 1), labels = scales::percent_format()) +
                geom_hline(aes(yintercept = .25), color = "grey", linetype = "dashed") +
                geom_hline(aes(yintercept = .50), color = "grey", linetype = "dashed") +
                geom_hline(aes(yintercept = .75), color = "grey", linetype = "dashed") +
                geom_bar(stat = "count", position = position_dodge2(width=0.7, preserve = "single"), width = 0.7, color="white") +
                geom_text(aes(label=round(..prop..*100,0) ), stat="count", vjust=-.2, 
                          position=position_dodge2(.7, preserve = "single"), size = 3) +
                theme_bw() +
                # facet_grid(~dply_num, space = "free_x") +
                # colScale +
                labs(fill = "Neighborhood",
                     x = "Frequency",
                     y = "Proportion",
                     # caption = "SaniPath, 2018",
                     title = paste(strwrap(labeltitle, width = 70), collapse = "\n"), #wrap title
                     subtitle = paste0(toupper(labelpop), " - Percent by Neighborhood"))
}



# create ggplot graphs for household surveys - by deployment
# dff <- df.c
# var <- "c_sf_a"
# var <- "s_l_a"
# var <- "c_d_a"

freq.graph.cs.neighb <- function(dff, var){
        # get variables
        vars <- c(paste0(var,"_3"), paste0(var,"_2"), paste0(var,"_1"), paste0(var,"_0"), paste0(var,"_na")) 
        varspaste <- paste(vars, collapse = "|")
        df <- select(dff, neighb_num, dply_num, matches(varspaste)) 
        
        
        labeling <- filter(answers.cs, grepl(var, variable)) %>% pull(answer)            #label for answers
        labeling <- labeling[!labeling == "Do not know"]
        
        labelpop <- questions.cs$population[questions.cs$variable == paste0(var, "_note")]   #label for population
        labeltitle <- questions.cs$question[questions.cs$variable == paste0(var, "_note")]   #label for question
        
        # myColors <- brewer.pal(nrow(meta_dply), "Set1")   #ifelse(prop == TRUE, "Set2", "Set1")
        # names(myColors) <- unique(meta_dply$city)
        # colScale <- scale_fill_manual(values = myColors)
        
        
        
        df %>% group_by(neighb_num, dply_num) %>% summarise(a3 = sum(eval(parse(text = vars[1])), na.rm = T),
                                                a2 = sum(eval(parse(text = vars[2])), na.rm = T),
                                                a1 = sum(eval(parse(text = vars[3])), na.rm = T),
                                                a0 = sum(eval(parse(text = vars[4])), na.rm = T),
                                                sum = sum(a3, a2, a1, a0),
                                                aa3 = a3/sum,
                                                aa2 = a2/sum,
                                                aa1 = a1/sum,
                                                aa0 = a0/sum) %>%
                data.frame() %>%
                select(neighb_num, dply_num, aa3, aa2, aa1, aa0) %>% 
                setnames(., c("neighb_num", "dply_num", labeling)) %>%
                melt(., id.vars= c("neighb_num", "dply_num")) %>%
                filter(!is.na(value)) %>%
                ggplot(., aes(x = variable, y = value, 
                              group = neighb_num,
                              fill = factor(neighb_num ))) + 
                scale_y_continuous(limits = c(0, 1), labels = scales::percent_format()) +
                geom_hline(aes(yintercept = .25), color = "grey", linetype = "dashed") +
                geom_hline(aes(yintercept = .50), color = "grey", linetype = "dashed") +
                geom_hline(aes(yintercept = .75), color = "grey", linetype = "dashed") +
                geom_bar(stat = "identity",  position = position_dodge(width=0.7), width = .7, color="white") + #position = "dodge"
                geom_text(aes(label=round(value*100,0), group = neighb_num), vjust=-.2,
                          position = position_dodge(.7), size = 3 ) +
                theme_bw() +
                # facet_grid(~dply_num, space = "free_x") +
                colScale +
                labs(fill = "Neighborhood",
                     x = "Frequency",
                     y = "Proportion",
                     title = paste(strwrap(labeltitle, width = 70), collapse = "\n"), #wrap title
                     subtitle = paste0(toupper(labelpop), " - Percent by Neighborhood") ) 
}



# test
# var <- "d_a"
# title1 <- "Open Drains"
# 
# p1 <- freq.graph.h.neighb( df.h, paste0("h_", var)) +
#         labs(title = paste0(title1, ": Household")) +
#         theme(legend.position="bottom", plot.subtitle=element_text(size=8))
# p2 <- freq.graph.cs.neighb(df.c, paste0("c_", var)) +
#         labs(title = paste0(title1, ": Community")) +
#         theme(legend.position="bottom", plot.subtitle=element_text(size=8))
# p3 <- freq.graph.cs.neighb(df.s, paste0("s_", var)) +
#         labs(title = paste0(title1, ": School")) +
#         theme(legend.position="bottom", plot.subtitle=element_text(size=8))
# 
# grid_arrange_shared_legend(p1, p2, p3, ncol = 3, nrow = 1, position="right")
