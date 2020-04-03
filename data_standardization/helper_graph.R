# graph function
# for plotting behavioral data

library(data.table)
library(ggplot2)
library(dplyr)
library(RColorBrewer)


# ************************************************************************************************************
# Frequency Graphs ----
# ************************************************************************************************************

# load 
answers <- read.csv(paste0(getwd(), "/data/meta_data/", "meta__answers", ".csv"), 
                      sep=",", stringsAsFactors=F, header=T)
questions <- read.csv(paste0(getwd(), "/data/meta_data/", "meta__questions", ".csv"), 
                      stringsAsFactors=F, header=T)


# create ggplot graphs for household surveys - by deployment
# dff <- df.h
# x.var <- "h_d_a"

freq.graph.h <- function(dff, x.var){
        # x.var <- rlang::sym(x.var)
        label1 <- questions$answer[questions$variable == x.var]         #answer frequency
        labeling <- answers[answers$answer == label1, ][,-1]
        labelpop <- questions$population[questions$variable == x.var]   #label for population
        labeltitle <- questions$question[questions$variable == x.var]   #label for question
        
        myColors <- c(brewer.pal(nrow(meta_dply)-1, "Set3"), "coral")
        names(myColors) <- unique(meta_dply$citylabel)
        colScale <- scale_fill_manual(values = myColors)
        
        dff %>% select(citylabel, "coding" = x.var) %>%
                left_join(., labeling, by="coding") %>%
                mutate(label = factor(.$label, levels = factor(labeling$label))) %>%
                filter(!is.na(label)) %>%
                filter(!label %in% c("Do not know", "N/A")) %>%  #remove two answer options
                
                ggplot(., aes(x = label, 
                              y = ..prop..,
                              group = citylabel,
                              fill = factor(citylabel) )) +
                scale_y_continuous(limits = c(0, 1), labels = scales::percent_format()) +
                geom_hline(aes(yintercept = .25), color = "grey", linetype = "dashed") +
                geom_hline(aes(yintercept = .50), color = "grey", linetype = "dashed") +
                geom_hline(aes(yintercept = .75), color = "grey", linetype = "dashed") +
                geom_bar(stat = "count", position = position_dodge(width=0.7), width = 0.7, color="white") +
                geom_text(aes(label=round(..prop..*100,0) ), stat="count", vjust=-.2, 
                          position=position_dodge(.7), size = 3) +
                theme_bw() +
                colScale +
                labs(fill = "Deployment",
                     x = "Frequency",
                     y = "Proportion",
                     # caption = "SaniPath, 2018",
                     title = paste(strwrap(labeltitle, width = 70), collapse = "\n"), #wrap title
                     subtitle = paste0(toupper(labelpop), " - Percent by Deployment"))
}


# example
#freq.graph(dataframe, variable, caluclate % (Y/N))

# freq.graph(df.h, "h_f_c", T)
# freq.graph(df.h, "h_f_c", F)

# ggsave(plot = last_plot(), paste0(getwd(), "/images/", "h_f_c", ".png"), dpi = 300, width = 6, height = 4, units = "in")



answers.cs <- read.csv(paste0(getwd(), "/data/meta_data/", "meta__answers_cs", ".csv"), 
                       sep=",", stringsAsFactors=F, header=T)
questions.cs <- read.csv(paste0(getwd(), "/data/meta_data/", "meta__questions_cs", ".csv"), 
                         stringsAsFactors=F, header=T)



# create ggplot graphs for household surveys - by deployment
# dff <- df.c
# var <- "c_sf_a"
# var <- "s_l_a"
# var <- "c_d_a"

freq.graph.cs <- function(dff, var){
        # get variables
        vars <- c(paste0(var,"_3"), paste0(var,"_2"), paste0(var,"_1"), paste0(var,"_0"), paste0(var,"_na")) 
        varspaste <- paste(vars, collapse = "|")
        df <- select(dff, citylabel, matches(varspaste)) 
        
        
        labeling <- filter(answers.cs, grepl(var, variable)) %>% pull(answer)            #label for answers
        labeling <- labeling[!labeling == "Do not know"]
        
        labelpop <- questions.cs$population[questions.cs$variable == paste0(var, "_note")]   #label for population
        labeltitle <- questions.cs$question[questions.cs$variable == paste0(var, "_note")]   #label for question
        
        myColors <- c(brewer.pal(nrow(meta_dply)-1, "Set3"), "coral")
        names(myColors) <- unique(meta_dply$citylabel)
        colScale <- scale_fill_manual(values = myColors)
        
        
        
        df %>% group_by(citylabel) %>% summarise(a3 = sum(eval(parse(text = vars[1])), na.rm = T),
                                                a2 = sum(eval(parse(text = vars[2])), na.rm = T),
                                                a1 = sum(eval(parse(text = vars[3])), na.rm = T),
                                                a0 = sum(eval(parse(text = vars[4])), na.rm = T),
                                                sum = sum(a3, a2, a1, a0),
                                                aa3 = a3/sum,
                                                aa2 = a2/sum,
                                                aa1 = a1/sum,
                                                aa0 = a0/sum) %>%
                data.frame() %>%
                select(citylabel, aa3, aa2, aa1, aa0) %>% 
                setnames(., c("citylabel", labeling)) %>%
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
                colScale +
                labs(fill = "Deployment",
                     x = "Frequency",
                     y = "Proportion",
                     title = paste(strwrap(labeltitle, width = 70), collapse = "\n"), #wrap title
                     subtitle = paste0(toupper(labelpop), " - Percent by Deployment") ) 
}

# test
# freq.graph.c(df.c, "c_d_a")

# ggsave(plot = last_plot(), paste0(getwd(), "/images/", "c_d_a", ".png"), dpi = 300, width = 6, height = 4, units = "in")




# ************************************************************************************************************
# sometimes vs. never Graphs ----
# ************************************************************************************************************


# graph function
# some vs. never

library(ggplot2)
library(dplyr)
library(reshape2)
library(RColorBrewer)

# load 
answers <- read.csv(paste0(getwd(), "/data/meta_data/", "meta__answers", ".csv"), 
                    sep=",", stringsAsFactors=F, header=T)
questions <- read.csv(paste0(getwd(), "/data/meta_data/", "meta__questions", ".csv"), 
                      stringsAsFactors=F, header=T)

answers.cs <- read.csv(paste0(getwd(), "/data/meta_data/", "meta__answers_cs", ".csv"), 
                       sep=",", stringsAsFactors=F, header=T)
questions.cs <- read.csv(paste0(getwd(), "/data/meta_data/", "meta__questions_cs", ".csv"), 
                         stringsAsFactors=F, header=T)



#### Household -

# create ggplot graphs for household surveys - by deployment
# dff <- df.h
# x.var <- "h_d_a"

freq.graph.h.sn <- function(dff, x.var){
        # x.var <- rlang::sym(x.var)
        label1 <- questions$answer[questions$variable == x.var]         #answer frequency
        labeling <- answers[answers$answer == label1, ][,-1]
        labelpop <- questions$population[questions$variable == x.var]   #label for population
        labeltitle <- questions$question[questions$variable == x.var]   #label for question
        
        myColors <- c(brewer.pal(nrow(meta_dply)-1, "Set3"), "coral")
        names(myColors) <- unique(meta_dply$citylabel)
        colScale <- scale_fill_manual(values = myColors)
        
        dff %>% select(citylabel, "coding" = x.var) %>%
                group_by(citylabel) %>%
                summarise(some1 = sum(sum(coding==1, na.rm = T), sum(coding==2, na.rm = T), sum(coding==3, na.rm = T) ) ,
                          never1 = sum(coding==4, na.rm = T),
                          exclude = sum(sum(coding==5, na.rm = T), sum(coding==6, na.rm = T)),
                          na = sum(is.na(coding)),
                          sum1 = n(),
                          sum = sum1-exclude-na,
                          exposed = some1/sum,
                          not_exposed = never1/sum) %>%
                data.frame() %>%
                select(citylabel, exposed, not_exposed) %>%
                filter(!is.na(exposed), !is.na(not_exposed)) %>%
                melt(., id.vars= c("citylabel")) %>%
                ggplot(., aes(x = variable, 
                              y = value,
                              group = citylabel,
                              fill = factor(citylabel ) )) +
                scale_y_continuous(limits = c(0, 1), labels = scales::percent_format()) +
                geom_hline(aes(yintercept = .25), color = "grey", linetype = "dashed") +
                geom_hline(aes(yintercept = .50), color = "grey", linetype = "dashed") +
                geom_hline(aes(yintercept = .75), color = "grey", linetype = "dashed") +
                geom_bar(stat = "identity", position = position_dodge(width=0.7), width = 0.7, color="white") +
                geom_text(aes(label=round(value*100,0) ), stat="identity", vjust=-.2, 
                          position=position_dodge(.7), size = 3) +
                theme_bw() +
                facet_grid(~variable, scales = "free_x") +
                colScale +
                labs(fill = "Deployment",
                     x = "Frequency",
                     y = "Proportion",
                     title = paste(strwrap(labeltitle, width = 70), collapse = "\n"), #wrap title
                     subtitle = paste0(toupper(labelpop), " - Percent by Deployment"))
}


# example
#freq.graph(dataframe, variable)

# freq.graph(df.h, "h_f_c", T)
# freq.graph(df.h, "h_f_c", F)

# ggsave(plot = last_plot(), paste0(getwd(), "/images/", "h_f_c", ".png"), dpi = 300, width = 6, height = 4, units = "in")

#### Community and School -


# create ggplot graphs for household surveys - by deployment
# dff <- df.c
# var <- "c_sf_a"
# var <- "s_l_a"
# var <- "c_d_a"

freq.graph.cs.sn <- function(dff, var){
        # get variables
        vars <- c(paste0(var,"_3"), paste0(var,"_2"), paste0(var,"_1"), paste0(var,"_0"), paste0(var,"_na")) 
        varspaste <- paste(vars, collapse = "|")
        df <- select(dff, citylabel, matches(varspaste)) 
        
        
        labeling <- filter(answers.cs, grepl(var, variable)) %>% pull(answer)            #label for answers
        labeling <- labeling[!labeling == "Do not know"]
        
        labelpop <- questions.cs$population[questions.cs$variable == paste0(var, "_note")]   #label for population
        labeltitle <- questions.cs$question[questions.cs$variable == paste0(var, "_note")]   #label for question
        
        myColors <- c(brewer.pal(nrow(meta_dply)-1, "Set3"), "coral")
        names(myColors) <- unique(meta_dply$citylabel)
        colScale <- scale_fill_manual(values = myColors)
        
        
        
        df %>% group_by(citylabel) %>% summarise(a3 = sum(eval(parse(text = vars[1])), na.rm = T),
                                                a2 = sum(eval(parse(text = vars[2])), na.rm = T),
                                                a1 = sum(eval(parse(text = vars[3])), na.rm = T),
                                                a0 = sum(eval(parse(text = vars[4])), na.rm = T),
                                                sum = sum(a3, a2, a1, a0),
                                                some1 = (a3+a2+a1),
                                                never1 = a0,
                                                exposed = some1/sum,
                                                not_exposed = never1/sum) %>%
                data.frame() %>%
                select(citylabel, exposed, not_exposed) %>% 
                melt(., id.vars= "citylabel") %>%
                filter(!is.na(value)) %>%
                ggplot(., aes(x = variable, y = value, 
                              group = citylabel,
                              fill = factor(citylabel ))) + 
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
                     title = paste(strwrap(labeltitle, width = 70), collapse = "\n"), #wrap title
                     subtitle = paste0(toupper(labelpop), " - Percent by Deployment") ) 
}

# test
# freq.graph.c(df.c, "c_d_a")

# ggsave(plot = last_plot(), paste0(getwd(), "/images/", "c_d_a", ".png"), dpi = 300, width = 6, height = 4, units = "in")



# ************************************************************************************************************
# Shared Legend for Graphs ----
# ************************************************************************************************************

# grid plotting helper
# one common legend on the bottom

library(ggplot2)
library(gridExtra)
library(grid)

grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {
        
        plots <- list(...)
        position <- match.arg(position)
        g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
        legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
        lheight <- sum(legend$height)
        lwidth <- sum(legend$width)
        gl <- lapply(plots, function(x) x + theme(legend.position="none"))
        gl <- c(gl, ncol = ncol, nrow = nrow)
        
        combined <- switch(position,
                           "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                                  legend,
                                                  ncol = 1,
                                                  heights = unit.c(unit(1, "npc") - lheight, lheight)),
                           "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                                 legend,
                                                 ncol = 2,
                                                 widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
        
        grid.newpage()
        grid.draw(combined)
        
        # return gtable invisibly
        invisible(combined)
        
}