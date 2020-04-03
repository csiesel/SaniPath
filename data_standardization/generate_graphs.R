# create graphs

# household surveys - all deployments

source("helper_graph.R")
# source("helper_dataload_discovery.R")




# for loop for generating graphs
questions <- head(questions, -1)
# function cannot do select_multiple answers yet.

for (i in questions$variable){
        p <- freq.graph(df.h, i, F)
        ggsave(plot = p, paste0(getwd(), "/images/", i, ".png"), 
               dpi = 300, width = 6, height = 4, units = "in")
        p <- freq.graph(df.h, i, T)
        ggsave(plot = p, paste0(getwd(), "/images/", i, "_perc",".png"), 
               dpi = 300, width = 6, height = 4, units = "in")
}





