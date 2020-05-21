# Main functions for SaniPath analysis and plotting functions
sapply(grep('.R$', list.files('model', full.names = T), value=T), source)
# load("rsrc/mpn_tbl.rda")
#load("rsrc/intake.rda")
library(readr)
config <- yaml::yaml.load_file('model/config.yaml')
config$mpn_tbl <- read_csv('rsrc/mpn_tbl.csv') %>% as.matrix()
config$intake <- read_csv('rsrc/intake.csv') %>% as.matrix()


