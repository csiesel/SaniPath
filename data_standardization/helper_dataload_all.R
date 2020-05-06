# file to load data for data discovery

path <- "/Users/caseysiesel/Desktop/SaniPath/data_standardization/"

#### load meta data ####
meta_dply <- read.table(paste0(path, "/data/meta_data/", "meta_deployments", ".csv"), 
                        sep=",", stringsAsFactors=F, header=T)
meta_neighb <- read.table(paste0(path, "/data/meta_data/", "meta_neighborhoods", ".csv"), 
                          sep=",", stringsAsFactors=F, header=T)
meta_produce <- read.table(paste0(path, "/data/meta_data/", "meta_produce", ".csv"), 
                           sep=",", stringsAsFactors=F, header=T)
meta_streetfood <- read.table(paste0(path, "/data/meta_data/", "meta_streetfood", ".csv"), 
                              sep=",", stringsAsFactors=F, header=T)
meta_sampleID <- read.table(paste0(path, "/data/meta_data/", "meta_sampleID", ".csv"), 
                            sep=",", stringsAsFactors=F, header=T)

# rearranging order and formatting date
meta_dply$date <- as.Date(meta_dply$date, format="%m/%d/%Y")
meta_dply <- meta_dply %>% arrange((date))
meta_dply <- meta_dply %>% mutate(citylabel = factor(.$citylabel, levels = factor(meta_dply$citylabel)))
meta_dply <- meta_dply %>% mutate(country = factor(.$country, levels = factor(unique(meta_dply$country))))


meta_sampleID <- meta_sampleID %>% 
  mutate(sample_type_name = factor(.$sample_type_name, levels = factor(meta_sampleID$sample_type_name)))

# exposure data
filenames <- list.files(paste0(path, "/data/"), pattern="multicity_")
max <- max((sub(".*\\_", "", sub("\\.csv.*", "", filenames))))
file <- grep(max, filenames, fixed=TRUE, value=T)
df.multi <- read.csv(paste0(path, "/data/", file), stringsAsFactors = F)

df.multi <- plyr::arrange(df.multi, date)
df.multi$citylabel <-  factor(df.multi$citylabel, levels = unique(df.multi$citylabel))

df.multi <- df.multi %>% 
  mutate(sample_type_name = factor(.$sample_type_name, levels = factor(meta_sampleID$sample_type_name)))
df.multi$sample_type_name

#### load data ####
# automatically choose latest available file based on the date
# col
filenames <- list.files(paste0(path, "/data/"), pattern="col_merged")
max <- max((sub(".*\\_", "", sub("\\.csv.*", "", filenames)))) #remove everything before the date and after in filename
file <- grep(max, filenames, fixed=TRUE, value=T)
df.col <- read.csv(paste0(path, "/data/", file))

# lab
filenames <- list.files(paste0(path, "/data/"), pattern="lab_merged")
max <- max((sub(".*\\_", "", sub("\\.csv.*", "", filenames))))
file <- grep(max, filenames, fixed=TRUE, value=T)
df.lab <- read.csv(paste0(path, "/data/", file))

# h
filenames <- list.files(paste0(path, "/data/"), pattern="h_merged")
max <- max((sub(".*\\_", "", sub("\\.csv.*", "", filenames))))
file <- grep(max, filenames, fixed=TRUE, value=T)
df.h <- read.csv(paste0(path, "/data/", file))

# s
filenames <- list.files(paste0(path, "/data/"), pattern="s_merged")
max <- max((sub(".*\\_", "", sub("\\.csv.*", "", filenames))))
file <- grep(max, filenames, fixed=TRUE, value=T)
df.s <- read.csv(paste0(path, "/data/", file))

# c
filenames <- list.files(paste0(path, "/data/"), pattern="c_merged")
max <- max((sub(".*\\_", "", sub("\\.csv.*", "", filenames))))
file <- grep(max, filenames, fixed=TRUE, value=T)
df.c <- read.csv(paste0(path, "/data/", file))

# ec data
filenames <- list.files(paste0(path, "/data/"), pattern="ec_data")
max <- max((sub(".*\\_", "", sub("\\.csv.*", "", filenames))))
file <- grep(max, filenames, fixed=TRUE, value=T)
df.ecdata <- read.csv(paste0(path, "/data/", file), stringsAsFactors = F)


