library(ggplot2)
library(ggrepel)
library(ggmap)
library(gridExtra)
make_plots <- function(obj,
                       type,
                       output_dir='./plots/',
                       width=NA,
                       height=NA,
                       units='in',
                       dpi=72,
                       convert_px=T,
                       parallel=T,
                       nc=detectCores(),
                       lab_MF=F,
                       save=T,
                       .return_plots=F
                      ) {
  # Make plots for the analyzed data and output pngs to a directory
  # __________________________________________________
  # obj => the list object that has each unique pathway, sample, population
  # type => type of plot to make. can be "pie", "hist", or "ppl"
  # output_dir => the directory where the plots will be stored
  # __________________________________________________
  # returns an updated list with filename appended to each element
  if (!dir.exists(output_dir)) dir.create(output_dir)

  types <- c('pie', 'hist', 'ppl')
  if (!(type %in% types)) stop(sprintf('Invalid plot type "%s".  Options: %s',type, paste0(types, collapse=', ')))
  if (!parallel) nc <- 1
  print(nc)
  plot_func <- switch(type,
                'pie' = make_pie,
                'hist' = partial(make_histogram, lab_MF=lab_MF),
                'ppl' = partial(make_pplplot, lab_MF=lab_MF))

  if (convert_px) {
    width = convert_pixels(width, dpi)
    height = convert_pixels(height, dpi)
  }

  obj <- mclapply(obj, function(x) {
    # make the plot
    p <- plot_func(x)
    if (save & !is_null(p)) {
      x$fn <- fname(output_dir, type, x$fn)
      # if there's already a file there, remove the old one before saving a new one
      if (file.exists(x$fn)) file.remove(x$fn)

      ggsave(x$fn, p, width=width, height=height, units=units)
      if (file.exists(x$fn)) {
        cat('File successfully saved at', x$fn, '\n')
      } else {
        cat('Failed to save', x$fn, '\n')
      }
    }
    if (.return_plots) {
      # this will re turn the actual ggplot objects for plotting interactively
      return(p)
    } else {
      # otherwise just return the list of info we had with an updated fn attribute
      return(x)
    }
  }, mc.cores = nc)

  # # because i didn't think about this before...
  # if (!.return_plots) {
  #   obj %<>% bind_rows() %>%
  #     rename('filename' = fn,
  #            'pathway_code' = s,
  #            'pathway_label' = sample,
  #            neighborhood_code = neighb) %>%
  #     mutate(width= width,
  #            height = height,
  #            dpi = dpi,
  #            units= units)
  #
  # }

  return(obj)
}

add_breaks_to_title <- function(title, n_words=4) {
  split_title <- strsplit(title, ' ') %>% unlist()
  n_words <- length(split_title)
  if (n_words>n_words) {
    split_title[n_words] %<>% paste0('\n')
  }
  title <- paste(split_title, collapse=' ')
  return(title)
}

make_pie <- function(freq, title=NULL, subtitle=NULL, caption=NULL) {
  # this will make a pie charts based on the input freq values

  # first let's regroup the data into a table that can be used
  # for plotting.  it will have 4 columns, neighborhood, age, answer, Freq

  if (is.null(title)) title = sprintf("%s", freq$sample) %>% add_breaks_to_title()
  if (is.null(subtitle)) subtitle = sprintf("%s\n%s (N=%s)",
                                            freq$neighborhood,
                                            freq$age,
                                            length(freq$data)
                                            )
  if (is.null(caption)) caption= ''


  tbl <- .create_freqTbl(freq$data, freq$s)
  if (nrow(tbl) > 0) {
    p <- suppressWarnings(ggpie(tbl,
                                'answer',
                                'Freq',
                              nudgex=.6))
    p = p + labs(title=title,
                 subtitle=subtitle,
                 caption=caption)
    return(p)
  } else {
    return(NULL)
  }
}

.create_freqTbl <- function(freq_vector, sample_type) {
  # convert the answers from the frequency calculation funcitons into
  # a table for plotting
  labels <- unlist(ifelse(sample_type=='dw', list(c("everyday","4-6/wk","1-3/wk","never","don't know","NA")),
                          ifelse(sample_type=="p" | sample_type=="l" | sample_type=="f" | sample_type=="sf" | sample_type=="bw",list(c(">10/wk","6-10/wk","1-5/wk","never","don't know")),
                                 list(c(">10/mo","6-10/mo","1-5/mo","never","don't know","NA"))))
  )
  #colors <- c('#00FF00', '#99FF00', '#FF6600', '#FF0000', '#333333')
  colors <- c("#F8766D", "#A3A500", "#00BF7D", "#00B0F6", "#E76BF3","#45545f")
  
  tbl <- as.data.frame(table('answer'= freq_vector))
  tbl$color <- colors[factor_to_numeric(tbl$answer)] #why using +1 in this line and next line???
  tbl$answer <- labels[factor_to_numeric(tbl$answer)]
  tbl$Freq <- factor_to_numeric(tbl$Freq)
  
  
  tbl$breaks <- cumsum(tbl$Freq) - tbl$Freq / 2
  if (nrow(tbl)) {
    tbl$labels = paste(tbl$answer, "\n", paste0(round(tbl$Freq / sum(tbl$Freq) * 100, 1),"%"))
  }
  return(tbl)
}

make_histogram <- function(conc, title=NULL, subtitle=NULL, caption=NULL, lab_MF=F) {
  # make a histogram with a specific path data
  d <- data.frame(x= log10(as.numeric(conc$data)), stringsAsFactors = F)
 
  if (is.null(title)) title = sprintf("%s", conc$sample) %>% add_breaks_to_title()
  if (is.null(subtitle)) subtitle = sprintf("%s (N=%s)\n",
                                            conc$neighborhood,
                                            length(which(!is.null(conc$data) & !is.na(conc$data))))
  if (is.null(caption) & lab_MF==T) {
    caption= ifelse(conc$s %in% c('p','sf'),expression(paste("log10 ", italic("E. coli "), "concentration (CFU/serving)")),
                    ifelse(conc$s=='l',expression(paste("log10 ", italic("E. coli "), "concentration (CFU/swab)")),
                           ifelse(conc$s=='pa',expression(paste("log10 ", italic("E. coli "), "concentration (CFU/gram)")),
                                  expression(paste("log10 ", italic("E. coli "), "concentration (CFU/100mL)")))))
  } else if (is.null(caption) & lab_MF==F) {
    caption= ifelse(conc$s %in% c('p','sf'),expression(paste("log10 ", italic("E. coli "), "concentration (MPN/serving)")),
                    ifelse(conc$s=='l',expression(paste("log10 ", italic("E. coli "), "concentration (MPN/swab)")),
                           ifelse(conc$s=='pa',expression(paste("log10 ", italic("E. coli "), "concentration (MPN/gram)")),
                                  expression(paste("log10 ", italic("E. coli "), "concentration (MPN/100mL)")))))
  }
 
  histogram <- ggplot(d, aes(x)) +
    geom_histogram(aes(y=(..count..)/sum(..count..)),
                   binwidth = 1, fill='skyblue', colour='black') +
    xlim(-2,10)  +
    scale_y_continuous(labels=scales::percent,
                       limits=c(0,1),
                       breaks=seq(.2, 1, by=.2)) +
    theme(panel.background = element_blank()) +
    labs(title = title,
         subtitle= subtitle,
         caption = caption,
         y='% of Samples', x='')
  return(histogram)
}

make_pplplot <- function(ps.freq, title=NULL, subtitle=NULL, caption=NULL, lab_MF=F) {
  # a light wrapper around PS_Plot to stay consistent with the other plotting apis
  if (is.null(title)) title = sprintf("%s", ps.freq$sample) %>% add_breaks_to_title()
  # Change to be 1st line: Pathway; 2nd line: Neighborhood Name; 3rd line: Adults vs. Children; 4th line: % exposed; 5th line: Dose
  if (is.null(subtitle)) subtitle= sprintf("%s\n%s\n%s exposed\n%s %s",
                                           ps.freq$neighborhood,
                                           ps.freq$age,
                                           scales::percent(round(ps.freq$n/100, 3)),
                                           paste(round(if_else(is_null(ps.freq$dose), 0, ps.freq$dose), 1),'Log10'),
                                           lab_label(lab_MF)
  )
  if (is.null(caption)) caption= 'Dose is displayed in log scale'
  
  ppl_plot <- PS_Plot(ps.freq) +
    labs(title= title,
         subtitle= subtitle,
         caption= caption)
  
  
  return(ppl_plot)
}

make_survey_maps <- function(..., neighborhood_mapping=NULL) {
  type_names <- list('c' = 'Community',
                     's' = 'School',
                     'h' = 'Household',
                     'col' = 'Sample Collection')
  cols <- c('latitude', 'longitude', 'neighborhood', 'type') 
  lat_long <- lapply(list(...),
                     function(x)  {
                       x %<>% select(matches('(latitude|longitude)'), matches("_neighborhood$"))# %>%
                         # mutate_if(.predicate = is.factor, .funs = as.numeric)
                       # names(x) %<>% gsub("_|(location|coordinates)|^[h,c,s,col]", '', .)
                       x$type <- type_names[sapply(names(type_names), function(j) any(grep(paste0("^", j, "_"), names(x))))] %>% unlist()
                       names(x) <- cols[sapply(cols, function(j) grep(j, names(x)))]
                       x %<>% filter(latitude != 'n/a' | longitude != 'n/a')
                       x %<>% mutate_at(vars(latitude, longitude), .funs = factor_to_numeric)
                       x
                     }) %>%
    bind_rows() %>%
    rename('lat' = 'latitude',
           'lon' = 'longitude')
  if (!is_null(neighborhood_mapping)) {
    lat_long %<>%
      left_join(tibble('neighborhood_code' = unlist(neighborhood_mapping),
                       'neighborhood_name' = names(neighborhood_mapping)),
                by=c('neighborhood' = 'neighborhood_code')) %>%
      select(-neighborhood) %>%
      rename(neighborhood = neighborhood_name)
  } else {
    lat_long %<>% mutate(neighborhood = sprintf('Neighborhood %s', neighborhood))
  }
  neighborhoods <- unique(lat_long$neighborhood)
  #delete some outliers and subset data
  #outliers section was lat_long$lon > 24
  # lat_long <- lat_long[which(lat_long$type %in% c("Household", "Sample Collection")),]
  #turn type and neighborhood into factors
  lat_long$type <- as.factor(lat_long$type)
  lat_long$neighborhood <- as.factor(lat_long$neighborhood)
  #calculate neighborhood centroids by median to exclude extreme outliers
  # sub_lat_long <- lat_long %>% aggregate(.~neighborhood, data=., FUN =median)
  sub_lat_long <- lat_long %>%
    group_by(neighborhood) %>%
    summarise_at(vars(lat, lon), .funs=partial(median, na.rm=T))
  #adjusting bounding box to filter out extreme outliers
  bounding_box <- make_bbox(lon = lon,
                            lat = lat,
                            data = lat_long %>%
                              subset(lon < median(lon, na.rm=T)+5) %>%
                              subset(lon > median(lon)-5, na.rm=T) %>%
                              subset(lat < median(lat)+5, na.rm=T) %>%
                              subset(lat > median(lat)-5, na.rm=T),
                            f=1)


  zoom <- calc_zoom(bounding_box)
  map_fixed <- F

  #google maps is more reliable in download but still very difficult in adjusting zoom,
  #we now stemp through zoom levels until an approriate one is found
  sq_map <- ggmap(get_stamenmap(bounding_box, zoom = zoom))
  map_plot <- sq_map +
    geom_point(data = sub_lat_long,
               aes(x = lon, y = lat),
               size =3,
               alpha=.8,
               shape=24,
               fill="red") +
    geom_text_repel(data = sub_lat_long,
                    aes(label= neighborhood),
                    size=3.5) +
    labs(x = "Longitude", y = "Latitude")
    # ggtitle("Neighborhood Locations")


  # #pulling maps can be finicky, building in some error checking
  # sq_map <- NULL
  # attempt <- 1
  # while(is.null(sq_map) & attempt <= 5){
  #   sq_map <- get_map(bounding_box, zoom = 12)}
  #
  # if(!is.null(sq_map)) {
  #   map <- ggmap(sq_map) +
  #     geom_point(data = sub_lat_long, aes(x = lon, y = lat), size =7, shape=24, fill="red") +
  #     geom_label(data = sub_lat_long, aes(label=paste("Neighborhood",neighborhood)),hjust=-.1, vjust=0) +
  #     labs(x = "Longitude", y = "Latitude") +
  #     ggtitle("Neighborhood Centroids")
  # } else {
  #   # Something went wrong, but we don't want to kill the whole thing
  #   map <- NULL
  # }
  #
  # map <- lapply(neighborhoods, function(i) {
  #   sub_lat_long <- lat_long %>% filter(neighborhood == i)
  #   bounding_box <- make_bbox(lon = lon,
  #                             lat = lat,
  #                             data= sub_lat_long, #lat_long[which(lat_long$neighborhood == neighborhoods[i]),],
  #                             f=.1)
  #
  #
  #   map
  # })
  #
  return(map_plot)
}



## Graphing support -----------------------------------------------------
ggpie <- function (dat, group_by, value_column, nudgex=.5) {
  # found this function online to create pie charts using ggplot
  # pass the melted data set, group column (group_by) and value column (value_column)
  # to give credit where credit is due:
  # http://mathematicalcoffee.blogspot.com/2014/06/ggpie-pie-graphs-in-ggplot2.html
  # label repelling courtesy of ggrepel
  # https://cran.r-project.org/web/packages/ggrepel/vignettes/ggrepel.html

  dat0 <- data.frame(answer = factor(rep(dat$answer, times = dat$Freq), levels = dat$answer),
                     color = factor(rep(dat$color, times = dat$Freq), levels = dat$color))

  plot <-
    ggplot(dat, aes(x = factor(1), y = dat$Freq)) +
    geom_bar(stat = 'identity',
             size = 1,
             alpha = .5,
             fill=dat$color,
             colour="white") +
    coord_polar(theta = "y") +
    guides(fill = guide_legend(override.aes = list(colour = NA))) + # removes black borders from legend
    theme(
      plot.title=element_text(face='bold'),
      panel.background = element_blank(),
      axis.ticks = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_blank(),
      axis.title = element_blank(),
      panel.border = element_blank(),
      legend.position = 'none'
    ) +
    scale_y_continuous(breaks = cumsum(dat[[value_column]]) - dat[[value_column]] / 2)  +
    geom_text_repel(aes(y=breaks, label= labels),
                                     color='black', fontface='bold',
                                      box.padding = unit(.1, 'lines'),
                                      point.padding = unit(.1, 'lines'),
                                      nudge_y = 0,
                                      nudge_x= nudgex,# nudge sets how far out the labels show
                                      alpha=.7)
#
#     scale_fill_manual(values=dat$color) + scale_color_manual(values=dat$color)
#     geom_text_repel(aes(y=breaks, label= labels), color='black',
#                      box.padding = unit(.18, 'lines'), fontface='bold',
#                      point.padding = unit(.12, 'lines'),nudge_y = 0, nudge_x=nudgex # nudge sets how far out the labels show
#                      ,alpha=.60)


  return(plot)
}

# PLOTTING ====================================================================
## PIE CHARTS
create_shinyPieCharts <- function(dat, label_level, sample_filter=NULL, neighborhood_filter=NULL, age_filter=NULL, output) {

  if (is.null(sample_filter)) sample_filter <- unique(names(list.names(dat, sample)))
  if (is.null(neighborhood_filter)) neighborhood_filter <- unique(names(list.names(dat, neighborhood)))
  if (is.null(age_filter)) age_filter <- unique(names(list.names(dat, age)))

  dat <- dat[list.which(dat, sample %in% sample_filter &&
                          neighborhood %in% neighborhood_filter &&
                          age %in% age_filter)]
  for (i in dat) {
    local({
      my_i <- i
      p_name <- gsub(' ', '', paste0(my_i$path$sample,"-",my_i$neighborhood, '-', my_i$age))
      output[[p_name]] <- create_pieChart(my_i$data, my_i$sample, my_i[label_level])
    })
  }



  return(output)
}


ordered_shinyCharts <- function(dat, columns=2, level1_type=NULL, level2_type=NULL,
                                sample_filter=NULL, neighborhood_filter=NULL,
                                age_filter=NULL, width=450, height=400, chart_prefix='pie-', non_shiny=F,
                                shinySession=NULL) {

  # this will order the pie charts and people plots according to the levels specified
  # double check that the level types stated are correct and do not repeat
  correct_types <- c('sample', 'neighborhood', 'age')
  stated_types <- list(level1_type, level2_type)
  stated_types <- append(stated_types, correct_types[!(correct_types %in% stated_types)])

  filters <- list('sample' = sample_filter, 'neighborhood' = neighborhood_filter,'age' = age_filter)
  assign('level1_filter', filters[[stated_types[[1]]]])
  assign('level2_filter', filters[[stated_types[[2]]]])
  assign('level3_filter', filters[[stated_types[[3]]]])



  # check if there are any dupes
  dupe_check <- unlist(stated_types)
  dupe_types <- duplicated(dupe_check)
  if (any(dupe_types)) {
    stop(paste('Duplicate level type:', stated_types[dupe_check][dupe_types]))
  }


  # Check if all of the types stated exist
  # we return TRUE if the value is NULL because we check for NULL vals after
  correct_test <- sapply(stated_types, function(x) {if(!is.null(x)) x %in% correct_types else T})
  if(all(correct_test) != T) {
    stop(paste('Unrecognized level type:', stated_types[correct_test != T],
               '\nLevel type must be one of the following:', paste(correct_types, collapse=', ')))
  }

  # check for NULL values
  null_vals <- sapply(stated_types, is.null)
  if (any(null_vals)) {
    for (n in 1:length(stated_types[null_vals])) {
      # loop through the null parameters and fill in the first correct type available
      stated_types[null_vals][n] <- correct_types[!(correct_types %in% stated_types)][1]
    }
  }


  # if the filters are null, do everything.
  if (is.null(level1_filter)) level1_filter <- unique(names(list.names(dat, eval(parse(text=stated_types[1])))))
  if (is.null(level2_filter)) level2_filter <- unique(names(list.names(dat, eval(parse(text=stated_types[2])))))
  if (is.null(level3_filter)) level3_filter <- unique(names(list.names(dat, eval(parse(text=stated_types[3])))))

  ordered_list <- list()
  if (non_shiny == F) {
    # Shiny Generation
    withProgress({
      count <- 1
      nplot <- length(dat)
      for (l1 in level1_filter) {
        # add the first level header
        ordered_list <- append(ordered_list, list(fluidRow(h2(toupper(l1)))))
        ordered_list <- append(ordered_list, list(hr()))
        for (l2 in level2_filter) {
          # add the second level header
          ordered_list <- append(ordered_list, list(fluidRow(h4(toupper(l2)))))
          # subset the data down to l1 + l2.  this will be the third
          # level set of data.  Ex. if set to filter by sample then
          # neighborhood, this subset will contain adults and children
          # per combination of sample and neighborhood.
          l2_sub <- dat[list.which(dat, eval(parse(text=stated_types[1])) == l1 && # level 1 filter
                                     eval(parse(text=stated_types[2])) == l2 && # level 2 filter
                                     eval(parse(text=stated_types[3])) %in% level3_filter)] # match anything in the level 3 filter
          l2_sub <- l2_sub[unlist(lapply(l2_sub, function(x) length(x$data) > 0))]
          # make plot names for the options that matched by combining sample + neighborhood + age
          plot_names <- sapply(1:length(l2_sub), function(x) paste0(chart_prefix, l2_sub[x]$path$sample, '-', l2_sub[x]$path$neighborhood, "-", l2_sub[x]$path$age))
          # there are spaces in the names, since we're using the for labels too,
          # let's remove those for the actual plot names
          plot_names <- gsub(" ", "", plot_names)
          # count the plots we need to make
          num_plots <- length(l2_sub)
          # count the number of rows specified.
          num_rows <- ceiling(num_plots/columns)
          col_size <- 12/columns # this is how wide each column will be.

          if (num_rows > 0) {
            for (i in seq(1, num_plots, by=columns)) {
              # each value of i indicates a new row in the output
              # make the columns populated with the plots we want

              # grab the plots
              row <- plot_names[(i + 0:(columns - 1))]
              row <- row[!is.na(row)]
              row_names <- unlist(sapply(l2_sub[i + 0:(columns - 1)], function(x) x[stated_types[[3]]]))
              row_names <- row_names[!is.na(row_names) & !is.null(row_names)]
              # make each row element a column
              row <- sapply(1:length(row), function(a) paste0('column(',col_size,",align='center',h5('",row_names[a],"', align='center'), plotOutput('",row[a],"', height=", height,", width=", width,"))"))
              # make that into one long string appropriately separated by commas
              row <- paste(row, collapse = ", ")
              # add a row marker
              ordered_list <- append(ordered_list, list(eval(parse(text=paste0('fluidRow(',row,')')))))
            }
          }
          incProgress(count/nplot)
          count <- count + 1
        }
      }
    }, message='Arranging Charts')
  } else {
    # Non-Shiny Example of how it works
    for (l1 in level1_filter) {
      ordered_list <- append(ordered_list, paste("Level 1 Heading:",l1))
      for (l2 in level2_filter) {
        ordered_list <- append(ordered_list, paste('Level 2 heading:',l2))
        l2_sub <- dat[list.which(dat, eval(parse(text=stated_types[1])) == l1 && eval(parse(text=stated_types[2])) == l2)]
        plot_names <- sapply(1:length(l2_sub), function(x) paste0(chart_prefix, l2_sub[x]$path$sample, '-', l2_sub[x]$path$neighborhood, "-", l2_sub[x]$path$age))
        plot_names <- gsub(" ", "", plot_names)
        num_plots <- length(l2_sub)
        num_rows <- ceiling(num_plots/columns)
        col_size <- 12/columns # this is how wide each column will be.


        for (i in seq(1, num_plots, by=columns)) {
          row <- plot_names[(i + 0:(columns - 1))]
          row <- row[!is.na(row)]
          row <- sapply(1:length(row), function(a) paste0("column(",col_size,",h5('",l2_sub[[a]][stated_types[[3]]], "'), plotOutput('",row[a],"'))"))
          row <- paste(row, collapse = ", ")
          ordered_list <- append(ordered_list, list(row))
        }
      }
    }
  }
  return(ordered_list)
}

ordered_shinyHists <- function(dat, columns=2, level1_type=NULL,
                               sample_filter=NULL, neighborhood_filter=NULL) {

  # this is the same as ordered_shinyCharts, but for histograms.  it will organize by
  # the first level stated.  this only has one level since the histograms do not
  # go down to age of respondent.
  # double check that the level types stated are correct and do not repeat
  correct_types <- c('sample', 'neighborhood')
  stated_types <- list(level1_type)
  stated_types <- append(stated_types, correct_types[!(correct_types %in% stated_types)])

  filters <- list('sample' = sample_filter, 'neighborhood' = neighborhood_filter)
  assign('level1_filter', filters[[stated_types[[1]]]])
  assign('level2_filter', filters[[stated_types[[2]]]])



  # check if there are any dupes
  dupe_check <- unlist(stated_types)
  dupe_types <- duplicated(dupe_check)
  if (any(dupe_types)) {
    stop(paste('Duplicate level type:', stated_types[dupe_check][dupe_types]))
  }


  # Check if all of the types stated exist
  # we return TRUE if the value is NULL because we check for NULL vals after
  correct_test <- sapply(stated_types, function(x) {if(!is.null(x)) x %in% correct_types else T})
  if(all(correct_test) != T) {
    stop(paste('Unrecognized level type:', stated_types[correct_test != T],
               '\nLevel type must be one of the following:', paste(correct_types, collapse=', ')))
  }

  # check for NULL values
  null_vals <- sapply(stated_types, is.null)
  if (any(null_vals)) {
    for (n in 1:length(stated_types[null_vals])) {
      # loop through the null parameters and fill in the first correct type available
      stated_types[null_vals][n] <- correct_types[!(correct_types %in% stated_types)][1]
    }
  }


  # if the filters are empty, we have to do something, so
  if (is.null(level1_filter)) level1_filter <- unique(names(list.names(dat, eval(parse(text=stated_types[1])))))
  if (is.null(level2_filter)) level2_filter <- unique(names(list.names(dat, eval(parse(text=stated_types[2])))))

  ordered_list <- list()
  # Shiny Generation
  for (l1 in level1_filter) {
    # add the first level header
    ordered_list <- append(ordered_list, list(fluidRow(h2(toupper(l1)))))
    ordered_list <- append(ordered_list, list(hr()))
    # subset the data down to l1 + l2.  this will be the third
    # level set of data.  Ex. if set to filter by sample then
    # neighborhood, this subset will contain adults and children
    # per combination of sample and neighborhood.
    l2_sub <- dat[list.which(dat, eval(parse(text=stated_types[1])) == l1 && # level 1 filter
                               eval(parse(text=stated_types[2])) %in% level2_filter)] # match anything in the level 2 filter

    # make plot names for the options that matched by combining sample + neighborhood + age
    plot_names <- sapply(1:length(l2_sub), function(x) paste0('hist-',l2_sub[x]$conc$sample, '-', l2_sub[x]$conc$neighborhood))
    # there are spaces in the names, since we're using the for labels too,
    # let's remove those for the actual plot names
    plot_names <- gsub(" ", "", plot_names)
    # count the plots we need to make
    num_plots <- length(l2_sub)
    # count the number of rows specified.
    num_rows <- ceiling(num_plots/columns)
    col_size <- 12/columns # this is how wide each column will be.
    if(num_plots > 0) {

      for (i in seq(1, num_plots, by=columns)) {
        # each value of i indicates a new row in the output
        # make the columns populated with the plots we want

        # grab the plots
        row <- plot_names[(i + 0:(columns - 1))]
        row <- row[!is.na(row)]

        # make each row element a column
        row <- sapply(1:length(row), function(a) paste0('column(',col_size,",align='center', plotOutput('",row[a],"', height=350, width=300))"))
        # make that into one long string appropriately separated by commas
        row <- paste(row, collapse = ", ")
        # add a row marker
        ordered_list <- append(ordered_list, list(eval(parse(text=paste0('fluidRow(',row,')')))))
      }

    }

  }


  return(ordered_list)
}

convert_pixels <- function(px, dpi) {
  return(px/dpi)
}
