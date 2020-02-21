# functions for standalone people plot generator

library(png)
library(ggplot2)
# img.white <- readPNG("./rsrc/person.png")
img.red <- readPNG("./rsrc/red_person.png")
img.green <- readPNG("./rsrc/green_person.png")

.imgs <- list(img.red, img.green)

PS_Plot<-function(perc, dose){
        n<-ceiling(perc)
        c=ceiling(ifelse(dose<= 0, 1, dose) *10)
        if (c>100) c=100;
        if (c<1) c=1;
        if (n > 100 | n < 0) stop('Percent exposed (n) should be between 0 and 100!')
        
        person_images <- .imgs
        
        ramp1 <- colorRamp(c("ivory2","red1"))
        person_images[[1]] %<>% as.raster()
        colorramp <- ramp((rgb(ramp1(seq(0, 1, length = 101)), max = 255)[c+1]))
        
        person_images[[1]] %<>% img_to_colorramp(colorramp)
        
        base_plot <- ggplot() +
                theme(panel.background = element_blank(),
                      aspect.ratio = 1.4) # make sure the plot looks rectangular
        
        for (i in 0:99) {
                base_plot %<>% add_person(i, n, person_images)
        }
        return(base_plot)
}

brightness <- function(hex) {
        v <- col2rgb(hex)
        sqrt(0.299 * v[1]^2 + 0.587 * v[2]^2 + 0.114 * v[3]^2) /255
}

img_to_colorramp <- function(img, ramp) {
        cv <- as.vector(img)
        b <- sapply(cv, brightness)
        g <- ramp(b)
        a <- substr(cv, 8,9)     # get alpha values
        ga <- paste0(g, a)       # add alpha values to new colors
        img.grey <- matrix(ga, nrow(img), ncol(img), byrow=TRUE)
}

ramp <- function(colors)
        function(x) rgb(colorRamp(colors)(x), maxColorValue = 255)

add_person <- function(p, i, n, imgs) {
        # return a rasterized annotation for ggplot to use
        x = (i%%10*.1) # min val
        y = 1 - ((i%/%10) * .1)
        determine_person <- . %>% is_less_than(n) %>% ifelse(1, 2)
        
        p=  p + annotation_raster(imgs[[determine_person(i)]],
                                  xmax = x+.09,
                                  xmin = x,
                                  ymax = y,
                                  ymin= y-.09, interpolate=FALSE)
        return(p)
}


#####

add_breaks_to_title <- function(title, n_words=4) {
        split_title <- strsplit(title, ' ') %>% unlist()
        n_words <- length(split_title)
        if (n_words>n_words) {
                split_title[n_words] %<>% paste0('\n')
        }
        title <- paste(split_title, collapse=' ')
        return(title)
}

lab_label <- . %>% ifelse(., "CFU/Month E. coli", "MPN/Month E. coli")

make_pplplot <- function(sample, neighborhood, age, perc, dose, title=NULL, subtitle=NULL, caption=NULL, lab_MF=F) {
        # a light wrapper around PS_Plot to stay consistent with the other plotting apis
        if (is.null(title)) title = sprintf("%s", sample) %>% add_breaks_to_title()
        # Change to be 1st line: Pathway; 2nd line: Neighborhood Name; 3rd line: Adults vs. Children; 4th line: % exposed; 5th line: Dose
        if (is.null(subtitle)) subtitle= sprintf("%s\n%s\n%s exposed\n%s %s",
                                                 neighborhood,
                                                 age,
                                                 scales::percent(round(perc/100, 3)),
                                                 paste(round(dose, 1),'Log10'),
                                                 lab_label(lab_MF)
        )
        if (is.null(caption)) caption= 'Dose is displayed in log scale'
        
        ppl_plot <- PS_Plot(perc, dose) +
                labs(title= title,
                     subtitle= subtitle,
                     caption= caption)
        
        
        return(ppl_plot)
}


make_plots <- function(obj,
                       output_dir='./plots/',
                       width=NA,
                       height=NA,
                       units='in',
                       dpi=72,
                       convert_px=T,
                       parallel=F,
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
        
        if (convert_px) {
                width = convert_pixels(width, dpi)
                height = convert_pixels(height, dpi)
        }
        
        obj <- mclapply(obj, function(x) {
                # make the plot
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

