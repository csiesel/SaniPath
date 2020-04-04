library(png)
library(ggplot2)
# img.white <- readPNG("./rsrc/person.png")
img.red <- readPNG("./rsrc/red_person.png")
img.green <- readPNG("./rsrc/green_person.png")

.imgs <- list(img.red, img.green)

PS_Plot<-function(ps_freq_observation){
  n<-ceiling(ps_freq_observation$n)
  if (is_null(ps_freq_observation$dose)) {
    ps_freq_observation$dose = 0
  }
  c=ceiling(ifelse(ps_freq_observation$dose<= 0, 1, ps_freq_observation$dose) *10)
  if (c>100) c=100;
  if (c<1) c=1;
  if (n > 100 | n < 0) stop('Percent exposed (n) should be between 0 and 100!')

  person_images <- .imgs

  ramp1 <- colorRamp(c("#ffefef","red2"))
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
