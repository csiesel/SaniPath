# random stuff

# community survey, gender distribution -----
# gender composition check

df.c$c_participant_gender
# 0	All male
# 1	All female
# 2	A combination of male and female

table(df.c$citylabel, df.c$c_participant_gender)
table(df.s$dply_num, df.s$s_participant_gender)

table(df.c$c_participant_gender)
table(df.c$c_participant_female)
table(df.c$c_participant_male)

df.s$s_participant_gender
df.s$s_participant_male
df.s$s_participant_female
df.s$s_participant_num

df.c$c_participant_male[df.c$dply_num == 9]
df.c$c_participant_female[df.c$dply_num == 9]
df.c$c_participant_num[df.c$dply_num == 9]
df.c$c_participant_gender[df.c$dply_num == 9]


# tables were generated from here
df.c %>% select(citylabel, c_participant_gender) %>%
        table() %>% as.data.frame.matrix() -> df

df.c %>% group_by(citylabel) %>%
        summarise(male = sum(c_participant_male),
                  female = sum(c_participant_female),
                  total = sum(c_participant_num)) -> df1

df.c %>% filter(dply_num == 2) %>% select(c_UID, c_participant_gender, c_participant_male, c_participant_female, c_participant_num) -> df2

# hh function ------------------------

variable <- c("h_neighborhood",
              "h_home_type",
              "h_rainweek",
              "h_population",
              "h_c",
              "h_s_a",
              "h_s_c",
              "h_d_a",
              "h_d_c",
              "h_f_a",
              "h_f_c",
              "h_dw_a",
              "h_dw_c",
              "h_dw_e_wt",
              "h_bw_a",
              "h_bw_c",
              "h_p_a",
              "h_p_c",
              "h_p_e",
              "h_sf_a",
              "h_sf_c",
              "h_l_a",
              "h_l_c",
              "h_pl_a_th",
              "h_pl_a_tu",
              "h_pl_a_tw",
              "h_pl_a_tf")


for (i in variable) {
        title <- meta_questions %>% filter(variable == i) %>% pull(question) %>% as.character()       #droplevels()
        ansfreq <- meta_questions$answer[meta_questions$variable == i] %>% as.character()
        
        # if question has answer choices
        if(ansfreq != "integer" ) {
                ansfreq1 <- meta_answers[meta_answers$answer == ansfreq, ][,-1]
                #count
                hh %>% select(i, h_sex, "coding" = i) %>%
                        left_join(., ansfreq1, by="coding") %>%
                        mutate(label = factor(.$label, levels = factor(ansfreq1$label))) %>%
                        
                        ggplot(., aes(x = label, fill=factor(h_sex))) +
                        geom_bar(position = "dodge") +
                        geom_text(aes(label = ..count..), stat="count", vjust=-0.3, position = position_dodge(width = 1)) +
                        labs(title = title, 
                             subtitle = i,
                             x = i,
                             y = "Count",
                             fill = "Gender") +
                        theme_minimal() -> p1
                print(p1)
                
                #percent
                hh %>% select(i, h_sex, "coding" = i) %>%
                        left_join(., ansfreq1, by="coding") %>%
                        mutate(label = factor(.$label, levels = factor(ansfreq1$label))) %>%
                        
                        ggplot(., aes(x = label, fill=factor(h_sex))) +
                        geom_bar(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..]), position = position_dodge2(preserve = "single")) +
                        geom_text(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..], label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..]) ),
                                  stat="count", position=position_dodge(0.9), vjust=-0.5)+
                        scale_y_continuous(labels = scales::percent) +
                        labs(title = title, 
                             subtitle = i,
                             x = i,
                             y = "Percent of Response by Sex , %",
                             fill = "Gender") +
                        theme_minimal() -> p2
                print(p2)
                
                
        } else {
                # if question has integer answer options
                # count
                hh %>% ggplot(aes(factor(get(i)), fill=factor(h_sex)) ) +
                        geom_bar(position = "dodge") +
                        labs(title = title, 
                             subtitle = i,
                             x = i,
                             fill = "Gender") +
                        geom_text(aes(label = ..count..), stat="count", vjust=-0.3, position = position_dodge(width = 1)) +
                        theme_minimal() -> p3
                print(p3)
                
                # percent
                hh %>% ggplot(aes(factor(get(i)), fill=factor(h_sex)) ) +
                        geom_bar(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..]),
                                 position = position_dodge2(preserve = "single")) +
                        geom_text(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..],
                                       label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..]) ),
                                  stat="count", position=position_dodge(0.9), vjust=-0.5) +
                        scale_y_continuous(labels = scales::percent) +
                        labs(title = title,
                             subtitle = i,
                             x = i,
                             y = "Percent of Response by Sex , %",
                             fill = "Gender") +
                        theme_minimal() -> p4
                print(p4)
        }
        
        
        hh %>% group_by(get(i), h_sex) %>% 
                summarize(n = n()) %>%
                mutate(percentage = round((n/sum(n)*100), 2)) %>%
                data.frame() -> df
        colnames(df) <- c(eval(i), "Gender", "n", "percentage")
        
        # kable(df, format = "html") %>% 
        # kable_styling(bootstrap_options = c("hover", "bordered", "condensed"), full_width = F, position = "left")
        
        print(df, row.names = FALSE)
        
        
}

# multicity test ------------------------

library(readxl)

multi <- read_excel(paste0(getwd(), "/data/cross_country_data_2019-03-14.xlsx"), col_names = TRUE, na = "NA")

multi <- left_join(multi, meta_dply[c("citylabel", "date")])


multi <- plyr::arrange(multi, date)
# reorder levels
multi$citylabel <-  factor(multi$citylabel, levels = unique(multi$citylabel))


multi %>% filter(age == "a") -> df.a

ggplot(df.a, aes(x=dose, y=percent)) +
        geom_point(aes(color=citylabel), size = 3) +
        facet_wrap(~pathway, nrow = 2) +
        theme_bw() +
        labs(title = "Exposure by Pathway - by Deployment",
             color = "Deployment") 

# exposure calc test ------------------------
load(paste0(getwd(), "/data/expo.rda"))

table(dat.expo$neighb)           

is.na(dat.expo[, c(14,15)]) <- dat.expo[, c(14,15)] == 0
dat.expo[, c(14,15)]

rowMeans(dat.expo[, c(14,15)], na.rm = T)

# leaflet test ------------------------
library(leaflet)
# a manual legend
leaflet() %>% addTiles() %>% addLegend(
        position = "bottomright",
        colors = rgb(t(col2rgb(palette())) / 255),
        labels = palette(), opacity = 1,
        title = "An Obvious Legend"
)



# an automatic legend derived from the color palette
df <- local({
        n <- 300; x <- rnorm(n); y <- rnorm(n)
        z <- sqrt(x ^ 2 + y ^ 2); z[sample(n, 10)] <- NA
        data.frame(x, y, z)
})
pal <- colorNumeric("OrRd", df$z)
leaflet(df) %>%
        addTiles() %>%
        addCircleMarkers(~x, ~y, color = ~pal(z), group = "circles") %>%
        addLegend(pal = pal, values = ~z, group = "circles", position = "bottomleft") %>%
        addLayersControl(overlayGroups = c("circles"))

# format legend labels
df <- data.frame(x = rnorm(100), y = rexp(100, 2), z = runif(100))
pal <- colorBin("PuOr", df$z, bins = c(0, .1, .4, .9, 1))
leaflet(df) %>%
        addTiles() %>%
        addCircleMarkers(~x, ~y, color = ~pal(z), group = "circles") %>%
        addLegend(pal = pal, values = ~z, group = "circles", position = "bottomleft") %>%
        addLayersControl(overlayGroups = c("circles"))

leaflet(df) %>%
        addTiles() %>%
        addCircleMarkers(~x, ~y, color = ~pal(z), group = "circles") %>%
        addLegend(pal = pal, values = ~z, labFormat = labelFormat(
                prefix = "(", suffix = ")%", between = ", ",
                transform = function(x) 100 * x
        ),  group = "circles", position = "bottomleft" ) %>%
        addLayersControl(overlayGroups = c("circles"))

# plot on map ------------------------
# https://guangchuangyu.github.io/2016/12/scatterpie-for-plotting-pies-on-ggplot/

set.seed(123)
long <- rnorm(50, sd=100)
lat <- rnorm(50, sd=50)
d <- data.frame(long=long, lat=lat)
d <- with(d, d[abs(long) < 150 & abs(lat) < 70,])
n <- nrow(d)
d$region <- factor(1:n)
d$A <- abs(rnorm(n, sd=1))
d$B <- abs(rnorm(n, sd=2))
d$C <- abs(rnorm(n, sd=3))
d$D <- abs(rnorm(n, sd=4))
d$radius <- 6 * abs(rnorm(n))
head(d)

library(ggplot2)
library(scatterpie)

world <- map_data('world')
ggplot(world, aes(long, lat)) +
        geom_map(map=world, aes(map_id=region), fill=NA, color="black") +
        coord_quickmap() +
        theme_bw() + 
        geom_scatterpie(aes(x=long, y=lat, group=region, r=radius),
                        data=d, cols=LETTERS[1:4], color=NA, alpha=.8) +
        geom_scatterpie_legend(d$radius, x=-160, y=-55)

# denom error code ------------------------
# compare old denoms (100s everywhere) vs. new corrected ones

df.old <- read.table(paste0(getwd(), "/data/", "ec_data_2019-07-12", ".csv"), sep=",", stringsAsFactors=F, header=T)

df.new <- read.table(paste0(getwd(), "/data/", "ec_data_2019-08-02", ".csv"), sep=",", stringsAsFactors=F, header=T)


df.new <- select(df.new, UID, ec_conc)

df <- left_join(df.old, df.new, by = "UID")

df$logold <- log10(df$ec_conc.x)
df$lognew <- log10(df$ec_conc.y)

#1 
#2 produce
#3
#4
#5
#6
#7 latrine
#8 particulate
#9
#10 Streetfood

df$diff <- df$ec_conc.x - df$ec_conc.y
df$logdiff <- log10(abs(df$diff))

df.produce <- df %>% filter(sample_type == 2)
df.latrine <- df %>% filter(sample_type == 7)
df.particulate <- df %>% filter(sample_type == 8)
df.streetfood <- df %>% filter(sample_type == 10)


# ------------------------

# ------------------------

