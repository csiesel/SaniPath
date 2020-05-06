# data load
library(readxl)
library(dplyr)

# **************************************************************************************************
#### Loading data ####
meta_dply <- read.csv( "data/meta_deployments.csv", stringsAsFactors = F)

meta_neighb <- read.csv( "data/meta_neighborhoods.csv", stringsAsFactors = F)
meta_neighb <- meta_neighb %>% filter(neighb_UID != 501 & neighb_UID != 502)

meta_neighb$neighborhood[which(meta_neighb$neighb_UID==801)] <- "Maxaquene"
meta_neighb$neighborhood[which(meta_neighb$neighb_UID==802)] <- "Chamanculu"


meta_sampleID <- read.csv( "data/meta_sampleID.csv", stringsAsFactors = F)
df.behav.city <- read.csv( "data/behavior_all_city_neighborhood_03242020.csv", stringsAsFactors = F) #done
df.behav.city <- df.behav.city %>% filter(neighb_UID != 501 & neighb_UID != 502)
df.behav.city$neighborhood[which(df.behav.city$neighb_UID==801)] <- "Maxaquene"
df.behav.city$neighborhood[which(df.behav.city$neighb_UID==802)] <- "Chamanculu"

df.behav.all <- read.csv( "data/behavior_all_city_percent_04212020.csv", stringsAsFactors = F) #done
df.ecdata <- read.csv( "data/ec_data_2020-02-12.csv", stringsAsFactors = F) #done
df.ecdata <- df.ecdata %>% filter(neighb_UID != 501 & neighb_UID != 502)


df.col <- read.csv( "data/col_merged_2020-02-12.csv", stringsAsFactors = F) #done

df.exposure <- read.csv("data/multicity_exposure_2020-03-05.csv", stringsAsFactors = F) #done
df.exposure <- df.exposure %>% filter(neighb_id != 501 & neighb_id != 502)
df.exposure$neighborhood[which(df.exposure$neighb_id==801)] <- "Maxaquene"
df.exposure$neighborhood[which(df.exposure$neighb_id==802)] <- "Chamanculu"

df.hh <- read.csv( "data/h_merged_2020-02-10.csv", stringsAsFactors = F)
df.sc <- read.csv( "data/s_merged_2020-02-10.csv", stringsAsFactors = F)
df.cc <- read.csv( "data/c_merged_2020-02-10.csv", stringsAsFactors = F)
pathway.info <- read.csv( "data/pathway_info.csv", stringsAsFactors = F)


# **************************************************************************************************
#### modify behavior data ####
colnames(df.behav.city) <- c("neighb_UID", "sample_type", "pop", "10+", "6-10", "<5", "Never", "deployment_id", "country", "city",
                        "citylabel", "neighborhood")
df.behav.city$sum <- NULL
df.behav.city$pop <- factor(df.behav.city$pop)
df.behav.city[ is.na(df.behav.city) ] <- NA
df.behav.city$SES<-""
for(i in 1:nrow(df.behav.city)){
  df.behav.city$SES[i] <- meta_neighb$SES[which(meta_neighb$neighb_UID==df.behav.city$neighb_UID[i])]
}

df.behav.city$unit <- factor(df.behav.city$sample_type,
                                    levels=c("Open Drain Water", "Ocean", "Surface Water",
                                             "Bathing Water", "Floodwater", "Public Latrine", "Raw Produce", "Street Food",
                                             "Municipal Drinking Water", "Other Drinking Water"),
                                    labels=c("Open Drain Water"="Times/Month", "Ocean"="Month",
                                             "Surface Water"="Times/Month", "Bathing Water"="Times/Week",
                                             "Floodwater"="Times/Week", "Public Latrine"="Times/Week",
                                             "Raw Produce"="Times/Week", "Street Food"="Times/Week",
                                             "Municipal Drinking Water"="Days/Week", "Other Drinking Water"="Days/Week"))


colnames(df.behav.all) <- c("city", "sample_type", "pop", "sum", "10+", "6-10", "<5", "Never")
df.behav.all$sum <- NULL
df.behav.all$pop <- factor(df.behav.all$pop)
df.behav.all[ is.na(df.behav.all) ] <- NA
df.behav.all$unit <- factor(df.behav.all$sample_type,
                             levels=c("Open Drain Water", "Ocean", "Surface Water",
                                      "Bathing Water", "Floodwater", "Public Latrine", "Raw Produce", "Street Food",
                                      "Municipal Drinking Water", "Other Drinking Water"),
                             labels=c("Open Drain Water"="Times/Month", "Ocean"="Month",
                                      "Surface Water"="Times/Month", "Bathing Water"="Times/Week",
                                      "Floodwater"="Times/Week", "Public Latrine"="Times/Week",
                                      "Raw Produce"="Times/Week", "Street Food"="Times/Week",
                                      "Municipal Drinking Water"="Days/Week", "Other Drinking Water"="Days/Week"))

# **************************************************************************************************
#### rearranging order and formatting date and making meta files ####
meta_dply$date <- as.Date(meta_dply$date, format="%m/%d/%Y")
meta_dply <- meta_dply %>% arrange((date))
meta_dply <- meta_dply %>% mutate(citylabel = factor(.$citylabel, levels = factor(meta_dply$citylabel)))
meta_dply <- meta_dply %>% mutate(country = factor(.$country, levels = factor(unique(meta_dply$country))))
meta_full <- merge(meta_dply, meta_neighb, by.x="id", by.y="deployment_id")
cities <- unique(meta_dply$city)
hoods <- unique(meta_neighb$neighborhood)
samples <- as.character(unique(df.ecdata$sample_type_name))
colourCount = length(unique(meta_dply$city))
getPalette = colorRampPalette(c("#5F4690", "#1D6996", "#38A6A5","#0F8554","#73AF48","#EDAD08","#E17C05",
                                "#CC503E","#94346E","#6F4070","#994E95","#666666"))
# getPalette2 = colorRampPalette(brewer.pal(9, "Pastel1"))
getPalette2 = colorRampPalette(c("#E4E4E4", "#6FAAC0"))
colScale <- scale_fill_manual(values=getPalette(colourCount))


# **************************************************************************************************
#### Putting together df.ecdata files ####
meta_sampleID <- meta_sampleID %>% 
  mutate(sample_type_name = factor(.$sample_type_name, levels = factor(meta_sampleID$sample_type_name)))

df.ecdata <- df.ecdata %>% 
        left_join(., meta_sampleID, by = c("sample_type" = "id")) %>%
        left_join(., meta_dply[, c("id", "country", "city", "citylabel")], by = c("dply_num" = "id"))

# **************************************************************************************************
#### Standardizing e. coli data ####
df.ecdata$std_ec_conc <- 0

#change group_by(sample_type, 2ND VARIABLE HERE) depending on what is used to standardize (city right now)

max_min <- df.ecdata %>% group_by(sample_type, city) %>% summarise(max=max(ec_conc, na.rm=TRUE), min=min(ec_conc, na.rm=TRUE))
for(i in 1:nrow(df.ecdata)){
  if(is.na(df.ecdata$ec_conc[i])){
    df.ecdata$std_ec_conc[i]=NA
  }
  else{
    max<-log10(max_min$max[which(max_min$sample_type==df.ecdata$sample_type[i] & max_min$city==df.ecdata$city[i])])
    min<-log10(max_min$min[which(max_min$sample_type==df.ecdata$sample_type[i] & max_min$city==df.ecdata$city[i])])
    df.ecdata$std_ec_conc[i]=(log10(df.ecdata$ec_conc[i])-min)/(max-min)
  }
}

df.ecdata$hood <- ""
df.ecdata$SES <- ""
for(i in 1:nrow(df.ecdata)){
  if(is.na(df.ecdata$neighb_UID[i])){
    df.ecdata$hood[i]=""
    df.ecdata$SES[i]=""
  }
  else{
    df.ecdata$hood[i] = meta_neighb$neighborhood[which(meta_neighb$neighb_UID==df.ecdata$neighb_UID[i])]
    df.ecdata$SES[i] <- meta_neighb$SES[which(meta_neighb$neighborhood==df.ecdata$hood[i])]
  }
}


# **************************************************************************************************
#### determining dominant pathways ####
df.exposure$citylabel <-  factor(df.exposure$citylabel, levels = unique(df.exposure$citylabel))

df.exposure <- df.exposure %>% 
  mutate(sample_type_name = factor(.$sample_type_name, levels = factor(unique(sample_type_name))))
df.exposure <- df.exposure %>%
  group_by(neighborhood, age) %>%
  mutate(sum = sum(dose)) %>%
  mutate(perc = (dose / sum) * 100) %>%
  mutate(logexp = log10(popDose)) %>%
  mutate(dominantcount=0) %>%
  mutate(dominant="")

multiFinal = list()
sites <- unique(df.exposure$citylabel)
neighborhoods <- unique(df.exposure$neighborhood)


for(q in 1:length(neighborhoods)){
  tempmulti1 <- filter(df.exposure, neighborhood == neighborhoods[q], pop == 'a')
  tempmulti2 <- filter(df.exposure, neighborhood == neighborhoods[q], pop == 'c')
  for(a in 1:nrow(tempmulti1)){
    maxExpA = max(tempmulti1$logexp)
    botRangeA = maxExpA - 1
    if(tempmulti1$logexp[a]>= botRangeA){
      tempmulti1$dominant[a] = "Yes"
      tempmulti1$dominantcount[a] = 1
    }
    else{tempmulti1$dominant[a] = "No"}
  }
  
  for(c in 1:nrow(tempmulti2)){
    maxExpC = max(tempmulti2$logexp)
    botRangeC = maxExpC - 1
    if(tempmulti2$logexp[c]>= botRangeC){
      tempmulti2$dominant[c] = "Yes"
      tempmulti2$dominantcount[c] = 1
    }
    else{tempmulti2$dominant[c] = "No"}
  }
  multiFinal[[(length(multiFinal)+1)]] <- tempmulti1
  multiFinal[[(length(multiFinal)+1)]] <- tempmulti2
}

df.dominant = do.call(rbind, multiFinal)

df.dominant <- df.dominant %>%
  filter(., dominantcount==1) %>%
  select(., c("pathway", "neighborhood", "age", "city", "dominantcount"))

df.dominant$SES<-""
for(i in 1:nrow(df.dominant)){
  df.dominant$SES[i] <- meta_neighb$SES[which(meta_neighb$neighborhood==df.dominant$neighborhood[i])]
}


#### Custom widgetUserBox maker ####
widgetUserBoxCasey <- function(..., title = NULL, subtitle = NULL, type = NULL,
                               background = FALSE, backgroundUrl = NULL,
                               src = NULL, color = NULL, footer = NULL, footer_padding = TRUE,
                               width = 6, height = NULL, boxToolSize = "sm",
                               collapsible = TRUE, collapsed = FALSE, closable = FALSE) {
  
  cl <- "widget-user-header"
  if (!is.null(color) && background == FALSE) cl <- paste0(cl, " bg-", color)
  if (isTRUE(background)) cl <- paste0(cl, " bg-black")
  
  boxCl <- "box box-widget widget-user"
  if (!is.null(type)) boxCl <- paste0(boxCl, "-", type)
  if (collapsible && collapsed) {
    boxCl <- paste(boxCl, "collapsed-box")
  }
  
  style <- NULL
  if (!is.null(height)) {
    style <- paste0("height: ", shiny::validateCssUnit(height))
  }
  
  backgroundStyle <- NULL
  if (isTRUE(background)) {
    backgroundStyle <- paste0("background: url('", backgroundUrl, "') center center;background-size: cover;")
  }
  
  # collapseTag
  collapseTag <- NULL
  if (collapsible) {
    collapseIcon <- if (collapsed) 
      "fas fa-plus"
    else "fas fa-minus"
    collapseTag <- shiny::tags$button(
      class = paste0("btn btn-box-tool", " bg-", color, " btn-", boxToolSize), 
      type = "button",
      `data-widget` = "collapse",
      tags$i(class=collapseIcon, style = "color:#FFFFFF")
      # shiny::icon(collapseIcon)
    )
  }
  
  # closeTag
  closeTag <- NULL
  if (closable) {
    closeTag <- shiny::tags$button(
      class = paste0("btn btn-box-tool", " bg-", color, " btn-", boxToolSize),
      `data-widget` = "remove",
      type = "button",
      shiny::tags$i(class = "fa fa-times")
    )
  }
  
  shiny::column(
    width = width,
    shiny::tags$div(
      class = boxCl,
      style = style,
      
      # header
      shiny::tags$div(
        class = cl,
        style = backgroundStyle,
        
        # box header buttons
        shiny::tags$div(
          class = "pull-right box-tools",
          collapseTag,
          closeTag
        ),
        
        # image
        shiny::tags$div(
          class = "widget-user-image",
          shiny::tags$img(class = "img-circle", src = src)
        ),
        
        # titles
        shiny::tags$h3(class = "widget-user-username", title),
        shiny::tags$h5(class = "widget-user-desc", subtitle)
        
      ),
      
      # body
      shiny::tags$div(class = "box-body", ...),
      
      # footer
      shiny::tags$div(
        class = if (isTRUE(footer_padding)) "box-footer" else "box-footer no-padding", 
        footer
      )
    )
  )
}







shinyDashboardThemeDIYCasey <- function(
  appFontFamily, appFontColor, logoBackColor, bodyBackColor, headerButtonBackColor, headerButtonIconColor,
  headerButtonBackColorHover, headerButtonIconColorHover, headerBackColor, headerBoxShadowColor,
  headerBoxShadowSize, sidebarBackColor, sidebarPadding, sidebarShadowRadius, sidebarShadowColor,
  sidebarMenuBackColor, sidebarMenuPadding, sidebarMenuBorderRadius, sidebarUserTextColor, sidebarSearchBackColor,
  sidebarSearchIconColor, sidebarSearchBorderColor,  sidebarTabTextColor, sidebarTabTextSize, sidebarTabBorderStyle,
  sidebarTabBorderColor, sidebarTabBorderWidth, sidebarTabBackColorSelected, sidebarTabTextColorSelected,
  sidebarTabRadiusSelected, sidebarTabTextColorHover, sidebarTabBackColorHover, sidebarTabBorderStyleHover,
  sidebarTabBorderColorHover, sidebarTabBorderWidthHover, sidebarTabRadiusHover, boxBackColor, boxBorderRadius,
  boxShadowSize, boxShadowColor, boxTitleSize, boxDefaultColor, boxPrimaryColor, boxSuccessColor, boxWarningColor,
  boxDangerColor, tabBoxTabColor, tabBoxTabTextSize, tabBoxTabTextColor, tabBoxTabTextColorSelected, tabBoxBackColor,
  tabBoxHighlightColor, tabBoxBorderRadius, buttonBackColor, buttonTextColor, buttonBorderColor, buttonBorderRadius,
  buttonBackColorHover, buttonTextColorHover, buttonBorderColorHover, buttonHeight = 34, buttonPadding = "6px 12px",
  textboxBackColor, textboxBorderColor, textboxBorderRadius, textboxBackColorSelect, textboxBorderColorSelect,
  textboxHeight = 34, textboxPadding = "6px 12px", tableBackColor, tableBorderColor,
  tableBorderTopSize, tableBorderRowSize, primaryFontColor = "auto", successFontColor = "auto",
  warningFontColor = "auto", dangerFontColor = "auto", infoFontColor = "auto", boxInfoColor = "auto"
) {
  
  htmltools::tags$head(
    
    htmltools::tags$style(
      
      htmltools::HTML(
        
        paste0(
          
          '
          /* font: google import [OPTIONAL] */
          @import url("https://fonts.googleapis.com/css2?family=Lato:wght@300&display=swap");
          /* font */
          body, label, input, button, select, box,
          .h1, .h2, .h3, .h4, .h5, h1, h2, h3, h4, h5 {
            font-family: "',appFontFamily,'";
            color: ', appFontColor, ';
          }
          /* font: fix for h6 */
          /* messes up sidebar user section if included above */
          .h6, h6 {
            font-family: "',appFontFamily,'";
          }
          /* sidebar: logo */
          .skin-blue .main-header .logo {
            background: ', logoBackColor, ';
          }
          /* sidebar: logo hover */
          .skin-blue .main-header .logo:hover {
            background: ', logoBackColor, ';
          }
          /* sidebar: collapse button  */
          .skin-blue .main-header .navbar .sidebar-toggle {
            background: ', headerButtonBackColor, ';
            color:', headerButtonIconColor, ';
          }
          /* sidebar: collapse button hover */
          .skin-blue .main-header .navbar .sidebar-toggle:hover {
            background: ', headerButtonBackColorHover, ';
            color:', headerButtonIconColorHover, ';
          }
          /* header */
          .skin-blue .main-header .navbar {
            background: ', headerBackColor, ';
            box-shadow: ', headerBoxShadowSize, ' ', headerBoxShadowColor ,';
          }
          /* sidebar*/
          .skin-blue .main-sidebar {
            background: ', sidebarBackColor, ';
            box-shadow: ', sidebarShadowRadius, " ", sidebarShadowColor, ';
            padding-left: ', sidebarPadding, 'px;
            padding-right: ', sidebarPadding, 'px;
            /* padding-top: 60px; */
          }
          /* sidebar menu */
          .main-sidebar .user-panel, .sidebar-menu, .sidebar-menu>li.header {
            white-space: nowrap;
            background: ', sidebarMenuBackColor, ';
            padding: ', sidebarMenuPadding, 'px;
            border-radius: ', sidebarMenuBorderRadius, 'px;
          }
          /* fix for user panel */
          .user-panel>.info>p, .skin-blue .user-panel>.info {
            color: ', sidebarUserTextColor, ';
            font-size: 12px;
            font-weight: normal;
          }
          section.sidebar .user-panel {
            padding: 10px;
          }
          /* sidebar: tabs */
          .skin-blue .main-sidebar .sidebar .sidebar-menu a {
            color: ', sidebarTabTextColor, ';
            font-size: ', sidebarTabTextSize ,'px;
            border-style: ', sidebarTabBorderStyle, ';
            border-color: ', sidebarTabBorderColor, ';
            border-width: ', sidebarTabBorderWidth, 'px;
          }
          /* sidebar: tab selected */
          .skin-blue .main-sidebar .sidebar .sidebar-menu .active > a {
            color: ', sidebarTabTextColorSelected, ';
            font-size: ', sidebarTabTextSize, 'px;
            border-radius: ', sidebarTabRadiusSelected, ';
            border-style: ', sidebarTabBorderStyleHover, ';
            border-color: ', sidebarTabBorderColorHover, ';
            border-width: ', sidebarTabBorderWidthHover, 'px;
          }
          .skin-blue .sidebar-menu > li:hover > a,
          .skin-blue .sidebar-menu > li.active > a {
            color: ', sidebarTabTextColorSelected, ';
            background: ', sidebarTabBackColorSelected, ';
            border-radius: ', sidebarTabRadiusHover, ';
          }
          /* sidebar: tab hovered */
          .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover {
            background: '
          ,sidebarTabBackColorHover, ';'
          ,'color: ', sidebarTabTextColorHover, ';
            font-size: ', sidebarTabTextSize ,'px;
            border-style: ', sidebarTabBorderStyleHover, ';
            border-color: ', sidebarTabBorderColorHover, ';
            border-width: ', sidebarTabBorderWidthHover, 'px;
            border-radius: ', sidebarTabRadiusHover, ';
          }
          /* sidebar: subtab */
          .skin-blue .sidebar-menu > li > .treeview-menu {
          	margin: 0px;
            background: ', sidebarMenuBackColor, ';
          }
          .skin-blue .treeview-menu > li > a {
            background: ', sidebarMenuBackColor, ';
          }
          /* sidebar: subtab selected */
          .skin-blue .treeview-menu > li.active > a,
          .skin-blue .treeview-menu > li > a:hover {
            background: ', sidebarTabBackColorSelected, ';
          }
          /* sidebar: search text area */
          .skin-blue .sidebar-form input[type=text] {
            background: ', sidebarSearchBackColor, ';
            color: ', appFontColor, ';
            border-radius: ', textboxBorderRadius, 'px 0px 0px ', textboxBorderRadius, 'px;
            border-color: ', sidebarSearchBorderColor, ';
            border-style: solid none solid solid;
          }
          /* sidebar: search button */
          .skin-blue .input-group-btn > .btn {
            background: ', sidebarSearchBackColor, ';
            color: ', sidebarSearchIconColor, ';
            border-radius: 0px ', textboxBorderRadius, 'px ', textboxBorderRadius, 'px 0px;
            border-style: solid solid solid none;
            border-color: ', sidebarSearchBorderColor, ';
          }
          /* sidebar form */
          .skin-blue .sidebar-form {
            border-radius: 0px;
            border: 0px none rgb(255,255,255);
            margin: 10px;
          }
          /* body */
          .content-wrapper, .right-side {
            background: ', bodyBackColor, ';
          }
          /* box */
          .box {
            background: ', boxBackColor, ';
            border-radius: ', boxBorderRadius, 'px;
            box-shadow: ', boxShadowSize, ' ', boxShadowColor, ';
          }
          /* box: title */
          .box-header .box-title {
            font-size: ', boxTitleSize, 'px;
          }
          /* tabbox: title */
          .nav-tabs-custom>.nav-tabs>li.header {
            color: ', appFontColor, ';
            font-size: ', boxTitleSize, 'px;
          }
          /* tabbox: tab color */
          .nav-tabs-custom, .nav-tabs-custom .nav-tabs li.active:hover a, .nav-tabs-custom .nav-tabs li.active a {
            background: ', tabBoxTabColor, ';
            color: ', appFontColor, ';
            border-radius: ', boxBorderRadius, 'px;
          }
          .nav-tabs-custom {
            box-shadow: ', boxShadowSize, ' ', boxShadowColor, ';
          }
          /* tabbox: active tab bg */
          .nav-tabs-custom>.nav-tabs>li.active {
            border-radius: ', tabBoxBorderRadius, 'px;
            border-top-color: ', tabBoxHighlightColor, ';
            # box-shadow: ', boxShadowSize, ' ', boxShadowColor, ';
          }
          /* tabbox: font color */
          .nav-tabs-custom>.nav-tabs>li.active:hover>a, .nav-tabs-custom>.nav-tabs>li.active>a {
            border-bottom-color: ', tabBoxTabColor, ';
            border-top-color: ', tabBoxHighlightColor, ';
            border-right-color: ', tabBoxHighlightColor, ';
            border-left-color: ', tabBoxHighlightColor, ';
            color: ', tabBoxTabTextColorSelected, ';
            font-size: ', tabBoxTabTextSize, 'px;
            border-radius: ', tabBoxBorderRadius, 'px;
          }
          /* tabbox: inactive tabs background */
          .nav-tabs-custom>.nav-tabs>li>a {
            color: ', tabBoxTabTextColor, ';
            font-size: ', tabBoxTabTextSize, 'px;
          }
          /* tabbox: top area back color */
          .nav-tabs-custom, .nav-tabs-custom>.tab-content, .nav-tabs-custom>.nav-tabs {
            border-bottom-color: ', tabBoxHighlightColor, ';
            background: ', tabBoxBackColor, ';
          }
          /* tabbox: top area rounded corners */
          .nav-tabs-custom>.nav-tabs {
            margin: 0;
            border-radius: ', tabBoxBorderRadius, 'px;
          }
          /* infobox */
          .info-box {
            background: ', boxBackColor, ';
            border-radius: ', boxBorderRadius, 'px;
            box-shadow: ', boxShadowSize, ' ', boxShadowColor, ';
          }
          /* valuebox */
          .small-box {
            border-radius: ', boxBorderRadius, 'px;
            box-shadow: ', boxShadowSize, ' ', boxShadowColor, ';
          }
          /* valuebox: main font color */
          .small-box h3, .small-box p {
            color: rgb(255,255,255)
          }
          /* box: default color */
          .box.box-solid>.box-header, .box>.box-header {
            color: ', appFontColor, ';
          }
          .box.box-solid>.box-header {
            border-radius: ', boxBorderRadius, 'px;
          }
          .box.box-solid, .box {
            border-radius: ', boxBorderRadius, 'px;
            border-top-color: ', boxDefaultColor, ';
          }
          /* box: info color */
          .box.box-solid.box-info>.box-header h3, .box.box-info>.box-header h3 {
            color: ', infoFontColor, ';
          }
          .box.box-solid.box-info>.box-header {
            background: ', boxInfoColor, ';
            border-radius: ', boxBorderRadius, 'px;
          }
          .box.box-solid.box-info, .box.box-info {
            border-color: ', boxInfoColor, ';
            border-left-color: ', boxInfoColor, ';
            border-right-color: ', boxInfoColor, ';
            border-top-color: ', boxInfoColor, ';
            border-radius: ', boxBorderRadius, 'px;
          }
          /* box: primary color */
          .box.box-solid.box-primary>.box-header h3, .box.box-primary>.box-header h3 {
            color: ', primaryFontColor, ';
          }
          .box.box-solid.box-primary>.box-header {
            background: ', boxPrimaryColor, ';
            border-radius: ', boxBorderRadius, 'px;
          }
          .box.box-solid.box-primary, .box.box-primary {
            border-color: ', boxPrimaryColor, ';
            border-left-color: ', boxPrimaryColor, ';
            border-right-color: ', boxPrimaryColor, ';
            border-top-color: ', boxPrimaryColor, ';
            border-radius: ', boxBorderRadius, 'px;
          }
          /* box: success color */
          .box.box-solid.box-success>.box-header h3, .box.box-success>.box-header h3 {
            color: ', successFontColor, ';
          }
          .box.box-solid.box-success>.box-header {
            background: ', boxSuccessColor, ';
            border-radius: ', boxBorderRadius, 'px;
          }
          .box.box-solid.box-success, .box.box-success {
            border-color: ', boxSuccessColor, ';
            border-left-color: ', boxSuccessColor, ';
            border-right-color: ', boxSuccessColor, ';
            border-top-color: ', boxSuccessColor, ';
            border-radius: ', boxBorderRadius, 'px;
          }
          /* box: warning color */
          .box.box-solid.box-warning>.box-header h3, .box.box-warning>.box-header h3 {
            color: ', warningFontColor, ';
          }
          .box.box-solid.box-warning>.box-header {
            background: ', boxWarningColor, ';
            border-radius: ', boxBorderRadius, 'px;
          }
          .box.box-solid.box-warning, .box.box-warning {
            border-color: ', boxWarningColor, ';
            border-left-color: ', boxWarningColor, ';
            border-right-color: ', boxWarningColor, ';
            border-top-color: ', boxWarningColor, ';
            border-radius: ', boxBorderRadius, 'px;
          }
          /* box: danger color */
          .box.box-solid.box-danger>.box-header h3, .box.box-danger>.box-header h3 {
            color: ', dangerFontColor, ';
          }
          .box.box-solid.box-danger>.box-header {
            background: ', boxDangerColor, ';
            border-radius: ', boxBorderRadius, 'px;
          }
          .box.box-solid.box-danger, .box.box-danger {
            border-color: ', boxDangerColor, ';
            border-left-color: ', boxDangerColor, ';
            border-right-color: ', boxDangerColor, ';
            border-top-color: ', boxDangerColor, ';
            border-radius: ', boxBorderRadius, 'px;
          }
          /* button */
          .btn-default {
            background: ', buttonBackColor, ';
            color: ', buttonTextColor, ';
            border-color: ', buttonBorderColor, ';
            border-radius: ', buttonBorderRadius, 'px;
            height: ', buttonHeight, 'px;
            padding: ', buttonPadding, ';
          }
          /* button: hover */
          .btn-default:hover {
            background: ', buttonBackColorHover, ';
            color: ', buttonTextColorHover, ';
            border-color: ', buttonBorderColorHover, ';
          }
          /* button: focus */
          .btn-default:focus, .action-button:focus {
            background: ', buttonBackColor, ';
            color: ', buttonTextColor, ';
            border-color: ', buttonBorderColor, ';
          }
          /* button: active */
          .btn-default:active, .action-button:active {
            background: ', buttonBackColor, ';
            color: ', buttonTextColor, ';
            border-color: ', buttonBorderColor, ';
          }
          /* button: visited */
          .btn-default:visited {
            background: ', buttonBackColor, ';
            color: ', buttonTextColor, ';
            border-color: ', buttonBorderColor, ';
          }
          /* textbox */
          .form-control, .selectize-input, .selectize-control.single .selectize-input {
            background: ', textboxBackColor, ';
            color: ', appFontColor, ';
            border-color: ', textboxBorderColor, ';
            border-radius: ', textboxBorderRadius, 'px;
            height: ', textboxHeight, 'px;
            min-height: ', textboxHeight, 'px;
            padding: ', textboxPadding, ';
          }
          /* textbox: selected */
          .form-control:focus, .selectize-input.focus {
            color: ', appFontColor, ';
            background: ', textboxBackColorSelect, ';
            border-color: ', textboxBorderColorSelect, ';
            -webkit-box-shadow: inset 0px 0px 0px, 0px 0px 0px;
            box-shadow: inset 0px 0px 0px, 0px 0px 0px;
          }
          /* multi-row selectize input */
          .selectize-control.multi .selectize-input.has-items {
            height: auto;
          }
          /* verbatim text output */
          .qt pre, .qt code {
            font-family: ', appFontFamily, ' !important;
          }
          pre {
            color: ', appFontColor, ';
            background-color: ', textboxBackColor, ';
            border: 1px solid ', textboxBorderColor, ';
            border-radius: ', textboxBorderRadius, 'px;
          }
          /* drop-down menu */
          .selectize-dropdown, .selectize-dropdown.form-control {
            background: ', textboxBackColor, ';
            border-radius: 4px;
          }
          /* table */
          .table {
            background: ', tableBackColor, ';
            border-radius: ', textboxBorderRadius, 'px;
          }
          /* table: row border color*/
          .table>tbody>tr>td, .table>tbody>tr>th, .table>tfoot>tr>td, .table>tfoot>tr>th, .table>thead>tr>td, .table>thead>tr>th {
            border-top: ', tableBorderRowSize, 'px solid ', tableBorderColor, ';
          }
          /* table: top border color*/
          .table>thead>tr>th {
            border-bottom: ', tableBorderTopSize, 'px solid ', tableBorderColor, ';
          }
          /* table: hover row */
          .table-hover>tbody>tr:hover {
          background-color: ', tableBorderColor, ';
          }
          /* table: stripe row */
          .table-striped>tbody>tr:nth-of-type(odd) {
            background-color: ', tableBorderColor, ';
          }
          /* table: body colour */
          table.dataTable tbody tr {
            background-color: ', tableBackColor, ' !important;
          }
          /* table: footer border colour */
          table.dataTable {
            border: 0px !important;
          }
          /* datatable: selected row */
          table.dataTable tr.selected td, table.dataTable td.selected {
            background-color: ', boxSuccessColor, ' !important;
                    color: rgb(0,0,0) !important;
          }
          /* datatable: hover row */
          table.dataTable tr.hover td, table.dataTable td.hover {
            background-color: ', tableBorderColor, ' !important;
          }
          table.dataTable.hover tbody tr:hover, table.dataTable.display tbody tr:hover {
            background-color: ', tableBorderColor, ' !important;
          }
          table.dataTable.row-border tbody th, table.dataTable.row-border tbody td,
          table.dataTable.display tbody th, table.dataTable.display tbody td {
            border-top: 1px solid ', tableBorderColor, ' !important;
          }
          /* datatable: stripe row */
          table.dataTable.stripe tbody tr.odd, table.dataTable.display tbody tr.odd {
            background-color: ', tableBorderColor, ' !important;
          }
          /* datatable: page control */
          .dataTables_wrapper .dataTables_paginate .paginate_button {
            color: ', appFontColor, ' !important;
          }
          /* datatable: table info */
          .dataTables_wrapper .dataTables_paginate .paginate_button.disabled,
          .dataTables_wrapper .dataTables_paginate .paginate_button.disabled:hover,
          .dataTables_wrapper .dataTables_paginate .paginate_button.disabled:active {
            color: ', appFontColor, ' !important;
          }
          .dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter,
          .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing,
          .dataTables_wrapper .dataTables_paginate {
            color: ', appFontColor, ' !important;
          }
          .dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter,
          .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing,
          .dataTables_wrapper .dataTables_paginate {
            color: ', appFontColor, ' !important;
          }
          /* datatable search box */
          .dataTables_wrapper .dataTables_filter input {
            background-color: ', textboxBackColor, ';
            border: 1px solid ', textboxBorderColor, ';
            border-radius: ', textboxBorderRadius, 'px;
          }
          /* notification and progress bar */
          .progress-bar {
            background-color: ', boxSuccessColor, ';
          }
          .shiny-notification {
            height: 80px;
            font-family: ', appFontFamily, ';
            font-size: 15px;
            color: rgb(0,0,0);
            background-color: rgb(225,225,225);
            border-color: rgb(205,205,205);
            border-radius: 10px;
            margin-left: -450px !important;
          }
          /* horizontal divider line */
          hr {
            border-top: 1px solid rgb(215,215,215);
          }
          /* modal */
          .modal-body {
            background-color: ', boxBackColor, ';
          }
          .modal-footer {
            background-color: ', boxBackColor, ';
          }
          .modal-header {
            background-color: ', boxBackColor, ';
          }
          '
        )
      )
    )
  )
}
