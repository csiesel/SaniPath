darkred = "rgb(119,46,37)"
lightred = "rgb(196,69,54)"
lightlightred = "rgb(237,171,148)"
offwhite = "rgb(237,221,212)"
lightblue = "rgb(25,114,120)"
darkblue = "rgb(40,61,59)"

sanipath <- shinyDashboardThemeDIYCasey(
  
  # darkred = "rgb(119,46,37)",
  # lightred = "rgb(196,69,54)",
  # lightlightred = "rgb(237,171,148)",
  # offwhite = "rgb(237,221,212)",
  # lightblue = "rgb(25,114,120)",
  # darkblue = "rgb(40,61,59)",
  
  #SP Top BLue: 26,49,87
  #SP Light blue: 212, 234, 247
  #SP Logo light blue: 148, 202, 235
  #SP Med Blue: 72, 152, 210
  
  
  ### general
  appFontFamily = "Lato"
  ,appFontColor = "rgb(26,49,87)"
  ,primaryFontColor = "rgb(245,245,245)"
  ,infoFontColor = "rgb(245,245,245)"
  ,successFontColor = "rgb(33,37,41)"
  ,warningFontColor = "rgb(33,37,41)"
  ,dangerFontColor = "rgb(33,37,41)"
  ,bodyBackColor = "rgb(237,221,212)"
  
  ### header
  ,logoBackColor = darkred
  
  ,headerButtonBackColor = lightlightred
  ,headerButtonIconColor = darkred
  ,headerButtonBackColorHover = "rgb(212, 234, 247)"
  ,headerButtonIconColorHover = "rgb(26,49,87)"
  
  ,headerBackColor = darkred
  ,headerBoxShadowColor = "rgb(204, 204, 204)"
  ,headerBoxShadowSize = "2px 4px 4px"
  
  ### sidebar
  ,sidebarBackColor = lightlightred
  ,sidebarPadding = 0
  
  ,sidebarMenuBackColor = "inherit"
  ,sidebarMenuPadding = 0
  ,sidebarMenuBorderRadius = 0
  
  ,sidebarShadowRadius = "2px 4px 4px"
  ,sidebarShadowColor = "rgb(204, 204, 204)"
  
  ,sidebarUserTextColor = "rgb(255,255,255)"
  
  ,sidebarSearchBackColor = "rgb(255,255,255)"
  ,sidebarSearchIconColor = "rgb(44,62,80)"
  ,sidebarSearchBorderColor = "rgb(255,255,255)"
  
  ,sidebarTabTextColor = "rgb(54, 54, 54)"
  ,sidebarTabTextSize = 14
  ,sidebarTabBorderStyle = "none"
  ,sidebarTabBorderColor = "rgb(204, 204, 204)"
  ,sidebarTabBorderWidth = 1
  
  ,sidebarTabBackColorSelected = lightred
  ,sidebarTabTextColorSelected = offwhite
  ,sidebarTabRadiusSelected = "10px"
  
  ,sidebarTabBackColorHover = darkred
  ,sidebarTabTextColorHover = offwhite
  ,sidebarTabBorderStyleHover = "none"
  ,sidebarTabBorderColorHover = "rgb(204, 204, 204)"
  ,sidebarTabBorderWidthHover = 1
  ,sidebarTabRadiusHover = "10px"
  
  ### boxes
  ,boxBackColor = "rgb(245,245,245)"
  ,boxBorderRadius = 0
  ,boxShadowSize = "0px 0px 0px"
  ,boxShadowColor = ""
  ,boxTitleSize = 19
  ,boxDefaultColor = "rgb(52,152,219)"
  ,boxPrimaryColor = "rgb(44,62,80)"
  ,boxInfoColor = "rgb(52,152,219)"
  ,boxSuccessColor = "rgb(24, 188, 156)"
  ,boxWarningColor = "rgb(243,156,18)"
  ,boxDangerColor = "rgb(231,76,60)"
  
  ,tabBoxTabColor = "rgb(44,62,80)"
  ,tabBoxTabTextSize = 14
  ,tabBoxTabTextColor = "rgb(24, 188, 156)"
  ,tabBoxTabTextColorSelected = "rgb(255, 255, 255)"
  ,tabBoxBackColor = "rgb(255,255,255)"
  ,tabBoxHighlightColor = "rgb(255,255,255)"
  ,tabBoxBorderRadius = 10
  
  ### inputs
  ,buttonBackColor = "rgb(212, 234, 247)"
  ,buttonTextColor = "rgb(26,49,87)"
  ,buttonBorderColor = "rgb(26,49,87)"
  ,buttonBorderRadius = 5
  
  ,buttonBackColorHover = "rgb(148, 202, 235)"
  ,buttonTextColorHover = "rgb(26,49,87)"
  ,buttonBorderColorHover = "rgb(26,49,87)"

  
  ,textboxBackColor = "rgb(255,255,255)"
  ,textboxBorderColor = "rgb(206,212,218)"
  ,textboxBorderRadius = 5
  ,textboxBackColorSelect = "rgb(204, 204, 204)"
  ,textboxBorderColorSelect = "rgb(89,126,162)"
  
  ### tables
  ,tableBackColor = "none"
  ,tableBorderColor = "none"
  ,tableBorderTopSize = 1
  ,tableBorderRowSize = 1
  
)


logo_sanipath <- shinyDashboardLogoDIY(
  
  boldText = "Typhoid"
  ,mainText = "Surveillance Dashboard"
  ,textSize = 12
  ,badgeText = ""
  ,badgeTextColor = "white"
  ,badgeTextSize = "1px"
  ,badgeBackColor = "rgb(119,46,37)"
  ,badgeBorderRadius = 0
  
)