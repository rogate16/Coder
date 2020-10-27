library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(rfm)
library(recommenderlab)
library(rsample)

# Read Data
data <- read.csv("data_input/amazon_reviews.csv")
data$reviewTime <- as.Date(data$reviewTime)

# Theme
theme_plain <- theme(plot.background = element_rect(fill = rgb(0.86,0.86,0.86)),
                     panel.background = element_rect(fill = rgb(0.86,0.86,0.86)))

# Item List
item.list <- data %>% select(title, price, category)
item.list <- unique(item.list)
rownames(item.list) <- c()
colnames(item.list)[1] <- c("item")

# Daily Transaction
daily <- data %>% select(title, reviewTime)
daily$reviewTime <- as.Date(daily$reviewTime)
daily$month <- months(daily$reviewTime)

# Create RFM Table
rfm <- data %>% select(reviewerName, reviewTime, price)
rfm %>% 
  group_by(reviewerName, reviewTime) %>% 
  summarise(sum(price)) -> rfm
rfm[order(rfm$reviewerName),] <- rfm
colnames(rfm)[3] <- "revenue"
rfm$reviewTime <- as.Date(rfm$reviewTime)
rfm <- rfm_table_order(rfm, reviewerName, reviewTime, revenue, as.Date("2019-01-01"))
rfm <- rfm$rfm

# RFM Segmentation
rfm_analysis <- rfm %>%
  mutate(frequency_monetary = (frequency_score+monetary_score)/2) %>% 
  select(customer_id, recency_score, frequency_monetary)

rfm_analysis$segment <- case_when(
  rfm_analysis$recency_score == 5 & rfm_analysis$frequency_monetary >= 3.5 & rfm_analysis$frequency_monetary <= 5 ~ "Champions",
  rfm_analysis$recency_score %in% c(3,4) & rfm_analysis$frequency_monetary >= 3.5 & rfm_analysis$frequency_monetary <= 5 ~ "Loyal Customer",
  rfm_analysis$recency_score %in% c(4,5) & rfm_analysis$frequency_monetary > 1.5 & rfm_analysis$frequency_monetary <= 3.5 ~ "Potential Loyalist",
  rfm_analysis$recency_score == 5 & rfm_analysis$frequency_monetary <= 1.5 ~ "New Customer",
  rfm_analysis$recency_score == 4 & rfm_analysis$frequency_monetary <= 1.5 ~ "Promising",
  rfm_analysis$recency_score == 3 & rfm_analysis$frequency_monetary > 2.5 & rfm_analysis$frequency_monetary <= 3~ "Need Attention",
  rfm_analysis$recency_score ==3 & rfm_analysis$frequency_monetary <= 2.5 ~ "About To Sleep",
  rfm_analysis$recency_score <= 2 & rfm_analysis$frequency_monetary >= 2.5 & rfm_analysis$frequency_monetary <= 4.5 ~ "At Risk",
  rfm_analysis$recency_score <= 2 & rfm_analysis$frequency_monetary > 4.5 & rfm_analysis$frequency_monetary <= 5~ "Can't Lose Them",
  rfm_analysis$recency_score == 2 & rfm_analysis$frequency_monetary <= 2~ "Hibernating",
  rfm_analysis$recency_score<2 & rfm_analysis$frequency_monetary <= 2~ "Lost")

rfm_analysis$segment <- as.factor(rfm_analysis$segment)

# Merging
merged <- merge(data, rfm_analysis, by.x = "reviewerName", by.y = "customer_id", all.x = T)

# Matrix
real <- readRDS("data_input/matrix")

# Theme
### creating custom theme object
re.Theme <- dashboardthemes::shinyDashboardThemeDIY(
  
  ### general
  appFontFamily = "Times New Roman"
  ,appFontColor = "rgb(0,0,0)"
  ,primaryFontColor = "rgb(0,0,0)"
  ,infoFontColor = "rgb(0,0,0)"
  ,successFontColor = "rgb(0,0,0)"
  ,warningFontColor = "rgb(0,0,0)"
  ,dangerFontColor = "rgb(0,0,0)"
  ,bodyBackColor = "rgb(220, 220, 220)"
  
  ### header
  ,logoBackColor = "rgb(23,103,124)"
  
  ,headerButtonBackColor = "rgb(238,238,238)"
  ,headerButtonIconColor = "rgb(75,75,75)"
  ,headerButtonBackColorHover = "rgb(210,210,210)"
  ,headerButtonIconColorHover = "rgb(0,0,0)"
  
  ,headerBackColor = "rgb(238,238,238)"
  ,headerBoxShadowColor = "#aaaaaa"
  ,headerBoxShadowSize = "2px 2px 2px"
  
  ### sidebar
  ,sidebarBackColor = dashboardthemes::cssGradientThreeColors(
    direction = "down"
    ,colorStart = "rgb(20,97,117)"
    ,colorMiddle = "rgb(56,161,187)"
    ,colorEnd = "rgb(3,22,56)"
    ,colorStartPos = 0
    ,colorMiddlePos = 50
    ,colorEndPos = 100
  )
  ,sidebarPadding = 0
  
  ,sidebarMenuBackColor = "transparent"
  ,sidebarMenuPadding = 0
  ,sidebarMenuBorderRadius = 0
  
  ,sidebarShadowRadius = "3px 5px 5px"
  ,sidebarShadowColor = "#aaaaaa"
  
  ,sidebarUserTextColor = "rgb(255,255,255)"
  
  ,sidebarSearchBackColor = "rgb(55,72,80)"
  ,sidebarSearchIconColor = "rgb(153,153,153)"
  ,sidebarSearchBorderColor = "rgb(55,72,80)"
  
  ,sidebarTabTextColor = "rgb(255,255,255)"
  ,sidebarTabTextSize = 13
  ,sidebarTabBorderStyle = "none none solid none"
  ,sidebarTabBorderColor = "rgb(35,106,135)"
  ,sidebarTabBorderWidth = 1
  
  ,sidebarTabBackColorSelected = dashboardthemes::cssGradientThreeColors(
    direction = "right"
    ,colorStart = "rgba(44,222,235,1)"
    ,colorMiddle = "rgba(44,222,235,1)"
    ,colorEnd = "rgba(0,255,213,1)"
    ,colorStartPos = 0
    ,colorMiddlePos = 30
    ,colorEndPos = 100
  )
  ,sidebarTabTextColorSelected = "rgb(0,0,0)"
  ,sidebarTabRadiusSelected = "0px 20px 20px 0px"
  
  ,sidebarTabBackColorHover = dashboardthemes::cssGradientThreeColors(
    direction = "right"
    ,colorStart = "rgba(220,220,220,1)"
    ,colorMiddle = "rgba(220,220,220,1)"
    ,colorEnd = "rgba(0,255,213,1)"
    ,colorStartPos = 0
    ,colorMiddlePos = 30
    ,colorEndPos = 100
  )
  ,sidebarTabTextColorHover = "rgb(50,50,50)"
  ,sidebarTabBorderStyleHover = "none none solid none"
  ,sidebarTabBorderColorHover = "rgb(75,126,151)"
  ,sidebarTabBorderWidthHover = 1
  ,sidebarTabRadiusHover = "0px 20px 20px 0px"
  
  ### boxes
  ,boxBackColor = "rgb(220,220,220)"
  ,boxBorderRadius = 5
  ,boxShadowSize = "0px 1px 1px"
  ,boxShadowColor = "rgba(0,0,0,.1)"
  ,boxTitleSize = 16
  ,boxDefaultColor = "rgb(210,214,220)"
  ,boxPrimaryColor = "rgba(44,222,235,1)"
  ,boxInfoColor = "rgb(210,214,220)"
  ,boxSuccessColor = "rgba(0,255,213,1)"
  ,boxWarningColor = "rgb(244,156,104)"
  ,boxDangerColor = "rgb(255,88,55)"
  
  ,tabBoxTabColor = "rgb(255,255,255)"
  ,tabBoxTabTextSize = 14
  ,tabBoxTabTextColor = "rgb(0,0,0)"
  ,tabBoxTabTextColorSelected = "rgb(0,0,0)"
  ,tabBoxBackColor = "rgb(255,255,255)"
  ,tabBoxHighlightColor = "rgba(44,222,235,1)"
  ,tabBoxBorderRadius = 5
  
  ### inputs
  ,buttonBackColor = "rgb(245,245,245)"
  ,buttonTextColor = "rgb(0,0,0)"
  ,buttonBorderColor = "rgb(200,200,200)"
  ,buttonBorderRadius = 5
  
  ,buttonBackColorHover = "rgb(235,235,235)"
  ,buttonTextColorHover = "rgb(100,100,100)"
  ,buttonBorderColorHover = "rgb(200,200,200)"
  
  ,textboxBackColor = "rgb(255,255,255)"
  ,textboxBorderColor = "rgb(200,200,200)"
  ,textboxBorderRadius = 5
  ,textboxBackColorSelect = "rgb(245,245,245)"
  ,textboxBorderColorSelect = "rgb(200,200,200)"
  
  ### tables
  ,tableBackColor = "rgb(255,255,255)"
  ,tableBorderColor = "rgb(240,240,240)"
  ,tableBorderTopSize = 1
  ,tableBorderRowSize = 1
  
)