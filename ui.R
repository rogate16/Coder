library(shiny)
library(shinydashboard)
library(shinyWidgets) # Pretty Button
library(shinycssloaders) # ML Loading
library(slickR) # Image Slider
library(shinyBS) # Tooltip
library(shinyalert) # Modal

header <- dashboardHeader(title = dashboardthemes::shinyDashboardLogo(
  theme = "blue_gradient",
  boldText = "Coder",
  mainText = NULL,
  badgeText = "v1.0"
))

sidebar <- dashboardSidebar(
  tags$script(JS("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'hidden';")),
  sidebarMenu(
    menuItem(
     "Home", tabName = "h", icon = icon("home")
    ),
    
    menuItem(
      "Explore", tabName = "e", icon = icon("poll")
    ),
    
    menuItem(
      "Customer Segmentation", tabName = "cs", icon = icon("address-card")
    ),
    
    menuItem(
      "Personalized Recommendations", tabName = "pr", icon = icon("cart-plus")
    ),
    
    menuItem(
      "Read More", tabName = "rm", icon = icon("book")
    ),
    
    menuItem(
      "About", tabName = "a", icon = icon("bell")
    ),
    
    menuItem(
      "Data", tabName = "d", icon = icon("folder")
    )
  )
)

body <- dashboardBody(
  re.Theme, useShinyalert(),
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ), tags$head(tags$style(HTML("{color: red;
                                 font-size: 20px;
                                 font-style: italic;
                                 }"))),
  tabItems(
    tabItem("h",
            fluidRow(box(HTML('<left><img src="logo.png" width="500px"></left>'), width = 8),
            box(h3("Coder is a dashboard for customer analysis within online marketplace. This dashboard also
                   contains a recommender engine built with the advanced collaborative filtering algorithm",
                   style="color: black"),
                width = 4)),
            fluidRow(box(HTML("<h3>Explore</h3><hr>Explore the dataset used in this project. From top users, number of items, 
                     data distribution, etc."), width = 3, height = "250px", background = "green"),
            box(HTML("<h3>Customer Segmentation</h3><hr>Understand the behavior of the customers easily with the
                     use of RFM Analysis and Segmentation."), width = 3, height = "250px", background = "light-blue"),
            box(HTML("<h3>Personalized Recommendation</h3><hr>Pick a user and see which items are closely related
                     to their interest, predicted using an advanced machine learning algorithm."), width = 3, 
                     height = "250px", background="blue"),
            box(HTML("<h3>Read More</h3><hr>Curious about how recommender system works and the algorithm used 
                     in this project? You can see more here."), width = 3, height = "250px", background = "olive"))
            ),
    # box color : red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
    
    tabItem("e",
      
      valueBoxOutput("val1"),
      valueBoxOutput("val2"),
      valueBoxOutput("val3"),
      valueBoxOutput("val4"),
      valueBoxOutput("val5"),
      valueBoxOutput("val6"),
      fluidRow(box(
        HTML("<h4><b>Top Users</b></h4>"),
        dropdownButton(inputId = "set1",
          radioButtons("rad1", "Metrics", choiceValues = c("freq", "amt"), 
                       choiceNames = c("Frequency", "Amount"), inline = T),
          sliderInput("slide1", "Number of Users", 5, 30, 20),
          circle = TRUE, status = "danger", icon = icon("gear")
        ),
        bsTooltip("set1", "Plot Settings"),
        plotlyOutput("p1")),
      box(
        HTML("<h4><b>Top Items</b></h4>"),
        dropdownButton(inputId = "set2",
          radioButtons("rad2", "Metrics", choiceValues = c("freq", "amt"), 
                       choiceNames = c("Frequency", "Amount"), inline = T),
          sliderInput("slide2", "Number of Items", 5, 30, 20),
          circle = TRUE, status = "danger", icon = icon("gear")
        ),
        bsTooltip("set2", "Plot Settings"),
        plotlyOutput("p2"))),
      
      fluidRow(
        box(
          HTML("<h4><b>Data Distribution</b></h4>"),
          radioButtons("dist", NULL, choiceValues = c("i", "t", "r"), 
                       choiceNames = c("Items", "Transactions", "Ratings"), inline = T),
          plotlyOutput("p3"), height = "545px"),
          
        box(  
          HTML("<h4><b>Time Series</b></h4>"),
          radioButtons("ts", NULL, choiceValues = c("day", "month"), 
                       choiceNames = c("Daily", "Monthly"), inline = T),
          uiOutput("select"),
          plotlyOutput("p4"), height = "545px")
          )
    ),
    
    tabItem("cs", 
      HTML("<h3><b>RFM Analysis</b></h3>"),
      HTML("RFM Analysis is a method used to divide customers into different segments based on their recency, 
           frequency, and monetary score. Recency Score is calculated using the latest transaction date of a 
           customer. The more recent the transaction, the higher the recency score. Frequency is the number of 
           transactions a customer has done, and monetary is the total amount they spent on those transactions.
           <br><br>
           After acquiring these values, we can segment the customers by their recency score and average of 
           frequency and monetary score, and then treat each customer based on which segment or cluster they are in."),
      br(), br(),
      box(prettySwitch("sample", "Show Samples", F, "success", T), width = 3, height = "40px", 
          background = "teal"),
      uiOutput("resample"), br(),
      bsTooltip("sample", "Draw random samples from the data"),
      br(), br(), plotlyOutput("pc1") %>% withSpinner(type = 8),
      HTML("<br><br>Based on the RFM analysis above, we now have 11 segments of customer, in which each should be 
           given different approach based on their attributes (RFM Score). Here are the explanation of each segment and
           some recommendations on how should we treat them<br><br>"),
      DTOutput("dt1"),
      HTML("<br><br>Here is the data of all customers and their attributes<br><br>"),
      selectInput("sel_seg", "Choose Segment", unique(rfm_analysis$segment), "Champions"),
      DTOutput("dt2")
    ),
    
    tabItem("pr",
      fluidRow(
        box(box(HTML("<b>Pick a User</b>"), background = "green", width = 12),
            selectInput("sel1", "Segment", unique(merged$segment), "Champions"), 
            uiOutput("ui1"), width = 4, DTOutput("info")),
        box(box(HTML("<b>Transaction History</b><br><br>"), width = 12, height = "42px", background = "green"),
                   DTOutput("dth"), width = 8, solidHeader = T)),
      br(), 
      box(HTML("<center>What items would he/she like?</center>"), width = 12, background = "light-blue"),
      fluidRow(  
        box(numericInput("num1", "Number of Items to Recommend", 20, 5, 100),
            radioButtons("method", "Algorithm", choiceValues = c("libmf", "ubcf"), 
                         choiceNames = c("Matrix Factorization CF", "User-Based CF")),
            uiOutput("opts"), HTML("<br><br><br><br>"),
            actionButton("apply","Predict"), br(), br(), br(), uiOutput("note"), 
            width = 4),
        box(HTML("<b>Recommended Products</b><br><br>"), solidHeader = T,
                   DTOutput("dtp") %>% withSpinner(color = "#87ceeb", type = 8), width = 8))
    ),
    
    tabItem("d",
      fluidPage(box(DTOutput("full_dt"), width = 12))),
    
    tabItem("rm",
            fluidPage(HTML("<center><h3>
                    <b>Wanna know more about Recommender System?</b>
                     </h3></center>"),
                      box(solidHeader = T,
                          HTML("Recommender System is a system or a machine that produces recommendations for users based on the 
          interactions between users and items. It is one of the most common application of machine learning. 
          Almost all companies/platforms that has users and items is using recommender system. But how does it work?<br><br>"),
                          HTML("The most famous and most effective algorithm is called Collaborative Filtering. It calculates the 
          similarities between users and items and then recommend the items which have a high relationship with the
          user. For example, let’s say we have an online marketplace. We have users defined by username, items defined 
          by products, and values defined by the ratings given by the users. Using these features, 
          we can calculate the similarities between each users to find out the products that might suit their interest.<br><br><br>"),
                          height = "350px"),
                      box(img(src="cf_anim.gif", width="450px", height="300px"), solidHeader = T,
                          HTML("<br><br><br><br><br>"), height="350px"),
                      box(HTML("If used appropriately, such as having a good accuracy and variation, a recommendation system can boost 
          sales and user’s satisfaction. This will result in increasing loyalty and reducing the likeliness of churning. 
          According to a study by
          <a href=https://www.mckinsey.com/industries/retail/our-insights/how-retailers-can-keep-up-with-consumers>McKinsey</a>, 
          75% of what the consumers watch on Netflix comes from the recommendation
          system. Netflix executives Carlos A. Gomez-Uribe and Neil Hunt also claim that their recommender system saves
          them about $1 billion per year. This research from 
          <a href=https://research.aimultiple.com/recommendation-system/>AI Multiple</a> also stated that 
          Amazon credited their recommendation system for 35% of their revenue.
          <br><br><br>"),
                          width = 12, solidHeader = T),
                      br(), br(), br(), br(),
                      box(HTML("There are many recommendation system algorithms that have 
          emerged in recent years. After fitting the models several times, we can compare 8 different models based 
          on core attributes such as speed, reliability, and accuracy.<br><br><br>"), width = 12, solidHeader = T),
                      br(), br(), br(), br(), br(), br(), br(),
                      img(src="comparison.png", width="1000px", height="500px"),
                      DTOutput("dt_algo"),
                      box(width = 12, solidHeader = T, 
                          HTML("Note :<br>1. Random Recommender has a low error because it's predicting discrete
                    value, instead of continuous like Matrix Factorization or UBCF<br>2. As good as it
                    is, UBCF model has a limitation on minimum transaction a user has to collaborate with
                    since it's using a KNN Basic as an algorithm (minimum number of neighbors)<br>3. 
                    Even though we didn't apply the Popular algorithm, some Popular items are often used to
                    fill the recommendation page."))
            )
    ),
  
    tabItem("a",
        HTML("<h3><b>Background</b></h3>"), br(),
        HTML("A lot of implementations of recommender system nowadays are way too simple, such as 
          recommending Popular items only or even randomly chosen items. Considering the huge increase in the number of
          online actions during the modern era, using a model that is too simple will result in a bunch of inappropriate
          recommendations. This is proven by the complaints from a lot of customers on e-commerce or social media about
          how terrible the recommendations they received."),
        br(), br(), br(), br(),
        slickROutput("img_list", width = "85%", height = "50%"), br(), br(),
        HTML("<h3><b>Data</b></h3>"), br(),
        HTML("The dataset was collected from the <a href=https://nijianmo.github.io/amazon/index.html> open source 
             Amazon reviews</a> dataset, made available by Jianmo Ni. It originally has full reviews from 
             May 1996 to Oct 2018, but for this project I fetched the transactions from 2018 only."),
        br(), br(), br(), br(), br(), 
        HTML("<h3><b>Author</b></h3>"),
        HTML("<h5>Find me here</h5>
             <br>
             <a href=https://www.linkedin.com/in/asido-rogate-3493a01a4/>
             <img src=linkedin.png width=200px height=100px></img></a>
             <a href=https://www.github.com/rogate16/>
             <img src=github.png width=225px height=100px></img></a>"),
        br(), br(), br(), br(),
        HTML("<h3><b>References</b></h3>"),
        HTML("<a href=https://www.putler.com/rfm-analysis/>1. RFM Analysis</a>"),br(),
        HTML("<a href=https://community.spotify.com/t5/iOS-iPhone-iPad/Why-are-recommendations-so-terrible/td-p/4769866>2. Spotify Recommendations</a>"), br(),
        HTML("<a href=https://rpubs.com/tarashnot/recommender_comparison>3. Model Comparison</a>"),
        br(), br(), br(), br(), br(), br(), br()
      )
  )
)

dashboardPage(header, sidebar, body, "Coder")