server <- function(input, output){
  
  output$img_list <- renderSlickR({
    
    slickR(c("www/comp1.png", "www/comp2.png", "www/comp4.png"))
    
  })
  
  output$dt_algo <- renderDT({
    
    eval.algo <- read.csv("data_input/eval_algo.csv")
    
    datatable(eval.algo, options=list(lengthChange=F, dom="t", ordering=F),
              colnames = c("Algorithm", "Explanation", "Total Score", "Speed", "Reliability", "Error"))
    
  })
  
  output$val1 <- renderValueBox({
    
    t.user <- length(unique(data$reviewerName))
    valueBox(t.user, "Users", icon("user"),"blue")
    
  })
  
  output$val2 <- renderValueBox({
    
    t.total <- nrow(data)
    valueBox(t.total, "Transactions", icon("envelope-open-text"), "blue")
    
  })
  
  output$val3 <- renderValueBox({
    
    t.item <- length(unique(data$title))
    valueBox(t.item, "Items", icon("book"), "blue")
    
  })
  
  output$val4 <- renderValueBox({
    
    a.rating <- round(mean(data$overall),2)
    valueBox(a.rating, "Average Ratings", icon("star"), "green")
    
  })
  
  output$val5 <- renderValueBox({
    
    valueBox(9, "months of transactions", icon("calendar-alt"), "green")
    
  })
  
  output$val6 <- renderValueBox({
    
    t.cat <- length(unique(data$category))
    valueBox(t.cat, "Categories", icon("database"), "green")
    
  })
  
  output$p1 <- renderPlotly({
    
    users <- data %>% 
      group_by(reviewerName) %>% 
      summarise(frequency = length(price), amount = sum(price))
    
    if(input$rad1=="freq"){
      users <- users[order(users$frequency, decreasing = T),]
      up <- ggplot(head(users,input$slide1), aes(reorder(reviewerName,frequency), frequency,
                                       text = paste(reviewerName, "<br>Frequency : ", frequency))) + 
        geom_col(fill="skyblue") + coord_flip() +
      theme_minimal() + theme_plain +
      labs(title=paste("Top",input$slide1, "Users"), x = "Username", y = "Total Transactions")
      ggplotly(up, tooltip="text")
    } else {
      users <- users[order(users$amount, decreasing = T),]
      up <- ggplot(head(users,input$slide1), aes(reorder(reviewerName,amount), amount, 
                                       text = paste(reviewerName, "<br>Total Amount : $", amount, sep = ""))) + 
        geom_col(fill="skyblue") + coord_flip() + 
        theme_minimal() + theme_plain +
        labs(title=paste("Top",input$slide1, "Users"), x = "Username", y = "Total Amount")
      ggplotly(up, tooltip="text")
    }
    
  })
  
  output$p2 <- renderPlotly({
    
    data %>% 
      group_by(title) %>% 
      mutate(total = sum(price), frequency = length(price)) %>% 
      select(title, frequency, total, category) -> items
    
    unique(items) -> items
    
    if(input$rad2=="freq"){
      items <- items[order(items$frequency, decreasing = T),]
      ip <- ggplot(head(items,input$slide2), aes(reorder(title,frequency), frequency, 
                                        text = paste(title,"<br>Category :", category,
                                                     "<br>Frequency :",frequency))) + 
        geom_col(fill="skyblue") + coord_flip() + scale_y_discrete(label = function(x) stringr::str_trunc(x, 18)) +
        theme_minimal() + scale_x_discrete(labels = rev(c(1:input$slide2))) + theme_plain +
        labs(title=paste("Top",input$slide2, "Items"), x = "Rank", y = "Total Transactions")
      
      ggplotly(ip, tooltip="text")
    
    } else {
      items <- items[order(items$total, decreasing = T),]
      ip <- ggplot(head(items,input$slide2), aes(reorder(title,total), total, 
                                                 text = paste(title,"<br>Category : ", category,
                                                       "<br>Total Amount : $",total, sep = ""))) + 
        geom_col(fill="skyblue") + coord_flip() + scale_y_discrete(label = function(x) stringr::str_trunc(x, 18)) +
        theme_minimal() + scale_x_discrete(labels = rev(c(1:input$slide2))) + theme_plain +
        labs(title=paste("Top",input$slide2, "Items"), x = "Rank", y = "Total Amount")
      
      ggplotly(ip, tooltip="text")
      
    }
    
  })
  
  output$p3 <- renderPlotly({
    
    if(input$dist=="i"){
      data.item <- data[!duplicated(data$title),]
      item.dist <- as.data.frame(table(data.item$category))
      item.dist <- item.dist[order(item.dist$Freq, decreasing = T),]
      item.dist$prop <- round(item.dist$Freq/sum(item.dist$Freq),4)*100
      
      p3 <- ggplot(item.dist, aes(reorder(Var1, Freq), Freq, 
                                  text = paste(Freq, " Items<br>(",prop,"%)", sep=""))) + 
        geom_col(fill="skyblue") + coord_flip() + theme_minimal() + theme_plain +
        labs(title="Items in Categories", x="Categories", y="Total Items")
      
      ggplotly(p3, tooltip="text") 
    } else if(input$dist=="t"){
      trans.dist <- as.data.frame(table(data$category))
      trans.dist <- trans.dist[order(trans.dist$Freq, decreasing = T),]
      trans.dist$prop <- round(trans.dist$Freq/sum(trans.dist$Freq),4)*100
      
      p4 <- ggplot(trans.dist, aes(reorder(Var1, Freq), Freq, 
                                   text = paste(Freq, " Transactions<br>(",prop,"%)", sep=""))) + 
        geom_col(fill="skyblue") + coord_flip() + theme_minimal() + theme_plain +
        labs(title="Transactions in Categories", x="Categories", y="Total Transactions")
      
      ggplotly(p4, tooltip="text")
    } else if(input$dist=="r"){
      df.rating <- as.data.frame(table(data$overall))
      df.rating$prop <- round(df.rating$Freq/sum(df.rating$Freq),4)*100
      p_rate <- ggplot(df.rating, aes(Var1, Freq,
                                      text = paste("Total :", Freq, " Transactions<br>    (", prop,"%)", sep=""))) + 
        geom_col(fill="skyblue") + theme_minimal() +  theme_plain +
        labs(title="Ratings Distribution", x="Rating", y="Frequency")
      
      ggplotly(p_rate, tooltip="text")
      
    }
  })
  
  output$select <- renderUI({
    
    if(input$ts=="day"){
      
      selectInput("sel_month", "Choose Month", 
                  c("January", "February", "March", "April", "May", 
                    "June", "July", "August", "September"))
    }
    
  })
  
  output$p4 <- renderPlotly({
    
    if(input$ts=="day"){
      df <- daily %>% filter(month==input$sel_month)
      gb <- df %>% group_by(reviewTime) %>% summarise(total = length(reviewTime))
      p4 <- ggplot(gb, aes(reviewTime, total, text=paste("Date :",gb$reviewTime,"<br>Total Transactions :", gb$total),
                           group=1)) + 
        geom_line(color="skyblue") + 
        theme_minimal() + theme_plain +
        labs(title=paste("Daily Transaction in", input$sel_month))
      
      ggplotly(p4, tooltip = "text")
    } else {
      revTime <- data %>% select(reviewerName, reviewTime)
      revTime$month <- lubridate::month(revTime$reviewTime)
      dtime <- as.data.frame(table(revTime$month))
      dtime$Var1 <- as.numeric(dtime$Var1)
      p4 <- ggplot(dtime, aes(Var1, Freq)) + geom_line(color="skyblue", size=1.5) + 
        scale_x_continuous(breaks = seq(0, 12, by = 1)) + theme_minimal() + theme_plain +
         labs(title="Transactions per Month", x="Month", y="Total Transactions")
      
      ggplotly(p4, tooltip="y")
    }
    
  })
  
  output$resample <- renderUI({
    
    if(input$sample==T){
      
      actionButton("resample", "Resample")
      
    }
    
  })
  
  output$dt1 <- renderDT({
    
    cs.table <- read.csv("data_input/rfm.csv")
    colnames(cs.table)
    datatable(cs.table, options = list(pageLength=11, lengthChange=F, dom="t", ordering=F),
              colnames = c("Customer Segment", "Activity", "Number of Items To Recommend", "What To Do"))
    
  })
  
  output$dt2 <- renderDT({
    
    datatable(rfm_analysis %>% filter(segment==input$sel_seg), options = list(lengthChange=F),
              colnames = c("Customer ID", "Recency Score", "Frequency and Monetary Score", "Segment"))
    
  })
  
  seeds <- eventReactive(input$resample, {
    set.seed(Sys.time())
    d.dist <- rfm_analysis
    
    d.dist$frequency <- rfm$transaction_count
    d.dist$monetary <- rfm$amount
    d.dist$label <- paste("User :",d.dist$customer_id,"<br>Frequency :", d.dist$frequency,
                          "<br>Monetary :", d.dist$monetary)
    
    splitted <- initial_split(d.dist, 0.00575, "segment")
    samples <- training(splitted)
    
    pc <- ggplot(samples, aes(recency_score, frequency_monetary, color=segment,
                              text = label)) + 
      geom_jitter() +
      theme_minimal() +
      geom_text(data = df, aes(x,y, label=label), color="black", size=3, fontface = "bold") + 
      theme_plain + labs(title="Customer Segmentation", x="Recency Score", y="Frequency and Monetary Score") +
      theme(legend.position = "none")
    
    ggplotly(pc, tooltip = "text")
  })
  
  output$pc1 <- renderPlotly({
    prop <- as.data.frame(prop.table(table(rfm_analysis$segment)))
    prop$Freq <- round(prop$Freq, 4)*100
    label <- paste(prop$Var1, " (", prop$Freq, "%)", sep = "")
    
    x <- c(3, 1.5, 1.5, 5, 2, 1, 3.5, 3, 4, 4.5, 5)
    y <- c(1.5, 3.5, 5, 4.5, 1.5, 1.5, 4.5, 3, 1.5, 2.5, 1.5)
    df <- data.frame(x, y, label)
    
    if(input$sample==F){
      
      pc <- ggplot(rfm_analysis, aes(recency_score, frequency_monetary)) + 
        geom_tile(aes(fill=segment)) + theme_minimal() +
        scale_fill_manual(values = c("paleturquoise4", "darkorange3", "brown1", "skyblue3", "grey28",
                                     "black", "darkgray", "goldenrod", "lightgoldenrod3", "green4", 
                                     "darkgoldenrod")) +
        geom_text(data = df, aes(x,y, label=label), color="white", size=3, family="sans", fontface = "bold") + 
        theme_plain + labs(title="Customer Segmentation", x="Recency Score", y="Frequency and Monetary Score") +
        theme(legend.position = "none")
      
      pc <- ggplotly(pc, tooltip = F)
      
      pc
      
    } else if(input$sample==T){
      
      d.dist <- rfm_analysis
      
      d.dist$frequency <- rfm$transaction_count
      d.dist$monetary <- rfm$amount
      d.dist$label <- paste("User :",d.dist$customer_id,"<br>Frequency :", d.dist$frequency,
                            "<br>Monetary :", d.dist$monetary)
      
      if(input$resample==0){
        splitted <- initial_split(d.dist, 0.00575, "segment")
        samples <- training(splitted)
        samples$segment <- factor(samples$segment, 
                                  levels =  c("About To Sleep", "At Risk", "Can't Lose Them", "Champions", 
                                              "Hibernating", "Lost", "Loyal Customer", "Need Attention", 
                                              "New Customer", "Potential Loyalist", "Promising"))
      } else {
        set.seed(Sys.time())
        splitted <- initial_split(d.dist, 0.00575, "segment")
        samples <- training(splitted)
        samples$segment <- factor(samples$segment, 
                                  levels =  c("About To Sleep", "At Risk", "Can't Lose Them", "Champions", 
                                              "Hibernating", "Lost", "Loyal Customer", "Need Attention", 
                                              "New Customer", "Potential Loyalist", "Promising"))
      }
      
      pc <- ggplot(samples, aes(recency_score, frequency_monetary, color=segment,
                                text = label)) + 
        geom_jitter() +
        theme_minimal() +
        geom_text(data = df, aes(x,y, label=prop$Var1), color="black", size=3, fontface = "bold") + 
        scale_color_manual(breaks = c("About To Sleep", "At Risk", "Can't Lose Them", "Champions", 
                                      "Hibernating", "Lost", "Loyal Customer", "Need Attention", 
                                      "New Customer", "Potential Loyalist", "Promising"), 
                           values = c("About To Sleep" = "paleturquoise4", "At Risk" = "darkorange3", 
                                      "Can't Lose Them" = "brown1", "Champions" = "skyblue3",
                                      "Hibernating" = "grey28", "Lost" = "black", "Loyal Customer" = "darkgray",
                                      "Need Attention" = "goldenrod", "New Customer" = "lightgoldenrod3",
                                      "Potential Loyalist" = "green4", "Promising" = "darkgoldenrod")) +
        theme_plain + labs(title="Customer Segmentation", x="Recency Score", y="Frequency and Monetary Score") +
        theme(legend.position = "none")
      
      ggplotly(pc, tooltip = "text")

    }
    
  })
  
  output$ui1 <- renderUI({
    
    seg.choose <- input$sel1
    data.ui <- merged %>% filter(segment==seg.choose)
    
    selectizeInput("sel2", label = "Customer Name", choices = unique(data.ui$reviewerName),
                options=list(maxOptions = 2500))
    
  })
  
  output$info <- renderDT({
    
    user.data <- rfm %>% filter(customer_id %in% input$sel2)
    data.history <- merged %>% filter(segment %in% input$sel1) %>% filter(reviewerName %in% input$sel2)
    fav.cat <- as.data.frame(table(data.history$category))
    fav.cat <- fav.cat[order(fav.cat$Freq, decreasing = T),]
    info <- c(paste(user.data$customer_id), 
              paste(user.data$recency_score, "-", user.data$frequency_score, "-", user.data$monetary_score),
              paste(nrow(data.history)),
              paste("$", sum(data.history$price), sep=""),
              paste(fav.cat$Var1[1], " (", fav.cat$Freq[1], " transactions)", sep = ""))
    
    df.user <- data.frame(attr = 
                            c("Username", "R-F-M Score", "Total Transactions
                              ", "Total Spendings", "Favorite Category"))
    df.user$info <- info
    rownames(df.user) <- c()
    colnames(df.user) <- c()
    datatable(df.user, options = list(dom="t", lengthChange=F, ordering=F))
    
  })
  
  output$dth <- renderDT({
    
    data.history <- merged %>% filter(segment %in% input$sel1) %>% filter(reviewerName %in% input$sel2)
    datatable(data.history %>% select(title, category, price), 
              colnames = c("Item Name", "Category", "Price"),
              options=list(autoWidth=T, scrollX=T, pageLength=5, lengthChange=F, sDom  = '<"top">lrt<"bottom">ip'))
    
  })
  
  output$opts <- renderUI({
    
    if(input$method=="libmf"){
      box(
        HTML("Allow dynamic output?"),
        materialSwitch(
          inputId = "allow",
          label = NULL, 
          value = F,
          status = "success"
      ), width = 12, background = "light-blue")
    }
  })
  
  recommend.data <- eventReactive(input$apply,{
    if(input$method=="libmf"){
      set.seed(16)
      model <- Recommender(real, "LIBMF")
    } else {
      model <- Recommender(real, "UBCF")
    }
    
    if(input$apply==0){
      return(datatable(data.frame(Item = c("No data available"))))
    } else if (input$allow==F){
      set.seed(16)
      p <- predict(model, real[input$sel2], type="ratings")
      p <- as(p, "data.frame")
      p <- p[order(p$rating, decreasing = T),]
      p <- p[p$rating>4,]
      p$rating <- round(p$rating,4)
      
      p$item <- sub(".","",p$item)
      
      p <- p %>% left_join(item.list, by="item")
      p <- p %>% select(user, item, rating, category, price)
      
      rownames(p) <- c()
      return (datatable(head(p, input$num1),
                        colnames = c("User", "Recommended Item", "Predicted Rating", "Category", "Price"),
                        options=list(scrollX=T, pageLength=5, lengthChange=F)))
    } else if (input$allow==T){
      shinyalert(title = "Dynamic Output Is Activated", 
                 text = "The output will be different in each iteration.", 
                 type = "info")
      set.seed(as.integer(Sys.time()))
      p <- predict(model, real[input$sel2], type="ratings")
      p <- as(p, "data.frame")
      p <- p[order(p$rating, decreasing = T),]
      p <- p[p$rating>4,]
      p$rating <- round(p$rating,4)
      
      p$item <- sub(".","",p$item)
      
      p <- p %>% left_join(item.list, by="item")
      p <- p %>% select(user, item, rating, category, price)
      
      rownames(p) <- c()
      return (datatable(head(p, input$num1),
                        colnames = c("User", "Recommended Item", "Predicted Rating", "Category", "Price"),
                        options=list(scrollX=T, pageLength=5, lengthChange=F)))
    }

  })
  
  output$dtp <- renderDT({
    
    recommend.data()
    
  })
  
  notes <- eventReactive(input$apply, {
    
    if(input$apply==0){
      # do nothing
    } else {
      box(HTML("<h3>How to determine good prediction?</h3><br>The highest rating on the data is 5,
               so when the predicted rating is above 5, the model is very confident in 
               recommending the item. The confidence level for each item is different, which helps us to 
               choose which item to recommend first "), background = "green", width=12)
    }
    
  })
  
  output$note <- renderUI({
    
    notes()
    
  })
  
  output$full_dt <- renderDT({
    
    data <- data[order(data$reviewTime),]
    rownames(data) <- 1:nrow(data)
    data
    
    datatable(data, filter = "top", options = list(autoWidth=T),
              colnames = c("Username", "Item Name", "Price", "Rating", "Date", "Category"))
    
  })
  
}