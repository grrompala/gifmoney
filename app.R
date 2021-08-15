#######################
library(shinyMobile)
library(shiny)
library(rtweet)
library(tidytext)
library(dplyr)
library(ggplot2)
library(giphyr)
library(RPostgreSQL)
library(DBI)
library(dbplyr)
library(htmlwidgets)
library(DT)
library(plotly)
library(stringr)
#######################

# Token for twitter API -- stored as object named "twitter_token"

source("www/twitter_token.R")

#twitter_token <- create_token(
#  app = "shiny.tweets",
#  consumer_key = "xxxxxxxxxxxxxxxxxxxxxx",
#  consumer_secret = "xxxxxxxxxxxxxxxxxxxxxxxxxxx",
#  set_renv = TRUE,
#  access_token="xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx",
#  access_secret="xxxxxxxxxxxxxxxxxxxxxxxxxx")
  
######## USER INTERFACE

ui <- f7Page(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styling.css")),
  
  options=list(dark=TRUE),
     
      f7TabLayout(
          
          navbar=f7Navbar(
            title=p("Gif Money",style="font-size:30px"),
            leftPanel = TRUE),
          
          panels=tagList(
            f7Panel(side="left",theme="dark",effect="reveal")),
        
          f7Tabs(
          swipeable = T,animated = F,
          id="tabs",
          
## Log-In Tab
            f7Tab(tabName = "Log-In",
                  icon=f7Icon("signature"),
                  active=T,
                  uiOutput("title_giphy"),
                  f7Accordion(f7AccordionItem(title = "Sign in",f7Password(inputId = "password",label="Enter password here"),
                  f7Button(inputId = "passaction",label="Press to Log-in",color = "red")))
                  ),
               
# Transaction Tab
            f7Tab(tabName="Purchases",
                  
                  tags$head(
                  tags$script(async = NA, src = "https://platform.twitter.com/widgets.js")),
                  
                  icon=f7Icon("creditcard"),
                  
                  f7Accordion(
                    f7AccordionItem(title="Click to Enter Purchase Details",
                                    
                                    f7Text(inputId="Purchase",label = "Purchase"),
                                    
                                    f7Text(inputId="Price",label="Price"),
                                    
                                    f7Text(inputId="Quantity",label="Quantity"),
                                    
                                    f7Select(inputId="Category",label="Category",
                                             choices=c("grocery", "dining-out", 
                                                       "takeout", "rent", "subscriptions",
                                                       "other","travel","loans","recreation",
                                                       "beer","pet","sales_tax")),
            
                                    f7Button(color = "red",inputId ="confirmPurchase",
                                             label="Submit to Ledger")
                                    )
                    ),
                br(),
                
                uiOutput("giphy_spending"),
                
                f7Popup(id = "PopConfirmPurchase",
                        f7Text(inputId = "Confirmation",label = "","Confirmed."),
                
                        uiOutput("giphy"),
                
                        uiOutput("tweet")
                        )
                ),

# Ledger Tab
          f7Tab(tabName="Ledger",
                
                icon=f7Icon("book"),
                
                active=T,
                
                actionButton(width='100px',inputId="UpdateDB",
                             label="Update Ledger",icon=icon("save")
                             ),
                
                actionButton(width='100px',inputId="DELETE",
                             label="Delete Selected?",icon=icon("trash")
                             ),
                
                DT::dataTableOutput("LEDGER"),
                
                tags$style(HTML(".dataTables_wrapper .dataTables_length {
                  float: right;}
                  .dataTables_wrapper .dataTables_filter {
                  float: left;
                  text-align: left;}"
                                )
                           )
                ),

# Analysis Tab
          f7Tab(tabName = "Analysis",
                
                icon=f7Icon("graph_square"),
                
                active=T,
                
                f7Row(
                  f7Col(f7DatePicker("DateStart","From",value = "2021-01-01")),
                  
                  f7Col(f7DatePicker("DateEnd","To",value=Sys.Date()+1),gap=FALSE)
                  ),
                
                plotOutput("Categories"),
                
                f7Text(inputId = "scatterfilter",
                       label = "Filter by"
                      ),
                
                plotlyOutput("scatterall")
                ),
                
# Gift Card Tab             
          f7Tab(tabName="Gift Cards",
                
                icon = f7Icon("gift"),
                
                active=T,
                
                f7SmartSelect("GC_Select",
                              "Choose gift card to update",
                              choices="",
                              selected=""),
                
                f7Text("GC_used",label = "Credit used"),
                
                f7Button("update_GC_balance","Submit",color="red"),
                
                uiOutput("GIFTCARD")
                )
        ) # closes f7Tabs
)) # closes f7Tab Layout and f7Page


########## SERVER SIDE


server <- function(input,output,session){
  
# set reactive database tables
  
  ledger <- reactiveValues()  # all purchases
  
  gc <- reactiveValues() # all giftcard balances
  
  writeTable <- reactive({dbWriteTable(conn=dbConnect(
    PostgreSQL(), 
    host = "thriftysmackers.ckbxdrgolj49.us-east-2.rds.amazonaws.com",
    port=5432,
    dbname="",
    user = "postgres",
    password = input$password),
    "thriftysmackers", ledger$df, overwrite = TRUE)})
  
# Establish SQL connection and initializes database tables
  
  observeEvent(input$passaction,{
    
    database <- dbConnect(
      PostgreSQL(), 
      host = "thriftysmackers.ckbxdrgolj49.us-east-2.rds.amazonaws.com",
      port=5432,
      dbname="",
      user = "postgres",
      password = input$password
      )
    
    ledger$df <- data.frame(tbl(database,"thriftysmackers") %>% 
                          select(Date,Purchase,Price,Category,Quantity)) %>% 
                          mutate(Price=as.numeric(Price))
 
    gc$df <- data.frame(tbl(database,"gift_cards")%>% 
                       select(card_name,balance)) %>% 
                       mutate(balance=as.numeric(balance)) 
     })
 
# Giphy images
  output$title_giphy <- renderUI({tags$img(
    src ="https://media0.giphy.com/media/67ThRZlYBvibtdF9JH/giphy.gif",height="280px")})
  
  output$giphy_spending <- renderUI({
    tags$img(src=gif_search(sample(c("cheapskate","stingy","frugal","thrifty"),1)) %>% 
               select("original") %>% .[sample(1:10,1),] %>% as.character())
    })
  
  giphy <- reactive({ gif_search(input$Purchase)})
  
  output$giphy <- renderUI({
    req(input$Purchase)
    tags$img(src = giphy()$original[sample(1:10,1)], width=200, height=200)
  })  
  
# Update Ledger upon confirming purchase
  
  observeEvent(input$confirmPurchase,{
    
    append <- isolate(data.frame(Date=as.character(Sys.time()),
                                Purchase=input$Purchase,
                                Category=input$Category,
                                Quantity=input$Quantity,
                                Price=as.numeric(input$Price)))
   
    ledger$df <- isolate(rbind(ledger$df,append))
    
    writeTable()
  })

# Ledger Tab Table
  
  output$LEDGER <- DT::renderDataTable({
    
    req(input$password) 
                   
    DT::datatable(ledger$df %>% arrange(desc(Date)),
                  rownames = FALSE,
                  editable=TRUE,
                  options=list(
                    initComplete=JS("function(settings,json){
                                    ","$(this.api().table().header()).css({'color':'#fff'});","
                                    }"))) %>% 
      formatStyle(backgroundColor = "black",
                  columns=TRUE,
                  color = "white")})
  
# saves latest updates to table
  
  observeEvent(input$UpdateDB,{writeTable()}) 

# Allows for modification of dt table to be saved
  
  observeEvent(input$LEDGER_cell_edit,{ 
    ledger$df <- ledger$df %>% arrange(desc(Date))
    ledger$df[input$LEDGER_cell_edit$row,input$LEDGER_cell_edit$col+1] <- isolate(input$LEDGER_cell_edit$value)
  })
  
# Deletes selected rows from dt table
  
  observeEvent(input$DELETE,{
    ledger$df <- ledger$df %>% arrange(desc(Date)) %>% .[-(input$LEDGER_rows_selected),]
  })
  

# Bar plot analysis
  graphic <- reactive({ledger$df %>% filter(Date > input$DateStart & Date < input$DateEnd)})

  output$Categories <- renderPlot({
    
    req(input$password)
    
    ggplot(graphic(),aes(x=Category,y=Price,fill=Category))+ 
      geom_bar(stat="identity",color="black")+
      theme(panel.border = element_rect(colour = "black", fill=NA, size=3),
            axis.text.x = element_blank())+
      xlab("Categories")+
      ylab("Total Spent")
    })

## Scatter plot analysis
   
  filtered_plot <- reactive({
     if(input$scatterfilter!=""){ledger$df %>% 
         filter(str_detect(string=Purchase,pattern=regex(input$scatterfilter,ignore_case = T)
                           ))
     }else{ledger$df}
     })
  
  output$scatterall <- renderPlotly({
    ggplotly(ggplot(filtered_plot(),x=Date,y=Price)+
               geom_point(aes(x=as.Date(Date),y=Price,color=Category))+
               theme_bw())
    })
  
# Pop up confirming new purchase
  observeEvent(input$confirmPurchase,{
    updateF7Popup(id="PopConfirmPurchase")})
  
# Tweet accomanying the pop-up confirmation  
  output$tweet <- renderUI({
    req(input$Purchase)
    tagList(
    # Twitter widget
    tags$blockquote(class = "twitter-tweet",
                    tags$a(href = rtweet::search_tweets(input$Purchase,type = "popular") %>% 
                             select(status_url) %>% .[sample(c(1:10),size = 1),1] %>% 
                             as.character())),
    tags$script('twttr.widgets.load(document.getElementById("tweet"));')
    )
    })

# Password-activated gift card table
  
  output$GIFTCARD <- renderUI({req(input$password)
  f7Table(card = T,gc$df,colnames = c("Name of Card","Balance"))})

# Password-activated selections populate  
  observeEvent(input$passaction,{
    updateF7SmartSelect(inputId ="GC_Select",openIn="sheet",
                        choices=as.character(gc$df[,1]),selected=gc$df[1,1])})

# Subtract balance from selected gift card  
  observeEvent(input$update_GC_balance,{
    gc$df[gc$df["card_name"]==input$GC_Select,2] <- gc$df %>% 
      filter(card_name==input$GC_Select) %>% .[1,2] - as.numeric(input$GC_used)
    })

} # closes server

shiny::shinyApp(ui,server)