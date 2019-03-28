library(shiny)
library(ggplot2)
library(shinydashboard)
library(googleway)
library(leaflet)
library(ECharts2Shiny)
library(shinyWidgets)

header <- dashboardHeader(title="Awesome City")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("AAA", tabName = "tabItem1"),
    menuItem("BBB", tabName = "tabItem2"),
    menuItem("CCC", tabName = "tabItem3"),
    menuItem("DDD", tabName = "tabItem4"),
    menuItem("EEE", tabName = "tabItem5"),
    menuItem("FFF", tabName = "tabItem6"),
    menuItem("GGG", tabName = "tabItem7"),
    br(),
    br(),
    p("Sketched by me to you"), 
    p("Together we have", 
         a(href = "www", "code"), "-fun!")
    ))

body <- dashboardBody(
        fluidPage(
          img(src = "city.jpg", height = 72, width = "100%"),
        fluidRow(
          tabItems(
            tabItem(tabName = "tabItem1",
             column(width = 4),
             column(width = 12,
              p(strong("AAA", style = "font-family: 'times'; font-size: 25pt"),
              p("aaa",
              a(href = "www", "here"),
               leafletOutput("map")
                )))), 
            # or google_mapOutput("map")))))),
           
          tabItem(tabName = "tabItem2",
             column(width = 4),
             column(width = 12, 
               p(strong("BBB", style = "font-family: 'times'; font-size: 25pt"),
               p("bbb"),
               br(),
                plotOutput("plot1",
                           click = "plot_click",
                           dblclick = "plot_dblclick",
                           hover = "plot_hover",
                           brush = "plot_brush"
                ),
                verbatimTextOutput("info")
                ),
                 p("bbb"),
                br(),
                p("bbb")
                )),
        
           tabItem(tabName = "tabItem3",
             column(width = 4),
             column(width = 12,  
               p(strong("CCC", style = "font-family: 'times'; font-size: 25pt"),
               p("ccc"),
                box(width = 12, height = 500, 
                  splitLayout(
                      box(
                        title = "histogram", status = "primary", solidHeader = F, collapsible = F,
                        height = 480, width = "100%",
                        plotOutput("Hist")),
                        
                      box(
                        title = "map", status = "warning", width = "100%",
                        leafletOutput("mymap", height = 420)
                        )
                ))))),
        
           tabItem(tabName = "tabItem4",
             column(width = 4),
             column(width = 12, offset = 0,
               p(strong("DDD", style = "font-family: 'times'; font-size: 25pt"),
               p("ddd"),
                 loadEChartsLibrary(),
                 tags$div(id="test", style="width:50%;height:400px;"),
                 deliverChart(div_id = "test")
                 ))),
        
           tabItem(tabName = "tabItem5",
             column(width = 4),
             column(width = 12, offset = 0,
               p(strong("EEE", style = "font-family: 'times'; font-size: 25pt"),
               p("eee"),
               br(),
                tags$head(tags$script(src = "message-handler.js")),
                actionButton("do1", "Shoot it!"),
                br(),
                   fileInput("file1", "Add Picture or Short Video",
                           accept = c(
                             "image/*",
                             "video/*")
                 )))),
        
            tabItem(tabName = "tabItem6",
             column(width = 4),
             column(width = 12, offset = 0,
                 p(strong("FFF", style = "font-family: 'times'; font-size: 25pt"),
                 p("fff"),
                 textInput("caption", "Explain your problem a bit more", "Yeah! Start complaining..."),
                 verbatimTextOutput("complain"),
                 tags$head(tags$script(src = "message-handler.js")),
                 actionButton("do", "Change it!")
                 )
                 )),
        
            tabItem(tabName = "tabItem7",
             column(width = 4),
             column(width = 12, offset = 0,
                 p(strong("GGG", style = "font-family: 'times'; font-size: 25pt"),
                 p("ggg"),
                 checkboxGroupButtons(
                   inputId = "1",
                   label = "How do you rate this website?",
                   choices = c("Very Bad", 
                               "2", "3", "4", "Very Good"),
                   selected = c("3")
                 ))))
     )
   )
 )
)

ui <- dashboardPage(
       header,
       sidebar,
       body
       )

server <- function(input, output, session){
  
  # AAA
  
  output$map <- renderLeaflet(map)
  
  map <- leaflet() %>% 
    addTiles() %>% 
    setView(-93.65, 42.0285, zoom = 17)
  
  # or
  #api_key <- "your_api_key" ## get Google API here: https://developers.google.com/maps/documentation/embed/get-api-key
  #output$map <- renderGoogle_map({
    #google_map(key = api_key)
  #})
  
  # BBB
  
  output$plot1 <- renderPlot({
    
    plot(mtcars$wt, mtcars$mpg)
  })
  
  output$info <- renderText({
    xy_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
    }
    
    xy_range_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1), 
             " ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1))
    }
    
    paste0(
      "click: ", xy_str(input$plot_click),
      "dblclick: ", xy_str(input$plot_dblclick),
      "hover: ", xy_str(input$plot_hover),
      "brush: ", xy_range_str(input$plot_brush)
    )
  })
  
  # CCC
  
  output$Hist <- renderPlot({
    
    plot(mtcars$wt, mtcars$mpg)
  })
  
  output$mymap <- renderLeaflet(map)
    
    map <- leaflet() %>% addTiles() %>% setView(-93.65, 42.0285, zoom = 5)
    
  # DDD
    
    dat <- c(rep("Type-A", 8),
             rep("Type-B", 5),
             rep("Type-C", 1))
    
    renderPieChart(div_id = "test",
                   data = dat)
    
    output$contents <- renderTable({
      
      inFile <- input$file1
      
      if (is.null(inFile))
        return(NULL)
      
      read.csv(inFile$datapath, header = input$header)
      
      output$value <- renderPrint({input$num})
  })
    
  # EEE
    
    observeEvent(input$do1, {
      session$sendCustomMessage(type = 'testmessage',
                                message = 'Cheers!')
    })
    
  # FFF
    
    observeEvent(input$do, {
      session$sendCustomMessage(type = 'testmessage',
                                message = 'Cheers!')
    })
    
  # GGG
    
    output$value <- renderText({
      inputId$caption})
  }


shinyApp(ui, server)


