#===========================================================================================================
#
#                                 Complaint Dashboard Shiny App
#                                 
#                                 Last Update: 3/29/2021
#
#
#===========================================================================================================

#-----------------------------------------------------------------------------------------------------------
#install.packages("shiny",type="binary")
#install.packages("shinythemes")
#install.packages("devtools")
#devtools::install_github('rstudio/DT')
#install.packages("readxl")
#install.packages("openxlsx")
#install.packages("DT")
#install.packages("dplyr")
#install.packages("shinydashboard")
#install.packages("shinydashboardPlus")
#install.packages("plotly")
library(shiny)
library(shinythemes)
library(readxl)
library(openxlsx)
library(DT)
library(dplyr)
library(shinydashboard)
library(shinydashboardPlus)
#library(zoo)
library(lubridate)
library(plotly)


#===========================================================================================================
#
#                                 Data Read-in
#
#===========================================================================================================
complaint_count <- read.xlsx("C:/Users/QI15403/OneDrive - ConvaTec Limited/Desktop/Complaint Dashboard/R Shiny/Complaint_Dashboard/Complaint02282021.xlsx")
complaint_count$month <- as.Date(complaint_count$month, origin = "1899-12-30")
complaint_count$month <- as.Date(complaint_count$month, format="%m/%d/%Y")


#-----------------------------------------------------------------------------------------------------------
#
#                                         Define UI 
#
#-----------------------------------------------------------------------------------------------------------

header <- dashboardHeader(
                          # title = HTML('
                          #              <style>
                          #               .box {
                          #                     padding-left:0px;
                          #                     margin-right:0px;
                          #                }
                          #               img {
                          #                    height:100%;
                          #                    width:100%;
                          #                    margin:0;
                          #                    padding:0;
                          #                   }
                          #              </style>
                          #              <div class="box">
                          #                 <img src="aaa.png">
                          #              </div>
                          # 
                          #      '),
                          #     titleWidth  = 229,
                          #     enable_rightsidebar = TRUE,
                          #     rightSidebarIcon = "gears"
                          title = tags$a(href = 'https://www.convatec.com',
                                        tags$img(src = "ConvaTec_logo.PNG", height = '50', width = '230')),
                          titleWidth  = 230
)



sidebar <- dashboardSidebar(
   width = 230,
   tags$body(class="hold-transition skin-blue sidebar-collapse sidebar-mini"),
   sidebarMenu(
     id = "tabs",
     menuItemOutput("Complaint_Side"),
     menuItem("CPM Dashboard", icon = icon(" fa-external-link"), href = "https://convatec-mass.shinyapps.io/CPM_Shiny/"),
     menuItemOutput("About_Side")
     
)
)

body <- dashboardBody(
   uiOutput("dashboard")
)

ui <- dashboardPage(skin = "blue", header, sidebar, body)



#-----------------------------------------------------------------------------------------------------------
#
#                                         Define Server
#
#-----------------------------------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  #### source functions
  
  source('./script/convertInfoBox.R')
  
  output$dashboard <- renderUI({
    fluidPage(
      fluidRow(uiOutput("Complaint_Body")),
      fluidRow(uiOutput("CPM_Body")),
      fluidRow(uiOutput("About_Body"))
    )
  })
 
  output$Complaint_Side <- renderMenu({
    sidebarMenu(
      menuItem("Complaint Dashboard", expandedName = "cmplt", icon = icon("chart-bar"),
      fluidRow(
        dateRangeInput('Range1', label = 'Date Range',
                       start = "2020-01-01", end = "2020-12-31"),
        checkboxGroupInput("select_BU", "Select one or multiple Business Units",
                                      choices =  c("All B.U. Combined" = "all.BU", "All Non-IC Combined"= "NIC", "Ostomy Care" = "OC", "Wound Care" = "WC",
                                                   "Continence Care" = "CC", "Critical Care" = "C",
                                                   "Infusion Care" = "IC"),

                                      selected = NULL),
        actionButton('cmplt_run', 'Run')
      )
    ))
  })
  

  
  output$About_Side <- renderMenu({
    sidebarMenu(
      menuItem("About", expandedName = "about", icon = icon("info"),
               fluidRow(
                 
               ))
    )

  })
  
  
  ### body UI
  
  output$Complaint_Body <- renderUI({
    
    req(input$sidebarItemExpanded)
    
    if (input$sidebarItemExpanded=="cmplt") {
      fluidPage(
        fluidRow(
          column(width = 2,
                 box(
                   width = NULL, status = "primary",
                   uiOutput("box1")
                 ),
                 box(
                   title = "Title 1", width = NULL, solidHeader = TRUE, status = "primary",
                   "Box content"
                 ),
                 box(
                   width = NULL, background = "black",
                   "A box with a solid black background"
                 )
          ),
          
          column(width = 5,
                 box(
                   status = "warning", width = NULL,
                   "Box content"
                 ),
                 box(
                   title = "Title 3", width = NULL, solidHeader = TRUE, status = "warning",
                   "Box content"
                 ),
                 box(
                   title = "Title 5", width = NULL, background = "light-blue",
                   "A box with a solid light-blue background"
                 )
          ),
          
          column(width = 5,
                 box(
                   title = "Title 2", width = NULL, solidHeader = TRUE,
                   "Box content"
                 ),
                 box(
                   title = "Title 6", width = NULL, background = "maroon",
                   "A box with a solid maroon background"
                 )
          )
        )
      )

      #tableOutput("TEST")
      
      
    } else {
      NULL
    }
  })
  
  output$About_Body <- renderUI({
    req(input$sidebarItemExpanded)
    
    if (input$sidebarItemExpanded=="about") {
      verbatimTextOutput("txt1")
      
    } else {
      NULL
    }
  })
  

  Complaint <- eventReactive(input$cmplt_run,{
    
                             if(input$select_BU=="all.BU"){
                               complaint_count <- complaint_count[complaint_count$BU!="Eurotec",]
                               complaint_count <- aggregate(complaint_count$complaints, by = list(complaint_count$month), sum)
                               complaint_count$BU <- "All_BU"
                             }else{
                               if(input$select_BU=="NIC"){
                                 complaint_count <- complaint_count[complaint_count$BU!="IC" & complaint_count$BU!="Eurotec",]
                                 complaint_count <- aggregate(complaint_count$complaints, by = list(complaint_count$month), sum)
                                 complaint_count$BU <- "All_NIC"
                               }else{
                                 complaint_count <- complaint_count[complaint_count$BU %in% input$select_BU,]
                               }
                             }
    
                             return(complaint_count)
  })
  
  #output$complaint_trend <- renderPlotly({
   #                         plot_ly()
    
  #})
  
  #output$TEST <- renderTable({Complaint()})

###
  output$box1 <- renderUI({
    
    HTML(
      createInfoBox(10)

    )
  })
  # 
  
  #input file
  #calculate
  #render value
  #output function
  
  
  
  output$txt1 <- renderText({
    req(input$sidebarItemExpanded)
    
    input$sidebarItemExpanded
  })
  
}

#-----------------------------------------------------------------------------------------------------------
#                                         Run the application
#-----------------------------------------------------------------------------------------------------------

shinyApp(ui = ui, server = server)

