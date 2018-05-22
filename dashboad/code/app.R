# load the required packages
library(shiny)
require(shinydashboard)
library(plotly)
library(dplyr)
library(DT)


#load data
# d_dc <- read.csv("C:/Users/Mingqi.Wu/Desktop/dc.csv", header = T, stringsAsFactors = F)
# data_dc <- data.frame(Type=d_dc$type, y2017=d_dc$X2017, y2018=d_dc$X2018)
# saveRDS(data_dc, file="dc.rds")
# 
# 
# d_rdc <- read.csv("C:/Users/Mingqi.Wu/Desktop/rdc.csv", header = T, stringsAsFactors = F)
# data_rdc <- data.frame(Type=d_rdc$type, y2017=d_rdc$X2017, y2018=d_rdc$X2018)
# saveRDS(data_rdc, file="rdc.rds")
# 
# d_pack <- read.csv("C:/Users/Mingqi.Wu/Desktop/pack.csv", header = T, stringsAsFactors = F)
# data_pack <- data.frame(Type=d_pack$type, y2017=d_pack$X2017, y2018=d_pack$X2018)
# saveRDS(data_pack, file="pack.rds")
# 
# d_bulk <- read.csv("C:/Users/Mingqi.Wu/Desktop/bulk.csv", header = T, stringsAsFactors = F)
# data_bulk <- data.frame(Type=d_bulk$type, y2017=d_bulk$X2017, y2018=d_bulk$X2018)
# saveRDS(data_bulk, file="bulk.rds")

data_dc <- readRDS("./data/dc.rds")
data_rdc <- readRDS("./data/rdc.rds")
data_pack <- readRDS("./data/pack.rds")
data_bulk <- readRDS("./data/bulk.rds")

#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "Logistics KPI")

#Sidebar content of the dashboard
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("HSSE", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Finance", icon = icon("dashboard")),
    menuItem("Performance", icon = icon("dashboard")) 
    # menuItem("Finance", icon = icon("send",lib='glyphicon'), 
    #          href = "https://www.salesforce.com")
  )
)

frow1 <- fluidRow(
  valueBoxOutput("value1", width = 3)
  ,valueBoxOutput("value2",width = 3)
  ,valueBoxOutput("value3",width = 3)
  ,valueBoxOutput("value4",width = 3)
)

frow2 <- fluidRow(
  tags$head(
     tags$style(type='text/css', 
                ".nav-tabs {font-size: 24px; font-weight: bold}")
  )  
  ,tabBox(
   width = 6,
   title = tags$h1("Distribution Center"), 
   #id = "hsse_dc", 
   tabPanel("Plot", plotlyOutput("dc"), height = "200px"),
   tabPanel("Data", div(dataTableOutput('dcData'), style="font-size: 22px"))
  )
  ,tabBox(
   width = 6,
   title = tags$h1("Pack Transportation"),
   #id = "hsse_pack", 
   tabPanel("Plot", plotlyOutput("pack"), height = "200px"),
   tabPanel("Data", div(dataTableOutput('packData'), style="font-size: 22px"))
 )
)

frow3 <- fluidRow(
  tags$head(
     tags$style(type='text/css', 
                ".nav-tabs {font-size: 24px; font-weight: bold}")
  )  
  ,tabBox(
   width = 6,
   title = tags$h1("Regional Dist. Center"), 
   #id = "hsse_rdc", 
   tabPanel("Plot", plotlyOutput("rdc"), height = "200px"),
   tabPanel("Data", div(dataTableOutput('rdcData'), style="font-size: 22px"))
  )
  ,tabBox(
   width = 6,
   title = tags$h1("Bulk Transportation"),
   #id = "hsse_bulk", 
   tabPanel("Plot", plotlyOutput("bulk"), height = "200px"),
   tabPanel("Data", div(dataTableOutput('bulkData'), style="font-size: 22px"))
 )
)

## body
body <- dashboardBody(frow1, frow2, frow3,
                      
                      tags$head(tags$style(HTML('
                                      .main-sidebar {
                                      font-family: "Georgia", Times, "Times New Roman", serif;
                                      font-weight: bold;
                                      font-size: 22px;
                                      }

                                      .skin-red .main-header .navbar {
                                      background-color: #FFD500;
                                      }

                                      .skin-red .main-header .logo {
                                      background-color: #ED1C24;
                                      font-family: "Georgia", Times, "Times New Roman", serif;
                                      font-weight: bold;
                                      font-size: 28px;
                                      }

                                      .skin-red .main-sidebar .sidebar .sidebar-menu .active a{
                                      color: #FFD500;
                                      }

                                      .skin-red .main-sidebar .sidebar .sidebar-menu a:hover{
                                      color: #FFD500;
                                      }
                                       
                                  ')))
                      )

## ui
ui <- dashboardPage(title = 'LSC Logistics', header, sidebar, body, skin='red')

## server
server <- function(input, output) { 
  #creating the valueBoxOutput content
  output$value1 <- renderValueBox({
    valueBox(
      formatC("DC")
      , "Distribution Center"
      ,icon = icon("thumbs-up")
      ,color = "green")  
  })
  output$value2 <- renderValueBox({ 
    valueBox(
      formatC("RDC")
      , "Regional Dist. Center"
      ,icon = icon("thumbs-up")
      ,color = "green")  
  })
  output$value3 <- renderValueBox({
    valueBox(
      formatC("PACK TRANS")
      , "Pack Transportation"
      ,icon = icon("thumbs-up")
      ,color = "green")  
  })
  #a = 5
  #if (a==5) co="blue"
  output$value4 <- renderValueBox({
    valueBox(
      formatC("BULK TRANS")
      , "Bulk Transportation"
      ,icon = icon("thumbs-down")
      ,color = "red")   
  })
  
  #creating the plotOutput content
  output$dc <- renderPlotly({
     p <- plot_ly(x = data_dc$Type, y = data_dc$y2017, type = 'bar', name = '2017',
                 text=data_dc$y2017, textposition='auto',
                 marker = list(color='rgb(255, 213, 0)')) %>%
          add_trace(y = data_dc$y2018, name = '2018', text=data_dc$y2018,
                marker = list(color = "rgb(237, 28, 36)")) %>%
          layout(yaxis = list(title = 'Count'), barmode = 'group', font=list(size=15))
     
     p$elementId <- NULL
     p
  })

  output$rdc <- renderPlotly({
     p <- plot_ly( x = data_rdc$Type, y = data_rdc$y2017, type = 'bar', name = '2017',
                 text=data_rdc$y2017, textposition='auto',
                 marker = list(color='rgb(255, 213, 0)')) %>%
          add_trace(y = data_rdc$y2018, name = '2018', text=data_rdc$y2018, textposition='auto',
                marker = list(color = "rgb(237, 28, 36)")) %>%
          layout(yaxis = list(title = 'Count'), barmode = 'group', font=list(size=15))
     
     p$elementId <- NULL
     p
  })
  
  output$pack <- renderPlotly({
     p <- plot_ly( x = data_pack$Type, y = data_pack$y2017, type = 'bar', name = '2017',
                 text=data_pack$y2017, textposition='auto',
                 marker = list(color='rgb(255, 213, 0)')) %>%
          add_trace(y = data_pack$y2018, name = '2018', text=data_pack$y2018, textposition='auto',
                    marker = list(color = "rgb(237, 28, 36)")) %>%
          layout(yaxis = list(title = 'Count'), barmode = 'group', font=list(size=15))
     
     p$elementId <- NULL
     p
  })
  
  output$bulk <- renderPlotly({
     p <- plot_ly( x = data_bulk$Type, y = data_bulk$y2017, type = 'bar', name = '2017',
                 text=data_bulk$y2017, textposition='auto',
                 marker = list(color='rgb(255, 213, 0)')) %>%
          add_trace(y = data_bulk$y2018, name = '2018', text=data_bulk$y2018, textposition='auto',
                    marker = list(color = "rgb(237, 28, 36)")) %>%
          layout(yaxis = list(title = 'Count'), barmode = 'group', font=list(size=15))
     
     p$elementId <- NULL
     p
  })
  
  
  # Outputs data tables
  #output$dcData <- DT::renderDataTable({ DT::datatable(data_dc %>% dplyr::rename(year_2017=y2017, year_2018=y2018), options = list(pageLength=5))})
  output$dcData   <-renderDataTable({ data_dc %>% dplyr::rename(year_2017=y2017, year_2018=y2018)})
  output$rdcData  <-renderDataTable({ data_rdc %>% dplyr::rename(year_2017=y2017, year_2018=y2018)})
  output$packData <-renderDataTable({ data_pack %>% dplyr::rename(year_2017=y2017, year_2018=y2018)})
  output$bulkData <-renderDataTable({ data_bulk %>% dplyr::rename(year_2017=y2017, year_2018=y2018)})
  
}


## combine ui and server
shinyApp(ui, server)
