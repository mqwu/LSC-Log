## load the required packages
library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(DT)
library(kableExtra)
library(formattable)



## font set up for plot
f1 <- list(
  family = "Arial, sans-serif",
  size = 18,
  color = "white"
)

f2 <- list(
  family = "Old Standard TT, serif",
  size = 16,
  color = "white"
)

this_mon = "May"
last_mon = "April"


call_plotly <- function(x_title, y_title, x_val, y_val, col, text) {
	p = plot_ly (
			x = x_val, 
			y = y_val, 
			type = 'bar', 
			text = text, 
			textposition='auto',
			marker = list(color=col))  %>% 
			layout(
				paper_bgcolor='#222D32',
				plot_bgcolor='#222D32',
				xaxis = list(
					title = x_title,
					titlefont = f1 ,
					showticklabels = TRUE,
					tickfont = f2
					), 
				yaxis = list(
					title = y_title,
					titlefont = f1 ,
					showticklabels = TRUE,
					tickangle = 90,
					tickfont = f2
					),
				showlegend = FALSE
		      ) # layout end
	return(p)
}


##################################################################################################
# load data
##################################################################################################
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

## HSSE data
data_dc <- readRDS("./data/dc.rds")
data_rdc <- readRDS("./data/rdc.rds")
data_pack <- readRDS("./data/pack.rds")
data_bulk <- readRDS("./data/bulk.rds")

dc_data <- readRDS("./data/dc_data.rds")
rdc_data <- readRDS("./data/rdc_data.rds")

## Finance data
fin_data <- readRDS("./data/fin_data.rds")


## Performance data
# ware house
wh_lobp <- readRDS("./data/wh_lobp_data.rds")
wh_rdc  <- readRDS("./data/wh_rdc_data.rds")

# otif
otif_wk <- readRDS("./data/otif_wk_data.rds")
otif_by_plant <- readRDS("./data/otif_by_plant_data.rds")
otif_by_customer <- readRDS("./data/otif_by_customer_data.rds")

# carrier 
scar_n_by_carr <- readRDS("./data/scar_n_carr_data.rds")
scar_n_by_carr_plant <- readRDS("./data/scar_n_carr_plant_data.rds")


##################################################################################################
# Header
##################################################################################################
#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "Logistics KPI")


##################################################################################################
# Sidebbar
##################################################################################################
#Sidebar content of the dashboard
sidebar <- dashboardSidebar(
		sidebarMenu(
			menuItem("HSSE", tabName = "hsse_tab", icon = icon("dashboard")),
			menuItem("Finance", tabName = "finance_tab", icon = icon("dashboard")),
			menuItem("Performance", icon = icon("dashboard"), startExpanded = TRUE,
				menuSubItem("OTIF", tabName = "otif_tab"),
				menuSubItem("WARE HOUSE", tabName = "wh_tab"),
				menuSubItem("CARRIER", tabName = "carr_tab")
				) 
			)
		)


##################################################################################################
# Body
##################################################################################################
#------------------------------------------------------------------------------------------------
## HSSE tab
#------------------------------------------------------------------------------------------------
hsse_row1 <- fluidRow(
  valueBoxOutput("hsse_v1", width = 3)
  ,valueBoxOutput("hsse_v2",width = 3)
  ,valueBoxOutput("hsse_v3",width = 3)
  ,valueBoxOutput("hsse_v4",width = 3)
)

hsse_row2 <- fluidRow(
  tags$head(
     tags$style(type='text/css', 
                ".nav-tabs {font-size: 24px; font-weight: bold}")
  )  
  ,tabBox(
   width = 6,
   title = tags$h2("Distribution Center"), 
   tabPanel("Plot", plotlyOutput("hsse_dc"), height = "200px"),
   tabPanel("Data", div(dataTableOutput('hsse_dcData'), style="font-size: 16px"))
  )
  ,tabBox(
   width = 6,
   title = tags$h2("Pack Transportation"),
   tabPanel("Plot", plotlyOutput("hsse_pack"), height = "200px"),
   tabPanel("Data", div(dataTableOutput('hsse_packData'), style="font-size: 16px"))
 )
)

hsse_row3 <- fluidRow(
  tags$head(
     tags$style(type='text/css', 
                ".nav-tabs {font-size: 24px; font-weight: bold}")
  )  
  ,tabBox(
   width = 6,
   title = tags$h2("Regional Dist. Center"), 
   tabPanel("Plot", plotlyOutput("hsse_rdc"), height = "200px"),
   tabPanel("Data", div(dataTableOutput('hsse_rdcData'), style="font-size: 16px"))
  )
  ,tabBox(
   width = 6,
   title = tags$h2("Bulk Transportation"),
   tabPanel("Plot", plotlyOutput("hsse_bulk"), height = "200px"),
   tabPanel("Data", div(dataTableOutput('hsse_bulkData'), style="font-size: 16px"))
 )
)




#------------------------------------------------------------------------------------------------
# Finance tab 
#------------------------------------------------------------------------------------------------

######################################################
## finance tab row 1
######################################################
fin_row1 <- fluidRow(
  box(
    width = 4
    ,title = paste0("Logistics Spend (mln$)   ", this_mon, ", 2018")
    ,status = ifelse(fin_data[11,"now_est"] > fin_data[11, "now_plan"], "danger","success") 
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,htmlOutput("fin_log")
  ),
  box(
    width = 4
    ,title = paste0("Total OPEX (mln$)   ", this_mon, ", 2018")
    ,status = ifelse(fin_data[7,"now_est"] > fin_data[7, "now_plan"], "danger","success") 
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,htmlOutput("fin_opex")
  ),
  box(
    width = 4
    ,title = paste0("Unit OPEX (mln$)   ", this_mon, ", 2018")
    ,status = ifelse(fin_data[8,"now_est"] > fin_data[8, "now_plan"], "danger","success") 
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,htmlOutput("fin_unit_opex")
  )
)


######################################################
## finance tab row 2
######################################################
#----------------------------------------------------------------------------------------------
## Log Spend 
fin_row2_t1_1 <- fluidRow(
  box(
    width = 3
    ,title = "Logistics Spend (mln$)"
    ,status = ifelse(fin_data[11,"now_est"] > fin_data[11, "now_plan"], "danger","success") 
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("fin_log_now", height='200px')
  ),
  
  box(
    width = 3
    ,title = "" 
    ,status = ifelse(fin_data[11,"last_act"] > fin_data[11, "last_plan"], "danger","success") 
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("fin_log_last", height='200px')
  ),
  
  box(
    width = 3
    ,title = ""
    ,status = ifelse(fin_data[11,"ytd"] > fin_data[11, "ytd_plan"], "danger","success") 
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("fin_log_ytd", height='200px')
  ),
  
  box(
    width = 3
    ,title = ""
    ,status = ifelse(fin_data[11,"latest_est"] > fin_data[11, "fy_plan"], "danger","success") 
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("fin_log_fy", height='200px')
  )
)

fin_row2_t1_2 <- fluidRow(
  box(
    width = 3
    ,status = ifelse(fin_data[11,"now_est"] > fin_data[11, "now_plan"], "danger","success")
    ,plotlyOutput("fin_log_now_var", height='150px')
  ),

  box(
    width = 3
    ,status = ifelse(fin_data[11,"last_act"] > fin_data[11, "last_plan"], "danger","success")
    ,plotlyOutput("fin_log_last_var", height='150px')
  ),

  box(
    width = 3
    ,status = ifelse(fin_data[11,"ytd"] > fin_data[11, "ytd_plan"], "danger","success")
    ,plotlyOutput("fin_log_ytd_var", height='150px')
  ),

  box(
    width = 3
    ,status = ifelse(fin_data[11,"latest_est"] > fin_data[11, "fy_plan"], "danger","success")
    ,plotlyOutput("fin_log_fy_var", height='150px')
  )
)
#----------------------------------------------------------------------------------------------
## Log-Exp
fin_row2_t3_1 <- fluidRow(
  box(
    width = 3
    ,title = "Export (COGS) (mln$)"
    ,status = ifelse(fin_data[9,"now_est"] > fin_data[9, "now_plan"], "danger","success") 
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("fin_exp_now", height='200px')
  ),
  
  box(
    width = 3
    ,title = "" 
    ,status = ifelse(fin_data[9,"last_act"] > fin_data[9, "last_plan"], "danger","success") 
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("fin_exp_last", height='200px')
  ),
  
  box(
    width = 3
    ,title = ""
    ,status = ifelse(fin_data[9,"ytd"] > fin_data[9, "ytd_plan"], "danger","success") 
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("fin_exp_ytd", height='200px')
  ),
  
  box(
    width = 3
    ,title = ""
    ,status = ifelse(fin_data[9,"latest_est"] > fin_data[9, "fy_plan"], "danger","success") 
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("fin_exp_fy", height='200px')
  )
)

fin_row2_t3_2 <- fluidRow(
  box(
    width = 3
    ,status = ifelse(fin_data[9,"now_est"] > fin_data[9, "now_plan"], "danger","success")
    ,plotlyOutput("fin_exp_now_var", height='150px')
  ),

  box(
    width = 3
    ,status = ifelse(fin_data[9,"last_act"] > fin_data[9, "last_plan"], "danger","success")
    ,plotlyOutput("fin_exp_last_var", height='150px')
  ),

  box(
    width = 3
    ,status = ifelse(fin_data[9,"ytd"] > fin_data[9, "ytd_plan"], "danger","success")
    ,plotlyOutput("fin_exp_ytd_var", height='150px')
  ),

  box(
    width = 3
    ,status = ifelse(fin_data[9,"latest_est"] > fin_data[9, "fy_plan"], "danger","success")
    ,plotlyOutput("fin_exp_fy_var", height='150px')
  )
)
#----------------------------------------------------------------------------------------------
## Log-scme 
fin_row2_t4_1 <- fluidRow(
  box(
    width = 3
    ,title = "SCME (mln$)"
    ,status = ifelse(fin_data[10,"now_est"] > fin_data[10, "now_plan"], "danger","success") 
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("fin_scme_now", height='200px')
  ),
  
  box(
    width = 3
    ,title = "" 
    ,status = ifelse(fin_data[10,"last_act"] > fin_data[10, "last_plan"], "danger","success") 
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("fin_scme_last", height='200px')
  ),
  
  box(
    width = 3
    ,title = ""
    ,status = ifelse(fin_data[10,"ytd"] > fin_data[10, "ytd_plan"], "danger","success") 
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("fin_scme_ytd", height='200px')
  ),
  
  box(
    width = 3
    ,title = ""
    ,status = ifelse(fin_data[10,"latest_est"] > fin_data[10, "fy_plan"], "danger","success") 
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("fin_scme_fy", height='200px')
  )
)

fin_row2_t4_2 <- fluidRow(
  box(
    width = 3
    ,status = ifelse(fin_data[10,"now_est"] > fin_data[10, "now_plan"], "danger","success")
    ,plotlyOutput("fin_scme_now_var", height='150px')
  ),

  box(
    width = 3
    ,status = ifelse(fin_data[10,"last_act"] > fin_data[10, "last_plan"], "danger","success")
    ,plotlyOutput("fin_scme_last_var", height='150px')
  ),

  box(
    width = 3
    ,status = ifelse(fin_data[10,"ytd"] > fin_data[10, "ytd_plan"], "danger","success")
    ,plotlyOutput("fin_scme_ytd_var", height='150px')
  ),

  box(
    width = 3
    ,status = ifelse(fin_data[10,"latest_est"] > fin_data[10, "fy_plan"], "danger","success")
    ,plotlyOutput("fin_scme_fy_var", height='150px')
  )
)
#----------------------------------------------------------------------------------------------
## Log-opex
# opex
fin_row2_t2_t1_1 <- fluidRow(
  box(
    width = 3
    ,title = "Total OPEX (mln$)"
    ,status = ifelse(fin_data[7,"now_est"] > fin_data[7, "now_plan"], "danger","success") 
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("fin_opex_now", height='200px')
  ),
  
  box(
    width = 3
    ,title = "" 
    ,status = ifelse(fin_data[7,"last_act"] > fin_data[7, "last_plan"], "danger","success") 
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("fin_opex_last", height='200px')
  ),
  
  box(
    width = 3
    ,title = ""
    ,status = ifelse(fin_data[7,"ytd"] > fin_data[7, "ytd_plan"], "danger","success") 
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("fin_opex_ytd", height='200px')
  ),
  
  box(
    width = 3
    ,title = ""
    ,status = ifelse(fin_data[7,"latest_est"] > fin_data[7, "fy_plan"], "danger","success") 
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("fin_opex_fy", height='200px')
  )
)


fin_row2_t2_t1_2 <- fluidRow(
  box(
    width = 3
    ,status = ifelse(fin_data[7,"now_est"] > fin_data[7, "now_plan"], "danger","success")
    ,plotlyOutput("fin_opex_now_var", height='150px')
  ),

  box(
    width = 3
    ,status = ifelse(fin_data[7,"last_act"] > fin_data[7, "last_plan"], "danger","success")
    ,plotlyOutput("fin_opex_last_var", height='150px')
  ),

  box(
    width = 3
    ,status = ifelse(fin_data[7,"ytd"] > fin_data[7, "ytd_plan"], "danger","success")
    ,plotlyOutput("fin_opex_ytd_var", height='150px')
  ),

  box(
    width = 3
    ,status = ifelse(fin_data[7,"latest_est"] > fin_data[7, "fy_plan"], "danger","success")
    ,plotlyOutput("fin_opex_fy_var", height='150px')
  )
)
#----------------------------------------------------------------------------------------------
# unit opex
fin_row2_t2_t1_3 <- fluidRow(
  box(
    width = 3
    ,title = "Unit OPEX ($/gal)"
    ,status = ifelse(fin_data[8,"now_est"] > fin_data[8, "now_plan"], "danger","success") 
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("fin_uopex_now", height='200px')
  ),
  
  box(
    width = 3
    ,title = "" 
    ,status = ifelse(fin_data[8,"last_act"] > fin_data[8, "last_plan"], "danger","success") 
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("fin_uopex_last", height='200px')
  ),
  
  box(
    width = 3
    ,title = ""
    ,status = ifelse(fin_data[8,"ytd"] > fin_data[8, "ytd_plan"], "danger","success") 
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("fin_uopex_ytd", height='200px')
  ),
  
  box(
    width = 3
    ,title = ""
    ,status = ifelse(fin_data[8,"latest_est"] > fin_data[8, "fy_plan"], "danger","success") 
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("fin_uopex_fy", height='200px')
  )
)

fin_row2_t2_t1_4 <- fluidRow(
  box(
    width = 3
    ,status = ifelse(fin_data[8,"now_est"] > fin_data[8, "now_plan"], "danger","success")
    ,plotlyOutput("fin_uopex_now_var", height='150px')
  ),

  box(
    width = 3
    ,status = ifelse(fin_data[8,"last_act"] > fin_data[8, "last_plan"], "danger","success")
    ,plotlyOutput("fin_uopex_last_var", height='150px')
  ),

  box(
    width = 3
    ,status = ifelse(fin_data[8,"ytd"] > fin_data[8, "ytd_plan"], "danger","success")
    ,plotlyOutput("fin_uopex_ytd_var", height='150px')
  ),

  box(
    width = 3
    ,status = ifelse(fin_data[8,"latest_est"] > fin_data[8, "fy_plan"], "danger","success")
    ,plotlyOutput("fin_uopex_fy_var", height='150px')
  )
)
#----------------------------------------------------------------------------------------------
# sales vol 
fin_row2_t2_t2_1 <- fluidRow(
  box(
    width = 3
    ,title = "LSC Sales Volume (mln gal)"
    ,status = ifelse(fin_data[1,"now_est"] < fin_data[1, "now_plan"], "danger","success") 
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("fin_sales_now", height='200px')
  ),
  
  box(
    width = 3
    ,title = "" 
    ,status = ifelse(fin_data[1,"last_act"] < fin_data[1, "last_plan"], "danger","success") 
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("fin_sales_last", height='200px')
  ),
  
  box(
    width = 3
    ,title = ""
    ,status = ifelse(fin_data[1,"ytd"] < fin_data[1, "ytd_plan"], "danger","success") 
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("fin_sales_ytd", height='200px')
  ),
  
  box(
    width = 3
    ,title = ""
    ,status = ifelse(fin_data[1,"latest_est"] < fin_data[1, "fy_plan"], "danger","success") 
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("fin_sales_fy", height='200px')
  )
)

fin_row2_t2_t2_2 <- fluidRow(
  box(
    width = 3
    ,status = ifelse(fin_data[1,"now_est"] < fin_data[1, "now_plan"], "danger","success")
    ,plotlyOutput("fin_sales_now_var", height='150px')
  ),

  box(
    width = 3
    ,status = ifelse(fin_data[1,"last_act"] < fin_data[1, "last_plan"], "danger","success")
    ,plotlyOutput("fin_sales_last_var", height='150px')
  ),

  box(
    width = 3
    ,status = ifelse(fin_data[1,"ytd"] < fin_data[1, "ytd_plan"], "danger","success")
    ,plotlyOutput("fin_sales_ytd_var", height='150px')
  ),

  box(
    width = 3
    ,status = ifelse(fin_data[1,"latest_est"] < fin_data[1, "fy_plan"], "danger","success")
    ,plotlyOutput("fin_sales_fy_var", height='150px')
  )
)
#----------------------------------------------------------------------------------------------
# Primary frt
fin_row2_t2_t3_1 <- fluidRow(
  box(
    width = 3
    ,title = "Primary Freight (mln$)"
    ,status = ifelse(fin_data[2,"now_est"] > fin_data[2, "now_plan"], "danger","success") 
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("fin_1fr_now", height='200px')
  ),
  
  box(
    width = 3
    ,title = "" 
    ,status = ifelse(fin_data[2,"last_act"] > fin_data[2, "last_plan"], "danger","success") 
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("fin_1fr_last", height='200px')
  ),
  
  box(
    width = 3
    ,title = ""
    ,status = ifelse(fin_data[2,"ytd"] > fin_data[2, "ytd_plan"], "danger","success") 
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("fin_1fr_ytd", height='200px')
  ),
  
  box(
    width = 3
    ,title = ""
    ,status = ifelse(fin_data[2,"latest_est"] > fin_data[2, "fy_plan"], "danger","success") 
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("fin_1fr_fy", height='200px')
  )
)

fin_row2_t2_t3_2 <- fluidRow(
  box(
    width = 3
    ,status = ifelse(fin_data[2,"now_est"] > fin_data[2, "now_plan"], "danger","success")
    ,plotlyOutput("fin_1fr_now_var", height='150px')
  ),

  box(
    width = 3
    ,status = ifelse(fin_data[2,"last_act"] > fin_data[2, "last_plan"], "danger","success")
    ,plotlyOutput("fin_1fr_last_var", height='150px')
  ),

  box(
    width = 3
    ,status = ifelse(fin_data[2,"ytd"] > fin_data[2, "ytd_plan"], "danger","success")
    ,plotlyOutput("fin_1fr_ytd_var", height='150px')
  ),

  box(
    width = 3
    ,status = ifelse(fin_data[2,"latest_est"] > fin_data[2, "fy_plan"], "danger","success")
    ,plotlyOutput("fin_1fr_fy_var", height='150px')
  )
)
#----------------------------------------------------------------------------------------------
# 2nd frt
fin_row2_t2_t4_t1_1 <- fluidRow(
  box(
    width = 3
    ,title = "Secondary Freight (mln$)"
    ,status = ifelse(fin_data[3,"now_est"] > fin_data[3, "now_plan"], "danger","success") 
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("fin_2fr_now", height='200px')
  ),
  
  box(
    width = 3
    ,title = "" 
    ,status = ifelse(fin_data[3,"last_act"] > fin_data[3, "last_plan"], "danger","success") 
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("fin_2fr_last", height='200px')
  ),
  
  box(
    width = 3
    ,title = ""
    ,status = ifelse(fin_data[3,"ytd"] > fin_data[3, "ytd_plan"], "danger","success") 
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("fin_2fr_ytd", height='200px')
  ),
  
  box(
    width = 3
    ,title = ""
    ,status = ifelse(fin_data[3,"latest_est"] > fin_data[3, "fy_plan"], "danger","success") 
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("fin_2fr_fy", height='200px')
  )
)

fin_row2_t2_t4_t1_2 <- fluidRow(
  box(
    width = 3
    ,status = ifelse(fin_data[3,"now_est"] > fin_data[3, "now_plan"], "danger","success")
    ,plotlyOutput("fin_2fr_now_var", height='150px')
  ),

  box(
    width = 3
    ,status = ifelse(fin_data[3,"last_act"] > fin_data[3, "last_plan"], "danger","success")
    ,plotlyOutput("fin_2fr_last_var", height='150px')
  ),

  box(
    width = 3
    ,status = ifelse(fin_data[3,"ytd"] > fin_data[3, "ytd_plan"], "danger","success")
    ,plotlyOutput("fin_2fr_ytd_var", height='150px')
  ),

  box(
    width = 3
    ,status = ifelse(fin_data[3,"latest_est"] > fin_data[3, "fy_plan"], "danger","success")
    ,plotlyOutput("fin_2fr_fy_var", height='150px')
  )
)
#----------------------------------------------------------------------------------------------
# dir trans
fin_row2_t2_t4_t2_1 <- fluidRow(
  box(
    width = 3
    ,title = "Direct Trans (mln$)"
    ,status = ifelse(fin_data[4,"now_est"] > fin_data[4, "now_plan"], "danger","success") 
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("fin_2frdt_now", height='200px')
  ),
  
  box(
    width = 3
    ,title = "" 
    ,status = ifelse(fin_data[4,"last_act"] > fin_data[4, "last_plan"], "danger","success") 
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("fin_2frdt_last", height='200px')
  ),
  
  box(
    width = 3
    ,title = ""
    ,status = ifelse(fin_data[4,"ytd"] > fin_data[4, "ytd_plan"], "danger","success") 
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("fin_2frdt_ytd", height='200px')
  ),
  
  box(
    width = 3
    ,title = ""
    ,status = ifelse(fin_data[4,"latest_est"] > fin_data[4, "fy_plan"], "danger","success") 
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("fin_2frdt_fy", height='200px')
  )
)

fin_row2_t2_t4_t2_2 <- fluidRow(
  box(
    width = 3
    ,status = ifelse(fin_data[4,"now_est"] > fin_data[4, "now_plan"], "danger","success")
    ,plotlyOutput("fin_2frdt_now_var", height='150px')
  ),

  box(
    width = 3
    ,status = ifelse(fin_data[4,"last_act"] > fin_data[4, "last_plan"], "danger","success")
    ,plotlyOutput("fin_2frdt_last_var", height='150px')
  ),

  box(
    width = 3
    ,status = ifelse(fin_data[4,"ytd"] > fin_data[4, "ytd_plan"], "danger","success")
    ,plotlyOutput("fin_2frdt_ytd_var", height='150px')
  ),

  box(
    width = 3
    ,status = ifelse(fin_data[4,"latest_est"] > fin_data[4, "fy_plan"], "danger","success")
    ,plotlyOutput("fin_2frdt_fy_var", height='150px')
  )
)
#----------------------------------------------------------------------------------------------
# dfoa
fin_row2_t2_t4_t3_1 <- fluidRow(
  box(
    width = 3
    ,title = "DFOA (mln$)"
    ,status = ifelse(fin_data[5,"now_est"] > fin_data[5, "now_plan"], "danger","success") 
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("fin_2frdfoa_now", height='200px')
  ),
  
  box(
    width = 3
    ,title = "" 
    ,status = ifelse(fin_data[5,"last_act"] > fin_data[5, "last_plan"], "danger","success") 
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("fin_2frdfoa_last", height='200px')
  ),
  
  box(
    width = 3
    ,title = ""
    ,status = ifelse(fin_data[5,"ytd"] > fin_data[5, "ytd_plan"], "danger","success") 
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("fin_2frdfoa_ytd", height='200px')
  ),
  
  box(
    width = 3
    ,title = ""
    ,status = ifelse(fin_data[5,"latest_est"] > fin_data[5, "fy_plan"], "danger","success") 
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("fin_2frdfoa_fy", height='200px')
  )
)

fin_row2_t2_t4_t3_2 <- fluidRow(
  box(
    width = 3
    ,status = ifelse(fin_data[5,"now_est"] > fin_data[5, "now_plan"], "danger","success")
    ,plotlyOutput("fin_2frdfoa_now_var", height='150px')
  ),

  box(
    width = 3
    ,status = ifelse(fin_data[5,"last_act"] > fin_data[5, "last_plan"], "danger","success")
    ,plotlyOutput("fin_2frdfoa_last_var", height='150px')
  ),

  box(
    width = 3
    ,status = ifelse(fin_data[5,"ytd"] > fin_data[5, "ytd_plan"], "danger","success")
    ,plotlyOutput("fin_2frdfoa_ytd_var", height='150px')
  ),

  box(
    width = 3
    ,status = ifelse(fin_data[5,"latest_est"] > fin_data[5, "fy_plan"], "danger","success")
    ,plotlyOutput("fin_2frdfoa_fy_var", height='150px')
  )
)
#----------------------------------------------------------------------------------------------
# s&h
fin_row2_t2_t5_1 <- fluidRow(
  box(
    width = 3
    ,title = "S&H Expense (mln$)"
    ,status = ifelse(fin_data[6,"now_est"] > fin_data[6, "now_plan"], "danger","success") 
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("fin_sh_now", height='200px')
  ),
  
  box(
    width = 3
    ,title = "" 
    ,status = ifelse(fin_data[6,"last_act"] > fin_data[6, "last_plan"], "danger","success") 
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("fin_sh_last", height='200px')
  ),
  
  box(
    width = 3
    ,title = ""
    ,status = ifelse(fin_data[6,"ytd"] > fin_data[6, "ytd_plan"], "danger","success") 
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("fin_sh_ytd", height='200px')
  ),
  
  box(
    width = 3
    ,title = ""
    ,status = ifelse(fin_data[6,"latest_est"] > fin_data[6, "fy_plan"], "danger","success") 
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("fin_sh_fy", height='200px')
  )
)

fin_row2_t2_t5_2 <- fluidRow(
  box(
    width = 3
    ,status = ifelse(fin_data[6,"now_est"] > fin_data[6, "now_plan"], "danger","success")
    ,plotlyOutput("fin_sh_now_var", height='150px')
  ),

  box(
    width = 3
    ,status = ifelse(fin_data[6,"last_act"] > fin_data[6, "last_plan"], "danger","success")
    ,plotlyOutput("fin_sh_last_var", height='150px')
  ),

  box(
    width = 3
    ,status = ifelse(fin_data[6,"ytd"] > fin_data[6, "ytd_plan"], "danger","success")
    ,plotlyOutput("fin_sh_ytd_var", height='150px')
  ),

  box(
    width = 3
    ,status = ifelse(fin_data[6,"latest_est"] > fin_data[6, "fy_plan"], "danger","success")
    ,plotlyOutput("fin_sh_fy_var", height='150px')
  )
)


#==============================================================================================
## finance tab row 2 structure 
fin_row2 <- fluidRow(
  tags$head(
     tags$style(type='text/css', 
                ".nav-tabs {font-size: 24px; font-weight: bold}")
  )  
  ,tabBox(
   width = 12
   #,title = tags$h2("Logistics Spend")
   ,tabPanel("Logistics Spend", fin_row2_t1_1, fin_row2_t1_2, height = "200px")
   ,tabPanel("@Total OPEX"
		,tabBox( 
   			width = 12
		        #,title = tags$h2("Total OPEX")
		   	,tabPanel("Total OPEX", 
		           	  fin_row2_t2_t1_1, fin_row2_t2_t1_2, 
                                  fin_row2_t2_t1_3, fin_row2_t2_t1_4, height = "200px")
		   	,tabPanel("#LSC Sales Volume", fin_row2_t2_t2_1, fin_row2_t2_t2_2, height = "200px")
		   	,tabPanel("#Primary Freight", fin_row2_t2_t3_1, fin_row2_t2_t3_2, height = "200px")
		   	,tabPanel("#Secondary Freight"
				,tabBox(
					width = 12
					,tabPanel("Secondary Freight", 
				        	  fin_row2_t2_t4_t1_1, fin_row2_t2_t4_t1_2, height = "200px")
					,tabPanel("#Direct Trans", 
				        	  fin_row2_t2_t4_t2_1, fin_row2_t2_t4_t2_2, height = "200px")
					,tabPanel("#DFOA", 
				        	  fin_row2_t2_t4_t3_1, fin_row2_t2_t4_t3_2, height = "200px")
					)
				) # 2nd frt end
		   	,tabPanel("#S&H", fin_row2_t2_t5_1, fin_row2_t2_t5_2, height = "200px")
			) 
	)# tot opex end
   ,tabPanel("@Export (COGS)", fin_row2_t3_1, fin_row2_t3_2, height = "200px")
   ,tabPanel("@SCME", fin_row2_t4_1, fin_row2_t4_2, height = "200px")
   )
)
#==============================================================================================

#------------------------------------------------------------------------------------------------
## Performance : Ware House 
#------------------------------------------------------------------------------------------------
wh_row1 <- fluidRow(

		box(
			width = 6 
			,title = "WARE HOUSE: LOBP Weekly Capacity Trend"
			,status = "primary" 
			,solidHeader = TRUE 
			,collapsible = TRUE 

			,tags$head(
				tags$style(type='text/css', 
					".nav-tabs {font-size: 24px; font-weight: bold}")
				)  
			,tabBox(
				width = 12,
			#title = tags$h2("LOBPs Weekly Capacity"), 
				tabPanel("Plot", plotlyOutput("wh_lobp"), height = "200px"),
				tabPanel("Data", div(dataTableOutput('wh_lobpData'), style="font-size: 16px"))
			       )
		   ),

		box(
				width = 6 
				,title = "WARE HOUSE: RDC Weekly Capacity Trend"
				,status = "primary" 
				,solidHeader = TRUE 
				,collapsible = TRUE 

				,tags$head(
					tags$style(type='text/css', 
						".nav-tabs {font-size: 24px; font-weight: bold}")
					)  
				,tabBox(
					width = 12,
				#title = tags$h2("RDCs Weekly Capacity"),
					tabPanel("Plot", plotlyOutput("wh_rdc"), height = "200px"),
					tabPanel("Data", div(dataTableOutput('wh_rdcData'), style="font-size: 16px"))
				       )
		   )
		)


#------------------------------------------------------------------------------------------------
## Performance : OTIF 
#------------------------------------------------------------------------------------------------
# otif_row1 <- fluidRow(
#   tags$head(
#      tags$style(type='text/css', 
#                 ".nav-tabs {font-size: 24px; font-weight: bold}")
#   )  
#   ,tabBox(
#    width = 6,
#    #title = tags$h2("OTIF"), 
#    tabPanel("Plot", plotlyOutput("otif_wk"), height = "200px"),
#    tabPanel("Data", div(dataTableOutput('otif_wkData'), style="font-size: 16px"))
#   )
# )

otif_row1 <- fluidRow(
		box(
			width = 9 
			,title = "OTIF: Weekly Trend"
			,status = "primary" 
			,solidHeader = TRUE 
			,collapsible = TRUE 

			,tags$head(
				tags$style(type='text/css', 
					".nav-tabs {font-size: 24px; font-weight: bold}")
				)  
			,tabBox(
				width = 12,
			#title = tags$h2("OTIF"), 
				tabPanel("Plot", plotlyOutput("otif_wk"), height = "200px"),
				tabPanel("Data", div(dataTableOutput('otif_wkData'), style="font-size: 16px"))
			       )
		   )
		)

otif_row2 <- fluidRow(
		box(
			width = 9 
			,title = "OTIF: By Plant Type"
			,status = "primary" 
			,solidHeader = TRUE 
			,collapsible = TRUE 
			,tableOutput("otif_by_plant")
		   )
		)

otif_row3 <- fluidRow(
		box(
			width = 9 
			,title = "OTIF: By Sold To Customer (Top 10 With Lowest OTIF)"
			,status = "primary" 
			,solidHeader = TRUE 
			,collapsible = TRUE 
			,tableOutput("otif_by_customer")
		   )
		)


#------------------------------------------------------------------------------------------------
## Performance : CARRIER 
#------------------------------------------------------------------------------------------------
carr_row1 <- fluidRow(
		box(
			width = 4
			,title = "Select Carrier"
			,selectizeInput("carr1",
				label = "Carriers",
				choices = unique(scar_n_by_carr$Carrier),
				multiple = F,
				options = list(maxItems = 1, placeholder = 'Select a name'),
				selected = "CH ROBINSON")
		   ),

		box(
			width = 8 
			,title = "Total SCARs By Carrier"
			,plotlyOutput("scar_n_carr", height = "300px")
		   )
		)

carr_row2 <- fluidRow(
		box(
			width = 4
			,title = "Select Carrier"
			,selectizeInput("carr2",
				label = "Carriers",
				choices = unique(scar_n_by_carr_plant$Carrier),
				multiple = F,
				options = list(maxItems = 1, placeholder = 'Select a name'),
				selected = "CH ROBINSON")
			,selectizeInput("plant2",
				label = "Plant",
				choices = unique(scar_n_by_carr_plant$Plant),
				multiple = F,
				options = list(maxItems = 1, placeholder = 'Select a name'),
				selected = "U001")
		   ),

		box(
			width = 8 
			,title = "Total SCARs By Carrier By Plant"
			,plotlyOutput("scar_n_carr_plant", height = "300px")
		   )
		)

#==============================================================================================

#########
## body 
#########
body <- dashboardBody(
                      tabItems(
                        tabItem(tabName = "hsse_tab",
                                hsse_row1, 
                                hsse_row2, 
                                hsse_row3,
                                
                                # css style
                                tags$head(tags$style(HTML('
                                      .main-sidebar {
                                      font-family: "Georgia", Times, "Times New Roman", serif;
                                      font-weight: bold;
                                      font-size: 24px;
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

                        ),
                        
                        tabItem(tabName = "finance_tab",
                                fin_row1, 
                                fin_row2, 
                               
                                #CSS 
                                tags$style(HTML("
                                                
                                                .box.box-solid.box-primary>.box-header {
                                                }
                                                
                                                .box.box-solid.box-danger{
                                                  background:#222D32
                                                }

                                                .box-header h3 {
                                                  font-weight: bold;
                                                }
                                                
                                                "))
                                
                        ),
                        
                        tabItem(tabName = "otif_tab",
				otif_row1,
				otif_row2,
				otif_row3,

                                # css style
                                tags$head(tags$style(HTML('
                                      .main-sidebar {
                                      font-family: "Georgia", Times, "Times New Roman", serif;
                                      font-weight: bold;
                                      font-size: 24px;
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
                        ),

                        tabItem(tabName = "wh_tab",
				wh_row1,

                                # css style
                                tags$head(tags$style(HTML('
                                      .main-sidebar {
                                      font-family: "Georgia", Times, "Times New Roman", serif;
                                      font-weight: bold;
                                      font-size: 24px;
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
                        ),

                        tabItem(tabName = "carr_tab",
                                h2("Total SCARs by Carrier"),
				carr_row1,
                                h2("Total SCARs by Carrier by Plant"),
				carr_row2,
				
                                # css style
                                tags$head(tags$style(HTML('
                                      .main-sidebar {
                                      font-family: "Georgia", Times, "Times New Roman", serif;
                                      font-weight: bold;
                                      font-size: 24px;
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
                        
                      ) #tabItems end
                    ) #dashboardBoday end



##################################################################################################
# UI and Server
##################################################################################################
########
## ui 
########
ui <- dashboardPage(title = 'LSC Logistics', header, sidebar, body, skin='red')

############
## server 
############
server <- function(input, output) { 
  #----------------------------------------
  # HSSE output
  #----------------------------------------
  output$hsse_v1 <- renderValueBox({
    valueBox(
      formatC("DC")
      , "Distribution Center"
      ,icon = icon("thumbs-up")
      ,color = "green")  
  })
  output$hsse_v2 <- renderValueBox({ 
    valueBox(
      formatC("RDC")
      , "Regional Dist. Center"
      ,icon = icon("thumbs-up")
      ,color = "green")  
  })
  output$hsse_v3 <- renderValueBox({
    valueBox(
      formatC("PACK TRANS")
      , "Pack Transportation"
      ,icon = icon("thumbs-up")
      ,color = "green")  
  })
  #a = 5
  #if (a==5) co="blue"
  output$hsse_v4 <- renderValueBox({
    valueBox(
      formatC("BULK TRANS")
      , "Bulk Transportation"
      ,icon = icon("thumbs-down")
      ,color = "red")   
  })
  
  #creating the plotOutput content
  output$hsse_dc <- renderPlotly({
     p <- plot_ly(x = data_dc$Type, y = data_dc$y2017, type = 'bar', name = '2017',
                 text=data_dc$y2017, textposition='auto',
                 marker = list(color='rgb(255, 213, 0)')) %>%
          add_trace(y = data_dc$y2018, name = '2018', text=data_dc$y2018,
                marker = list(color = "rgb(237, 28, 36)")) %>%
          layout(yaxis = list(title = 'Count'), barmode = 'group', font=list(size=15))
     
     p$elementId <- NULL
     p
  })

  output$hsse_rdc <- renderPlotly({
     p <- plot_ly( x = data_rdc$Type, y = data_rdc$y2017, type = 'bar', name = '2017',
                 text=data_rdc$y2017, textposition='auto',
                 marker = list(color='rgb(255, 213, 0)')) %>%
          add_trace(y = data_rdc$y2018, name = '2018', text=data_rdc$y2018, textposition='auto',
                marker = list(color = "rgb(237, 28, 36)")) %>%
          layout(yaxis = list(title = 'Count'), barmode = 'group', font=list(size=15))
     
     p$elementId <- NULL
     p
  })
  
  output$hsse_pack <- renderPlotly({
     p <- plot_ly( x = data_pack$Type, y = data_pack$y2017, type = 'bar', name = '2017',
                 text=data_pack$y2017, textposition='auto',
                 marker = list(color='rgb(255, 213, 0)')) %>%
          add_trace(y = data_pack$y2018, name = '2018', text=data_pack$y2018, textposition='auto',
                    marker = list(color = "rgb(237, 28, 36)")) %>%
          layout(yaxis = list(title = 'Count'), barmode = 'group', font=list(size=15))
     
     p$elementId <- NULL
     p
  })
  
  output$hsse_bulk <- renderPlotly({
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
  output$hsse_dcData   <-renderDataTable({ dc_data })
  output$hsse_rdcData  <-renderDataTable({ rdc_data })
  output$hsse_packData <-renderDataTable({ data_pack %>% dplyr::rename(year_2017=y2017, year_2018=y2018)})
  output$hsse_bulkData <-renderDataTable({ data_bulk %>% dplyr::rename(year_2017=y2017, year_2018=y2018)})
  
  #----------------------------------------
  # Finance output
  #----------------------------------------
  opex_col = ifelse(fin_data[7,"now_est"] > fin_data[7, "now_plan"], "red", "green")
  output$fin_opex <- renderUI({
         
    HTML(paste0("<span style=\"color: #FFFFFF; font-size: 28pt; font-weight: bold\">", 
                formatC(fin_data[7, "now_est"], format="f", big.mark=",",digits=2), "</span>",
                "<span style=\"color:", opex_col, "; font-size: 20pt\">", " (", 
                formatC(fin_data[7, "now_plan_var"], format="f", big.mark=",", digits=2), ", ",
                formatC(fin_data[7, "now_plan_var_pct"]*100, format="d", big.mark=",", digits=0), 
                "%)", "</span>")
        )
    
  })
  
  unit_opex_col = ifelse(fin_data[8,"now_est"] > fin_data[8, "now_plan"], "red", "green")
  output$fin_unit_opex <- renderUI({
         
    HTML(paste0("<span style=\"color: #FFFFFF; font-size: 28pt; font-weight: bold\">", 
                formatC(fin_data[8, "now_est"], format="f", big.mark=",",digits=2), "</span>",
                "<span style=\"color:", unit_opex_col, "; font-size: 20pt\">", " (", 
                formatC(fin_data[8, "now_plan_var"], format="f", big.mark=",", digits=2), ", ",
                formatC(fin_data[8, "now_plan_var_pct"]*100, format="d", big.mark=",", digits=0), 
                "%)", "</span>")
        )
    
  })
  
  log_col = ifelse(fin_data[11,"now_est"] > fin_data[11, "now_plan"], "red", "green")
  output$fin_log <- renderUI({
         
    HTML(paste0("<span style=\"color: #FFFFFF; font-size: 28pt; font-weight: bold\">", 
                formatC(fin_data[11, "now_est"], format="f", big.mark=",",digits=2), "</span>",
                "<span style=\"color:", log_col, "; font-size: 20pt\">", " (", 
                formatC(fin_data[11, "now_plan_var"], format="f", big.mark=",", digits=2), ", ",
                formatC(fin_data[11, "now_plan_var_pct"]*100, format="d", big.mark=",", digits=0), 
                "%)", "</span>")
        )
    
  })
  
  ## Opex------------------------------------------------------------------------------------------  
   output$fin_opex_now <- renderPlotly({
    col1 = ifelse(fin_data[7,"now_est"]>fin_data[7, "now_plan"], "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = this_mon, 
		     y_title = "Opex (mln$)",
		     x_val = c("Est","Plan"), 
                     y_val = c(fin_data[7,"now_est"], fin_data[7, "now_plan"]),
                     col = c(col1, "rgb(255,213,0)"),
                     text = c(formatC(fin_data[7,"now_est"],format="f", digits=1), formatC(fin_data[7, "now_plan"],format="f",digits=1)) 
	            )
    
     p$elementId <- NULL
     p
   })

   output$fin_opex_last <- renderPlotly({
    col1 = ifelse(fin_data[7,"last_act"]>fin_data[7, "last_plan"], "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = last_mon, 
		     y_title = "Opex (mln$)",
		     x_val = c("Actual","Plan"), 
      		     y_val = c(fin_data[7,"last_act"], fin_data[7, "last_plan"]),
                     col = c(col1, "rgb(255,213,0)"),
                     text=c(formatC(fin_data[7,"last_act"],format="f",digits=1), formatC(fin_data[7, "last_plan"],format="f", digits=1))
	            )
     p$elementId <- NULL
     p
   })
  
   output$fin_opex_ytd <- renderPlotly({
    col1 = ifelse(fin_data[7,"ytd"]>fin_data[7, "ytd_plan"], "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = "YTD", 
		     y_title = "Opex (mln$)",
		     x_val = c("Actual","Plan"), 
                     y_val = c(fin_data[7,"ytd"], fin_data[7, "ytd_plan"]),
                     col = c(col1, "rgb(255,213,0)"),
                     text=c(formatC(fin_data[7,"ytd"],format="f",digits=1), formatC(fin_data[7, "ytd_plan"],format="f", digits=1)) 
	            )
     
     p$elementId <- NULL
     p
     
   })
  
   output$fin_opex_fy <- renderPlotly({
    col1 = ifelse(fin_data[7,"latest_est"]>fin_data[7, "fy_plan"], "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = "Full Year", 
		     y_title = "Opex (mln$)",
                     x_val = c("Latest Est", "Plan"),
                     y_val = c(fin_data[7,"latest_est"], fin_data[7, "fy_plan"]),
                     col = c(col1, "rgb(255,213,0)"),
                     text=c(formatC(fin_data[7,"latest_est"],format="f",digits=1), formatC(fin_data[7, "fy_plan"],format="f", digits=1))
	            )
     p$elementId <- NULL
     p
     
   })
   
   ## -----------------------------------------
   ## Opex Var 
   ## -----------------------------------------
   output$fin_opex_now_var <- renderPlotly({
    col1 = ifelse(fin_data[7,"now_plan_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    col2 = ifelse(fin_data[7,"now_py_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = this_mon, 
		     y_title = "Var (%)",
		     x_val = c("2018","2017"), 
                     y_val = c(fin_data[7,"now_plan_var_pct"]*100, fin_data[7, "now_py_var_pct"]*100),
                     col = c(col1, col2),
                     text=c(formatC(fin_data[7,"now_plan_var_pct"]*100,format="f", digits=0), formatC(fin_data[7, "now_py_var_pct"]*100,format="f",digits=0))
	            )

     p$elementId <- NULL
     p
   })

   output$fin_opex_last_var <- renderPlotly({
    col1 = ifelse(fin_data[7,"last_plan_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    col2 = ifelse(fin_data[7,"last_py_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = last_mon, 
		     y_title = "Var (%)",
		     x_val = c("2018","2017"), 
                     y_val = c(fin_data[7,"last_plan_var_pct"]*100, fin_data[7, "last_py_var_pct"]*100),
                     col = c(col1, col2),
      	             text=c(formatC(fin_data[7,"last_plan_var_pct"]*100,format="f", digits=0), formatC(fin_data[7, "last_py_var_pct"]*100,format="f", digits=0))
		   )
    
     p$elementId <- NULL
     p
   })
  
   output$fin_opex_ytd_var <- renderPlotly({
    col1 = ifelse(fin_data[7,"ytd_plan_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    col2 = ifelse(fin_data[7,"ytd_py_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = "YTD", 
		     y_title = "Var (%)",
                     x_val = c("2018", "2017"),
                     y_val = c(fin_data[7,"ytd_plan_var_pct"]*100, fin_data[7, "ytd_py_var_pct"]*100),
                     col = c(col1, col2),
                     text=c(formatC(fin_data[7,"ytd_plan_var_pct"]*100,format="f", digits=0), formatC(fin_data[7, "ytd_py_var_pct"]*100,format="f", digits=0))
	            )
     
     p$elementId <- NULL
     p
   })
  
   output$fin_opex_fy_var <- renderPlotly({
    col1 = ifelse(fin_data[7,"fy_plan_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    col2 = ifelse(fin_data[7,"py_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = "Full Year", 
		     y_title = "Var (%)",
                     x_val = c("2018", "2017"),
                     y_val = c(fin_data[7,"fy_plan_var_pct"]*100, fin_data[7, "py_var_pct"]*100),
                     col = c(col1, col2),
                     text=c(formatC(fin_data[7,"fy_plan_var_pct"]*100,format="f", digits=0), formatC(fin_data[7, "py_var_pct"]*100,format="f", digits=0))
	            )
     
     p$elementId <- NULL
     p
     
   })
   
  ## Sales Vol------------------------------------------------------------------------------------------  
   output$fin_sales_now <- renderPlotly({
    col1 = ifelse(fin_data[1,"now_est"]<fin_data[1, "now_plan"], "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = this_mon, 
		     y_title = "Sales Volume (mln gal)",
		     x_val = c("Est","Plan"), 
                     y_val = c(fin_data[1,"now_est"], fin_data[1, "now_plan"]),
                     col = c(col1, "rgb(255,213,0)"),
                     text = c(formatC(fin_data[1,"now_est"],format="f", digits=1), formatC(fin_data[1, "now_plan"],format="f",digits=1)) 
	            )
    
     p$elementId <- NULL
     p
   })

   output$fin_sales_last <- renderPlotly({
    col1 = ifelse(fin_data[1,"last_act"]<fin_data[1, "last_plan"], "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = last_mon, 
		     y_title = "Sales Volume (mln gal)",
		     x_val = c("Actual","Plan"), 
      		     y_val = c(fin_data[1,"last_act"], fin_data[1, "last_plan"]),
                     col = c(col1, "rgb(255,213,0)"),
                     text=c(formatC(fin_data[1,"last_act"],format="f",digits=1), formatC(fin_data[1, "last_plan"],format="f", digits=1))
	            )
     p$elementId <- NULL
     p
   })
  
   output$fin_sales_ytd <- renderPlotly({
    col1 = ifelse(fin_data[1,"ytd"]<fin_data[1, "ytd_plan"], "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = "YTD", 
		     y_title = "Sales Volume (mln gal)",
		     x_val = c("Actual","Plan"), 
                     y_val = c(fin_data[1,"ytd"], fin_data[1, "ytd_plan"]),
                     col = c(col1, "rgb(255,213,0)"),
                     text=c(formatC(fin_data[1,"ytd"],format="f",digits=1), formatC(fin_data[1, "ytd_plan"],format="f", digits=1)) 
	            )
     
     p$elementId <- NULL
     p
     
   })
  
   output$fin_sales_fy <- renderPlotly({
    col1 = ifelse(fin_data[1,"latest_est"]<fin_data[1, "fy_plan"], "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = "Full Year", 
		     y_title = "Sales Volume (mln gal)",
                     x_val = c("Latest Est", "Plan"),
                     y_val = c(fin_data[1,"latest_est"], fin_data[1, "fy_plan"]),
                     col = c(col1, "rgb(255,213,0)"),
                     text=c(formatC(fin_data[1,"latest_est"],format="f",digits=1), formatC(fin_data[1, "fy_plan"],format="f", digits=1))
	            )
     p$elementId <- NULL
     p
     
   })
   
   ## -----------------------------------------
   ## Sales  Var 
   ## -----------------------------------------
   output$fin_sales_now_var <- renderPlotly({
    col1 = ifelse(fin_data[1,"now_plan_var_pct"]<0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    col2 = ifelse(fin_data[1,"now_py_var_pct"]<0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = this_mon, 
		     y_title = "Var (%)",
		     x_val = c("2018","2017"), 
                     y_val = c(fin_data[1,"now_plan_var_pct"]*100, fin_data[1, "now_py_var_pct"]*100),
                     col = c(col1, col2),
                     text=c(formatC(fin_data[1,"now_plan_var_pct"]*100,format="f", digits=0), formatC(fin_data[1, "now_py_var_pct"]*100,format="f",digits=0))
	            )

     p$elementId <- NULL
     p
   })

   output$fin_sales_last_var <- renderPlotly({
    col1 = ifelse(fin_data[1,"last_plan_var_pct"]<0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    col2 = ifelse(fin_data[1,"last_py_var_pct"]<0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = last_mon, 
		     y_title = "Var (%)",
		     x_val = c("2018","2017"), 
                     y_val = c(fin_data[1,"last_plan_var_pct"]*100, fin_data[1, "last_py_var_pct"]*100),
                     col = c(col1, col2),
      	             text=c(formatC(fin_data[1,"last_plan_var_pct"]*100,format="f", digits=0), formatC(fin_data[1, "last_py_var_pct"]*100,format="f", digits=0))
		   )
    
     p$elementId <- NULL
     p
   })
  
   output$fin_sales_ytd_var <- renderPlotly({
    col1 = ifelse(fin_data[1,"ytd_plan_var_pct"]<0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    col2 = ifelse(fin_data[1,"ytd_py_var_pct"]<0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = "YTD", 
		     y_title = "Var (%)",
                     x_val = c("2018", "2017"),
                     y_val = c(fin_data[1,"ytd_plan_var_pct"]*100, fin_data[1, "ytd_py_var_pct"]*100),
                     col = c(col1, col2),
                     text=c(formatC(fin_data[1,"ytd_plan_var_pct"]*100,format="f", digits=0), formatC(fin_data[1, "ytd_py_var_pct"]*100,format="f", digits=0))
	            )
     
     p$elementId <- NULL
     p
   })
  
   output$fin_sales_fy_var <- renderPlotly({
    col1 = ifelse(fin_data[1,"fy_plan_var_pct"]<0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    col2 = ifelse(fin_data[1,"py_var_pct"]<0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = "Full Year", 
		     y_title = "Var (%)",
                     x_val = c("2018", "2017"),
                     y_val = c(fin_data[1,"fy_plan_var_pct"]*100, fin_data[1, "py_var_pct"]*100),
                     col = c(col1, col2),
                     text=c(formatC(fin_data[1,"fy_plan_var_pct"]*100,format="f", digits=0), formatC(fin_data[1, "py_var_pct"]*100,format="f", digits=0))
	            )
     
     p$elementId <- NULL
     p
     
   })


  ## Primary Freight-------------------------------------------------------------------------------  
   output$fin_1fr_now <- renderPlotly({
    col1 = ifelse(fin_data[2,"now_est"]>fin_data[2, "now_plan"], "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = this_mon, 
		     y_title = "Primary Freight (mln$)",
		     x_val = c("Est","Plan"), 
                     y_val = c(fin_data[2,"now_est"], fin_data[2, "now_plan"]),
                     col = c(col1, "rgb(255,213,0)"),
                     text = c(formatC(fin_data[2,"now_est"],format="f", digits=1), formatC(fin_data[2, "now_plan"],format="f",digits=1)) 
	            )
    
     p$elementId <- NULL
     p
   })

   output$fin_1fr_last <- renderPlotly({
    col1 = ifelse(fin_data[2,"last_act"]>fin_data[2, "last_plan"], "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = last_mon, 
		     y_title = "Primary Freight (mln$)",
		     x_val = c("Actual","Plan"), 
      		     y_val = c(fin_data[2,"last_act"], fin_data[2, "last_plan"]),
                     col = c(col1, "rgb(255,213,0)"),
                     text=c(formatC(fin_data[2,"last_act"],format="f",digits=1), formatC(fin_data[2, "last_plan"],format="f", digits=1))
	            )
     p$elementId <- NULL
     p
   })
  
   output$fin_1fr_ytd <- renderPlotly({
    col1 = ifelse(fin_data[2,"ytd"]>fin_data[2, "ytd_plan"], "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = "YTD", 
		     y_title = "Primary Freight (mln$)",
		     x_val = c("Actual","Plan"), 
                     y_val = c(fin_data[2,"ytd"], fin_data[2, "ytd_plan"]),
                     col = c(col1, "rgb(255,213,0)"),
                     text=c(formatC(fin_data[2,"ytd"],format="f",digits=1), formatC(fin_data[2, "ytd_plan"],format="f", digits=1)) 
	            )
     
     p$elementId <- NULL
     p
     
   })
  
   output$fin_1fr_fy <- renderPlotly({
    col1 = ifelse(fin_data[2,"latest_est"]>fin_data[2, "fy_plan"], "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = "Full Year", 
		     y_title = "Primary Freight (mln$)",
                     x_val = c("Latest Est", "Plan"),
                     y_val = c(fin_data[2,"latest_est"], fin_data[2, "fy_plan"]),
                     col = c(col1, "rgb(255,213,0)"),
                     text=c(formatC(fin_data[2,"latest_est"],format="f",digits=1), formatC(fin_data[2, "fy_plan"],format="f", digits=1))
	            )
     p$elementId <- NULL
     p
     
   })
   
   ## -----------------------------------------
   ## Primary Freight Var 
   ## -----------------------------------------
   output$fin_1fr_now_var <- renderPlotly({
    col1 = ifelse(fin_data[2,"now_plan_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    col2 = ifelse(fin_data[2,"now_py_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = this_mon, 
		     y_title = "Var (%)",
		     x_val = c("2018","2017"), 
                     y_val = c(fin_data[2,"now_plan_var_pct"]*100, fin_data[2, "now_py_var_pct"]*100),
                     col = c(col1, col2),
                     text=c(formatC(fin_data[2,"now_plan_var_pct"]*100,format="f", digits=0), formatC(fin_data[2, "now_py_var_pct"]*100,format="f",digits=0))
	            )

     p$elementId <- NULL
     p
   })

   output$fin_1fr_last_var <- renderPlotly({
    col1 = ifelse(fin_data[2,"last_plan_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    col2 = ifelse(fin_data[2,"last_py_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = last_mon, 
		     y_title = "Var (%)",
		     x_val = c("2018","2017"), 
                     y_val = c(fin_data[2,"last_plan_var_pct"]*100, fin_data[2, "last_py_var_pct"]*100),
                     col = c(col1, col2),
      	             text=c(formatC(fin_data[2,"last_plan_var_pct"]*100,format="f", digits=0), formatC(fin_data[2, "last_py_var_pct"]*100,format="f", digits=0))
		   )
    
     p$elementId <- NULL
     p
   })
  
   output$fin_1fr_ytd_var <- renderPlotly({
    col1 = ifelse(fin_data[2,"ytd_plan_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    col2 = ifelse(fin_data[2,"ytd_py_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = "YTD", 
		     y_title = "Var (%)",
                     x_val = c("2018", "2017"),
                     y_val = c(fin_data[2,"ytd_plan_var_pct"]*100, fin_data[2, "ytd_py_var_pct"]*100),
                     col = c(col1, col2),
                     text=c(formatC(fin_data[2,"ytd_plan_var_pct"]*100,format="f", digits=0), formatC(fin_data[2, "ytd_py_var_pct"]*100,format="f", digits=0))
	            )
     
     p$elementId <- NULL
     p
   })
  
   output$fin_1fr_fy_var <- renderPlotly({
    col1 = ifelse(fin_data[2,"fy_plan_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    col2 = ifelse(fin_data[2,"py_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = "Full Year", 
		     y_title = "Var (%)",
                     x_val = c("2018", "2017"),
                     y_val = c(fin_data[2,"fy_plan_var_pct"]*100, fin_data[2, "py_var_pct"]*100),
                     col = c(col1, col2),
                     text=c(formatC(fin_data[2,"fy_plan_var_pct"]*100,format="f", digits=0), formatC(fin_data[2, "py_var_pct"]*100,format="f", digits=0))
	            )
     
     p$elementId <- NULL
     p
     
   })
   

  ## Secondary Freight----------------------------------------------------------------------------  
   output$fin_2fr_now <- renderPlotly({
    col1 = ifelse(fin_data[3,"now_est"]>fin_data[3, "now_plan"], "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = this_mon, 
		     y_title = "Secondary Freight (mln$)",
		     x_val = c("Est","Plan"), 
                     y_val = c(fin_data[3,"now_est"], fin_data[3, "now_plan"]),
                     col = c(col1, "rgb(255,213,0)"),
                     text = c(formatC(fin_data[3,"now_est"],format="f", digits=1), formatC(fin_data[3, "now_plan"],format="f",digits=1)) 
	            )
    
     p$elementId <- NULL
     p
   })

   output$fin_2fr_last <- renderPlotly({
    col1 = ifelse(fin_data[3,"last_act"]>fin_data[3, "last_plan"], "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = last_mon, 
		     y_title = "Secondary Freight (mln$)",
		     x_val = c("Actual","Plan"), 
      		     y_val = c(fin_data[3,"last_act"], fin_data[3, "last_plan"]),
                     col = c(col1, "rgb(255,213,0)"),
                     text=c(formatC(fin_data[3,"last_act"],format="f",digits=1), formatC(fin_data[3, "last_plan"],format="f", digits=1))
	            )
     p$elementId <- NULL
     p
   })
  
   output$fin_2fr_ytd <- renderPlotly({
    col1 = ifelse(fin_data[3,"ytd"]>fin_data[3, "ytd_plan"], "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = "YTD", 
		     y_title = "Secondary Freight (mln$)",
		     x_val = c("Actual","Plan"), 
                     y_val = c(fin_data[3,"ytd"], fin_data[3, "ytd_plan"]),
                     col = c(col1, "rgb(255,213,0)"),
                     text=c(formatC(fin_data[3,"ytd"],format="f",digits=1), formatC(fin_data[3, "ytd_plan"],format="f", digits=1)) 
	            )
     
     p$elementId <- NULL
     p
     
   })
  
   output$fin_2fr_fy <- renderPlotly({
    col1 = ifelse(fin_data[3,"latest_est"]>fin_data[3, "fy_plan"], "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = "Full Year", 
		     y_title = "Secondary Freight (mln$)",
                     x_val = c("Latest Est", "Plan"),
                     y_val = c(fin_data[3,"latest_est"], fin_data[3, "fy_plan"]),
                     col = c(col1, "rgb(255,213,0)"),
                     text=c(formatC(fin_data[3,"latest_est"],format="f",digits=1), formatC(fin_data[3, "fy_plan"],format="f", digits=1))
	            )
     p$elementId <- NULL
     p
     
   })
   
   ## -----------------------------------------
   ## Secondary Freight Var 
   ## -----------------------------------------
   output$fin_2fr_now_var <- renderPlotly({
    col1 = ifelse(fin_data[3,"now_plan_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    col2 = ifelse(fin_data[3,"now_py_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = this_mon, 
		     y_title = "Var (%)",
		     x_val = c("2018","2017"), 
                     y_val = c(fin_data[3,"now_plan_var_pct"]*100, fin_data[3, "now_py_var_pct"]*100),
                     col = c(col1, col2),
                     text=c(formatC(fin_data[3,"now_plan_var_pct"]*100,format="f", digits=0), formatC(fin_data[3, "now_py_var_pct"]*100,format="f",digits=0))
	            )

     p$elementId <- NULL
     p
   })

   output$fin_2fr_last_var <- renderPlotly({
    col1 = ifelse(fin_data[3,"last_plan_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    col2 = ifelse(fin_data[3,"last_py_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = last_mon, 
		     y_title = "Var (%)",
		     x_val = c("2018","2017"), 
                     y_val = c(fin_data[3,"last_plan_var_pct"]*100, fin_data[3, "last_py_var_pct"]*100),
                     col = c(col1, col2),
      	             text=c(formatC(fin_data[3,"last_plan_var_pct"]*100,format="f", digits=0), formatC(fin_data[3, "last_py_var_pct"]*100,format="f", digits=0))
		   )
    
     p$elementId <- NULL
     p
   })
  
   output$fin_2fr_ytd_var <- renderPlotly({
    col1 = ifelse(fin_data[3,"ytd_plan_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    col2 = ifelse(fin_data[3,"ytd_py_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = "YTD", 
		     y_title = "Var (%)",
                     x_val = c("2018", "2017"),
                     y_val = c(fin_data[3,"ytd_plan_var_pct"]*100, fin_data[3, "ytd_py_var_pct"]*100),
                     col = c(col1, col2),
                     text=c(formatC(fin_data[3,"ytd_plan_var_pct"]*100,format="f", digits=0), formatC(fin_data[3, "ytd_py_var_pct"]*100,format="f", digits=0))
	            )
     
     p$elementId <- NULL
     p
   })
  
   output$fin_2fr_fy_var <- renderPlotly({
    col1 = ifelse(fin_data[3,"fy_plan_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    col2 = ifelse(fin_data[3,"py_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = "Full Year", 
		     y_title = "Var (%)",
                     x_val = c("2018", "2017"),
                     y_val = c(fin_data[3,"fy_plan_var_pct"]*100, fin_data[3, "py_var_pct"]*100),
                     col = c(col1, col2),
                     text=c(formatC(fin_data[3,"fy_plan_var_pct"]*100,format="f", digits=0), formatC(fin_data[3, "py_var_pct"]*100,format="f", digits=0))
	            )
     
     p$elementId <- NULL
     p
     
   })

  ## Secondary Freight DT ----------------------------------------------------------------------------  
   output$fin_2frdt_now <- renderPlotly({
    col1 = ifelse(fin_data[4,"now_est"]>fin_data[4, "now_plan"], "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = this_mon, 
		     y_title = "Direct Trans (mln$)",
		     x_val = c("Est","Plan"), 
                     y_val = c(fin_data[4,"now_est"], fin_data[4, "now_plan"]),
                     col = c(col1, "rgb(255,213,0)"),
                     text = c(formatC(fin_data[4,"now_est"],format="f", digits=1), formatC(fin_data[4, "now_plan"],format="f",digits=1)) 
	            )
    
     p$elementId <- NULL
     p
   })

   output$fin_2frdt_last <- renderPlotly({
    col1 = ifelse(fin_data[4,"last_act"]>fin_data[4, "last_plan"], "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = last_mon, 
		     y_title = "Direct Trans (mln$)",
		     x_val = c("Actual","Plan"), 
      		     y_val = c(fin_data[4,"last_act"], fin_data[4, "last_plan"]),
                     col = c(col1, "rgb(255,213,0)"),
                     text=c(formatC(fin_data[4,"last_act"],format="f",digits=1), formatC(fin_data[4, "last_plan"],format="f", digits=1))
	            )
     p$elementId <- NULL
     p
   })
  
   output$fin_2frdt_ytd <- renderPlotly({
    col1 = ifelse(fin_data[4,"ytd"]>fin_data[4, "ytd_plan"], "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = "YTD", 
		     y_title = "Direct Trans (mln$)",
		     x_val = c("Actual","Plan"), 
                     y_val = c(fin_data[4,"ytd"], fin_data[4, "ytd_plan"]),
                     col = c(col1, "rgb(255,213,0)"),
                     text=c(formatC(fin_data[4,"ytd"],format="f",digits=1), formatC(fin_data[4, "ytd_plan"],format="f", digits=1)) 
	            )
     
     p$elementId <- NULL
     p
     
   })
  
   output$fin_2frdt_fy <- renderPlotly({
    col1 = ifelse(fin_data[4,"latest_est"]>fin_data[4, "fy_plan"], "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = "Full Year", 
		     y_title = "Direct Trans (mln$)",
                     x_val = c("Latest Est", "Plan"),
                     y_val = c(fin_data[4,"latest_est"], fin_data[4, "fy_plan"]),
                     col = c(col1, "rgb(255,213,0)"),
                     text=c(formatC(fin_data[4,"latest_est"],format="f",digits=1), formatC(fin_data[4, "fy_plan"],format="f", digits=1))
	            )
     p$elementId <- NULL
     p
     
   })
   
   ## -----------------------------------------
   ## Secondary Freight DT Var 
   ## -----------------------------------------
   output$fin_2frdt_now_var <- renderPlotly({
    col1 = ifelse(fin_data[4,"now_plan_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    col2 = ifelse(fin_data[4,"now_py_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = this_mon, 
		     y_title = "Var (%)",
		     x_val = c("2018","2017"), 
                     y_val = c(fin_data[4,"now_plan_var_pct"]*100, fin_data[4, "now_py_var_pct"]*100),
                     col = c(col1, col2),
                     text=c(formatC(fin_data[4,"now_plan_var_pct"]*100,format="f", digits=0), formatC(fin_data[4, "now_py_var_pct"]*100,format="f",digits=0))
	            )

     p$elementId <- NULL
     p
   })

   output$fin_2frdt_last_var <- renderPlotly({
    col1 = ifelse(fin_data[4,"last_plan_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    col2 = ifelse(fin_data[4,"last_py_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = last_mon, 
		     y_title = "Var (%)",
		     x_val = c("2018","2017"), 
                     y_val = c(fin_data[4,"last_plan_var_pct"]*100, fin_data[4, "last_py_var_pct"]*100),
                     col = c(col1, col2),
      	             text=c(formatC(fin_data[4,"last_plan_var_pct"]*100,format="f", digits=0), formatC(fin_data[4, "last_py_var_pct"]*100,format="f", digits=0))
		   )
    
     p$elementId <- NULL
     p
   })
  
   output$fin_2frdt_ytd_var <- renderPlotly({
    col1 = ifelse(fin_data[4,"ytd_plan_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    col2 = ifelse(fin_data[4,"ytd_py_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = "YTD", 
		     y_title = "Var (%)",
                     x_val = c("2018", "2017"),
                     y_val = c(fin_data[4,"ytd_plan_var_pct"]*100, fin_data[4, "ytd_py_var_pct"]*100),
                     col = c(col1, col2),
                     text=c(formatC(fin_data[4,"ytd_plan_var_pct"]*100,format="f", digits=0), formatC(fin_data[4, "ytd_py_var_pct"]*100,format="f", digits=0))
	            )
     
     p$elementId <- NULL
     p
   })
  
   output$fin_2frdt_fy_var <- renderPlotly({
    col1 = ifelse(fin_data[4,"fy_plan_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    col2 = ifelse(fin_data[4,"py_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = "Full Year", 
		     y_title = "Var (%)",
                     x_val = c("2018", "2017"),
                     y_val = c(fin_data[4,"fy_plan_var_pct"]*100, fin_data[4, "py_var_pct"]*100),
                     col = c(col1, col2),
                     text=c(formatC(fin_data[4,"fy_plan_var_pct"]*100,format="f", digits=0), formatC(fin_data[4, "py_var_pct"]*100,format="f", digits=0))
	            )
     
     p$elementId <- NULL
     p
     
   })


  ## Secondary Freight DFOA ----------------------------------------------------------------------------  
   output$fin_2frdfoa_now <- renderPlotly({
    col1 = ifelse(fin_data[5,"now_est"]>fin_data[5, "now_plan"], "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = this_mon, 
		     y_title = "DFOA (mln$)",
		     x_val = c("Est","Plan"), 
                     y_val = c(fin_data[5,"now_est"], fin_data[5, "now_plan"]),
                     col = c(col1, "rgb(255,213,0)"),
                     text = c(formatC(fin_data[5,"now_est"],format="f", digits=1), formatC(fin_data[5, "now_plan"],format="f",digits=1)) 
	            )
    
     p$elementId <- NULL
     p
   })

   output$fin_2frdfoa_last <- renderPlotly({
    col1 = ifelse(fin_data[5,"last_act"]>fin_data[5, "last_plan"], "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = last_mon, 
		     y_title = "DFOA (mln$)",
		     x_val = c("Actual","Plan"), 
      		     y_val = c(fin_data[5,"last_act"], fin_data[5, "last_plan"]),
                     col = c(col1, "rgb(255,213,0)"),
                     text=c(formatC(fin_data[5,"last_act"],format="f",digits=1), formatC(fin_data[5, "last_plan"],format="f", digits=1))
	            )
     p$elementId <- NULL
     p
   })
  
   output$fin_2frdfoa_ytd <- renderPlotly({
    col1 = ifelse(fin_data[5,"ytd"]>fin_data[5, "ytd_plan"], "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = "YTD", 
		     y_title = "DFOA (mln$)",
		     x_val = c("Actual","Plan"), 
                     y_val = c(fin_data[5,"ytd"], fin_data[5, "ytd_plan"]),
                     col = c(col1, "rgb(255,213,0)"),
                     text=c(formatC(fin_data[5,"ytd"],format="f",digits=1), formatC(fin_data[5, "ytd_plan"],format="f", digits=1)) 
	            )
     
     p$elementId <- NULL
     p
     
   })
  
   output$fin_2frdfoa_fy <- renderPlotly({
    col1 = ifelse(fin_data[5,"latest_est"]>fin_data[5, "fy_plan"], "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = "Full Year", 
		     y_title = "DFOA (mln$)",
                     x_val = c("Latest Est", "Plan"),
                     y_val = c(fin_data[5,"latest_est"], fin_data[5, "fy_plan"]),
                     col = c(col1, "rgb(255,213,0)"),
                     text=c(formatC(fin_data[5,"latest_est"],format="f",digits=1), formatC(fin_data[5, "fy_plan"],format="f", digits=1))
	            )
     p$elementId <- NULL
     p
     
   })
   
   ## -----------------------------------------
   ## Secondary Freight DFOA Var 
   ## -----------------------------------------
   output$fin_2frdfoa_now_var <- renderPlotly({
    col1 = ifelse(fin_data[5,"now_plan_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    col2 = ifelse(fin_data[5,"now_py_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = this_mon, 
		     y_title = "Var (%)",
		     x_val = c("2018","2017"), 
                     y_val = c(fin_data[5,"now_plan_var_pct"]*100, fin_data[5, "now_py_var_pct"]*100),
                     col = c(col1, col2),
                     text=c(formatC(fin_data[5,"now_plan_var_pct"]*100,format="f", digits=0), formatC(fin_data[5, "now_py_var_pct"]*100,format="f",digits=0))
	            )

     p$elementId <- NULL
     p
   })

   output$fin_2frdfoa_last_var <- renderPlotly({
    col1 = ifelse(fin_data[5,"last_plan_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    col2 = ifelse(fin_data[5,"last_py_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = last_mon, 
		     y_title = "Var (%)",
		     x_val = c("2018","2017"), 
                     y_val = c(fin_data[5,"last_plan_var_pct"]*100, fin_data[5, "last_py_var_pct"]*100),
                     col = c(col1, col2),
      	             text=c(formatC(fin_data[5,"last_plan_var_pct"]*100,format="f", digits=0), formatC(fin_data[5, "last_py_var_pct"]*100,format="f", digits=0))
		   )
    
     p$elementId <- NULL
     p
   })
  
   output$fin_2frdfoa_ytd_var <- renderPlotly({
    col1 = ifelse(fin_data[5,"ytd_plan_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    col2 = ifelse(fin_data[5,"ytd_py_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = "YTD", 
		     y_title = "Var (%)",
                     x_val = c("2018", "2017"),
                     y_val = c(fin_data[5,"ytd_plan_var_pct"]*100, fin_data[5, "ytd_py_var_pct"]*100),
                     col = c(col1, col2),
                     text=c(formatC(fin_data[5,"ytd_plan_var_pct"]*100,format="f", digits=0), formatC(fin_data[5, "ytd_py_var_pct"]*100,format="f", digits=0))
	            )
     
     p$elementId <- NULL
     p
   })
  
   output$fin_2frdfoa_fy_var <- renderPlotly({
    col1 = ifelse(fin_data[5,"fy_plan_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    col2 = ifelse(fin_data[5,"py_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = "Full Year", 
		     y_title = "Var (%)",
                     x_val = c("2018", "2017"),
                     y_val = c(fin_data[5,"fy_plan_var_pct"]*100, fin_data[5, "py_var_pct"]*100),
                     col = c(col1, col2),
                     text=c(formatC(fin_data[5,"fy_plan_var_pct"]*100,format="f", digits=0), formatC(fin_data[5, "py_var_pct"]*100,format="f", digits=0))
	            )
     
     p$elementId <- NULL
     p
     
   })


  ## S&H Expense----------------------------------------------------------------------------  
   output$fin_sh_now <- renderPlotly({
    col1 = ifelse(fin_data[6,"now_est"]>fin_data[6, "now_plan"], "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = this_mon, 
		     y_title = "S&H Expense (mln$)",
		     x_val = c("Est","Plan"), 
                     y_val = c(fin_data[6,"now_est"], fin_data[6, "now_plan"]),
                     col = c(col1, "rgb(255,213,0)"),
                     text = c(formatC(fin_data[6,"now_est"],format="f", digits=1), formatC(fin_data[6, "now_plan"],format="f",digits=1)) 
	            )
    
     p$elementId <- NULL
     p
   })

   output$fin_sh_last <- renderPlotly({
    col1 = ifelse(fin_data[6,"last_act"]>fin_data[6, "last_plan"], "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = last_mon, 
		     y_title = "S&H Expense (mln$)",
		     x_val = c("Actual","Plan"), 
      		     y_val = c(fin_data[6,"last_act"], fin_data[6, "last_plan"]),
                     col = c(col1, "rgb(255,213,0)"),
                     text=c(formatC(fin_data[6,"last_act"],format="f",digits=1), formatC(fin_data[6, "last_plan"],format="f", digits=1))
	            )
     p$elementId <- NULL
     p
   })
  
   output$fin_sh_ytd <- renderPlotly({
    col1 = ifelse(fin_data[6,"ytd"]>fin_data[6, "ytd_plan"], "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = "YTD", 
		     y_title = "S&H Expense (mln$)",
		     x_val = c("Actual","Plan"), 
                     y_val = c(fin_data[6,"ytd"], fin_data[6, "ytd_plan"]),
                     col = c(col1, "rgb(255,213,0)"),
                     text=c(formatC(fin_data[6,"ytd"],format="f",digits=1), formatC(fin_data[6, "ytd_plan"],format="f", digits=1)) 
	            )
     
     p$elementId <- NULL
     p
     
   })
  
   output$fin_sh_fy <- renderPlotly({
    col1 = ifelse(fin_data[6,"latest_est"]>fin_data[6, "fy_plan"], "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = "Full Year", 
		     y_title = "S&H Expense (mln$)",
                     x_val = c("Latest Est", "Plan"),
                     y_val = c(fin_data[6,"latest_est"], fin_data[6, "fy_plan"]),
                     col = c(col1, "rgb(255,213,0)"),
                     text=c(formatC(fin_data[6,"latest_est"],format="f",digits=1), formatC(fin_data[6, "fy_plan"],format="f", digits=1))
	            )
     p$elementId <- NULL
     p
     
   })
   
   ## -----------------------------------------
   ## S&H Var 
   ## -----------------------------------------
   output$fin_sh_now_var <- renderPlotly({
    col1 = ifelse(fin_data[6,"now_plan_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    col2 = ifelse(fin_data[6,"now_py_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = this_mon, 
		     y_title = "Var (%)",
		     x_val = c("2018","2017"), 
                     y_val = c(fin_data[6,"now_plan_var_pct"]*100, fin_data[6, "now_py_var_pct"]*100),
                     col = c(col1, col2),
                     text=c(formatC(fin_data[6,"now_plan_var_pct"]*100,format="f", digits=0), formatC(fin_data[6, "now_py_var_pct"]*100,format="f",digits=0))
	            )

     p$elementId <- NULL
     p
   })

   output$fin_sh_last_var <- renderPlotly({
    col1 = ifelse(fin_data[6,"last_plan_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    col2 = ifelse(fin_data[6,"last_py_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = last_mon, 
		     y_title = "Var (%)",
		     x_val = c("2018","2017"), 
                     y_val = c(fin_data[6,"last_plan_var_pct"]*100, fin_data[6, "last_py_var_pct"]*100),
                     col = c(col1, col2),
      	             text=c(formatC(fin_data[6,"last_plan_var_pct"]*100,format="f", digits=0), formatC(fin_data[6, "last_py_var_pct"]*100,format="f", digits=0))
		   )
    
     p$elementId <- NULL
     p
   })
  
   output$fin_sh_ytd_var <- renderPlotly({
    col1 = ifelse(fin_data[6,"ytd_plan_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    col2 = ifelse(fin_data[6,"ytd_py_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = "YTD", 
		     y_title = "Var (%)",
                     x_val = c("2018", "2017"),
                     y_val = c(fin_data[6,"ytd_plan_var_pct"]*100, fin_data[6, "ytd_py_var_pct"]*100),
                     col = c(col1, col2),
                     text=c(formatC(fin_data[6,"ytd_plan_var_pct"]*100,format="f", digits=0), formatC(fin_data[6, "ytd_py_var_pct"]*100,format="f", digits=0))
	            )
     
     p$elementId <- NULL
     p
   })
  
   output$fin_sh_fy_var <- renderPlotly({
    col1 = ifelse(fin_data[6,"fy_plan_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    col2 = ifelse(fin_data[6,"py_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = "Full Year", 
		     y_title = "Var (%)",
                     x_val = c("2018", "2017"),
                     y_val = c(fin_data[6,"fy_plan_var_pct"]*100, fin_data[6, "py_var_pct"]*100),
                     col = c(col1, col2),
                     text=c(formatC(fin_data[6,"fy_plan_var_pct"]*100,format="f", digits=0), formatC(fin_data[6, "py_var_pct"]*100,format="f", digits=0))
	            )
     
     p$elementId <- NULL
     p
     
   })



  ## Unit Opex------------------------------------------------------------------------------------------  
   output$fin_uopex_now <- renderPlotly({
    col1 = ifelse(fin_data[8,"now_est"]>fin_data[8, "now_plan"], "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = this_mon, 
		     y_title = "Unit Opex ($/gal)",
		     x_val = c("Est","Plan"), 
                     y_val = c(fin_data[8,"now_est"], fin_data[8, "now_plan"]),
                     col = c(col1, "rgb(255,213,0)"),
                     text = c(formatC(fin_data[8,"now_est"],format="f", digits=1), formatC(fin_data[8, "now_plan"],format="f",digits=1)) 
	            )
    
     p$elementId <- NULL
     p
   })

   output$fin_uopex_last <- renderPlotly({
    col1 = ifelse(fin_data[8,"last_act"]>fin_data[8, "last_plan"], "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = last_mon, 
		     y_title = "Unit Opex ($/gal)",
		     x_val = c("Actual","Plan"), 
      		     y_val = c(fin_data[8,"last_act"], fin_data[8, "last_plan"]),
                     col = c(col1, "rgb(255,213,0)"),
                     text=c(formatC(fin_data[8,"last_act"],format="f",digits=1), formatC(fin_data[8, "last_plan"],format="f", digits=1))
	            )
     p$elementId <- NULL
     p
   })
  
   output$fin_uopex_ytd <- renderPlotly({
    col1 = ifelse(fin_data[8,"ytd"]>fin_data[8, "ytd_plan"], "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = "YTD", 
		     y_title = "Unit Opex ($/gal)",
		     x_val = c("Actual","Plan"), 
                     y_val = c(fin_data[8,"ytd"], fin_data[8, "ytd_plan"]),
                     col = c(col1, "rgb(255,213,0)"),
                     text=c(formatC(fin_data[8,"ytd"],format="f",digits=1), formatC(fin_data[8, "ytd_plan"],format="f", digits=1)) 
	            )
     
     p$elementId <- NULL
     p
     
   })
  
   output$fin_uopex_fy <- renderPlotly({
    col1 = ifelse(fin_data[8,"latest_est"]>fin_data[8, "fy_plan"], "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = "Full Year", 
		     y_title = "Unit Opex ($/gal)",
                     x_val = c("Latest Est", "Plan"),
                     y_val = c(fin_data[8,"latest_est"], fin_data[8, "fy_plan"]),
                     col = c(col1, "rgb(255,213,0)"),
                     text=c(formatC(fin_data[8,"latest_est"],format="f",digits=1), formatC(fin_data[8, "fy_plan"],format="f", digits=1))
	            )
     p$elementId <- NULL
     p
     
   })
   
   ## -----------------------------------------
   ## Unit Opex Var 
   ## -----------------------------------------
   output$fin_uopex_now_var <- renderPlotly({
    col1 = ifelse(fin_data[8,"now_plan_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    col2 = ifelse(fin_data[8,"now_py_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = this_mon, 
		     y_title = "Var (%)",
		     x_val = c("2018","2017"), 
                     y_val = c(fin_data[8,"now_plan_var_pct"]*100, fin_data[8, "now_py_var_pct"]*100),
                     col = c(col1, col2),
                     text=c(formatC(fin_data[8,"now_plan_var_pct"]*100,format="f", digits=0), formatC(fin_data[8, "now_py_var_pct"]*100,format="f",digits=0))
	            )

     p$elementId <- NULL
     p
   })

   output$fin_uopex_last_var <- renderPlotly({
    col1 = ifelse(fin_data[8,"last_plan_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    col2 = ifelse(fin_data[8,"last_py_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = last_mon, 
		     y_title = "Var (%)",
		     x_val = c("2018","2017"), 
                     y_val = c(fin_data[8,"last_plan_var_pct"]*100, fin_data[8, "last_py_var_pct"]*100),
                     col = c(col1, col2),
      	             text=c(formatC(fin_data[8,"last_plan_var_pct"]*100,format="f", digits=0), formatC(fin_data[8, "last_py_var_pct"]*100,format="f", digits=0))
		   )
    
     p$elementId <- NULL
     p
   })
  
   output$fin_uopex_ytd_var <- renderPlotly({
    col1 = ifelse(fin_data[8,"ytd_plan_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    col2 = ifelse(fin_data[8,"ytd_py_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = "YTD", 
		     y_title = "Var (%)",
                     x_val = c("2018", "2017"),
                     y_val = c(fin_data[8,"ytd_plan_var_pct"]*100, fin_data[8, "ytd_py_var_pct"]*100),
                     col = c(col1, col2),
                     text=c(formatC(fin_data[8,"ytd_plan_var_pct"]*100,format="f", digits=0), formatC(fin_data[8, "ytd_py_var_pct"]*100,format="f", digits=0))
	            )
     
     p$elementId <- NULL
     p
   })
  
   output$fin_uopex_fy_var <- renderPlotly({
    col1 = ifelse(fin_data[8,"fy_plan_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    col2 = ifelse(fin_data[8,"py_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = "Full Year", 
		     y_title = "Var (%)",
                     x_val = c("2018", "2017"),
                     y_val = c(fin_data[8,"fy_plan_var_pct"]*100, fin_data[8, "py_var_pct"]*100),
                     col = c(col1, col2),
                     text=c(formatC(fin_data[8,"fy_plan_var_pct"]*100,format="f", digits=0), formatC(fin_data[8, "py_var_pct"]*100,format="f", digits=0))
	            )
     
     p$elementId <- NULL
     p
     
   })



  ## Logistics Spend---------------------------------------------------------------------------------  
   output$fin_log_now <- renderPlotly({
    col1 = ifelse(fin_data[11,"now_est"]>fin_data[11, "now_plan"], "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = this_mon, 
		     y_title = "Logistics Spend (mln$)",
		     x_val = c("Est","Plan"), 
                     y_val = c(fin_data[11,"now_est"], fin_data[11, "now_plan"]),
                     col = c(col1, "rgb(255,213,0)"),
                     text = c(formatC(fin_data[11,"now_est"],format="f", digits=1), formatC(fin_data[11, "now_plan"],format="f",digits=1)) 
	            )
    
     p$elementId <- NULL
     p
   })

   output$fin_log_last <- renderPlotly({
    col1 = ifelse(fin_data[11,"last_act"]>fin_data[11, "last_plan"], "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = last_mon, 
		     y_title = "Logistics Spend (mln$)",
		     x_val = c("Actual","Plan"), 
      		     y_val = c(fin_data[11,"last_act"], fin_data[11, "last_plan"]),
                     col = c(col1, "rgb(255,213,0)"),
                     text=c(formatC(fin_data[11,"last_act"],format="f",digits=1), formatC(fin_data[11, "last_plan"],format="f", digits=1))
	            )
     p$elementId <- NULL
     p
   })
  
   output$fin_log_ytd <- renderPlotly({
    col1 = ifelse(fin_data[11,"ytd"]>fin_data[11, "ytd_plan"], "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = "YTD", 
		     y_title = "Logistics Spend (mln$)",
		     x_val = c("Actual","Plan"), 
                     y_val = c(fin_data[11,"ytd"], fin_data[11, "ytd_plan"]),
                     col = c(col1, "rgb(255,213,0)"),
                     text=c(formatC(fin_data[11,"ytd"],format="f",digits=1), formatC(fin_data[11, "ytd_plan"],format="f", digits=1)) 
	            )
     
     p$elementId <- NULL
     p
     
   })
  
   output$fin_log_fy <- renderPlotly({
    col1 = ifelse(fin_data[11,"latest_est"]>fin_data[11, "fy_plan"], "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = "Full Year", 
		     y_title = "Logistics Spend (mln$)",
                     x_val = c("Latest Est", "Plan"),
                     y_val = c(fin_data[11,"latest_est"], fin_data[11, "fy_plan"]),
                     col = c(col1, "rgb(255,213,0)"),
                     text=c(formatC(fin_data[11,"latest_est"],format="f",digits=1), formatC(fin_data[11, "fy_plan"],format="f", digits=1))
	            )
     p$elementId <- NULL
     p
     
   })
   
   ## -----------------------------------------
   ## Log Spend Var 
   ## -----------------------------------------
   output$fin_log_now_var <- renderPlotly({
    col1 = ifelse(fin_data[11,"now_plan_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    col2 = ifelse(fin_data[11,"now_py_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = this_mon, 
		     y_title = "Var (%)",
		     x_val = c("2018","2017"), 
                     y_val = c(fin_data[11,"now_plan_var_pct"]*100, fin_data[11, "now_py_var_pct"]*100),
                     col = c(col1, col2),
                     text=c(formatC(fin_data[11,"now_plan_var_pct"]*100,format="f", digits=0), formatC(fin_data[11, "now_py_var_pct"]*100,format="f",digits=0))
	            )

     p$elementId <- NULL
     p
   })

   output$fin_log_last_var <- renderPlotly({
    col1 = ifelse(fin_data[11,"last_plan_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    col2 = ifelse(fin_data[11,"last_py_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = last_mon, 
		     y_title = "Var (%)",
		     x_val = c("2018","2017"), 
                     y_val = c(fin_data[11,"last_plan_var_pct"]*100, fin_data[11, "last_py_var_pct"]*100),
                     col = c(col1, col2),
      	             text=c(formatC(fin_data[11,"last_plan_var_pct"]*100,format="f", digits=0), formatC(fin_data[11, "last_py_var_pct"]*100,format="f", digits=0))
		   )
    
     p$elementId <- NULL
     p
   })
  
   output$fin_log_ytd_var <- renderPlotly({
    col1 = ifelse(fin_data[11,"ytd_plan_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    col2 = ifelse(fin_data[11,"ytd_py_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = "YTD", 
		     y_title = "Var (%)",
                     x_val = c("2018", "2017"),
                     y_val = c(fin_data[11,"ytd_plan_var_pct"]*100, fin_data[11, "ytd_py_var_pct"]*100),
                     col = c(col1, col2),
                     text=c(formatC(fin_data[11,"ytd_plan_var_pct"]*100,format="f", digits=0), formatC(fin_data[11, "ytd_py_var_pct"]*100,format="f", digits=0))
	            )
     
     p$elementId <- NULL
     p
   })
  
   output$fin_log_fy_var <- renderPlotly({
    col1 = ifelse(fin_data[11,"fy_plan_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    col2 = ifelse(fin_data[11,"py_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = "Full Year", 
		     y_title = "Var (%)",
                     x_val = c("2018", "2017"),
                     y_val = c(fin_data[11,"fy_plan_var_pct"]*100, fin_data[11, "py_var_pct"]*100),
                     col = c(col1, col2),
                     text=c(formatC(fin_data[11,"fy_plan_var_pct"]*100,format="f", digits=0), formatC(fin_data[11, "py_var_pct"]*100,format="f", digits=0))
	            )
     
     p$elementId <- NULL
     p
     
   })



  ## Logistics Spend Export------------------------------------------------------------------------------  
   output$fin_exp_now <- renderPlotly({
    col1 = ifelse(fin_data[9,"now_est"]>fin_data[9, "now_plan"], "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = this_mon, 
		     y_title = "Export (COGS) (mln$)",
		     x_val = c("Est","Plan"), 
                     y_val = c(fin_data[9,"now_est"], fin_data[9, "now_plan"]),
                     col = c(col1, "rgb(255,213,0)"),
                     text = c(formatC(fin_data[9,"now_est"],format="f", digits=1), formatC(fin_data[9, "now_plan"],format="f",digits=1)) 
	            )
    
     p$elementId <- NULL
     p
   })

   output$fin_exp_last <- renderPlotly({
    col1 = ifelse(fin_data[9,"last_act"]>fin_data[9, "last_plan"], "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = last_mon, 
		     y_title = "Export (COGS) (mln$)",
		     x_val = c("Actual","Plan"), 
      		     y_val = c(fin_data[9,"last_act"], fin_data[9, "last_plan"]),
                     col = c(col1, "rgb(255,213,0)"),
                     text=c(formatC(fin_data[9,"last_act"],format="f",digits=1), formatC(fin_data[9, "last_plan"],format="f", digits=1))
	            )
     p$elementId <- NULL
     p
   })
  
   output$fin_exp_ytd <- renderPlotly({
    col1 = ifelse(fin_data[9,"ytd"]>fin_data[9, "ytd_plan"], "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = "YTD", 
		     y_title = "Export (COGS) (mln$)",
		     x_val = c("Actual","Plan"), 
                     y_val = c(fin_data[9,"ytd"], fin_data[9, "ytd_plan"]),
                     col = c(col1, "rgb(255,213,0)"),
                     text=c(formatC(fin_data[9,"ytd"],format="f",digits=1), formatC(fin_data[9, "ytd_plan"],format="f", digits=1)) 
	            )
     
     p$elementId <- NULL
     p
     
   })
  
   output$fin_exp_fy <- renderPlotly({
    col1 = ifelse(fin_data[9,"latest_est"]>fin_data[9, "fy_plan"], "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = "Full Year", 
		     y_title = "Export (COGS) (mln$)",
                     x_val = c("Latest Est", "Plan"),
                     y_val = c(fin_data[9,"latest_est"], fin_data[9, "fy_plan"]),
                     col = c(col1, "rgb(255,213,0)"),
                     text=c(formatC(fin_data[9,"latest_est"],format="f",digits=1), formatC(fin_data[9, "fy_plan"],format="f", digits=1))
	            )
     p$elementId <- NULL
     p
     
   })
   
   ## -----------------------------------------
   ## Log Spend Var 
   ## -----------------------------------------
   output$fin_exp_now_var <- renderPlotly({
    col1 = ifelse(fin_data[9,"now_plan_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    col2 = ifelse(fin_data[9,"now_py_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = this_mon, 
		     y_title = "Var (%)",
		     x_val = c("2018","2017"), 
                     y_val = c(fin_data[9,"now_plan_var_pct"]*100, fin_data[9, "now_py_var_pct"]*100),
                     col = c(col1, col2),
                     text=c(formatC(fin_data[9,"now_plan_var_pct"]*100,format="f", digits=0), formatC(fin_data[9, "now_py_var_pct"]*100,format="f",digits=0))
	            )

     p$elementId <- NULL
     p
   })

   output$fin_exp_last_var <- renderPlotly({
    col1 = ifelse(fin_data[9,"last_plan_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    col2 = ifelse(fin_data[9,"last_py_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = last_mon, 
		     y_title = "Var (%)",
		     x_val = c("2018","2017"), 
                     y_val = c(fin_data[9,"last_plan_var_pct"]*100, fin_data[9, "last_py_var_pct"]*100),
                     col = c(col1, col2),
      	             text=c(formatC(fin_data[9,"last_plan_var_pct"]*100,format="f", digits=0), formatC(fin_data[9, "last_py_var_pct"]*100,format="f", digits=0))
		   )
    
     p$elementId <- NULL
     p
   })
  
   output$fin_exp_ytd_var <- renderPlotly({
    col1 = ifelse(fin_data[9,"ytd_plan_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    col2 = ifelse(fin_data[9,"ytd_py_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = "YTD", 
		     y_title = "Var (%)",
                     x_val = c("2018", "2017"),
                     y_val = c(fin_data[9,"ytd_plan_var_pct"]*100, fin_data[9, "ytd_py_var_pct"]*100),
                     col = c(col1, col2),
                     text=c(formatC(fin_data[9,"ytd_plan_var_pct"]*100,format="f", digits=0), formatC(fin_data[9, "ytd_py_var_pct"]*100,format="f", digits=0))
	            )
     
     p$elementId <- NULL
     p
   })
  
   output$fin_exp_fy_var <- renderPlotly({
    col1 = ifelse(fin_data[9,"fy_plan_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    col2 = ifelse(fin_data[9,"py_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = "Full Year", 
		     y_title = "Var (%)",
                     x_val = c("2018", "2017"),
                     y_val = c(fin_data[9,"fy_plan_var_pct"]*100, fin_data[9, "py_var_pct"]*100),
                     col = c(col1, col2),
                     text=c(formatC(fin_data[9,"fy_plan_var_pct"]*100,format="f", digits=0), formatC(fin_data[9, "py_var_pct"]*100,format="f", digits=0))
	            )
     
     p$elementId <- NULL
     p
     
   })
   

  ## Logistics Spend SCME------------------------------------------------------------------------------  
   output$fin_scme_now <- renderPlotly({
    col1 = ifelse(fin_data[10,"now_est"]>fin_data[10, "now_plan"], "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = this_mon, 
		     y_title = "SCME (mln$)",
		     x_val = c("Est","Plan"), 
                     y_val = c(fin_data[10,"now_est"], fin_data[10, "now_plan"]),
                     col = c(col1, "rgb(255,213,0)"),
                     text = c(formatC(fin_data[10,"now_est"],format="f", digits=1), formatC(fin_data[10, "now_plan"],format="f",digits=1)) 
	            )
    
     p$elementId <- NULL
     p
   })

   output$fin_scme_last <- renderPlotly({
    col1 = ifelse(fin_data[10,"last_act"]>fin_data[10, "last_plan"], "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = last_mon, 
		     y_title = "SCME (mln$)",
		     x_val = c("Actual","Plan"), 
      		     y_val = c(fin_data[10,"last_act"], fin_data[10, "last_plan"]),
                     col = c(col1, "rgb(255,213,0)"),
                     text=c(formatC(fin_data[10,"last_act"],format="f",digits=1), formatC(fin_data[10, "last_plan"],format="f", digits=1))
	            )
     p$elementId <- NULL
     p
   })
  
   output$fin_scme_ytd <- renderPlotly({
    col1 = ifelse(fin_data[10,"ytd"]>fin_data[10, "ytd_plan"], "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = "YTD", 
		     y_title = "SCME (mln$)",
		     x_val = c("Actual","Plan"), 
                     y_val = c(fin_data[10,"ytd"], fin_data[10, "ytd_plan"]),
                     col = c(col1, "rgb(255,213,0)"),
                     text=c(formatC(fin_data[10,"ytd"],format="f",digits=1), formatC(fin_data[10, "ytd_plan"],format="f", digits=1)) 
	            )
     
     p$elementId <- NULL
     p
     
   })
  
   output$fin_scme_fy <- renderPlotly({
    col1 = ifelse(fin_data[10,"latest_est"]>fin_data[10, "fy_plan"], "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = "Full Year", 
		     y_title = "SCME (mln$)",
                     x_val = c("Latest Est", "Plan"),
                     y_val = c(fin_data[10,"latest_est"], fin_data[10, "fy_plan"]),
                     col = c(col1, "rgb(255,213,0)"),
                     text=c(formatC(fin_data[10,"latest_est"],format="f",digits=1), formatC(fin_data[10, "fy_plan"],format="f", digits=1))
	            )
     p$elementId <- NULL
     p
     
   })
   
   ## -----------------------------------------
   ## Log Spend Var 
   ## -----------------------------------------
   output$fin_scme_now_var <- renderPlotly({
    col1 = ifelse(fin_data[10,"now_plan_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    col2 = ifelse(fin_data[10,"now_py_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = this_mon, 
		     y_title = "Var (%)",
		     x_val = c("2018","2017"), 
                     y_val = c(fin_data[10,"now_plan_var_pct"]*100, fin_data[10, "now_py_var_pct"]*100),
                     col = c(col1, col2),
                     text=c(formatC(fin_data[10,"now_plan_var_pct"]*100,format="f", digits=0), formatC(fin_data[10, "now_py_var_pct"]*100,format="f",digits=0))
	            )

     p$elementId <- NULL
     p
   })

   output$fin_scme_last_var <- renderPlotly({
    col1 = ifelse(fin_data[10,"last_plan_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    col2 = ifelse(fin_data[10,"last_py_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = last_mon, 
		     y_title = "Var (%)",
		     x_val = c("2018","2017"), 
                     y_val = c(fin_data[10,"last_plan_var_pct"]*100, fin_data[10, "last_py_var_pct"]*100),
                     col = c(col1, col2),
      	             text=c(formatC(fin_data[10,"last_plan_var_pct"]*100,format="f", digits=0), formatC(fin_data[10, "last_py_var_pct"]*100,format="f", digits=0))
		   )
    
     p$elementId <- NULL
     p
   })
  
   output$fin_scme_ytd_var <- renderPlotly({
    col1 = ifelse(fin_data[10,"ytd_plan_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    col2 = ifelse(fin_data[10,"ytd_py_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = "YTD", 
		     y_title = "Var (%)",
                     x_val = c("2018", "2017"),
                     y_val = c(fin_data[10,"ytd_plan_var_pct"]*100, fin_data[10, "ytd_py_var_pct"]*100),
                     col = c(col1, col2),
                     text=c(formatC(fin_data[10,"ytd_plan_var_pct"]*100,format="f", digits=0), formatC(fin_data[10, "ytd_py_var_pct"]*100,format="f", digits=0))
	            )
     
     p$elementId <- NULL
     p
   })
  
   output$fin_scme_fy_var <- renderPlotly({
    col1 = ifelse(fin_data[10,"fy_plan_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    col2 = ifelse(fin_data[10,"py_var_pct"]>0, "rgb(221,75,57)", "rgb(32, 207, 155)")
    p <- call_plotly(x_title = "Full Year", 
		     y_title = "Var (%)",
                     x_val = c("2018", "2017"),
                     y_val = c(fin_data[10,"fy_plan_var_pct"]*100, fin_data[10, "py_var_pct"]*100),
                     col = c(col1, col2),
                     text=c(formatC(fin_data[10,"fy_plan_var_pct"]*100,format="f", digits=0), formatC(fin_data[10, "py_var_pct"]*100,format="f", digits=0))
	            )
     
     p$elementId <- NULL
     p
     
   })



  #----------------------------------------
  # Performance: WH 
  #----------------------------------------
  output$wh_lobp <- renderPlotly({
     p <- plot_ly(x = wh_lobp$time, y = wh_lobp$Brockville, name = 'Brockville', 
		  type = 'scatter', mode = 'lines+markers') %>%
	add_trace(y = wh_lobp$CO, name='CO', mode ='lines+markers') %>%
	add_trace(y = wh_lobp$HLP, name='HLP', mode ='lines+markers') %>%
	add_trace(y = wh_lobp$LALP, name='LALP', mode ='lines+markers') %>%
        layout(
 		title = "LOBPs Weekly Capacity",
   		titlefont=list(size = 18),
		yaxis = list(title = 'Capacity Utilization (%)', 
                   titlefont=list(size=18), range=c(0,100), tickfont=list(size=16)),
		xaxis = list(title='Date', titlefont=list(size=18), tickfont=list(size=16)))
     
     #p$elementId <- NULL
     p
  })

  output$wh_rdc <- renderPlotly({
     p <- plot_ly(x = wh_rdc$time, y = wh_rdc$Fontana, name = 'Fontana', 
		  type = 'scatter', mode = 'lines+markers') %>%
	add_trace(y = wh_rdc$HRDC, name='HRDC', mode ='lines+markers') %>%
	add_trace(y = wh_rdc$Metro, name='Metro', mode ='lines+markers') %>%
	add_trace(y = wh_rdc$ORDC, name='ORDC', mode ='lines+markers') %>%
	add_trace(y = wh_rdc$Titan, name='Titan', mode ='lines+markers') %>%
	add_trace(y = wh_rdc$Wills, name='Wills', mode ='lines+markers') %>%
        layout(
 		title = "RDCs Weekly Capacity",
   		titlefont=list(size = 18),
		yaxis = list(title = 'Capacity Utilization (%)', 
                   titlefont=list(size=18), range=c(0,120), tickfont=list(size=16)),
		xaxis = list(title='Date', titlefont=list(size=18), tickfont=list(size=16)))
     
     #p$elementId <- NULL
     p
  })


  output$wh_lobpData   <- renderDataTable({ wh_lobp })
  output$wh_rdcData   <- renderDataTable({ wh_rdc })
   
  #----------------------------------------
  # Performance: OTIF 
  #----------------------------------------
  ## weekly trend
  output$otif_wk <- renderPlotly({
     p <- plot_ly(x = otif_wk$Week, y = otif_wk$Target, name = 'Target', 
		  type = 'scatter', mode = 'lines+markers', line=list(color='rgb(234,48,51)')) %>%
	add_trace(y = otif_wk$Bulk, name='Bulk', mode ='lines+markers', line=list(color='rgb(7,98,241)')) %>%
	add_trace(y = otif_wk$Pack, name='Pack', mode ='lines+markers', line=list(color='rgb(0,166,85)')) %>%
	add_trace(y = otif_wk$Network, name='Network', mode ='lines+markers', line=list(color='rgb(203,0,247)')) %>%
        layout(
 		title = "12-Week OTIF Trending", 
   		titlefont=list(size = 18),
		yaxis = list(title = 'OTIF (%)', 
                   titlefont=list(size=18), c(min(otif_wk[,-1])-10,100), tickfont=list(size=16)),
		xaxis = list(title='Week 2018', titlefont=list(size=18), tickfont=list(size=16)))
     
     #p$elementId <- NULL
     p
  })

  output$otif_wkData   <- renderDataTable({ otif_wk })

  ## segment by plant 
  output$otif_by_plant <- function() {
	otif_by_plant %>% 
	mutate(OTIF_Pct = color_bar("lightgreen")(OTIF_Pct )) %>% 
        kable( escape = F, align = c(rep("l", 4), rep('r', 6))) %>%  
	kable_styling(bootstrap_options = c("striped", "hover", "condensed"),full_width = T, position = "left") %>%
	column_spec(1, bold = T) %>%
	collapse_rows(columns = 1:3, valign = "top")
  }

  ## segment by customer 
  output$otif_by_customer <- function() {
  	otif_by_customer %>% 
        mutate(OTIF_Pct = color_bar("lightgreen")(OTIF_Pct )) %>% 
  	kable( escape = F, align = c(rep("l", 4), rep('r', 6))) %>% 
  	kable_styling(bootstrap_options = c("striped", "hover", "condensed"),full_width = T, position = "left") %>%
  	column_spec(1, bold = T) %>%
	collapse_rows(columns = 1:3, valign = "top")
  }

  #----------------------------------------
  # Performance: CARRIER 
  #----------------------------------------
  ## SCAR n by Carr 
  output$scar_n_carr <- renderPlotly({
		  if (length(input$carr1) == 0) {
		  print("Please select at least one carrier")
		  } else {
		  tot_carr <- scar_n_by_carr[scar_n_by_carr$Carrier == input$carr1, ]
		  ggplot(tot_carr, aes(x = Month, y = n,  color = Carrier)) +
		  geom_line(size=1, alpha=1) +
		  geom_point(size=2) +
		  labs(x = "Month", y = "Total SCARs") +
		  scale_x_continuous(name="Month", limits=c(1, 5))+
		  theme_minimal() +
		  theme(
			  legend.text = element_text(size=12),
			  legend.title = element_text(size=12),
			  axis.title.x = element_text(size=14),
			  axis.title.y = element_text(size=14),
			  plot.title = element_text(lineheight=.8, face="bold", size=18)
#panel.background = element_blank()
		       ) 

		  }
  })


  output$scar_n_carr_plant <- renderPlotly({
		  tot_carr_plant <- scar_n_by_carr_plant[scar_n_by_carr_plant$Carrier == input$carr2 & scar_n_by_carr_plant$Plant == input$plant2, ]
		  validate(
		    if (dim(tot_carr_plant)[1]==0){
			    'No Data for this combination, please select another one'
		    }
			)

		  ggplot(tot_carr_plant, aes(x = Month, y = n,  color = Carrier)) +
		  geom_line(size=1, alpha=1) +
		  geom_point(size=2) +
		  labs(x = "Month", y = "Total SCARs") +
		  scale_x_continuous(name="Month", limits=c(1, 5))+
		  theme_minimal() +
		  theme(
			  legend.text = element_text(size=12),
			  legend.title = element_text(size=12),
			  axis.title.x = element_text(size=14),
			  axis.title.y = element_text(size=14),
			  plot.title = element_text(lineheight=.8, face="bold", size=18),
			  legend.position="none"
		       )
		  })


} # server end

#########################
## combine ui and server
#########################
shinyApp(ui, server)

