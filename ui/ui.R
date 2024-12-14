library(shiny)
library(shinydashboard)
library(bslib)
library(fontawesome)

source('ui/tabs/sidebar.R')
source('ui/tabs/dashbody.R')


ui <- dashboardPage(
  
  dashboardHeader(title = "Análisis de Datos Biológicos - Pesqueros\n Salmón Chinook"),
  
  dashboardSidebar(
    
    sidebarMenu(
      # Cargar Datos
      carga_datos_tab(),
      
      # Pesquería    
      Descarga_tasa_captura_tab(),
      
      # Biología
      Biologia_tab()
      
    )
    
  ),
  
  dashboardBody(
    tabItems(
      
      # Tab: Capturas
      Capturas_Tab_item(),
      
      # Tab: Escape
      Escape_Tab_item(),
      
      # Tab: Tasa de Captura
      Tasa_captura_Tab_item(),
      
      # Tab:General
      General_Tab_item(),
      
      # Tab: Tallas     
      Tallas_Tab_item(),
      
      # Tab: Tallas     
      IGS_Tab_item()
      
    )
  )
)



