library(shinydashboard)
library(shinyWidgets)

carga_datos_tab <- function(){
  
  menuItem("Cargar Datos", icon = icon("upload"), tabName = "carga_datos_tab",
           
           fileInput(inputId = "captura_tasa_file_tab", 
                     label = "Archivo de Captura Tasa captura",
                     multiple = FALSE,
                     accept = '.csv',
                     buttonLabel = "Sube csv"),
           tags$hr(),
           
           fileInput(inputId = "biologico_file_tab", 
                     label = "Archivo Biológico",
                     multiple = FALSE,
                     accept = '.csv',
                     buttonLabel = "Sube csv"),
           tags$hr()
  )
  
}


Descarga_tasa_captura_tab <- function(){
  # Biomasa y Densidad
  menuItem("Capturas y Tasa Captura", icon = icon("chart-line"), tabName = "captura_tasa_captura_tab",
           menuSubItem("Capturas", tabName = "captura_tab"),
           menuSubItem("Escapes", tabName = "escape_tab"),
           menuSubItem("Tasa Captura", tabName = "tasa_captura_tab"),
           menuSubItem("General", tabName = "both_tab")
  )
}



Biologia_tab <- function(){
  # Biomasa y Densidad
  menuItem("Biologia", icon = icon("fish"), tabName = "biologia_tab",
           menuSubItem("Tallas", tabName = "tallas_tab"),
           menuSubItem("IGS", tabName = "igs_tab")
  )
}







