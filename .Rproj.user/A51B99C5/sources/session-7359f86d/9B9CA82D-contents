

Capturas_Tab_item <- function(){
  
  tabItem(tabName = "captura_tab",
          h3("Capturas del Salmón Chinook"),
          
          tags$hr(),
          plotOutput("landing_plots_facet",height = 800),    
          
  )
  
}

Escape_Tab_item <- function(){
  
  tabItem(tabName = "escape_tab",

          h3("Escapes del Salmón Chinook"),
          tags$hr(),          
          plotOutput("escape_plots_facet",height = 800)
          
  )
  
}


Tasa_captura_Tab_item <- function(){
  
  tabItem(tabName = "tasa_captura_tab",
          h3("Tasas de captura del Salmón Chinook"),
          
          tags$hr(),
          plotOutput("tasa_catch_plots_facet",height = 800),          
          
  )
  
}

General_Tab_item <- function(){
  
  tabItem(tabName = "both_tab",
          h3("Capturas, Escapes y Tasas de Captura"),
          
          # Filtros
          fluidRow(
            column(4,
                   uiOutput('options_variable_catch')
            )
          ),
          
          tags$hr(),
          plotOutput("general_first_plots_facet",height = 800),        
          
          tags$hr(),
          plotOutput("general_temporada_plots",height = 600),         
          
  )
  
}


Tallas_Tab_item <- function(){
  
  tabItem(tabName = "tallas_tab",
          h3("Estructura de Tallas del Salmón Chinook"),
          
          # Filtros
          fluidRow(
            column(4,
                   uiOutput('options_sex_tallas')
            )
          ),
          
          # Gráficos
          tags$hr(),
          plotOutput("talla_media_plot"),
          
          tags$hr(),
          plotOutput("talla_boxplot_sex"),
          
          # Filtros
          fluidRow(
            column(4,
                   uiOutput('options_sex_tallas_gridges')
            )
          ),
          
          tags$hr(),
          plotOutput("talla_gridges_sex", height = 900)
  )
  
}


IGS_Tab_item <- function(){
  
  tabItem(tabName = "igs_tab",
          
          h3("Índice Gonadosomático del Salmón Chinook"),
          
          # Filtros
          fluidRow(
            column(4,
                   uiOutput('options_sex_igs')
            )
          ),
          
          # Gráficos
          tags$hr(),
          plotOutput("igs_plot"),
          
  )
  
}


