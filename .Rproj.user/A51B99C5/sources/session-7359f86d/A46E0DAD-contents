source('server/funciones.R')


server <- function(input, output, session) {
  
  data_total <- reactiveValues(captura = NULL, salmones_length = NULL, salmones_length_sex = NULL)
  
  observeEvent( input$captura_tasa_file_tab, {
    
    req(input$captura_tasa_file_tab)  
    captura = read.csv(file = input$captura_tasa_file_tab$datapath)
    data_total$captura = process_catch_data(captura)

  })
  
  
  observeEvent( input$biologico_file_tab, {
    
    req(input$biologico_file_tab)  
    
    salmones = read.csv(file = input$biologico_file_tab$datapath)
    
    salmones_length = filter_only_length(salmones, umbral_length = 150) 
    salmones_length_sex = filter_only_male_female(salmones_length) 
    
    data_total$salmones_length = salmones_length
    data_total$salmones_length_sex = salmones_length_sex
    
    
  })
  
  
  output$landing_plots_facet <- renderPlot({
    
    req(data_total$captura)

    plot_desembarques_n_facet(data = data_total$captura,
                              var = 'Captura_n',
                              ylab = "Capturas (N)",
                              step_days = 5)
    
  })
  
 
  output$escape_plots_facet <- renderPlot({
    
    req(data_total$captura)
    
    plot_desembarques_n_facet(data = data_total$captura,
                              var = 'Escape_n',
                              ylab = "Escapes (N)",
                              step_days = 5)
    
  })  
  
  
  output$tasa_catch_plots_facet <- renderPlot({
    
    req(data_total$captura)
    
    plot_desembarques_n_facet(data = data_total$captura %>% filter(!is.na(Escape_n)),
                              var = 'tasa_captura',
                              ylab = "Tasa Captura",
                              step_days = 5)
    
  })  
 

  observeEvent(data_total$captura,{
    
    output$options_variable_catch <- renderUI({
      checkboxGroupInput(
        inputId = 'select_variable_catch',
        label = 'Seleccionar Variable',
        choices = c('Captura','Escapes','Tasa Captura'),
        selected = c('Captura','Escapes','Tasa Captura')
        
      )
    })
  })
  
  
  
  output$general_first_plots_facet <- renderPlot({
    
    req(data_total$captura)
    
    variable_map <- c('Captura' = 'Captura_n', 'Escapes' = 'Escape_n', 'Tasa Captura' = 'tasa_captura')
    
    # Obtener las variables seleccionadas
    selected_vars <- input$select_variable_catch
    
    # Validar que al menos una variable esté seleccionada
    req(selected_vars)
    
    # Mapear las opciones seleccionadas a sus nombres de columna
    vars_to_plot <- as.character(variable_map[selected_vars])
    
    ylab <- case_when(
      length(vars_to_plot) == 1 & vars_to_plot == 'Captura_n' ~ "Captura (N)",
      length(vars_to_plot) == 1 & vars_to_plot == 'Escape_n' ~ "Escape (N)",
      length(vars_to_plot) == 1 & vars_to_plot == 'tasa_captura' ~ "Tasa Captura",
      TRUE ~ "" # Cuando hay más de una variable o no hay coincidencias
    )
    
    # Llamar a la función de gráficos con las variables seleccionadas
    plot_desembarques_n_facet(
      data = data_total$captura,
      var = vars_to_plot,
      ylab = ylab,
      step_days = 5
    )
    
  })  
  
 
  output$general_temporada_plots <- renderPlot({
    
    req(data_total$captura)
    
    variable_map <- c('Captura' = 'Captura_n', 'Escapes' = 'Escape_n', 'Tasa Captura' = 'tasa_captura')
    
    # Obtener las variables seleccionadas
    selected_vars <- input$select_variable_catch
    
    # Validar que al menos una variable esté seleccionada
    req(selected_vars)
    
    # Mapear las opciones seleccionadas a sus nombres de columna
    vars_to_plot <- as.character(variable_map[selected_vars])
    
    ylab <- case_when(
      length(vars_to_plot) == 1 & vars_to_plot == 'Captura_n' ~ "Captura (N)",
      length(vars_to_plot) == 1 & vars_to_plot == 'Escape_n' ~ "Escape (N)",
      length(vars_to_plot) == 1 & vars_to_plot == 'tasa_captura' ~ "Tasa Captura",
      TRUE ~ "" # Cuando hay más de una variable o no hay coincidencias
    )
    
    # Llamar a la función de gráficos con las variables seleccionadas
    plot_desembarques_temporada(
      data = data_total$captura,
      var = vars_to_plot,
      ylab = ylab,
      size_axis_text = 14
    )
    
  })  
  
  
  observeEvent(data_total$salmones_length,{
    
    output$options_sex_tallas <- renderUI({
      checkboxGroupInput(
        inputId = 'select_sex_tallas',
        label = 'Seleccionar Sexo',
        choices = unique(data_total$salmones_length$SEX),
        selected = unique(data_total$salmones_length$SEX)
        
      )
    })
  })
  
  
  output$talla_media_plot <- renderPlot({
    
    req(data_total$salmones_length)
    
    sex_info <- get_sex_colores_y_labels(data_total$salmones_length, input$select_sex_tallas)
    
    plot_mean_length(data = data_total$salmones_length %>% filter(SEX %in% input$select_sex_tallas),
                     colores = sex_info$colores,
                     labels = sex_info$labels)
  })
  
  
  output$talla_boxplot_sex <- renderPlot({
    
    req(data_total$salmones_length)
    
    sex_info <- get_sex_colores_y_labels(data_total$salmones_length, input$select_sex_tallas)
    
    plot_boxplot_sex(data = data_total$salmones_length %>% filter(SEX %in% input$select_sex_tallas),
                     colores = sex_info$colores,
                     labels = sex_info$labels,
                     alpha = 0.5)
    
  })
  
  
  observeEvent(data_total$salmones_length,{
    
    output$options_sex_tallas_gridges <- renderUI({
      checkboxGroupInput(
        inputId = 'select_sex_tallas_grid',
        label = 'Seleccionar',
        choices = c(unique(data_total$salmones_length$SEX),'Total'),
        selected = c(unique(data_total$salmones_length$SEX),'Total')
        
      )
    })
  })
  
  
  output$talla_gridges_sex <- renderPlot({
    req(data_total$salmones_length)
    
    # Filtrar los datos según la selección del usuario
    data_filtered <- data_total$salmones_length %>%
      filter(SEX %in% input$select_sex_tallas_grid)
    
    # Verificar si "Total" está seleccionado y si es así, incluir todos los datos (sin filtrado por SEX)
    if ("Total" %in% input$select_sex_tallas_grid) {
      data_filtered <- data_total$salmones_length
    }
    
    # Determinar las etiquetas según las opciones seleccionadas
    selected_labels <- c()
    if ("Macho" %in% input$select_sex_tallas_grid) {
      selected_labels <- c(selected_labels, "Macho")
    }
    if ("Hembra" %in% input$select_sex_tallas_grid) {
      selected_labels <- c(selected_labels, "Hembra")
    }
    if ("Indet" %in% input$select_sex_tallas_grid) {
      selected_labels <- c(selected_labels, "Indet")
    }
    if ("Total" %in% input$select_sex_tallas_grid) {
      selected_labels <- c(selected_labels, "Total")
    }
    
    # Llamar a la función para crear el gráfico
    plot_tallas_gridges(data_filtered, 
                        show = input$select_sex_tallas_grid,
                        colores = c("blue", "red", "green", "grey"),
                        labels = selected_labels
    )
  })
  
  
  observeEvent(data_total$salmones_length_sex,{
    
    output$options_sex_igs <- renderUI({
      checkboxGroupInput(
        inputId = 'select_sex_igs',
        label = 'Seleccionar Sexo',
        choices = c('Macho','Hembra'),
        selected = c('Macho','Hembra')
        
      )
    })
  })
  
  
  output$igs_plot <- renderPlot({
    
    req(data_total$salmones_length)
    
    salmones_IGS = filter_only_IGS(data_total$salmones_length_sex, umbral_IGS = 15) 

    sex_info <- get_sex_colores_y_labels(salmones_IGS, input$select_sex_igs)
    
    plot_boxplot_igs_sex(data = salmones_IGS %>% filter(SEX %in% input$select_sex_igs),
                     colores = sex_info$colores,
                     labels = sex_info$labels)
    
  })
  
  
}