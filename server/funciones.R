library(ggplot2)
library(dplyr)
library(ggh4x)
library(tidyverse)
library(ggridges)

convert_dates <- function(dates, formats = c("%d/%m/%Y", "%Y-%m-%d", "%d-%b-%Y")) {
  parsed_dates <- NULL
  for (fmt in formats) {
    parsed_dates <- suppressWarnings(as.Date(dates, format = fmt))
    if (all(!is.na(parsed_dates))) break
  }
  if (any(is.na(parsed_dates))) {
    stop("Algunas fechas no pudieron ser convertidas con los formatos proporcionados.")
  }
  return(parsed_dates)
}

process_catch_data <- function(captura) {
  
  meses = c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Set","Oct","Nov","Dic")
    
  captura$Fecha <- convert_dates(captura$Fecha)
  
  captura$Captura_n <- round(captura$Captura)
  captura$Escape_n <- round(captura$Escape)
  
  captura$total <- apply(captura[, c("Captura_n", "Escape_n")], 1, sum, na.rm = TRUE)
  captura$total <- na_if(captura$total, 0)
  
  captura$tasa_captura <- captura$Captura_n / captura$total
  
  captura$week <- week(captura$Fecha)
  captura$year <- year(captura$Fecha)
  
  captura <- captura %>%
    mutate(week_final = (week - min(week)) + 1, .by = year)
  
  captura = captura %>%
    mutate(fecha = paste(day(Fecha), meses[month(Fecha)], sep = "-"))
  
  return(captura)
}


filter_only_male_female = function(data) {
  
  sex_NA_eliminados = sum(is.na(data$SEX))
  
  if(sex_NA_eliminados > 0){
    cat("Hay", sex_NA_eliminados, "registros con sexo NA, serán eliminados\n\n")
  }
  
  sexo_eliminados = table(data[!(data$SEX %in% c("Hembra","Macho")),]$SEX)
  
  if(length(sexo_eliminados) > 0){
    for(i in 1:length(sexo_eliminados)){
      cat("Hay", as.numeric(sexo_eliminados[i]), "registros con sexo", names(sexo_eliminados[i]),", serán eliminados\n\n")
    }
  }
  
  data_final = data[data$SEX %in% c("Macho","Hembra"),]
  
  if(nrow(data) != nrow(data_final)){
    cat("En total se eliminaron", (nrow(data) - nrow(data_final)),"filas de los datos\n\n")
  }
  
  return(data_final)
  
}


filter_only_length = function(data, umbral_length = 150) {
  
  if(any(data$TL_CM > umbral_length, na.rm = TRUE)){
    cat("Hay", sum(data$TL_CM > umbral_length, na.rm  = TRUE) ,"tallas de más de",umbral_length, "cm, serán elimminadas; se recomienda revisar\n\n")
    table(data[data$TL_CM > umbral_length,]$TL_CM)
  }
  
  if(sum(is.na(data$TL_CM)) > 0){
    cat("Hay", sum(is.na(data$TL_CM)) ,"tallas con valores NA, serán elimminadas; se recomienda revisar\n\n")
  }
  
  data = data[data$TL_CM <= umbral_length,]
  data_final = data[!is.na(data$TL_CM),]
  
  if(nrow(data) != nrow(data_final)){
    cat("En total se eliminaron", (nrow(data) - nrow(data_final)),"filas de los datos\n\n")
  }
  
  return(data_final)
  
}

processing_data_land_n = function(data, start_year){
  
  first_years = seq(from = start_year, length.out = (ncol(data)-1))
  second_years = seq(from = start_year + 1, length.out = (ncol(data)-1))
  
  seasons = paste0(first_years, "-", second_years)
  
  names(data) = c("fecha", seasons)
  
  data = data %>% gather(season, var, -fecha)
  
  return(data)
  
}


plot_desembarques_n_facet <- function(data, var, ylab, step_days = 5, angle_text_x = 90, 
                                      size_title_axis = 14, size_axis_text = 10, size_title_facet = 14,
                                      size_legend_text = 12, size_legend_title = 14) {
  
  # Verificar si `var` contiene más de una columna
  if (length(var) > 1) {
    # Convertir a formato largo para manejar múltiples variables
    data <- data %>%
      pivot_longer(cols = all_of(var), names_to = "Variable", values_to = "Value") %>%
      group_by(Temporada, Variable)
  } else {
    # Usar una sola columna si `var` tiene solo un valor
    data <- data %>% mutate(Variable = var, Value = .data[[var]])
  }
  
  # Crear la columna temporal `t` y construir el gráfico
  data <- data %>%
    group_by(Temporada, Variable) %>%
    mutate(t = row_number()) %>%
    ungroup()
  
  # Si "Tasa Captura" está en las variables seleccionadas, multiplicarla por un factor de escala
  if ('tasa_captura' %in% var) {
    # Si 'tasa_captura' es la única variable seleccionada, no hacer el reescalado
    if (length(var) > 1) {
      # Calculamos el valor máximo de las variables no "tasa_captura" para la reescalación
      max_value <- max(data %>% filter(Variable != "tasa_captura") %>% pull(Value), na.rm = TRUE)
      # Multiplicamos "tasa_captura" por el valor máximo de las otras variables para reescalar
      data <- data %>%
        mutate(Value = ifelse(Variable == 'tasa_captura', Value * max_value, Value))
    }
  }
  
  plot_line <- data %>%
    ggplot(aes(x = t, y = Value, color = Variable, group = interaction(Temporada, Variable))) +
    geom_line() +
    geom_point() +
    facet_wrap(~Temporada, scales = 'free_x') +
    scale_x_continuous(breaks = seq(1, nrow(data) / length(unique(data$Variable)), step_days),
                       labels = data$fecha[seq(1, nrow(data) / length(unique(data$Variable)), step_days)]) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = angle_text_x, vjust = 0.5),
          axis.text = element_text(color = "black", size = size_axis_text),
          axis.title = element_text(size = size_title_axis),
          strip.text = element_text(size = size_title_facet),
          legend.text = element_text(size = size_legend_text),
          legend.title = element_text(size = size_legend_title)) +
    labs(x = "", y = ylab, color = "") +
    
    # Si 'Tasa Captura' está presente en las variables seleccionadas, agregar eje secundario
    { 
      if (all(var == 'tasa_captura')) {
        scale_y_continuous(
          name = "Tasa Captura",
          sec.axis = sec_axis(~., name = "Tasa Captura", breaks = seq(0, 1, by = 0.1))
        )
      } else if ('tasa_captura' %in% var) {
        # Ajustamos el eje secundario con el rango reescalado
        scale_y_continuous(
          sec.axis = sec_axis(~ . / max_value, name = "Tasa Captura", breaks = seq(0, 1, by = 0.1))
        )
      } else {
        scale_y_continuous()
      }
    }
  
  print(plot_line)
}




plot_desembarques_temporada <- function(data, var, ylab, angle_text_x = 90, 
                                        size_title_axis = 14, size_axis_text = 10, size_title_facet = 14,
                                        size_legend_text = 12, size_legend_title = 14) {
  
  # Realizar el cálculo sobre `data` (sumar y calcular tasa de captura por Temporada)
  data <- data %>%
    filter(!is.na(Escape_n)) %>%
    group_by(Temporada) %>%
    summarise(
      Captura_n = sum(Captura_n, na.rm = TRUE),
      Escape_n = sum(Escape_n, na.rm = TRUE)
    ) %>%
    mutate(
      total = Captura_n + Escape_n,
      tasa_captura = Captura_n / total
    ) %>%
    ungroup()  # Para que no quede agrupado por Temporada
  
  # Verificar si `var` contiene más de una columna
  if (length(var) > 1) {
    # Convertir a formato largo para manejar múltiples variables
    data_long <- data %>%
      pivot_longer(cols = all_of(var), names_to = "Variable", values_to = "Value")
  } else {
    # Usar una sola columna si `var` tiene solo un valor
    data_long <- data %>% mutate(Variable = var, Value = .data[[var]])
  }
  
  # Si "Tasa Captura" está en las variables seleccionadas, multiplicarla por un factor de escala
  max_value <- NULL  # Iniciar `max_value` como NULL
  if ('tasa_captura' %in% var && length(var) > 1) {
    # Solo calcular `max_value` si hay más de una variable seleccionada
    max_value <- max(data_long %>% filter(Variable != "tasa_captura") %>% pull(Value), na.rm = TRUE)
    # Multiplicamos "tasa_captura" por el valor máximo de las otras variables para reescalar
    data_long <- data_long %>%
      mutate(Value = ifelse(Variable == 'tasa_captura', Value * max_value, Value))
  }
  
  plot_line <- data_long %>%
    ggplot(aes(x = Temporada, y = Value, color = Variable, group = Variable)) +
    geom_line() +
    geom_point() +
    theme_bw() +
    theme(axis.text.x = element_text(angle = angle_text_x, vjust = 0.5),
          axis.text = element_text(color = "black", size = size_axis_text),
          axis.title = element_text(size = size_title_axis),
          legend.text = element_text(size = size_legend_text),
          legend.title = element_text(size = size_legend_title)) +
    labs(x = "", y = ylab, color = "") +
    
    # Si 'Tasa Captura' está presente en las variables seleccionadas, agregar eje secundario
    {
      if (!is.null(max_value)) {
        scale_y_continuous(
          sec.axis = sec_axis(~ . / max_value, name = "Tasa Captura", breaks = seq(0, 1, by = 0.1))
        )
      } else {
        scale_y_continuous()
      }
    }
  
  print(plot_line)
}




# Función para obtener los colores y etiquetas
get_sex_colores_y_labels <- function(data, input_sex) {
  data_plot <- data %>% filter(SEX %in% input_sex)
  
  # Colores
  colores_sex <- c("Hembra" = "pink", "Macho" = "blue", "Indet" = "green")
  sex_levels <- unique(data_plot$SEX)
  colores_sex <- colores_sex[sex_levels]
  
  # Etiquetas
  labels_sex <- sex_levels
  
  list(colores = colores_sex, labels = labels_sex)
}


plot_mean_length = function(data, colores, labels, size_axis_text = 12, angle_text_x = 90, size_title_axis = 14){
  
  tmedia_sex <- data %>%
    mutate(SEX = factor(SEX, levels = c("Hembra","Macho","Indet"))) %>%
    group_by(season, SEX) %>%
    reframe(tmedia = mean(TL_CM, na.rm = TRUE))
  
  p = tmedia_sex %>%
    ggplot() +
    geom_line(aes(x = season, y = tmedia, group = SEX, col = SEX)) +
    geom_point(aes(x = season, y = tmedia, group = SEX, col = SEX)) +
    scale_color_manual(values = colores, labels = labels) +
    guides(colour = guide_legend(override.aes = list(size = 2))) +
    theme_bw() +
    labs(y = "Longitud Total Promedio (cm)\n", x = "\nTemporada", col = "") +
    theme(axis.text = element_text(colour = "black", size = size_axis_text),
          axis.text.x = element_text(angle = angle_text_x),
          legend.text = element_text(size = 12),
          legend.key.size = unit(0.8,"cm"),
          axis.title = element_text(size = size_title_axis))
  
  print(p)
  
}



plot_boxplot_sex = function(data, colores, labels, alpha){
  
  plot_bx_sex = data  %>%
    mutate(SEX = factor(SEX, levels = labels, labels = labels)) %>%
    ggplot() +
    geom_boxplot(aes(x = interaction(SEX, season, sep = "&"), y = TL_CM, fill = SEX), alpha = alpha)  +
    scale_x_discrete(guide = guide_axis_nested(delim = "&")) +
    labs(y = "Longitud Total (cm)", x = "", size = "n", fill = "Sexo") +
    scale_fill_manual(values = colores, labels = labels) +
    theme_bw() +
    theme(axis.text = element_text(color = "black", size = 10))
  
  print(plot_bx_sex)
  
}


plot_tallas_gridges <- function(data, 
                                show = c("Macho", "Hembra", "Total"), 
                                order_years = "decreasing", 
                                colores, 
                                labels,
                                size_axis_text = 12,
                                angle_text_x = 0, 
                                size_title_axis = 14) {
  
  # Establish year order
  if(order_years == "decreasing") {
    lvls <- unique(rev(data$season))
  } else if (order_years == "increasing") {
    lvls <- unique(data$season)
  } else {
    stop("The 'order_years' argument must be 'decreasing' or 'increasing'.")
  }
  
  # Prepare data
  data_filtered <- data %>%
    mutate(
      season = factor(season, levels = lvls),
      SEX = factor(SEX, 
                   levels = c("Hembra", "Macho", "Indet"), 
                   labels = c("Hembra", "Macho", "Indet"))
    )
  
  # Create plot base
  p_final <- ggplot(data_filtered, aes(x = TL_CM, y = season, fill = SEX))
  
  # Add density ridges layers based on show parameter
  if ("Macho" %in% show) {
    p_final <- p_final + 
      geom_density_ridges(
        data = data_filtered %>% filter(SEX == "Macho"),
        scale = 1, 
        alpha = 0.6
      )
  }
  
  if ("Hembra" %in% show) {
    p_final <- p_final + 
      geom_density_ridges(
        data = data_filtered %>% filter(SEX == "Hembra"),
        scale = 1, 
        alpha = 0.6
      )
  }
  
  if ("Indet" %in% show) {
    p_final <- p_final + 
      geom_density_ridges(
        data = data_filtered %>% filter(SEX == "Indet"),
        scale = 1, 
        alpha = 0.6
      )
  }
  
  if ("Total" %in% show) {
    p_final <- p_final + 
      geom_density_ridges(
        scale = 1, 
        alpha = 0.6, 
        fill = "grey"
      )
  }
  
  # Customize plot
  p_final <- p_final +
    scale_fill_manual(
      values = colores, 
      labels = labels
    ) +
    labs(
      x = "Longitud Total (cm)", 
      y = "", 
      fill = ""
    ) +
    theme_bw() +
    theme(
      axis.text = element_text(colour = "black", size = size_axis_text),
      axis.text.x = element_text(angle = angle_text_x),
      legend.text = element_text(size = 12),
      legend.key.size = unit(0.8, "cm"),
      axis.title = element_text(size = size_title_axis)
    )
  
  # Return plot
  return(p_final)
}


filter_only_IGS = function(data, umbral_IGS = 15) {
  
  if(any(data$IGS > umbral_IGS, na.rm = TRUE)){
    cat("Hay", sum(data$IGS > umbral_IGS, na.rm = TRUE) ,"valores de IGS mayores a ",umbral_IGS,", serán elimminadas; se recomienda revisar\n\n")
    table(data[data$IGS > umbral_IGS,]$IGS)
  }
  
  
  if(sum(is.na(data$IGS)) > 0){
    cat("Hay", sum(is.na(data$IGS)) ,"valores de IGS con NA, serán elimminadas; se recomienda revisar\n\n")
  }
  
  data_final = data[data$IGS <= umbral_IGS & !is.na(data$IGS),]
  
  if(nrow(data) != nrow(data_final)){
    cat("En total se eliminaron", (nrow(data) - nrow(data_final)),"filas de los datos\n\n")
  }
  
  return(data_final)
  
}


plot_boxplot_igs_sex = function(data, colores, labels, size_axis_text = 10, size_title_axis = 14) {
  
  lvls_sex = unique(data$SEX)
  
  if(length(lvls_sex) != length(labels)) {
    stop("verificar los labels, los levels son ", paste0(lvls_sex," "))
  }
  
  bxp_sex_season = data %>%
    mutate(mes = factor(MONTH, levels = c(9, 10,11, 12, 1, 2, 3)),
           SEX = factor(SEX, levels = lvls_sex, labels = labels)) %>%
    filter(!is.na(mes)) %>%
    ggplot(aes(x = interaction(mes, SEX, season, sep = "&"), y = IGS, fill = SEX)) +
    geom_boxplot() +
    scale_x_discrete(guide = guide_axis_nested(delim = "&")) +
    scale_fill_manual(values = colores, labels = labels) +
    labs(x = "") +
    theme_bw() +
    theme(legend.title = element_blank(),
          panel.grid.major = element_blank(),
          legend.key.height = unit(0.4, "cm"),
          legend.key.width = unit(0.3, "cm"),
          axis.title = element_text(size = size_title_axis),
          axis.text = element_text(color = "black", size = size_axis_text),
          legend.text = element_text(size = 14)) +
    guides(fill = guide_legend(ncol = 1))
  
  print(bxp_sex_season)
  
}



