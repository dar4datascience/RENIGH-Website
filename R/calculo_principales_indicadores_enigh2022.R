## Carga el paquete foreign, el cual auxiliará para cargar los datos en diferentes formatos
#(DBF, SPSS, STATA, SAS)
library(foreign)
## Carga el paquete survey. Esta librería sirve para el cálculo del diseño muestral
library(survey)
## Librería que permite hacer un ordenamiento de la tabla según el ingreso
library(doBy)
## Librería que incluye la función para el cálculo del GINI
library(reldist)
## Opción para tratar los casos de los estratos con una sola una UPM
options(survey.lonely.psu = "adjust")

get_tbl_concentrado_hogar <- function(file_path) {
  # Read the DBF file
  Conc <- read.dbf(file_path, as.is = TRUE)
  
  # Select the variables of interest
  selected_vars <- c("folioviv", "foliohog", "ing_cor", "ingtrab", "trabajo", "negocio",
                     "otros_trab", "rentas", "utilidad", "arrenda", "transfer", "jubilacion",
                     "becas", "donativos", "remesas", "bene_gob", "transf_hog", "trans_inst",
                     "estim_alqu", "otros_ing", "factor", "upm", "est_dis")
  
  Conc <- Conc[selected_vars]
  
  # Create a flag for numbering households
  Conc$Nhog <- 1
  
  return(Conc)
}

generate_columna_deciles <- function() {
  Numdec <- c("Total", "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X")
  return(Numdec)
}

# Sort Data Function
sort_data_by_income_and_location <- function(data) {
  # Sort the data frame by income ('ing_cor'), household ID ('folioviv'), and household number ('foliohog')
  sorted_data <- orderBy(~ +ing_cor + folioviv + foliohog, data = data)
  return(sorted_data)
}


# Compute Totals Function
compute_total_households <- function(data) {
  # Calculate the total households and total factor for each group
  total_hh <- sum(data$factor)
  return(total_hh)
}

# Define a function to split and update the data
split_and_update <- function(data, threshold) {
  a1 <- data[dim(data[data$ACUMULA < threshold, ])[1] + 1, ]$factor
  data_split <- rbind(data[1:(dim(data[data$ACUMULA < threshold, ])[1] + 1), ],
                      data[(dim(data[data$ACUMULA < threshold, ])[1] + 1):dim(data)[1], ])
  b1 <- threshold - data[dim(data[data$ACUMULA < threshold, ])[1], ]$ACUMULA
  data_split[(dim(data_split[data_split$ACUMULA < threshold, ])[1] + 1), ]$factor <- b1
  data_split[(dim(data_split[data_split$ACUMULA < threshold, ])[1] + 2), ]$factor <- (a1 - b1)
  return(data_split)
}

# Define a function to assign deciles based on cumulative sums
assign_deciles <- function(data, thresholds) {
  data <- map_dfr(thresholds, ~ split_and_update(data, .x))
  
  # Assign deciles based on cumulative sums
  data <- data |>
    mutate(DECIL = 0) |>
    group_by(ACUMULA2) |>
    mutate(DECIL = ifelse(row_number() == 1, 1, DECIL)) |>
    ungroup() |>
    filter(DECIL > 0)
  
  # Assign decile 10 to rows where 'DECIL' is 0 (if any)
  data <- data |>
    mutate(DECIL = ifelse(DECIL == 0, 10, DECIL))
  
  return(data)
}

# Calculate Deciles Function
calculate_decile_values <- function(data, total_hh) {
  cat("Calculating deciles...\n")
  
  # Create a copy of the data frame
  BD1 <- data
  
  # Create the MAXT variable
  BD1$MAXT <- BD1$ing_cor
  
  # Sort BD1
  BD1 <- BD1[with(BD1, order(rank(MAXT))),]
  
  # Calculate cumulative sums
  BD1$ACUMULA <- cumsum(BD1$factor)
  
  # Initialize variables
  tam_dec <- trunc(total_hh / 10)
  BD1$tam_dec <- tam_dec
  BD1$DECIL <- 0
  
  # Calculate deciles using the split_and_update and assign_deciles functions
  thresholds <- seq(0, max(BD1$ACUMULA2), length.out = 10)
  BD1 <- assign_deciles(split_and_update(BD1, thresholds), thresholds)
  
  cat("Deciles calculated.\n")
  
  return(BD1)
}


# Calculate Average Income Function
calculate_average_income <- function(data, total_hh) {
  cat("Calculating average income...\n")
  
  # Calculate the average income for households and deciles
  x <- tapply(data$factor, data$Nhog, sum)
  y <- tapply(data$factor, data$DECIL, sum)
  ing_cormed_t <- tapply(data$factor * data$ing_cor, data$Nhog, sum) / x
  ing_cormed_d <- tapply(data$factor * data$ing_cor, data$DECIL, sum) / y
  
  cat("Average income calculated.\n")
  
  return(list(ing_cormed_t = ing_cormed_t, ing_cormed_d = ing_cormed_d))
}

# Main Function - Calculate Deciles
calculate_deciles <- function(data) {
  cat("Starting decile calculation...\n")
  
  # Sort the data
  sorted_data <- sort_data(data)
  
  # Compute totals
  total_hh <- compute_totals(sorted_data)
  
  # Calculate decile values and assign decile numbers
  decile_data <- calculate_decile_values(sorted_data, total_hh)
  
  # Calculate average income
  avg_income <- calculate_average_income(decile_data, total_hh)
  
  cat("Decile calculation completed.\n")
  
  return(list(decile_data = decile_data, avg_income = avg_income))
}

calculate_and_display_decile_income_gini <- function(data) {
  # Calculate deciles and average income
  decile_results <- calculate_decile_values(data, sum(data$factor))
  
  # Create a data frame with decile labels and average income
  prom_rub <- data.frame(ing_cormed_t = decile_results$ing_cormed_t, ing_cormed_d = decile_results$ing_cormed_d)
  
  # Add row names
  row.names(prom_rub) <- generate_columna_deciles()
  
  # Calculate GINI coefficient
  deciles_hog_ingcor <- data.frame(
    hogaresxdecil = rep(x, 10),
    ingreso = ing_cormed_d
  )
  
  # Calculate GINI coefficient
  a <- gini(deciles_hog_ingcor$ingreso, weights = deciles_hog_ingcor$hogares)
  
  # Rename columns
  names(prom_rub) <- "INGRESO CORRIENTE"
  names(a) <- "GINI"
  
  # Print results
  cat("Average Income:\n")
  print(round(prom_rub, 2))
  cat("\nGINI Coefficient:\n")
  print(round(a, 3))
}

generate_column_entidades <- function() {
  # Define the vector of entidades federativas
  Entidades <- c("Estados Unidos Mexicanos", "Aguascalientes", "Baja California",
                 "Baja California Sur", "Campeche", "Coahuila de Zaragoza", "Colima",
                 "Chiapas", "Chihuahua", "Ciudad de México", "Durango", "Guanajuato",
                 "Guerrero", "Hidalgo", "Jalisco", "Estado de México", "Michoacán de Ocampo",
                 "Morelos", "Nayarit", "Nuevo León", "Oaxaca", "Puebla", "Querétaro",
                 "Quintana Roo", "San Luis Potosí", "Sinaloa", "Sonora", "Tabasco",
                 "Tamaulipas", "Tlaxcala", "Veracruz de Ignacio de la Llave", "Yucatán",
                 "Zacatecas")
  
  # Return the vector of entidades federativas
  return(Entidades)
}



# Define a function to compute totals and entidad for an income indicator
compute_indicator_totals_and_entidad <- function(data, indicator_name, pb) {
  cat("Computing totals and entidad for", indicator_name, "...\n")
  
  # Create the survey design
  mydesign <- svydesign(id = ~upm, strata = ~est_dis, data = data, weights = ~factor)
  
  # Compute totals
  total_result <- svyratio(as.formula(paste("~", indicator_name)), denominator = ~Nhog, design = mydesign)
  
  # Compute entidad
  entidad_results <- list()
  for (entidad in unique(data$entidad)) {
    entidad_result <- svyby(as.formula(paste("~", indicator_name)), denominator = ~Nhog, by = ~entidad, design = mydesign, svyratio)
    entidad_results[[as.character(entidad)]] <- entidad_result
    pb$tick()
  }
  
  cat("Totals and entidad for", indicator_name, "computed.\n")
  
  # Return a list with results for total and entidad
  return(list(
    total_result = total_result,
    entidad_results = entidad_results
  ))
}

# Define a function to load income indicators for total and entidad with a progress bar
load_indicadores_ingreso_total_y_nacional <- function(data, income_indicators) {
  cat("Loading income indicators for total and nacional...\n")
  

  
  # Create a progress bar
  pb <- progress_bar$new(
    format = "[:bar] :percent",
    total = length(income_indicators),
    clear = TRUE,
    width = 60
  )
  
  # Initialize a list to store results
  indicator_results <- list()
  
  # Compute totals and entidad for each income indicator
  for (indicator in income_indicators) {
    print(paste("Calculating for indicator:", indicator))
    results <- compute_indicator_totals_and_entidad(data, indicator, pb)
    indicator_results[[indicator]] <- results
  }
  
  cat("Income indicators loaded.\n")
  
  # Return the loaded indicators
  return(indicator_results)
}


extract_estimaciones <- function(income_results) {
  estimaciones <- income_results |>
    imap(function(indicator, results) {
      list(
        paste0("ES_", indicator, "Tot") = results[["total_result"]],
        paste0("ES_", indicator, "Ent") = results[["entidad_results"]]
      )
    }) |>  
    reduce(list)
  
  return(estimaciones)
}

calculate_and_extract_standard_errors <- function(indicators) {
  standard_errors <- indicators |>
    set_names(paste0("SE_", names(.))) |>
    map(~ SE(.x))
  
  return(standard_errors)
}

calculate_and_extract_coefficient_of_variation <- function(indicators) {
  coefficients_of_variation <- indicators |>
    set_names(paste0("CV_", names(.))) |>
    map(~ cv(.x))
  
  return(coefficients_of_variation)
}


calculate_and_extract_lower_limits <- function(indicators, confidence_level = 0.90) {
  lower_limits <- indicators |>
    set_names(paste0("LI_", names(.))) |>
    map(~ confint(.x, level = confidence_level)[, 1])
  
  return(lower_limits)
}

calculate_and_extract_upper_limits <- function(indicators, confidence_level = 0.90) {
  upper_limits <- indicators |>
    set_names(paste0("LS_", names(.))) |>
    map(~ confint(.x, level = confidence_level)[, 2])
  
  return(upper_limits)
}


build_presentation_tables_ingreso_corriente_trimestral <- function(income_results) {
  # Extract estimaciones
  estimaciones <- extract_estimaciones(income_results)
  
  # Calculate standard errors
  standard_errors <- calculate_and_extract_standard_errors(estimaciones)
  
  # Calculate coefficients of variation
  coefficients_of_variation <- calculate_and_extract_coefficient_of_variation(estimaciones)
  
  # Calculate lower limits
  lower_limits <- calculate_and_extract_lower_limits(estimaciones)
  
  # Calculate upper limits
  upper_limits <- calculate_and_extract_upper_limits(estimaciones)
  
  # Combine the results into data frames
  c_ent_ES <- data.frame(estimaciones)
  c_ent_SE <- data.frame(standard_errors)
  c_ent_CV <- data.frame(coefficients_of_variation)
  c_ent_LI <- data.frame(lower_limits)
  c_ent_LS <- data.frame(upper_limits)
  
  # Se renombran las variables
  variable_names <- c("ING COR", "TRABAJO", "SUBORDINADO", "NEGOCIOS", "OTROS TRAB",
                      "RENTAS", "UTILIDAD", "ARRENDA", "TRANSFER", "JUBILACION", "BECAS",
                      "DONATIVOS", "REMESAS", "BENE GOBIERNO", "TRANS HOG", "TRANS INST",
                      "ESTIM ALQU", "OTROS INGRESOS")
  
  list(
    "Estimación" = c_ent_ES,
    "Error Estándar" = c_ent_SE,
    "Coeficiente de Variación" = c_ent_CV,
    "Límite Inferior" = c_ent_LI,
    "Límite Superior" = c_ent_LS
  ) |>
    imap(function(data_frame, name) {
      names(data_frame) <- variable_names
      return(data_frame)
    })
  
  # Round the data frames
  c_ent_ES <- round(c_ent_ES)
  c_ent_SE <- round(c_ent_SE)
  c_ent_CV <- round(c_ent_CV, 4) * 100
  c_ent_LI <- round(c_ent_LI)
  c_ent_LS <- round(c_ent_LS)
  
  # Print the rounded data frames
  print(c_ent_ES)
  print(c_ent_SE)
  print(c_ent_CV)
  print(c_ent_LI)
  print(c_ent_LS)
  
  # Return the main result
  return(
    list(
      "Estimación" = c_ent_ES,
      "Error Estándar" = c_ent_SE,
      "Coeficiente de Variación" = c_ent_CV,
      "Límite Inferior" = c_ent_LI,
      "Límite Superior" = c_ent_LS
    )
  )
}

load_indicadores_gasto_total_y_nacional <- function(data, indicadores_de_gasto) {
  cat("Loading gasto indicators for total and nacional...\n")
  
  # Create the survey design
  mydesign <- svydesign(id = ~upm, strata = ~est_dis, data = data, weights = ~factor)
  
  # Initialize a list to store results
  indicator_results <- list()
  
  # Compute totals and entidad for each gasto indicator
  for (indicator_name in indicadores_de_gasto) {
    cat("Computing totals and entidad for", indicator_name, "...\n")
    
    # Compute totals
    total_result <- svyratio(as.formula(paste("~", indicator_name)), denominator = ~Nhog, design = mydesign)
    
    # Compute entidad
    entidad_results <- svyby(as.formula(paste("~", indicator_name)), denominator = ~Nhog, by = ~entidad, design = mydesign, svyratio)
    
    # Store the results in a list
    indicator_results[[indicator_name]] <- list(
      total_result = total_result,
      entidad_results = entidad_results
    )
    
    cat("Totals and entidad for", indicator_name, "computed.\n")
  }
  
  cat("Gasto indicators loaded.\n")
  
  # Return the loaded indicators
  return(indicator_results)
}


# Define the extract_gasto_ES function (if not already defined)
extract_gasto_ES <- function(gasto_results) {
  gasto_indicators <- c(
    "gasto_mon", "alimentos", "vesti_calz", "vivienda", "limpieza",
    "salud", "transporte", "educa_espa", "personales", "transf_gas"
  )
  
  ES_list <- list()
  
  for (indicator in gasto_indicators) {
    ES_list[[paste0("ES_", indicator, "Tot")]] <- gasto_results[[indicator]][[1]]
    ES_list[[paste0("ES_", indicator, "Ent")]] <- gasto_results[[indicator]][[2]]
  }
  
  return(ES_list)
}

# Create a function to calculate and extract standard errors for a list of indicators
calculate_and_extract_standard_errors_for_gasto <- function(indicators) {
  standard_errors <- calculate_and_extract_standard_errors(indicators)
  
  # Rename the standard errors with the appropriate names
  names(standard_errors) <- sub("SE_", "SE_M_", names(standard_errors))
  
  return(standard_errors)
}

# Define a function to calculate and extract CV for gasto indicators
calculate_and_extract_coefficient_of_variation_for_gasto <- function(gasto_indicators) {
  # Calculate and extract CV for gasto indicators
  CV_for_gasto <- calculate_and_extract_coefficient_of_variation(gasto_indicators)
  
  # Rename the list elements to match the indicator names
  names(CV_for_gasto) <- gasto_indicators
  
  return(CV_for_gasto)
}

# Define a function to calculate and extract lower limits (LI) for gasto indicators
calculate_and_extract_lower_limits_for_gasto <- function(gasto_indicators) {
  # Calculate and extract LI for gasto indicators
  LI_for_gasto <- calculate_and_extract_lower_limits(gasto_indicators, confidence_level = 0.90)
  
  # Rename the list elements to match the indicator names
  names(LI_for_gasto) <- gasto_indicators
  
  return(LI_for_gasto)
}

# Define a function to calculate and extract upper limits (LS) for gasto indicators
calculate_and_extract_upper_limits_for_gasto <- function(gasto_indicators) {
  # Calculate and extract LS for gasto indicators
  LS_for_gasto <- calculate_and_extract_upper_limits(gasto_indicators, confidence_level = 0.90)
  
  # Rename the list elements to match the indicator names
  names(LS_for_gasto) <- gasto_indicators
  
  return(LS_for_gasto)
}


# Define a function to build presentation tables for gasto corriente
build_presentation_tables_gasto_corriente <- function(gasto_results) {
  # Extract ES, SE, CV, LI, and LS
  ES_for_gasto <- extract_estimaciones(gasto_results)
  SE_for_gasto <- calculate_and_extract_standard_errors(ES_for_gasto)
  CV_for_gasto <- calculate_and_extract_coefficient_of_variation(ES_for_gasto)
  LI_for_gasto <- calculate_and_extract_lower_limits(ES_for_gasto)
  LS_for_gasto <- calculate_and_extract_upper_limits(ES_for_gasto)
  
  # Combine the results into data frames
  c_gas_ES <- data.frame(ES_for_gasto)
  c_gas_SE <- data.frame(SE_for_gasto)
  c_gas_CV <- data.frame(CV_for_gasto)
  c_gas_LI <- data.frame(LI_for_gasto)
  c_gas_LS <- data.frame(LS_for_gasto)
  
  # Set variable names
  variable_names <- c("GASTO MON", "ALIMENTOS", "VEST y CALZ", "VIVIENDA", "LIMPIEZA",
                      "SALUD", "TRANSPORTE", "EDUCACION", "PERSONALES", "TRANS DE GASTO")
  
  # Rename the columns with variable names
  names(c_gas_ES) <- names(c_gas_SE) <- names(c_gas_CV) <- names(c_gas_LI) <- names(c_gas_LS) <- variable_names
  
  # Round the data frames
  c_gas_ES <- round(c_gas_ES)
  c_gas_SE <- round(c_gas_SE)
  c_gas_CV <- round(c_gas_CV, 4) * 100
  c_gas_LI <- round(c_gas_LI)
  c_gas_LS <- round(c_gas_LS)
  
  # Print the rounded data frames
  print(c_gas_ES)
  print(c_gas_SE)
  print(c_gas_CV)
  print(c_gas_LI)
  print(c_gas_LS)
  
  # Return the main result as a named list
  return(
    list(
      "Estimación" = c_gas_ES,
      "Error Estándar" = c_gas_SE,
      "Coeficiente de Variación" = c_gas_CV,
      "Límite Inferior" = c_gas_LI,
      "Límite Superior" = c_gas_LS
    )
  )
}

library(dplyr)

build_data_4_gasto_corriente_trimestral <- function(data) {
  transformed_data <- data |>
    mutate(
      tam = ifelse(tam_loc <= 3, 1, 2),
      tam0 = 0,
      A00 = as.integer(gasto_mon > 0),
      A01 = as.integer(alimentos > 0),
      A05 = as.integer(vesti_calz > 0),
      A08 = as.integer(vivienda > 0),
      A13 = as.integer(limpieza > 0),
      A17 = as.integer(salud > 0),
      A18 = as.integer(transporte > 0),
      A24 = as.integer(educa_espa > 0),
      A28 = as.integer(personales > 0),
      A32 = as.integer(transf_gas > 0)
    ) |>
    rename_with(~ paste0("B", formatC(.x, width = 2, flag = "0")), starts_with("A")) |>
    select(folioviv, foliohog, upm, est_dis, factor, tam, tam0, starts_with("B")) |>
    gather(ING, Pob, starts_with("B")) |>
    select(folioviv, foliohog, upm, est_dis, factor, tam, tam0, H_TIPO = ING, HOG = Pob, I_TIPO = ING, ING = Pob)
  
  return(transformed_data)
}

calculate_statistics_gasto_corriente <- function(Conc6) {
  library(survey)
  
  print("Creating survey design...")
  # Diseño muestral
  mydesign6 <- svydesign(id = ~upm, strata = ~est_dis, data = Conc6, weights = ~factor)
  
  print("Calculating statistics for households (I2)...")
  # Calculate statistics for households (I2)
  I2 <- svyby(~ING, by = ~I_TIPO + ~tam0, mydesign6, svytotal)
  H2 <- svyby(~HOG, by = ~H_TIPO + ~tam0, mydesign6, svytotal)
  
  print("Calculating statistics for households (I3)...")
  # Calculate statistics for households (I3)
  I3 <- svyby(~ING, by = ~I_TIPO + ~tam, mydesign6, svytotal)
  H3 <- svyby(~HOG, by = ~H_TIPO + ~tam, mydesign6, svytotal)
  
  # Etiquetas de filas
  gastos <- c(
    "GASTO CORRIENTE MONETARIO", "ALIMENTOS, BEBIDAS Y TABACO", "VESTIDO Y CALZADO",
    "VIVIENDA Y SERVICIOS DE CONSERVACIÓN", "ARTÍCULOS Y SERVICIOS PARA LA LIMPIEZA, CUIDADOS DE LA CASA, ENSERES DOMÉSTICOS",
    "CUIDADOS DE LA SALUD", "TRANSPORTE; ADQUISICIÓN, MANTENIMIENTO, ACCESORIOS Y SERVICIOS",
    "SERVICIOS DE EDUCACIÓN, ARTÍCULOS EDUCATIVOS, ARTÍCULOS DE ESPARCIMIENTO",
    "CUIDADOS PERSONALES, ACCESORIOS Y EFECTOS", "TRANSFERENCIAS DE GASTO"
  )
  
  print("Renaming variables...")
  # Renombrar variables
  colnames(I2)[2] <- colnames(I3)[2] <- colnames(H2)[2] <- colnames(H3)[2] <- "tam"
  
  print("Combining survey results (IN1 and HO1)...")
  # Combine the survey results for individuals (IN1) and households (HO1)
  IN1 <- rbind(I2, I3)
  HO1 <- rbind(H2, H3)
  
  print("Calculating statistics for households...")
  # Precisiones estadísticas for households
  ES_H1 <- coef(HO1)
  EE_H1 <- SE(HO1)
  CV_H1 <- cv(HO1) * 100
  LI_H1 <- confint(HO1, level = 0.90)[, 1]
  LS_H1 <- confint(HO1, level = 0.90)[, 2]
  H <- cbind(HO1[, 1:2], ES_H1, EE_H1, CV_H1, LI_H1, LS_H1)
  
  # Subset and reshape the data for households
  ES_H2 <- H[c("H_TIPO", "tam", "ES_H1")]
  EE_H2 <- H[c("H_TIPO", "tam", "EE_H1")]
  CV_H2 <- H[c("H_TIPO", "tam", "CV_H1")]
  LI_H2 <- H[c("H_TIPO", "tam", "LI_H1")]
  LS_H2 <- H[c("H_TIPO", "tam", "LS_H1")]
  
  ES_H3 <- spread(ES_H2, tam, ES_H1)  # Spread the data to a wide format
  EE_H3 <- spread(EE_H2, tam, EE_H1)
  CV_H3 <- spread(CV_H2, tam, CV_H1)
  LI_H3 <- spread(LI_H2, tam, LI_H1)
  LS_H3 <- spread(LS_H2, tam, LS_H1)
  
  print("Calculating statistics for individuals...")
  # Precisiones estadísticas for individuals
  ES_I1 <- coef(IN1) / 1000
  EE_I1 <- SE(IN1) / 1000
  CV_I1 <- cv(IN1) * 100
  LI_I1 <- confint(IN1, level = 0.90)[, 1] / 1000
  LS_I1 <- confint(IN1, level = 0.90)[, 2] / 1000
  
  I <- cbind(IN1[, 1:2], ES_I1, EE_I1, CV_I1, LI_I1, LS_I1)
  
  # Subset and reshape the data for individuals
  ES_I2 <- I[c("I_TIPO", "tam", "ES_I1")]
  EE_I2 <- I[c("I_TIPO", "tam", "EE_I1")]
  CV_I2 <- I[c("I_TIPO", "tam", "CV_I1")]
  LI_I2 <- I[c("I_TIPO", "tam", "LI_I1")]
  LS_I2 <- I[c("I_TIPO", "tam", "LS_I1")]
  
  ES_I3 <- spread(ES_I2, tam, ES_I1)  # Spread the data to a wide format
  EE_I3 <- spread(EE_I2, tam, EE_I1)
  CV_I3 <- spread(CV_I2, tam, CV_I1)
  LI_I3 <- spread(LI_I2, tam, LI_I1)
  LS_I3 <- spread(LS_I2, tam, LS_I1)
  
  return(
    list(
      "Hogares" = list(
        "Estimación" = ES_H3,
        "Error Estándar" = EE_H3,
        "Coeficiente de Variación" = CV_H3,
        "Límite Inferior" = LI_H3,
        "Límite Superior" = LS_H3
      ),
      "Individuos" = list(
        "Estimación" = ES_I3,
        "Error Estándar" = EE_I3,
        "Coeficiente de Variación" = CV_I3,
        "Límite Inferior" = LI_I3,
        "Límite Superior" = LS_I3
      )
    )
  )
}

create_tables_gasto_corriente <- function(stats) {
  print("Creating tables...")
  
  # Define the column labels
  NOM <- c(
    "TOTAL HOGARES",
    "TOTAL GASTO",
    "MÁS DE 2 500 HOGARES",
    "MÁS DE 2 500 GASTO",
    "MENOS DE 2 500 HOGARES",
    "MENOS DE 2 500 GASTO"
  )
  
  # Create a function to generate a table
  create_table <- function(data) {
    table <- data.frame(data[, 2:4], data[, 2:4])
    colnames(table) <- c("Estimación", "Error Estándar", "Coeficiente de Variación")
    return(table)
  }
  
  # Create tables
  TOTAL <- create_table(stats$Hogares$Estimación)
  SE_TOTAL <- create_table(stats$Hogares$Error Estándar)
  CV_TOTAL <- create_table(stats$Hogares$Coeficiente de Variación)
  LI_TOTAL <- create_table(stats$Hogares$Límite Inferior)
  LS_TOTAL <- create_table(stats$Hogares$Límite Superior)
  
  # Set column labels
  colnames(TOTAL) <- colnames(SE_TOTAL) <- colnames(CV_TOTAL) <- colnames(LI_TOTAL) <- colnames(LS_TOTAL) <- NOM
  
  # Set row labels
  rownames(TOTAL) <- rownames(SE_TOTAL) <- rownames(CV_TOTAL) <- rownames(LI_TOTAL) <- rownames(LS_TOTAL) <- gastos
  
  print("Displaying results...")
  # Display the results
  print(round(TOTAL, 8))
  print(round(SE_TOTAL, 8))
  print(round(CV_TOTAL, 8))
  print(round(LI_TOTAL, 8))
  print(round(LS_TOTAL, 8))
}

library(dplyr)

prepare_pers_data <- function(Pers) {
  Pers <- Pers |>
    mutate(
      ID = paste(folioviv, foliohog, numren, sep = "."),
      int = ifelse((parentesco >= 400 & parentesco < 500) | (parentesco >= 700 & parentesco < 800), 0, 1),
      d_1 = ifelse(disc_camin %in% c(1, 2), 1, 0),
      d_2 = ifelse(disc_ver %in% c(1, 2), 1, 0),
      d_3 = ifelse(disc_brazo %in% c(1, 2), 1, 0),
      d_4 = ifelse(disc_apren %in% c(1, 2), 1, 0),
      d_5 = ifelse(disc_oir %in% c(1, 2), 1, 0),
      d_6 = ifelse(disc_vest %in% c(1, 2), 1, 0),
      d_7 = ifelse(disc_habla %in% c(1, 2), 1, 0),
      d_8 = ifelse(disc_acti %in% c(1, 2), 1, 0),
      d_9 = ifelse(disc_camin %in% "&", 1, 0),
      d_10 = ifelse(d_1 == 1 | d_2 == 1 | d_3 == 1 | d_4 == 1 | d_5 == 1 | d_6 == 1 | d_7 == 1 | d_8 == 1 | d_9 == 1, 1, 0),
      d_11 = ifelse(d_1 == 0 & d_2 == 0 & d_3 == 0 & d_4 == 0 & d_5 == 0 & d_6 == 0 & d_7 == 0 & d_8 == 0 & d_9 == 0, 1, 0)
    )
  
  return(Pers)
}

# Usage:
# Call this function with your "Pers" data frame as an argument
# For example:
# Pers_data <- prepare_pers_data(Pers)


prepare_conc_ingr_pers_data <- function() {
  # Read the DBF file for Conc
  Conc <- read.dbf("concentradohogar.dbf", as.is = TRUE)
  
  # Select the variables of interest for Conc
  selected_vars_conc <- c("folioviv", "foliohog", "ubica_geo", "tam_loc", "factor", "upm", "est_dis")
  Conc <- Conc[selected_vars_conc]
  
  # Create a special ID at the row level for reference
  Conc$ID <- paste(Conc$folioviv, Conc$foliohog, sep = ".")
  
  # Read the DBF file for Ingr
  Ingr <- read.dbf("ingresos.dbf", as.is = TRUE)
  
  # Select the variables of interest for Ingr
  selected_vars_ingr <- c("folioviv", "foliohog", "numren", "clave", "ing_tri")
  Ingr <- Ingr[selected_vars_ingr]
  
  # Aggregate income by person from the Ingr table
  ingr1 <- aggregate(ing_tri ~ folioviv + foliohog + numren, Ingr, sum)
  
  # Create a special ID at the row level for reference
  ingr1$ID <- paste(ingr1$folioviv, ingr1$foliohog, ingr1$numren, sep = ".")
  
  # Read the DBF file for Pers
  Pers <- read.dbf("poblacion.dbf", as.is = TRUE)
  
  # Select the variables of interest for Pers
  selected_vars_pers <- c(
    "folioviv", "foliohog", "numren", "parentesco", "edad", "sexo",
    "disc_camin", "disc_ver", "disc_brazo", "disc_apren",
    "disc_oir", "disc_vest", "disc_habla", "disc_acti"
  )
  Pers <- Pers[selected_vars_pers]
  # Use the prepare_pers_data function to preprocess Pers
  Pers <- prepare_pers_data(Pers)
  
  return(list("Conc" = Conc, "Ingr" = ingr1, "Pers" = Pers))
}

# Encapsulated Functions

# Function to create a special ID column
create_special_id <- function(data, id_columns) {
  # Combine specified columns to create a special ID
  data$ID <- do.call(paste, c(data[id_columns], sep = "."))
  return(data)
}

# Function to count and calculate disabilities and income
count_and_calculate_disabilities <- function(data, disability_columns, income_column) {
  for (i in 1:11) {
    # Calculate disability indicator columns
    data[[paste0("d_", i)]] <- ifelse(data[[disability_columns[i]]] %in% c(1, 2), 1, 0)
    
    # Calculate income for individuals with disabilities
    data[[paste0("ing_d_", i)]] <- ifelse(
      data$d_10 == 1 & data[[income_column]] > 0, data[[income_column]], 0
    )
  }
  
  return(data)
}

# Function to calculate survey design
calculate_survey_design <- function(data, upm_column, est_dis_column, factor_column) {
  mydesign <- svydesign(
    id = ~data[[upm_column]],
    strata = ~data[[est_dis_column]],
    data = data,
    weights = ~data[[factor_column]]
  )
  
  return(mydesign)
}

# Function to calculate summary statistics
calculate_summary_statistics <- function(design, variable, FUN = svymean) {
  result <- FUN(as.formula(paste("~", variable)), design)
  return(result)
}
