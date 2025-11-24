# =============================================================================
# CONFIGURACIÓN INICIAL - LIBRERÍAS Y DATOS
# =============================================================================

# Librerías críticas
library(tidyverse)
library(sf)
library(osmdata)
library(leaflet)
library(randomForest)
library(caret)
library(htmlwidgets)
library(dplyr)
library(tidyr)
library(purrr)


cat("✅ LIBRERÍAS CARGADAS CORRECTAMENTE\n")

# Cargar datos
train <- read_csv("https://www.dropbox.com/scl/fi/uy4oyx7v6udfxvck54x0g/train.csv?rlkey=80wyu4upvbdcxpq3nck6wwjzk&st=ss3u48q1&dl=1")  
test <- read_csv("https://www.dropbox.com/scl/fi/tk81o2h1yjx3b55evru8m/test.csv?rlkey=1np25kzap6e3zj8qdxbvpxue8&st=hkatxr60&dl=1")


cat(" DATOS CARGADOS:\n")
cat("Train:", dim(train), "| Test:", dim(test), "\n\n")

names(train)
names(test)
# =============================================================================
# PASO 1: DIAGNÓSTICO INICIAL DE MISSINGS
# =============================================================================

cat("=== DIAGNÓSTICO INICIAL DE MISSINGS ===\n")

# Missings por variable
nas_train <- colSums(is.na(train))
nas_test <- colSums(is.na(test))

cat("MISSINGS EN TRAIN:\n")
print(nas_train[nas_train > 0])

cat("\nMISSINGS EN TEST:\n")
print(nas_test[nas_test > 0])

# =============================================================================
# PASO 2:IMPUTACIÓN ROBUSTA CORREGIDA
# =============================================================================

cat("🔧 INICIANDO IMPUTACIÓN ROBUSTA...\n")

imputar_robusto <- function(train, test) {
  
  # Crear copias para no modificar los originales
  train_imp <- train
  test_imp <- test
  
  # Convertir variables categóricas a character para evitar problemas de contrast
  train_imp$property_type <- as.character(train_imp$property_type)
  train_imp$city <- as.character(train_imp$city)
  test_imp$property_type <- as.character(test_imp$property_type)
  test_imp$city <- as.character(test_imp$city)
  
  # 1. IMPUTACIÓN DE BATHROOMS (usando solo bedrooms para evitar problemas)
  cat("Imputando bathrooms...\n")
  bathroom_model <- lm(bathrooms ~ bedrooms, 
                       data = train_imp[!is.na(train_imp$bathrooms), ])
  
  # Aplicar a train
  na_bath_train <- is.na(train_imp$bathrooms)
  if(sum(na_bath_train) > 0) {
    train_imp$bathrooms[na_bath_train] <- predict(bathroom_model, newdata = train_imp[na_bath_train, ])
  }
  
  # Aplicar a test
  na_bath_test <- is.na(test_imp$bathrooms)
  if(sum(na_bath_test) > 0) {
    test_imp$bathrooms[na_bath_test] <- predict(bathroom_model, newdata = test_imp[na_bath_test, ])
  }
  
  # Redondear y asegurar valores mínimos
  train_imp$bathrooms <- pmax(round(train_imp$bathrooms), 1)
  test_imp$bathrooms <- pmax(round(test_imp$bathrooms), 1)
  
  # 2. IMPUTACIÓN DE SURFACE_TOTAL (modelo simple y robusto)
  cat("Imputando surface_total...\n")
  surface_model <- lm(surface_total ~ bedrooms + bathrooms,
                      data = train_imp[!is.na(train_imp$surface_total), ])
  
  # Aplicar a train
  na_surf_train <- is.na(train_imp$surface_total)
  if(sum(na_surf_train) > 0) {
    train_imp$surface_total[na_surf_train] <- predict(surface_model, newdata = train_imp[na_surf_train, ])
  }
  
  # Aplicar a test  
  na_surf_test <- is.na(test_imp$surface_total)
  if(sum(na_surf_test) > 0) {
    test_imp$surface_total[na_surf_test] <- predict(surface_model, newdata = test_imp[na_surf_test, ])
  }
  
  # Asegurar valores razonables
  train_imp$surface_total <- pmax(pmin(train_imp$surface_total, 500), 20)
  test_imp$surface_total <- pmax(pmin(test_imp$surface_total, 500), 20)
  
  # 3. IMPUTACIÓN DE SURFACE_COVERED (usar surface_total como base)
  cat("Imputando surface_covered...\n")
  na_covered_train <- is.na(train_imp$surface_covered)
  if(sum(na_covered_train) > 0) {
    train_imp$surface_covered[na_covered_train] <- train_imp$surface_total[na_covered_train] * 0.9
  }
  
  na_covered_test <- is.na(test_imp$surface_covered)
  if(sum(na_covered_test) > 0) {
    test_imp$surface_covered[na_covered_test] <- test_imp$surface_total[na_covered_test] * 0.9
  }
  
  # 4. IMPUTACIÓN DE ROOMS (usar bedrooms como proxy)
  na_rooms_train <- is.na(train_imp$rooms)
  if(sum(na_rooms_train) > 0) {
    train_imp$rooms[na_rooms_train] <- train_imp$bedrooms[na_rooms_train] + 1
  }
  
  na_rooms_test <- is.na(test_imp$rooms)
  if(sum(na_rooms_test) > 0) {
    test_imp$rooms[na_rooms_test] <- test_imp$bedrooms[na_rooms_test] + 1
  }
  
  # 5. VERIFICACIÓN FINAL
  cat("IMPUTACIÓN COMPLETADA\n")
  cat("NAs en train:", sum(is.na(train_imp[, c("surface_total", "surface_covered", "bathrooms", "rooms")])), "\n")
  cat("NAs en test:", sum(is.na(test_imp[, c("surface_total", "surface_covered", "bathrooms", "rooms")])), "\n")
  
  return(list(train = train_imp, test = test_imp))
}

# Aplicar imputación robusta
datos_imputados <- imputar_robusto(train, test)
train_imputado <- datos_imputados$train
test_imputado <- datos_imputados$test

# =============================================================================
# PASO 3: VARIABLES DE TEXTO
# =============================================================================

cat("\n=== CREANDO VARIABLES DE TEXTO ===\n")

crear_variables_texto <- function(df) {
  df %>%
    mutate(
      # Texto limpio
      title_clean = tolower(iconv(ifelse(is.na(title), "", title), to = "ASCII//TRANSLIT")),
      description_clean = tolower(iconv(ifelse(is.na(description), "", description), to = "ASCII//TRANSLIT")),
      
      # Longitud del texto
      title_length = nchar(title_clean),
      description_length = nchar(description_clean),
      tiene_descripcion = as.integer(!is.na(description) & description != ""),
      
      # Palabras clave estratégicas
      dummy_lujoso = as.integer(str_detect(title_clean, "lujos|exclusiv|premium|alto standing")),
      dummy_remodelado = as.integer(str_detect(title_clean, "remodelad|renov|nuev|a estrenar")),
      dummy_parqueadero = as.integer(str_detect(title_clean, "parqueader|garaje|estacionamiento")),
      dummy_ascensor = as.integer(str_detect(title_clean, "ascensor|elevador")),
      dummy_balcon = as.integer(str_detect(title_clean, "balc|terraza|patio")),
      
      # Score compuesto de calidad
      score_calidad_texto = (
        dummy_lujoso * 3 +
          dummy_remodelado * 2 +
          dummy_parqueadero * 2 +
          dummy_ascensor * 1 +
          dummy_balcon * 1
      )
    ) %>%
    select(-title_clean, -description_clean)
}

# Aplicar a los datos imputados
train_final <- crear_variables_texto(train_imputado)
test_final <- crear_variables_texto(test_imputado)

cat("Variables de texto creadas\n")

# =============================================================================
# PASO 4: EXTRACCIÓN DATOS OSM CHAPINERO
# =============================================================================

cat("\n=== EXTRACCIÓN DATOS OSM CHAPINERO ===\n")

# Bounding box de Chapinero
chapinero_bbox <- getbb("Chapinero Bogotá Colombia")
cat("Bounding Box Chapinero:\n")
print(chapinero_bbox)

# Extraer puntos de interés
cat("Extrayendo datos de OpenStreetMap...\n")

# Parques
parques <- opq(bbox = chapinero_bbox) |>
  add_osm_feature(key = "leisure", value = "park") |>
  osmdata_sf()

# Estaciones de TransMilenio
transmilenio <- opq(bbox = chapinero_bbox) |>
  add_osm_feature(key = "amenity", value = "bus_station") |>
  osmdata_sf()

# Puntos comerciales alternativos
restaurantes <- opq(bbox = chapinero_bbox) |>
  add_osm_feature(key = "amenity", value = "restaurant") |>
  osmdata_sf()

bancos <- opq(bbox = chapinero_bbox) |>
  add_osm_feature(key = "amenity", value = "bank") |>
  add_osm_feature(key = "amenity", value = "atm") |>
  osmdata_sf()

# Combinar puntos comerciales
puntos_comerciales <- NULL
if(!is.null(restaurantes$osm_points)) puntos_comerciales <- restaurantes$osm_points
if(!is.null(bancos$osm_points)) {
  if(is.null(puntos_comerciales)) {
    puntos_comerciales <- bancos$osm_points
  } else {
    columnas_comunes <- intersect(names(puntos_comerciales), names(bancos$osm_points))
    puntos_comerciales <- rbind(
      puntos_comerciales %>% select(all_of(columnas_comunes)),
      bancos$osm_points %>% select(all_of(columnas_comunes))
    )
  }
}

# Universidades
universidades <- opq(bbox = chapinero_bbox) |>
  add_osm_feature(key = "amenity", value = "university") |>
  osmdata_sf()

cat("EXTRACCIÓN OSM COMPLETADA:\n")
cat("Parques:", ifelse(!is.null(parques$osm_polygons), nrow(parques$osm_polygons), 0), "\n")
cat("Estaciones TM:", ifelse(!is.null(transmilenio$osm_points), nrow(transmilenio$osm_points), 0), "\n")
cat("Puntos comerciales:", ifelse(!is.null(puntos_comerciales), nrow(puntos_comerciales), 0), "\n")
cat("Universidades:", ifelse(!is.null(universidades$osm_polygons), nrow(universidades$osm_polygons), 0), "\n")

# =============================================================================
# PASO 5: CÁLCULO DE DISTANCIAS Y VARIABLES ESPACIALES
# =============================================================================

cat("\n=== CALCULANDO DISTANCIAS Y VARIABLES ESPACIALES ===\n")

# Función para calcular distancia mínima
calcular_distancia_minima <- function(puntos_propiedad, puntos_interes) {
  if(is.null(puntos_interes) || nrow(puntos_interes) == 0) return(NA_real_)
  tryCatch({
    distancias <- st_distance(puntos_propiedad, puntos_interes)
    min_distancias <- apply(distancias, 1, min, na.rm = TRUE)
    return(as.numeric(min_distancias))
  }, error = function(e) {
    return(rep(NA_real_, nrow(puntos_propiedad)))
  })
}

# Convertir propiedades a formato sf
cat("Convirtiendo propiedades a formato espacial...\n")
train_sf <- st_as_sf(train_final, coords = c("lon", "lat"), crs = 4326)
test_sf <- st_as_sf(test_final, coords = c("lon", "lat"), crs = 4326)

# Preparar puntos de interés
parques_centroides <- if(!is.null(parques$osm_polygons)) st_centroid(parques$osm_polygons) else NULL
universidades_centroides <- if(!is.null(universidades$osm_polygons)) st_centroid(universidades$osm_polygons) else NULL

# Calcular distancias
cat("Calculando distancias...\n")
train_final$dist_parque <- calcular_distancia_minima(train_sf, parques_centroides)
train_final$dist_tm <- calcular_distancia_minima(train_sf, transmilenio$osm_points)
train_final$dist_comercio <- calcular_distancia_minima(train_sf, puntos_comerciales)
train_final$dist_universidad <- calcular_distancia_minima(train_sf, universidades_centroides)

test_final$dist_parque <- calcular_distancia_minima(test_sf, parques_centroides)
test_final$dist_tm <- calcular_distancia_minima(test_sf, transmilenio$osm_points)
test_final$dist_comercio <- calcular_distancia_minima(test_sf, puntos_comerciales)
test_final$dist_universidad <- calcular_distancia_minima(test_sf, universidades_centroides)

# =============================================================================
# PASO 6: CREAR VARIABLES ESPACIALES AVANZADAS
# =============================================================================

cat("\n=== CREANDO VARIABLES ESPACIALES AVANZADAS ===\n")

crear_variables_espaciales <- function(df) {
  df %>%
    mutate(
      # Scores de accesibilidad
      score_parque = 1 / (dist_parque / 1000 + 0.1),
      score_tm = 1 / (dist_tm / 500 + 0.1),
      score_comercio = 1 / (dist_comercio / 500 + 0.1),
      score_universidad = 1 / (dist_universidad / 1000 + 0.1),
      
      # Score compuesto
      score_accesibilidad = (score_tm * 0.4 + score_comercio * 0.3 + score_parque * 0.2 + score_universidad * 0.1),
      
      # Variables de eficiencia (USANDO VARIABLES ORIGINALES IMPUTADAS)
      m2_por_bedroom = surface_total / ifelse(bedrooms == 0, 1, bedrooms),
      m2_por_bathroom = surface_total / ifelse(bathrooms == 0, 1, bathrooms),
      
      # Interacciones
      casa_score_ubicacion = as.integer(property_type == "Casa") * score_accesibilidad,
      apto_score_ubicacion = as.integer(property_type == "Apartamento") * score_accesibilidad,
      
      # Densidad de servicios
      densidad_servicios = score_tm + score_comercio
    )
}

# Aplicar a ambos datasets
train_final <- crear_variables_espaciales(train_final)
test_final <- crear_variables_espaciales(test_final)

cat("Variables espaciales creadas correctamente\n")

# =============================================================================
# PASO 7: ESTRATEGIA SIN BALANCEO PERO CON PESOS DE CLASE
# =============================================================================

cat("\n ESTRATEGIA: MANTENER DISTRIBUCIÓN ORIGINAL CON PESOS\n")

# Calcular pesos para clases (para que el modelo preste atención a las minorías)
pesos_clase <- train_final %>%
  count(property_type) %>%
  mutate(peso = n / sum(n),
         peso_inverso = 1 / peso)

cat("PESOS POR CLASE:\n")
print(pesos_clase)

# Crear columna de pesos (útil para modelos que soportan sample_weights)
train_final <- train_final %>%
  mutate(
    peso_modelo = case_when(
      property_type == "Casa" ~ 4.08,  # 1/0.245 ≈ 4.08
      property_type == "Apartamento" ~ 1.32,  # 1/0.755 ≈ 1.32
      TRUE ~ 1
    )
  )

cat("\n ESTRATEGIA DEFINIDA:\n")
cat("- Mantenemos distribución original de train\n")
cat("- Usaremos pesos de clase en el modelado\n")
cat("- El modelo aprenderá a predecir ambas clases bien\n")
cat("- Las métricas reflejarán el desempeño real\n")

cat("\nDIMENSIONES FINALES (SIN BALANCEO):\n")
cat("Train:", dim(train_final), "\n")
cat("Test:", dim(test_final), "\n")

# =============================================================================
# PASO 8 : RECONSTRUIR OBJETOS PERDIDOS (CORRECCIÓN)
# =============================================================================

cat("\n🔧 RECONSTRUYENDO OBJETOS PERDIDOS\n")

# Recrear train_sin_peso (eliminar peso_modelo si existe)
train_sin_peso <- train_final %>% select(-any_of("peso_modelo"))
test_sin_peso <- test_final %>% select(-any_of("peso_modelo"))

# Redefinir variables_prioritarias
variables_prioritarias <- c(
  "surface_total", "bedrooms", "bathrooms", "lat", "lon",
  "score_accesibilidad", "dist_tm", "dist_comercio",
  "score_calidad_texto", "m2_por_bedroom", "property_type",
  "dummy_parqueadero", "dummy_ascensor",
  "rooms", "surface_covered", "dist_parque", "dist_universidad",
  "title_length", "dummy_lujoso", "dummy_remodelado"
)

# Cargar función step_interact
step_interact <- recipes::step_interact

# =============================================================================
# PASO 9: PREPROCESAMIENTO MANUAL 
# =============================================================================

cat("\n🔧 PREPROCESAMIENTO MANUAL - SIN RECIPE\n")

# 1. SELECCIÓN DE VARIABLES
train_manual <- train_sin_peso %>% 
  select(all_of(c("price", variables_prioritarias)))

test_manual <- test_sin_peso %>% 
  select(all_of(variables_prioritarias))

# 2. IMPUTACIÓN MANUAL 
imputar_medianas <- function(df) {
  df %>% mutate(across(where(is.numeric), ~ifelse(is.na(.), median(., na.rm = TRUE), .)))
}

train_manual <- imputar_medianas(train_manual)
test_manual <- imputar_medianas(test_manual)

# 3. VARIABLES CATEGÓRICAS A DUMMIES 
train_manual <- train_manual %>%
  mutate(property_type_Casa = as.integer(property_type == "Casa")) %>%
  select(-property_type)

test_manual <- test_manual %>%
  mutate(property_type_Casa = as.integer(property_type == "Casa")) %>%
  select(-property_type)

# 4. INTERACCIONES MANUALES
train_manual <- train_manual %>%
  mutate(
    surface_bedroom_interaction = surface_total * bedrooms,
    lat_lon_interaction = lat * lon
  )

test_manual <- test_manual %>%
  mutate(
    surface_bedroom_interaction = surface_total * bedrooms,
    lat_lon_interaction = lat * lon
  )

# 5. NORMALIZACIÓN MANUAL (excluyendo lat y lon)
normalizar_variables <- function(df, means = NULL, sds = NULL) {
  numeric_vars <- names(df)[sapply(df, is.numeric)]
  # Excluir coordenadas y price
  numeric_vars <- setdiff(numeric_vars, c("lat", "lon", "price"))
  
  if(is.null(means)) {
    # Calcular estadísticas de train
    means <- sapply(df[numeric_vars], mean, na.rm = TRUE)
    sds <- sapply(df[numeric_vars], sd, na.rm = TRUE)
  }
  
  # Aplicar normalización
  for(var in numeric_vars) {
    df[[var]] <- (df[[var]] - means[var]) / sds[var]
  }
  
  return(list(data = df, means = means, sds = sds))
}

# Normalizar train
norm_result <- normalizar_variables(train_manual)
train_processed <- norm_result$data
train_means <- norm_result$means
train_sds <- norm_result$sds

# Normalizar test con estadísticas de train
test_norm <- normalizar_variables(test_manual, train_means, train_sds)
test_processed <- test_norm$data

# VERIFICACIÓN FINAL
cat("REPROCESAMIENTO MANUAL COMPLETADO\n")
cat("Train procesado:", dim(train_processed), "\n")
cat("Test procesado:", dim(test_processed), "\n")
cat("NAs en train:", sum(is.na(train_processed)), "\n")
cat("NAs en test:", sum(is.na(test_processed)), "\n")

# Mostrar primeras variables
cat("\nVARIABLES EN TRAIN:\n")
print(names(train_processed))


# =============================================================================
# PASO 10: CV ESPACIAL
# =============================================================================

cat("\n⚡ CV SESPACIALES\n")

# Cargar rsample para vfold_cv
library(rsample)

# Función simplificada que SI funciona
cv_simple_y_efectivo <- function(data, v = 4) {
  cat(" CONFIGURANDO CV ESTRATIFICADO...\n")
  cat(" Observaciones:", nrow(data), "| Folds:", v, "\n")
  
  # Convertir a dataframe regular manteniendo lat/lon
  df_cv <- as.data.frame(data) %>% 
    select(-any_of("geometry"))
  
  # CV estratificado por latitud (aproximación espacial)
  cv_result <- vfold_cv(df_cv, v = v, strata = "lat")
  
  cat("✅ CV CONFIGURADO EXITOSAMENTE\n")
  cat("📈 Folds creados:", length(cv_result$splits), "\n")
  
  return(cv_result)
}

# EJECUTAR CV 
spatial_folds <- cv_simple_y_efectivo(train_processed, v = 4)

# VERIFICACIÓN RÁPIDA
cat("\n VERIFICACIÓN DE FOLDS:\n")
for(i in 1:length(spatial_folds$splits)) {
  train_data <- training(spatial_folds$splits[[i]])
  val_data <- testing(spatial_folds$splits[[i]])
  cat(sprintf("Fold %d: ✅ Train=%d | Val=%d\n", i, nrow(train_data), nrow(val_data)))
}

# PREPARAR DATOS PARA SUPER LEARNER
X_train <- train_processed %>% select(-price)
y_train <- train_processed$price
X_test <- test_processed

# Limpiar paralelización si existe
try(plan(sequential), silent = TRUE)

# =============================================================================
# PASO 11: SUPER LEARNER CON TUNING EFICIENTE
# =============================================================================

cat("=== SUPER LEARNER CON TUNING COMPLETO ===\n")

# Cargar librerías
library(sl3)
library(origami)
library(xgboost)
library(ranger)
library(glmnet)
library(nnet)
library(rpart)

# =============================================================================
# PASO 12: CONFIGURACIÓN DE DATOS Y FOLDS
# =============================================================================

cat("PREPARANDO DATOS PARA TUNING\n")

# Definir variables predictoras
feature_names <- names(X_train)
n_features <- length(feature_names)
cat("Variables predictoras:", n_features, "\n")

# Crear dataframe para sl3
data_sl <- bind_cols(price = y_train, X_train)

# Convertir folds a formato origami
folds_origami <- lapply(1:length(spatial_folds$splits), function(i) {
  training_idx <- spatial_folds$splits[[i]]$in_id
  validation_idx <- spatial_folds$splits[[i]]$out_id
  list(training = training_idx, validation = validation_idx)
})

# Crear task
task_sl <- make_sl3_Task(
  data = data_sl,
  covariates = feature_names,
  outcome = "price",
  folds = folds_origami
)

cat("Datos y folds configurados\n")

# =============================================================================
# PASO 13: TUNING EFICIENTE DE MODELOS
#=============================================================================

cat("=== INICIANDO TUNING EFICIENTE ===\n")

# 1. ELASTIC NET
cat("[1/5] TUNING ELASTIC NET\n")
best_alpha <- NULL
best_mse_glmnet <- Inf

for(alpha in c(0, 0.25, 0.5, 0.75, 1)) {
  cat("   - Probando alpha =", alpha, "\n")
  tryCatch({
    model <- Lrnr_glmnet$new(alpha = alpha, nlambda = 50, nfolds = 3)
    fit <- model$train(task_sl)
    preds <- fit$predict()
    mse <- mean((y_train - preds)^2)
    
    cat("     MSE:", round(mse, 2), "\n")
    if(mse < best_mse_glmnet) {
      best_mse_glmnet <- mse
      best_alpha <- alpha
      cat("     NUEVO MEJOR\n")
    }
  }, error = function(e) {
    cat("     Error\n")
  })
}

cat("MEJOR alpha:", best_alpha, "con MSE:", round(best_mse_glmnet, 2), "\n")

# 2. XGBOOST
cat("\n[2/5] TUNING XGBOOST\n")
best_xgb_params <- NULL
best_mse_xgb <- Inf

for(nrounds in c(50, 100, 150)) {
  for(max_depth in c(4, 6, 8)) {
    for(eta in c(0.01, 0.1, 0.3)) {
      cat("   - Probando: nrounds=", nrounds, "max_depth=", max_depth, "eta=", eta, "\n")
      tryCatch({
        model <- Lrnr_xgboost$new(nrounds = nrounds, max_depth = max_depth, eta = eta, verbose = FALSE)
        fit <- model$train(task_sl)
        preds <- fit$predict()
        mse <- mean((y_train - preds)^2)
        
        if(mse < best_mse_xgb) {
          best_mse_xgb <- mse
          best_xgb_params <- list(nrounds = nrounds, max_depth = max_depth, eta = eta)
          cat("     NUEVO MEJOR - MSE:", round(mse, 2), "\n")
        }
      }, error = function(e) {
        cat("     Error\n")
      })
    }
  }
}

cat("MEJORES PARÁMETROS XGBoost:\n")
print(best_xgb_params)

# 3. RANDOM FOREST
cat("\n[3/5] TUNING RANDOM FOREST\n")
best_rf_params <- NULL
best_mse_rf <- Inf

for(num.trees in c(100, 200, 300)) {
  for(mtry_val in c(floor(sqrt(n_features)), floor(n_features/3))) {
    for(min.node.size in c(5, 10, 20)) {
      cat("   - Probando: trees=", num.trees, "mtry=", mtry_val, "min.node=", min.node.size, "\n")
      tryCatch({
        model <- Lrnr_ranger$new(num.trees = num.trees, mtry = mtry_val, min.node.size = min.node.size)
        fit <- model$train(task_sl)
        preds <- fit$predict()
        mse <- mean((y_train - preds)^2)
        
        if(mse < best_mse_rf) {
          best_mse_rf <- mse
          best_rf_params <- list(num.trees = num.trees, mtry = mtry_val, min.node.size = min.node.size)
          cat("     NUEVO MEJOR - MSE:", round(mse, 2), "\n")
        }
      }, error = function(e) {
        cat("     Error\n")
      })
    }
  }
}

cat("MEJORES PARÁMETROS Random Forest:\n")
print(best_rf_params)

# 4. DECISION TREE
cat("\n [4/5] TUNING DECISION TREE\n")
best_dt_params <- NULL
best_mse_dt <- Inf

for(cp_val in c(0.001, 0.01, 0.05, 0.1)) {
  for(maxdepth_val in c(5, 10, 15, 20)) {
    cat("   - Probando: cp=", cp_val, "maxdepth=", maxdepth_val, "\n")
    tryCatch({
      model <- Lrnr_rpart$new(cp = cp_val, maxdepth = maxdepth_val)
      fit <- model$train(task_sl)
      preds <- fit$predict()
      mse <- mean((y_train - preds)^2)
      
      if(mse < best_mse_dt) {
        best_mse_dt <- mse
        best_dt_params <- list(cp = cp_val, maxdepth = maxdepth_val)
        cat("     NUEVO MEJOR - MSE:", round(mse, 2), "\n")
      }
    }, error = function(e) {
      cat("     Error\n")
    })
  }
}

cat("MEJORES PARÁMETROS Decision Tree:\n")
print(best_dt_params)

# 5. RED NEURONAL
cat("\n[5/5] TUNING RED NEURONAL\n")
best_nn_params <- NULL
best_mse_nn <- Inf

for(size_val in c(10, 20, 30)) {
  for(decay_val in c(0, 0.001, 0.01)) {
    cat("   - Probando: size=", size_val, "decay=", decay_val, "\n")
    tryCatch({
      model <- Lrnr_nnet$new(size = size_val, decay = decay_val, maxit = 200, linout = TRUE, trace = FALSE)
      fit <- model$train(task_sl)
      preds <- fit$predict()
      mse <- mean((y_train - preds)^2)
      
      if(mse < best_mse_nn) {
        best_mse_nn <- mse
        best_nn_params <- list(size = size_val, decay = decay_val)
        cat("     NUEVO MEJOR - MSE:", round(mse, 2), "\n")
      }
    }, error = function(e) {
      cat("     Error\n")
    })
  }
}

cat("MEJORES PARÁMETROS Red Neuronal:\n")
print(best_nn_params)

cat(" TUNING COMPLETADO - TODOS LOS MODELOS OPTIMIZADOS\n")

#=============================================================================
# PASO 14: SUPER LEARNER CON MODELOS TUNEADOS
#=============================================================================

cat("=== CONFIGURANDO SUPER LEARNER CON MODELOS TUNEADOS ===\n")

# Crear learners con los mejores parámetros
learners_tuned <- Stack$new(
  # Elastic Net tuneado
  Lrnr_glmnet$new(alpha = best_alpha, nlambda = 50, nfolds = 3),
  
  # XGBoost tuneado
  Lrnr_xgboost$new(
    nrounds = best_xgb_params$nrounds, 
    max_depth = best_xgb_params$max_depth, 
    eta = best_xgb_params$eta,
    verbose = FALSE
  ),
  
  # Random Forest tuneado
  Lrnr_ranger$new(
    num.trees = best_rf_params$num.trees,
    mtry = best_rf_params$mtry,
    min.node.size = best_rf_params$min.node.size
  ),
  
  # Decision Tree tuneado
  Lrnr_rpart$new(
    cp = best_dt_params$cp,
    maxdepth = best_dt_params$maxdepth
  ),
  
  # Red Neuronal tuneada
  Lrnr_nnet$new(
    size = best_nn_params$size, 
    decay = best_nn_params$decay, 
    maxit = 200, 
    linout = TRUE, 
    trace = FALSE
  ),
  
  # Modelos simples de referencia
  Lrnr_glm$new(),
  Lrnr_mean$new()
)

# Metalearner con NNLS
metalearner_nnls <- Lrnr_nnls$new()

# Super Learner
super_learner <- Lrnr_sl$new(
  learners = learners_tuned,
  metalearner = metalearner_nnls
)

# =============================================================================
# PASO 15: CREAR FOLDS BALANCEADOS PARA SUPER LEARNER
# =============================================================================

cat("=== CREANDO FOLDS BALANCEADOS ===\n")

# Crear folds balanceados manualmente
set.seed(123)
n_obs <- nrow(data_sl)
n_folds <- 4

# Crear folds estratificados por precio (aproximadamente)
price_quantiles <- cut(data_sl$price, breaks = 4, labels = FALSE)
folds_balanced <- make_folds(n = n_obs, V = n_folds, strata_ids = price_quantiles)

# Verificar los folds balanceados
cat(" VERIFICANDO FOLDS BALANCEADOS:\n")
for(i in 1:length(folds_balanced)) {
  fold <- folds_balanced[[i]]
  train_size <- length(fold$training)
  val_size <- length(fold$validation)
  cat(sprintf("Fold %d: Train=%d | Val=%d (%.1f%%)\n", 
              i, train_size, val_size, val_size/n_obs*100))
}

# Crear task con folds balanceados
task_sl_balanced <- make_sl3_Task(
  data = data_sl,
  covariates = feature_names,
  outcome = "price", 
  folds = folds_balanced
)

cat("Task con folds balanceados creado\n")

#=============================================================================
# PASO 16: ENTRENAR SUPER LEARNER CON FOLDS BALANCEADOS
#=============================================================================

cat("ENTRENANDO SUPER LEARNER CON FOLDS BALANCEADOS...\n")
start_time <- Sys.time()

sl_fit <- super_learner$train(task_sl_balanced)

end_time <- Sys.time()
training_time <- round(as.numeric(end_time - start_time, units = "mins"), 2)

cat("Super Learner entrenado correctamente en", training_time, "minutos\n")

#=============================================================================
# PASO 17: RESULTADOS FINALES
#=============================================================================

cat("=== RESULTADOS DEL SUPER LEARNER ===\n")

# Obtener predicciones
preds_sl <- sl_fit$predict()

# Métricas
mse_sl <- mean((data_sl$price - preds_sl)^2)
rmse_sl <- sqrt(mse_sl)
mae_sl <- mean(abs(data_sl$price - preds_sl))

cat(" MÉTRICAS FINALES:\n")
cat("   - RMSE:", round(rmse_sl, 2), "\n")
cat("   - MAE:", round(mae_sl, 2), "\n")
cat("   - Error relativo:", round(mae_sl/mean(data_sl$price)*100, 1), "%\n")

# Pesos óptimos
cat("\nPESOS ÓPTIMOS DEL SUPER LEARNER:\n")
if(!is.null(sl_fit$coefficients)) {
  weights <- sl_fit$coefficients
  model_names <- c("Elastic Net", "XGBoost", "Random Forest", "Decision Tree", 
                   "Red Neuronal", "Regresión Lineal", "Media")
  
  weight_df <- data.frame(
    Modelo = model_names,
    Peso = round(weights, 4),
    Porcentaje = paste0(round(weights * 100, 1), "%")
  ) %>% arrange(desc(Peso))
  
  print(weight_df)
  cat("\n Modelo más importante:", weight_df$Modelo[1], "\n")
  cat(" Número de modelos con peso > 0:", sum(weights > 0), "\n")
}

cat("\n SUPER LEARNER COMPLETADO EXITOSAMENTE CON VALIDACIÓN CRUZADA\n")

# =============================================================================
# PASO 18: PREDICCIONES FINALES CON REDONDEO
# =============================================================================

cat(" HACIENDO PREDICCIONES FINALES EN TEST SET...\n")

# Crear task para test set
task_test <- make_sl3_Task(
  data = test_processed,
  covariates = feature_names
)

# Hacer predicciones
cat(" Generando predicciones...\n")
preds_test_continuas <- sl_fit$predict(task_test)

#=============================================================================
# PASO 19: REDONDEO SUAVE A 500,000 COP
#=============================================================================

cat(" APLICANDO REDONDEO SUAVE A 500,000 COP...\n")

# Redondeo a 500,000 (balance perfecto)
preds_test_redondeadas <- round(preds_test_continuas / 500000) * 500000

# Verificar el impacto del redondeo
diferencia_promedio <- mean(abs(preds_test_continuas - preds_test_redondeadas))
diferencia_relativa <- (diferencia_promedio / mean(preds_test_continuas)) * 100

cat(" IMPACTO DEL REDONDEO:\n")
cat("   - Diferencia promedio:", format(round(diferencia_promedio), big.mark = ","), "COP\n")
cat("   - Error relativo:", round(diferencia_relativa, 3), "%\n")
cat("   - Múltiplo de redondeo: 500,000 COP\n")

# Mostrar ejemplos antes/después
cat("\n EJEMPLOS DE REDONDEO:\n")
ejemplos <- data.frame(
  Original = format(head(preds_test_continuas), big.mark = ",", scientific = FALSE),
  Redondeado = format(head(preds_test_redondeadas), big.mark = ",", scientific = FALSE),
  Diferencia = format(head(round(preds_test_redondeadas - preds_test_continuas)), big.mark = ",", scientific = FALSE)
)
print(ejemplos)

#=============================================================================
# PASO 20: CREAR ARCHIVO DE SUBMISIÓN
#=============================================================================

cat("\n CREANDO ARCHIVO DE SUBMISIÓN...\n")

# Crear dataframe final
submission_df <- data.frame(
  property_id = test_final$property_id,
  price = preds_test_redondeadas
)

# Verificar estructura
cat(" VERIFICANDO ESTRUCTURA:\n")
cat("   - Total predicciones:", nrow(submission_df), "\n")
cat("   - Variables:", paste(names(submission_df), collapse = ", "), "\n")
cat("   - ¿Precios enteros?:", all(submission_df$price == round(submission_df$price)), "\n")

# Mostrar primeras filas
cat("\n MUESTRA DEL ARCHIVO FINAL:\n")
print(head(submission_df))

# =============================================================================
# PASO 21: GUARDAR PREDICCIONES
# =============================================================================

cat("\n GUARDANDO PREDICCIONES...\n")

# Guardar como CSV
write_csv(submission_df, "predicciones_super_learner.csv")

# Verificar archivo guardado
cat("ARCHIVO GUARDADO: predicciones_super_learner.csv\n")
cat(" VERIFICANDO ARCHIVO GUARDADO...\n")

archivo_guardado <- read_csv("predicciones_super_learner.csv", n_max = 5)
print(archivo_guardado)

# =============================================================================
# PASO 22:RESUMEN FINAL
# =============================================================================

cat("\n PROCESO COMPLETADO EXITOSAMENTE!\n")
cat("============================================\n")
cat(" RESUMEN FINAL:\n")
cat("   - Modelo: Super Learner (Random Forest 66% + XGBoost 34%)\n")
cat("   - Predicciones: ", nrow(submission_df), "propiedades\n")
cat("   - Redondeo: Múltiplos de 500,000 COP\n")
cat("   - Error de redondeo: ", round(diferencia_relativa, 3), "%\n")
cat("   - Archivo: predicciones_super_learner.csv\n")
cat("   - Formato: property_id,price\n")
cat("============================================\n")

# Mostrar algunos precios finales
cat("\n EJEMPLOS DE PRECIOS FINALES:\n")
precios_finales <- unique(head(preds_test_redondeadas, 6))
for(i in 1:length(precios_finales)) {
  cat("   ", i, ".", format(precios_finales[i], big.mark = ",", scientific = FALSE), "COP\n")
}

# =============================================================================
#  MAPAS PARA ANÁLISIS EXPLORATORIO Y TABLAS 
# =============================================================================

# Filtrar coordenadas válidas
train_con_coords <- train %>% filter(!is.na(lat) & !is.na(lon))
test_con_coords <- test %>% filter(!is.na(lat) & !is.na(lon))

# Convertir a datos espaciales
train_sf <- st_as_sf(train_con_coords, coords = c("lon", "lat"), crs = 4326)
test_sf <- st_as_sf(test_con_coords, coords = c("lon", "lat"), crs = 4326)

# Crear mapa interactivo
mapa_interactivo <- leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(
    data = train_sf,
    radius = 4,
    color = "blue",
    fillOpacity = 0.6,
    popup = ~paste("<b>Train</b><br>Precio: $", format(price, big.mark = ",")),
    group = "Train"
  ) %>%
  addCircleMarkers(
    data = test_sf,
    radius = 4, 
    color = "red",
    fillOpacity = 0.6,
    popup = ~paste("<b>Test</b><br>Área:", surface_total, "m²"),
    group = "Test"
  ) %>%
  addLayersControl(
    overlayGroups = c("Train", "Test")
  ) %>%
  addLegend(
    position = "topright",
    colors = c("blue", "red"),
    labels = c("Train", "Test")
  )

# Guardar como HTML
saveWidget(mapa_interactivo, 
           "mapa_interactivo.html",
           title = "Mapa Interactivo - Propiedades Chapinero",
           selfcontained = TRUE)

cat("✅ Mapa guardado como 'mapa_interactivo.html'")

# 6. Verificar que se guardó
if(file.exists("mapa_interactivo.html")) {
  print("✅ Mapa guardado correctamente como 'mapa_interactivo.html'")
  print(paste("📁 Ubicación:", getwd()))
} else {
  print("❌ Error: El archivo no se guardó")
}



library(leaflet)
library(sf)
library(dplyr)

# Función segura para nombres
safe_name <- function(sf_obj) {
  if("name" %in% names(sf_obj)) {
    return(ifelse(is.na(sf_obj$name), "Sin nombre", sf_obj$name))
  } else {
    return(rep("Sin nombre", nrow(sf_obj)))
  }
}

# 1. FILTRAR PROPIEDADES EN CHAPINERO
cat("=== MAPA INTERACTIVO CHAPINERO ===\n")

propiedades_chapinero <- train_final %>%
  filter(
    lat >= chapinero_bbox["y", "min"] & lat <= chapinero_bbox["y", "max"] &
      lon >= chapinero_bbox["x", "min"] & lon <= chapinero_bbox["x", "max"]
  )

cat("Propiedades en Chapinero:", nrow(propiedades_chapinero), "\n")

# 2. CREAR MAPA BASE (MISMA ESTRUCTURA)
mapa_chapinero <- leaflet() %>%
  addTiles() %>%
  
  # PROPIEADES EN CHAPINERO - AZUL como en tu imagen
  addCircleMarkers(
    data = propiedades_chapinero,
    lng = ~lon, lat = ~lat,
    radius = 4,
    color = "blue",
    fillColor = "blue", 
    fillOpacity = 0.7,
    popup = ~paste("<b>CHAPINERO</b><br>",
                   "Tipo:", property_type, "<br>",
                   "Precio: $", format(price, big.mark = ","), "<br>",
                   "Bedrooms:", bedrooms, "<br>",
                   "Bathrooms:", bathrooms),
    group = "Propiedades"
  )

# 3. AGREGAR PUNTOS DE INTERÉS (MISMA LÓGICA)

# PARQUES
if(!is.null(parques$osm_polygons) && nrow(parques$osm_polygons) > 0) {
  parques_centroides <- st_centroid(parques$osm_polygons)
  parques_nombres <- safe_name(parques_centroides)
  
  mapa_chapinero <- mapa_chapinero %>%
    addMarkers(
      data = parques_centroides,
      icon = makeIcon(
        iconUrl = "https://cdn-icons-png.flaticon.com/512/684/684908.png",
        iconWidth = 20, iconHeight = 20
      ),
      popup = ~parques_nombres,
      label = ~paste("Parque:", parques_nombres),
      group = "Parques"
    )
}

# TRANSMILENIO
if(!is.null(transmilenio$osm_points) && nrow(transmilenio$osm_points) > 0) {
  tm_nombres <- safe_name(transmilenio$osm_points)
  
  mapa_chapinero <- mapa_chapinero %>%
    addMarkers(
      data = transmilenio$osm_points,
      icon = makeIcon(
        iconUrl = "https://cdn-icons-png.flaticon.com/512/984/984500.png", 
        iconWidth = 15, iconHeight = 15
      ),
      popup = ~tm_nombres,
      label = ~paste("TM:", tm_nombres),
      group = "TransMilenio"
    )
}

# UNIVERSIDADES
if(!is.null(universidades$osm_polygons) && nrow(universidades$osm_polygons) > 0) {
  uni_centroides <- st_centroid(universidades$osm_polygons)
  uni_nombres <- safe_name(uni_centroides)
  
  mapa_chapinero <- mapa_chapinero %>%
    addMarkers(
      data = uni_centroides,
      icon = makeIcon(
        iconUrl = "https://cdn-icons-png.flaticon.com/512/1001/1001371.png",
        iconWidth = 18, iconHeight = 18
      ),
      popup = ~uni_nombres,
      label = ~paste("Universidad:", uni_nombres),
      group = "Universidades"
    )
}

# COMERCIOS
if(!is.null(puntos_comerciales) && nrow(puntos_comerciales) > 0) {
  if(inherits(puntos_comerciales, "sf")) {
    if(all(st_geometry_type(puntos_comerciales) %in% c("POINT"))) {
      comercios_points <- puntos_comerciales
    } else {
      comercios_points <- st_centroid(puntos_comerciales)
    }
    
    comercios_nombres <- safe_name(comercios_points)
    
    mapa_chapinero <- mapa_chapinero %>%
      addMarkers(
        data = comercios_points,
        icon = makeIcon(
          iconUrl = "https://cdn-icons-png.flaticon.com/512/869/869869.png",
          iconWidth = 16, iconHeight = 16
        ),
        popup = ~comercios_nombres,
        label = ~paste("Comercio:", comercios_nombres),
        group = "Comercios"
      )
  }
}

# 4. CONTROLES FINALES (MISMA ESTRUCTURA)
mapa_chapinero <- mapa_chapinero %>%
  addLayersControl(
    overlayGroups = c("Propiedades", "Parques", "TransMilenio", "Universidades", "Comercios"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  setView(
    lng = mean(chapinero_bbox["x", ]),
    lat = mean(chapinero_bbox["y", ]), 
    zoom = 14
  )

print(mapa_chapinero)
cat("✅ Mapa interactivo de Chapinero creado\n")

# GUARDAR MAPA COMO HTML
htmlwidgets::saveWidget(mapa_chapinero, 
                        "mapa_chapinero_interactivo.html",
                        title = "Mapa Interactivo - Propiedades en Chapinero",
                        selfcontained = TRUE)

cat("✅ Mapa guardado como: mapa_chapinero_interactivo.html\n")
cat("📁 Ubicación:", getwd(), "\n")

# GUARDAR MAPA COMO HTML
htmlwidgets::saveWidget(mapa_chapinero, 
                        "mapa_chapinero_interactivo.html",
                        title = "Mapa Interactivo - Propiedades en Chapinero",
                        selfcontained = TRUE)

cat("✅ Mapa guardado como: mapa_chapinero_interactivo.html\n")
cat("📁 Ubicación:", getwd(), "\n")


vars_struct <- c("surface_total", "bedrooms", "bathrooms", "surface_covered")

tabla_struct <- bind_rows(
  train_final %>%
    select(all_of(vars_struct)) %>%
    summarise(across(everything(),
                     list(Media = ~mean(.), Mediana = ~median(.), SD = ~sd(.))),
              .groups = "drop") %>%
    mutate(muestra = "Train"),
  
  test_final %>%
    select(all_of(vars_struct)) %>%
    summarise(across(everything(),
                     list(Media = ~mean(.), Mediana = ~median(.), SD = ~sd(.))),
              .groups = "drop") %>%
    mutate(muestra = "Test")
) %>%
  relocate(muestra)



vars_text_dummies <- c("dummy_lujoso", "dummy_remodelado",
                       "dummy_parqueadero", "dummy_ascensor",
                       "dummy_balcon")

tabla_text <- train_final %>%
  summarise(across(all_of(vars_text_dummies), ~mean(.))) %>%
  pivot_longer(cols = everything(),
               names_to = "Variable",
               values_to = "Proporcion") %>%
  mutate(Proporcion = round(Proporcion, 3))


vars_dist <- c("dist_parque", "dist_tm", "dist_comercio", "dist_universidad")
vars_score <- c("score_parque", "score_tm", "score_comercio", "score_universidad", "score_accesibilidad")

tabla_spatial <- train_final %>%
  select(all_of(c(vars_dist, vars_score))) %>%
  summarise(across(everything(),
                   list(Mediana = ~median(.),
                        IQR = ~IQR(.)))) %>%
  pivot_longer(cols = everything(),
               names_to = c("Variable", ".value"),
               names_sep = "_")

tabla_struct
tabla_text
tabla_spatial

names(train_final)


vars_dist <- c("dist_parque", "dist_tm", "dist_comercio", "dist_universidad")

tabla_spatial_test <- test_final %>%
  select(all_of(vars_dist)) %>%
  summarise(across(everything(),
                   list(Mediana = ~round(median(.),1),
                        IQR = ~round(IQR(.),1)))) %>%
  pivot_longer(cols = everything(),
               names_to = c("Variable", ".value"),
               names_sep = "_")

tabla_spatial_test



# =============================================================================
# GRÁFICO DE IMPORTANCIA DE VARIABLES (Usando XGBoost)
# =============================================================================

library(ggplot2)
library(dplyr)
library(xgboost)

cat("=== GENERANDO GRÁFICO DE IMPORTANCIA (FUENTE: XGBOOST) ===\n")

# 1. BUSCAR EL MODELO XGBOOST DENTRO DEL ENSAMBLE
# Obtenemos los nombres de todos los modelos entrenados
nombres_modelos <- names(sl_fit$learner_fits)

# Filtramos el que diga "xgboost" (usamos el primero que encuentre)
nombre_xgb <- nombres_modelos[grep("xgboost", nombres_modelos, ignore.case = TRUE)][1]

if (is.na(nombre_xgb)) {
  stop("❌ No se encontró ningún modelo XGBoost en el ensamble.")
}

cat(paste("   > Extrayendo importancia del modelo:", nombre_xgb, "\n"))

# 2. EXTRAER EL OBJETO NATIVO
modelo_xgb <- sl_fit$learner_fits[[nombre_xgb]]$fit_object

# 3. CALCULAR IMPORTANCIA (GAIN)
matriz_imp <- xgb.importance(model = modelo_xgb)


# 4. PREPARAR DATOS (FILTRANDO LAT/LON)
top_vars <- matriz_imp %>%
  filter(!Feature %in% c("lat", "lon")) %>% # <--- AQUÍ QUITAMOS LAS COORDENADAS
  arrange(desc(Gain)) %>%
  head(15) %>% 
  mutate(Feature = reorder(Feature, Gain)) 

print(top_vars)

# 5. GENERAR GRÁFICO (SIN TÍTULO EN EJE Y)
p_imp <- ggplot(top_vars, aes(x = Gain, y = Feature)) +
  geom_col(fill = "#2A9D8F", width = 0.7) +
  labs(
    title = "Variables más Influyentes en el Precio (Chapinero)",
    subtitle = "(XGBoost)",
    x = "Importancia Relativa",
    y = NULL  # Sin etiqueta en el eje Y
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.y = element_text(size = 10, face = "bold"),
    panel.grid.major.y = element_blank()
  )

print(p_imp)

# 6. GUARDAR
ggsave("top15_variables_importantes.png", p_imp, width = 10, height = 7, bg = "white")
cat("✅ Gráfico guardado (sin lat/lon).\n")

