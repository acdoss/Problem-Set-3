# =============================================================================
# CÓDIGO MAESTRO: PREDICCIÓN INMOBILIARIA BOGOTÁ (SUPER LEARNER + EDA)
# =============================================================================

# 1. LIMPIEZA Y LIBRERÍAS ----------------------------------------------------
rm(list = ls())
gc()

required_packages <- c("tidyverse", "tidymodels", "recipes", "readr", "dplyr", 
                       "sf", "nnls", "xgboost", "ranger", "glmnet", "stringr", 
                       "sl3", "origami", "tictoc", "osmdata", "reticulate", 
                       "keras3", "leaflet", "htmlwidgets", "scales", "spatialsample")

new_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]
if(length(new_packages)) install.packages(new_packages, dependencies = TRUE)

if (!require(sl3)) remotes::install_github("tlverse/sl3")

library(tidyverse)
library(tidymodels)
library(recipes)
library(sf)
library(sl3)
library(origami)
library(tictoc)
library(stringr)
library(osmdata)
library(reticulate)
library(keras3)
library(leaflet)
library(scales)
library(spatialsample)

# 2. CONFIGURACIÓN DE ENTORNO (PYTHON LOCAL) ---------------------------------
tryCatch({
  use_python("C:/Users/pinta/AppData/Local/Python/pythoncore-3.10-64/python.exe", required = TRUE)
}, error = function(e) cat("⚠️ Advertencia: Revisa tu ruta de Python.\n"))

# =============================================================================
# 3. CARGA Y FILTRADO ESPACIAL VELOZ (st_is_within_distance)
# =============================================================================
cat("=== CARGANDO Y FILTRANDO DATOS (MÉTODO OPTIMIZADO) ===\n")

# 1. Cargar datos crudos
test_raw <- read_csv("https://www.dropbox.com/scl/fi/seif0x3qoy95gh0s2nw0u/test.csv?rlkey=lqzviqjqxwatlt708bzoezsnc&st=csqy50ti&dl=1")
train_raw <- read_csv("https://www.dropbox.com/scl/fi/pnw43edk4uewsd13g1oxj/train.csv?rlkey=vxuc9vej4ruom9dg0y5b2zh0c&st=1zp7ez1e&dl=1")

# 2. Convertir a Espacial Planar (Metros - EPSG:3116)
convertir_a_metros <- function(df) {
  df %>%
    filter(!is.na(lat), !is.na(lon)) %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
    st_transform(crs = 3116)
}

train_sf_metros <- convertir_a_metros(train_raw)
test_sf_metros  <- convertir_a_metros(test_raw)

cat("   > Calculando vecinos cercanos (5km)...\n")

# 3. EL TRUCO DE VELOCIDAD: st_is_within_distance
# Esto devuelve una lista. Para cada punto de Train, nos dice los índices de Test cercanos.
# sparse = TRUE hace que sea muy eficiente en memoria.
indices_cercanos <- st_is_within_distance(
  train_sf_metros, 
  test_sf_metros, 
  dist = 5000 # 5 km
)

# 4. Filtrar
# Si la longitud del vector es > 0, significa que tiene al menos un vecino en Test cerca.
filtro_booleano <- lengths(indices_cercanos) > 0

train_filtrado_sf <- train_sf_metros[filtro_booleano, ]

# 5. Recuperar formato Dataframe
sf_a_df <- function(sf_obj) {
  coords <- st_coordinates(st_transform(sf_obj, 4326))
  sf_obj %>%
    st_drop_geometry() %>%
    mutate(lon = coords[,1], lat = coords[,2]) %>%
    as_tibble()
}

train <- sf_a_df(train_filtrado_sf)
test  <- test_raw # Test queda igual

# Reporte
cat(sprintf("✅ Filtrado completado.\n   Antes: %d -> Después: %d\n", nrow(train_raw), nrow(train)))
# 4. DATOS ESPACIALES (PARQUES OSM) ------------------------------------------
cat("=== DESCARGANDO PARQUES DE BOGOTÁ ===\n")
sf::sf_use_s2(FALSE) # Desactivar motor esférico para evitar errores de geometría

q_bogota <- opq(bbox = getbb("Bogota Colombia")) %>% add_osm_feature(key = "leisure", value = "park")
parques_sf <- osmdata_sf(q_bogota)$osm_polygons %>% 
  select(osm_id) %>% filter(!is.na(osm_id)) %>% st_make_valid()

centroides_parques <- st_centroid(parques_sf)

# Puntos Ancla (Valorización)
puntos_clave <- tibble(
  lugar = c("Andino_ZonaT", "Club_Nogal", "ZonaG", "Parque93", "Javeriana", "Centro_Fin_72"),
  lat = c(4.6668, 4.6575, 4.6545, 4.6765, 4.6285, 4.6535),
  lon = c(-74.0534, -74.0505, -74.0555, -74.0482, -74.0648, -74.0550)
)
puntos_sf_clave <- st_as_sf(puntos_clave, coords = c("lon", "lat"), crs = 4326)

# 5. FUNCIONES DE INGENIERÍA -------------------------------------------------

# A. Procesamiento General
procesar_completo <- function(df, sf_parques, sf_centroides) {
  cat("   > Ingeniería de variables (Texto + Espacial)...\n")
  df <- df %>%
    mutate(texto_completo = paste(tolower(replace_na(title, "")), tolower(replace_na(description, "")))) %>%
    mutate(
      estrato_txt = str_extract(texto_completo, "estrato\\s*:?\\s*([1-6])"),
      estrato_num = parse_number(estrato_txt),
      tiene_terraza = as.integer(str_detect(texto_completo, "terraza|balc[oó]n|patio")),
      tiene_remodelado = as.integer(str_detect(texto_completo, "remodelad|renovad")),
      tiene_seguridad = as.integer(str_detect(texto_completo, "vigilancia|porter[ií]a|24h|seguridad")),
      tiene_ascensor = as.integer(str_detect(texto_completo, "ascensor|elevador")),
      tiene_parqueadero = as.integer(str_detect(texto_completo, "garaje|parqueadero|parking"))
    ) %>%
    mutate(
      surface_total = pmax(surface_total, surface_covered, na.rm = TRUE),
      m2_por_habitacion = ifelse(bedrooms > 0, surface_covered / bedrooms, 0),
      ratio_espacio_abierto = (surface_total - surface_covered) / surface_total
    ) %>% replace_na(list(ratio_espacio_abierto = 0))
  
  if (all(c("lat", "lon") %in% names(df))) {
    mask <- !is.na(df$lat) & !is.na(df$lon)
    df_sf <- st_as_sf(df[mask, ], coords = c("lon", "lat"), crs = 4326)
    
    # Puntos Clave
    for(i in 1:nrow(puntos_clave)) {
      dist <- st_distance(df_sf, puntos_sf_clave[i, ])
      df[[paste0("dist_", puntos_clave$lugar[i])]] <- NA
      df[[paste0("dist_", puntos_clave$lugar[i])]][mask] <- as.numeric(dist)
    }
    # Parques
    idx <- st_nearest_feature(df_sf, sf_centroides)
    dist_p <- st_distance(df_sf, sf_centroides[idx, ], by_element = TRUE)
    area_p <- st_area(sf_parques[idx, ])
    
    df$dist_parque_min <- NA; df$area_parque_cercano <- NA
    df$dist_parque_min[mask] <- as.numeric(dist_p)
    df$area_parque_cercano[mask] <- as.numeric(area_p)
  }
  return(df %>% select(-texto_completo, -estrato_txt))
}

# B. Imputación Inteligente (Mediana)
imputar_inteligente <- function(df) {
  cat("   > Imputación Estratificada (Mediana por Grupo)...\n")
  df <- df %>%
    mutate(property_type = str_to_title(trimws(as.character(property_type)))) %>%
    mutate(property_type = ifelse(is.na(property_type), "Apartamento", property_type))
  
  df <- df %>% mutate(grupo_imp = paste(property_type, coalesce(as.character(estrato_num), "3")))
  vars_num <- c("surface_total", "surface_covered", "rooms", "bedrooms", "bathrooms")
  vars_presentes <- intersect(vars_num, names(df))
  
  for (col in vars_presentes) {
    medianas <- df %>% group_by(grupo_imp) %>% summarise(val = median(.data[[col]], na.rm=TRUE), .groups="drop")
    global <- median(df[[col]], na.rm=TRUE)
    df <- df %>% left_join(medianas, by="grupo_imp") %>%
      mutate(!!col := ifelse(is.na(.data[[col]]), coalesce(val, global), .data[[col]])) %>% select(-val)
  }
  return(df %>% select(-grupo_imp))
}

# 6. EJECUCIÓN DE PIPELINE ---------------------------------------------------
cat("=== PROCESANDO DATOS ===\n")
if("operation_type" %in% names(train)) train <- train %>% select(-operation_type)

train_eng <- procesar_completo(train, parques_sf, centroides_parques)
test_eng <- procesar_completo(test, parques_sf, centroides_parques)

train_eng <- train_eng %>% mutate(estrato_final = coalesce(estrato_num, 3))
test_eng <- test_eng %>% mutate(estrato_final = coalesce(estrato_num, 4))

train_eng <- imputar_inteligente(train_eng)
test_eng <- imputar_inteligente(test_eng)

# =============================================================================
# 7. ANÁLISIS VISUAL Y EXPLORATORIO (EDA) 📊
# =============================================================================
cat("=== GENERANDO GRÁFICOS (EDA) ===\n")

df_viz <- train_eng %>%
  filter(!is.na(lat), !is.na(lon), !is.na(price), !is.na(surface_covered)) %>%
  mutate(price_m2_millon = (price / surface_covered) / 1e6) %>%
  filter(price_m2_millon < 25, price_m2_millon > 0.5)

# A. Histograma
p1 <- ggplot(df_viz, aes(x = price_m2_millon)) +
  geom_histogram(aes(y = ..density..), fill = "#3a5e8c", color = "white", bins = 30, alpha = 0.8) +
  geom_density(color = "red", linewidth = 1) +
  labs(title = "Distribución de Precios (Millones/m2)", x = "Precio m2", y = "Densidad") + theme_minimal()
print(p1)
ggsave("eda_histograma.png", p1, width = 8, height = 6)

# B. Mapa Interactivo
tryCatch({
  df_mapa <- df_viz %>% sample_n(min(1000, nrow(df_viz))) %>%
    mutate(color = ifelse(property_type == "Apartamento", "#2A9D8F", "#F4A261"))
  
  mapa <- leaflet(df_mapa) %>% addTiles() %>%
    addCircles(lng = ~lon, lat = ~lat, color = ~color, radius = 30, fillOpacity = 0.6,
               popup = ~paste0("<b>Precio:</b> $", format(price, big.mark=","))) %>%
    addLegend("bottomright", colors = c("#2A9D8F", "#F4A261"), labels = c("Apto", "Casa"))
  print(mapa)
}, error = function(e) cat("⚠️ Mapa no generado.\n"))

# =============================================================================
# 8. PREPROCESAMIENTO, FOLDS ESPACIALES Y TASK
# =============================================================================
cat("=== PREPARANDO MODELADO ===\n")

# A. Recipes (One-Hot)
vars_excluir <- c("description", "title", "city", "month", "year", "l3", "l4", "l5", "l6")
train_eng <- train_eng %>% select(-any_of(vars_excluir))

receta_sl <- recipe(price ~ ., data = train_eng) %>%
  update_role(property_id, new_role = "id") %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
  step_impute_median(all_numeric_predictors()) %>% 
  step_zv(all_predictors())

receta_prep <- prep(receta_sl)
train_processed <- bake(receta_prep, new_data = train_eng)

test_temp <- test_eng
if(!"price" %in% names(test_temp)) test_temp$price <- NA
test_processed <- bake(receta_prep, new_data = test_temp)

vars_finales <- setdiff(names(train_processed), c("price", "property_id"))

# --- B. DEFINICIÓN DE FOLDS ESPACIALES VELOZ (K-MEANS DIRECTO) ---
cat("   > Generando Folds Espaciales (Algoritmo Rápido)...\n")

# 1. Obtener Coordenadas Planas (Metros)
# Solo necesitamos la matriz de números X e Y, no el objeto pesado SF
coords_matrix <- train_eng %>%
  select(lon, lat) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(crs = 3116) %>%
  st_coordinates()

# 2. Clustering Matemático Directo (K-Means)
# Esto hace lo mismo que spatial_clustering_cv pero sin la sobrecarga de 'sf'
set.seed(2025)
# nstart=10 lo hace correr 10 veces y elige la mejor (aún así toma < 1 segundo)
km_clusters <- kmeans(coords_matrix, centers = 5, nstart = 10)

# 3. Asignación Directa de Folds
# kmeans ya nos devuelve un vector de 1 a 5 para cada fila. ¡Es directo!
fold_ids <- km_clusters$cluster

# 4. Visualización Rápida (Opcional - Para verificar)
# Solo tomamos una muestra para graficar rápido
df_plot <- data.frame(x = coords_matrix[,1], y = coords_matrix[,2], fold = as.factor(fold_ids))
print(ggplot(df_plot[sample(nrow(df_plot), 2000),], aes(x, y, color = fold)) + 
        geom_point(size=0.5) + ggtitle("Folds Espaciales (K-Means Rápido)") + theme_void())

# 5. Crear objeto folds para sl3
folds_espaciales <- make_folds(fold_ids)

cat("✅ Folds espaciales generados instantáneamente.\n")

# =============================================================================
# 9. SUPER LEARNER (STACK ROBUSTO)
# =============================================================================
cat("=== ENTRENANDO SUPER LEARNER ===\n")

# Lista Plana de Modelos (Bypass de errores Stack)
modelos_lista <- list(
  sl3::Lrnr_mean$new(),
  sl3::Lrnr_glmnet$new(alpha = 0),
  sl3::Lrnr_xgboost$new(nrounds = 500, max_depth = 6, eta = 0.05),
  sl3::Lrnr_xgboost$new(nrounds = 500, max_depth = 8, eta = 0.1),
  sl3::Lrnr_ranger$new(num.trees = 500, min.node.size = 5)
)
modelos_validos <- Filter(function(x) !is.null(x) && inherits(x, "Lrnr_base"), modelos_lista)

metalearner <- sl3::Lrnr_nnls$new()
sl <- sl3::Lrnr_sl$new(learners = modelos_validos, metalearner = metalearner)



# =============================================================================
# DEFINICIÓN DEL TASK (QUE FALTABA)
# =============================================================================

cat("=== CREANDO EL OBJETO TASK ===\n")

# Verificar que los ingredientes existen
if(!exists("train_processed")) stop("Falta 'train_processed'")
if(!exists("folds_espaciales")) stop("Falta 'folds_espaciales'")

# Crear la Tarea (Task) para el Super Learner
task <- sl3_Task$new(
  data = train_processed,       # Tus datos numéricos (One-Hot)
  covariates = vars_finales,    # Las variables X (sin precio ni ID)
  outcome = "price",            # La variable Y
  outcome_type = "continuous",  # Regresión
  id = "property_id",           # Identificador de filas
  folds = folds_espaciales      # ¡Los folds rápidos de K-Means!
)

cat("✅ Objeto 'task' creado exitosamente. Ahora puedes entrenar.\n")





tic("Entrenamiento SL")
sl_fit <- sl$train(task)
toc()

# 1. Renombrar correctamente usando el nombre real ("risk")
tabla_final <- tabla_resultados %>%
  rename(MAE_Promedio = risk, Desviacion = se) %>% 
  arrange(MAE_Promedio) %>%                        
  select(learner, MAE_Promedio, Desviacion)        

# 2. IMPRIMIR EL RANKING
cat("\n=== 🏆 RANKING DE MODELOS (MENOR ERROR ARRIBA) ===\n")
print(tabla_final)

# 3. RESULTADO DEL GANADOR
mejor_mae <- tabla_final$MAE_Promedio[1]
mejor_modelo <- tabla_final$learner[1]

cat(paste0("\n✅ El modelo ganador es: ", mejor_modelo))
cat(paste0("\n💰 Error Promedio (MAE): $", format(mejor_mae, big.mark=","), "\n"))




task_test <- sl3_Task$new(data = test_processed, covariates = vars_finales, id = "property_id")
preds <- sl_fit$predict(task_test)
if(any(is.na(preds))) preds[is.na(preds)] <- mean(preds, na.rm=TRUE)

write_csv(data.frame(property_id = test_processed$property_id, price = preds), "predicciones_finales_v5.csv")
cat("✅ PROCESO COMPLETADO.\n")
