# Problem-Set-3333

Este proyecto desarrolla un modelo predictivo para estimar precios de propiedades en la localidad de Chapinero en Bogotá, Colombia. Utilizando datos de Properati y variables espaciales de OpenStreetMap, implementamos múltiples algoritmos de machine learning para optimizar las compras de propiedades minimizando el riesgo de sobrestimación.
## Estructura del Repositorio

Una guía rápida para navegar el repositorio:

*   `documents/`: Contiene el documento final del proyecto en formato PDF y Latex.
*   `scripts/`: Alberga el scritp de R donde se realizó el ejercicio.
*   `stores/`: Las bases utilizadas en el script son llamadas directamente desde dropbox, por lo que esta carpeta no existe.
*   `views/`: Contiene todas las figuras y tablas generadas por los scripts.
*   `Prediction/`: Contiene el csv con la predicción del modelo con mejores resultados.

## Contribuciones

Este proyecto es el resultado del trabajo colaborativo. Las contribuciones sustanciales de cada miembro incluyen:


## Marlon Angulo Ramos
Extracción de datos de OpenStreetMap para Chapinero

Desarrollo de modelos base: Linear Regression, Elastic Net, CART

Engineering de variables espaciales (distancias y scores)

Análisis de feature importance y selección de variables

## Martin Pinto Talero
Optimización de hiperparámetros con Grid Search y Bayesian Optimization

Desarrollo de XGBoost con regularización avanzada

Implementación de Neural Networks para capturar no-linealidades

Análisis comparativo de métricas de performance

## Elian Moreno Cuellar
Desarrollo de Random Forest y ensemble methods

Implementación de modelos alternativos y benchmarking

Validación de resultados en conjunto de test

Análisis de overfitting y técnicas de regularización

## Camilo Ávila Araque
Procesamiento de datos y feature engineering con variables OSM

Desarrollo del pipeline de Super Learner con stacking ensemble

Implementación de validación cruzada espacial para evitar overfitting

Integración final de modelos y documentación en LaTeX

