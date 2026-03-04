# =============================================================================
# PROYECTO: Análisis Descriptivo de Población Mundial
# Dataset:  World Bank Population Data (1960-2023)
# Autor:    Felipe Iraola
# =============================================================================


# -----------------------------------------------------------------------------
# PASO 1: Cargar librerías
# -----------------------------------------------------------------------------

install.packages(c("tidyverse", "skimr", "corrplot"))

library(tidyverse)   # incluye dplyr (manipulación) y ggplot2 (visualización)
library(skimr)       # resumen estadístico más completo que summary()
library(corrplot)    # visualización de matrices de correlación


# -----------------------------------------------------------------------------
# PASO 2: Cargar y explorar los datos
# -----------------------------------------------------------------------------

pop <- read_csv("population.csv")

glimpse(pop)       # tipo de cada columna y primeros valores
head(pop, 10)      # primeras 10 filas
tail(pop, 5)       # últimas 5 filas
nrow(pop)          # cantidad de filas
ncol(pop)          # cantidad de columnas
names(pop)         # nombres de las columnas


# -----------------------------------------------------------------------------
# PASO 3: Limpiar los nombres de columnas
# -----------------------------------------------------------------------------

pop <- pop %>%
  rename(
    country_name = `Country Name`,
    country_code = `Country Code`,
    year         = Year,
    population   = Value
  )

names(pop)


# -----------------------------------------------------------------------------
# PASO 4: Estadísticas descriptivas globales
# -----------------------------------------------------------------------------

# summary() da mínimo, máximo, media, mediana y cuartiles
summary(pop$population)

# skim() da un resumen mucho más completo: histograma, % de NAs, desvío estándar
skim(pop)

# Calcular manualmente las métricas clave


pop %>%
  summarise(
    media    = mean(population, na.rm = TRUE),   # na.rm ignora valores faltantes
    mediana  = median(population, na.rm = TRUE),
    desvio   = sd(population, na.rm = TRUE),
    varianza = var(population, na.rm = TRUE),
    minimo   = min(population, na.rm = TRUE),
    maximo   = max(population, na.rm = TRUE),
    rango    = maximo - minimo,
    q1       = quantile(population, 0.25, na.rm = TRUE),
    q3       = quantile(population, 0.75, na.rm = TRUE),
    iqr      = IQR(population, na.rm = TRUE)     # rango intercuartílico
  )


# -----------------------------------------------------------------------------
# PASO 5: Filtrar países de Latinoamérica
# -----------------------------------------------------------------------------
# %in% es como el operador "includes" de JavaScript

latam <- pop %>%
  filter(country_code %in% c("URY", "ARG", "BRA", "CHL", "COL", "PER", "MEX"))

unique(latam$country_name)


# -----------------------------------------------------------------------------
# PASO 6: Estadísticas descriptivas por país
# -----------------------------------------------------------------------------

stats_por_pais <- latam %>%
  group_by(country_name) %>%
  summarise(
    media_poblacion  = mean(population, na.rm = TRUE),
    mediana          = median(population, na.rm = TRUE),
    desvio_std       = sd(population, na.rm = TRUE),
    min_historico    = min(population, na.rm = TRUE),
    max_historico    = max(population, na.rm = TRUE),
    n_años           = n()    # cuenta cuántas filas hay por grupo
  ) %>%
  arrange(desc(media_poblacion))   # ordenar de mayor a menor

print(stats_por_pais)


# -----------------------------------------------------------------------------
# PASO 7: Visualización — Evolución temporal (línea)
# -----------------------------------------------------------------------------

ggplot(data = latam,
       aes(x = year, y = population / 1e6,  # dividimos por millón para simplificar el eje
           color = country_name)) +          # cada país tiene su color
  geom_line(linewidth = 1) +                 # capa de líneas
  labs(
    title    = "Evolución de la Población en Latinoamérica (1960-2023)",
    subtitle = "Fuente: Banco Mundial",
    x        = "Año",
    y        = "Población (millones)",
    color    = "País"
  ) +
  theme_minimal()    # estilo limpio sin fondo gris


# -----------------------------------------------------------------------------
# PASO 8: Visualización — Boxplot por país
# -----------------------------------------------------------------------------

ggplot(data = latam,
       aes(x = country_name, y = population / 1e6, fill = country_name)) +
  geom_boxplot(alpha = 0.7) +    # alpha controla la transparencia
  labs(
    title = "Distribución Histórica de Población por País",
    x     = "País",
    y     = "Población (millones)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")   # ocultamos la leyenda porque el eje ya identifica


# -----------------------------------------------------------------------------
# PASO 9: Visualización — Histograma
# -----------------------------------------------------------------------------
# El histograma muestra cómo se distribuyen los valores de una variable

# Filtramos un solo año para no mezclar períodos
pop_2023 <- pop %>% filter(year == 2023, !is.na(population))

ggplot(data = pop_2023,
       aes(x = population / 1e6)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "white") +
  scale_x_log10() +    # escala logarítmica porque hay países muy grandes y muy pequeños
  labs(
    title = "Distribución de Población Mundial en 2023",
    x     = "Población (millones, escala log)",
    y     = "Cantidad de países"
  ) +
  theme_minimal()


# -----------------------------------------------------------------------------
# PASO 10: Análisis de correlación
# -----------------------------------------------------------------------------
# Analizamos si la población de distintos países creció de forma correlacionada

# Pivotamos a formato ancho: cada país es una columna, cada fila un año
latam_wide <- latam %>%
  select(country_name, year, population) %>%
  pivot_wider(
    names_from  = country_name,   # los países pasan a ser columnas
    values_from = population
  ) %>%
  select(-year)    # quitamos el año para la matriz de correlación

# Calculamos la matriz de correlación (solo columnas numéricas)
matriz_cor <- cor(latam_wide, use = "complete.obs")

print(round(matriz_cor, 2))   # redondeamos a 2 decimales para leer mejor

# Visualizamos la correlación
corrplot(
  matriz_cor,
  method = "color",      # celdas de color
  type   = "upper",      # solo triángulo superior (evita repetición)
  addCoef.col = "black", # muestra el número dentro de cada celda
  tl.col = "black",      # color de las etiquetas
  tl.srt = 45            # ángulo de las etiquetas
)


# -----------------------------------------------------------------------------
# PASO 11: Detectar outliers
# -----------------------------------------------------------------------------
# Un outlier típico está más allá de 1.5 * IQR del cuartil 1 o 3

pop_2023 <- pop_2023 %>%
  mutate(
    q1     = quantile(population, 0.25, na.rm = TRUE),
    q3     = quantile(population, 0.75, na.rm = TRUE),
    iqr    = q3 - q1,
    limite_superior = q3 + 1.5 * iqr,
    es_outlier = population > limite_superior
  )

# Países con población considerada outlier (los más grandes)
pop_2023 %>%
  filter(es_outlier == TRUE) %>%
  select(country_name, population) %>%
  arrange(desc(population))


# -----------------------------------------------------------------------------
# PASO 12: Exportar resultados
# -----------------------------------------------------------------------------

write_csv(stats_por_pais, "estadisticas_latam.csv")



# =============================================================================
# FIN DEL SCRIPT
# Próximo paso: convertír este script en un R Markdown (.Rmd)
# para generar un reporte HTML con texto + código + gráficos integrados.
# =============================================================================