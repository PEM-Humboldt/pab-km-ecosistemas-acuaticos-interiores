#**********************************************************
# Título: Cálculo de integridad por demanda
#
# Autor(a): Alejandra Narváez Vallejo
#
# Descripción:
#**********************************************************
# Título: Cálculo de integridad por demanda
#
# Autor(a): Alejandra Narváez Vallejo
#
# Descripción:
# Este script calcula un índice de integridad hidrológica para microcuencas,
# basado en la relación entre la demanda hídrica y el caudal ecológico estimado
# a partir del método de Tennant. Este índice permite evaluar si las demandas
# hídricas afectan negativamente la capacidad ecológica del río.
#
# Se utiliza el método de Tennant (también conocido como método del porcentaje del caudal medio),
# que estima caudales ecológicos a partir del caudal medio anual (Qm) bajo diferentes escenarios:
# - 10% de Qm: mínimo para la sobrevivencia de formas de vida acuática.
# - 30% de Qm: suficiente para mantener un hábitat adecuado.
# - 60% de Qm: condiciones excelentes para ecosistemas acuáticos.
#
# A partir de estos umbrales se calcula la reducción relativa (R) y un índice de integridad hidrológica (IIH),
# el cual varía linealmente de 0.01 a 1, siendo 1 una condición ideal (sin sobreexplotación) y
# valores cercanos a 0, una condición crítica de presión sobre el caudal.
#
# Interpretación del índice:
# Para el escalamiento mencionado Y según los umbrales descritos se tuvo cuenta lo siguiente:
# - Si la reducción del caudal debido a la demanda no supera el 60% del caudal medio anual (Qm),
#   se considera que la integridad hidrológica es excelente (índice cercano a 1).
# - Si la demanda supera el caudal ecológico mínimo (correspondiente al 10% del Qm),
#   entonces se considera que la integridad está severamente comprometida y se asigna un valor mínimo de 0.01.
#
# Metodología aplicada:
#
# 1. Lee múltiples archivos CSV con datos de demanda de agua por microcuenca.
# 2. Agrega las demandas anuales por código y las convierte a caudales en m³/s.
# 3. Une esta información con datos de caudal medio anual por microcuenca.
# 4. Calcula el caudal ecológico (Q60, Q30, Q10) y la reducción relativa (R).
# 5. Genera un índice de integridad hidrológica (0.01 a 1) según el R10.
# 6. Une los resultados a un shapefile con geometría de las microcuencas.
# 7. Exporta el resultado espacial actualizado.
#
# Descripción de los datos:
# - Los archivos CSV de la carpeta 'Datos/Demand' contienen columnas:
#   - Code: identificador de la microcuenca
#   - Value: volumen anual demandado (en m³/año)
# - El archivo Excel 'KEA_hidro_física.xlsx' contiene:
#   - Codigo Microcuenca: código de microcuenca
#   - Mean annual flow (m3/s): caudal medio anual en m³/s
# - El shapefile de integridad contiene geometría y un campo "CodeMcr" para unión espacial
#
#**********************************************************

#**********************************************************
# Cargar librerías ----------------------------------------
#**********************************************************
library(dplyr)
library(readr)
library(purrr)
library(stringr)
library(readxl)
library(sf)
library(tidyr)

#**********************************************************
# Definir directorios de trabajo --------------------------
#**********************************************************
setwd(file.path(this.path::this.path(), "..", ".."))  # Subir dos niveles desde el script

dir_datos <- file.path("Datos")
dir_Intermedios <- file.path("Res_Intermedios")
dir_Resultados <- file.path("Resultados")

#**********************************************************
# Cargar y procesar datos de demanda hídrica --------------
#**********************************************************

# 1. Listar archivos CSV que terminan en 'MM.csv'
archivos_mm <- list.files(path = file.path(dir_datos, "Demand"), pattern = "MM\\.csv$", full.names = TRUE)

# 2. Leer y combinar los archivos, agregando el nombre del archivo como columna
datos_mm <- map_df(archivos_mm, ~ read_csv(.x, show_col_types = FALSE) %>% 
                     mutate(archivo = basename(.x)))

# 3. Sumar los valores de demanda del año por cuenca y archivo
resumen_por_archivo <- datos_mm %>%
  group_by(Code, archivo) %>%
  summarise(suma_value = sum(Value, na.rm = TRUE), .groups = "drop")


# 4. Sumar los valores totales por cuenca (sin distinguir archivo)
# 5. Convertir los valores de m³/año a m³/s
resumen_total <- resumen_por_archivo %>%
  group_by(Code) %>%
  summarise(total_value = sum(suma_value), .groups = "drop") %>%
  mutate(m3_por_segundo = total_value / 31557600)  # 1 año en segundos

#**********************************************************
# Unir con datos de caudal natural ------------------------
#**********************************************************

# 6. Cargar archivo Excel con datos de caudal natural
ruta_archivo <- file.path(dir_Intermedios, "KEA_hidro_física.xlsx")
KEA0 <- read_excel(ruta_archivo, sheet = 1)

# 7. Unir resumen_total con KEA usando 'Code' y 'Codigo Microcuenca'
resultado_final <- resumen_total %>%
  left_join(KEA0, by = c("Code" = "Codigo Microcuenca"))

#**********************************************************
# Cálculo del índice de integridad hidrológica ------------
#**********************************************************

resultado_final <- resultado_final %>%
  mutate(
    Q60 = `Mean annual flow (m3/s)` * 0.6,
    Q30 = `Mean annual flow (m3/s)` * 0.3,
    Q10 = `Mean annual flow (m3/s)` * 0.1,
    
    reduccion = `Mean annual flow (m3/s)` - m3_por_segundo,
    
    R60 = reduccion / Q60,
    R30 = reduccion / Q30,
    R10 = reduccion / Q10
  ) %>%
  mutate(
    R10 = ifelse(is.na(R10), 1, R10),  # Asignar 1 donde R10 sea NA
    
    # Índice de integridad con base en R10 (escalado lineal entre 0.01 y 1)
    indice_R10 = case_when(
      R10 < 1 ~ 0.01,
      R10 >= 6 ~ 1,
      TRUE ~ 0.01 + ((R10 - 1) / (6 - 1)) * (1 - 0.01)
    )
  )

#**********************************************************
# Unir con shapefile de integridad y exportar resultado ---
#**********************************************************

# 8. Seleccionar solo columnas clave para unir con shapefile
resultado_reducido <- resultado_final %>%
  select(1:4, (ncol(.) - 7):ncol(.)) %>% 
  mutate(Code = as.character(Code))  # Asegurar que Code sea carácter

# 9. Cargar shapefile de integridad
Integridad <- read_sf("~/TNC/Res_Intermedios/Integridad_corregida-20250318T211628Z-001/Integridad_corregida/Integridad_total_cor.shp")

# 10. Unir shapefile con resultado usando 'CodeMcr' y 'Code'
Integridad <- left_join(Integridad, resultado_reducido, by = c("CodeMcr" = "Code")) %>%
  mutate(indice_R10 = ifelse(is.na(indice_R10), 1, indice_R10))  # Asignar 1 donde no se unió

# 11. Graficar índice de integridad
plot(Integridad["indice_R10"])

# 12. Guardar nuevo shapefile
st_write(
  Integridad,
  "~/TNC/Res_Intermedios/Integridad_corregida-20250318T211628Z-001/Integridad_corregida/Integridad_total_cor2.shp",
  delete_dsn = TRUE  # Sobrescribir si ya existe
)


