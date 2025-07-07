#**********************************************************
# Título: Preparación de las tipologías de humedales y ríos para el área de estudio
# Autor: Alejandro Narváez Vallejo
# Descripción: Este código procesa las clasificación de los ecosistemas lóticos y lénticos necesarias para el cálculo de indicadores.
# El Código es específico para la Cuenca del Caquetá ya que en esta Cuenca se requirieron correcciones específicas (ej: Corrección de nombres). Se puede usar en otras cuencas pero se debe revisar el código para saber qué partes son innecesarias o se deben agregar.
# La documentación completa de los procesos se encuentra en el documento xxxxxxxxxxxxxxxxxxx


# Fuentes:
#* Microcuencas y  Subcuencas (TNC, 2024a)
#* Mapas de ecosistemas continentales, marinos y costeros (MEC) a escala 1:100.000 del IDEAM para los años 2017 y 2024 (IDEAM, 2017 y 2024)
#* Red hídrica (TNC, 2024a) derivada de un modelo de elevación digital corregido a una resolución de 90 m.
#* Los atributos ecológicos clave (TNC, 2024a)
#* Mapa de ecorregiones, Abell et al. (2008)
#* 
#* Las citas se encuentran en la página principal del repositorio

# para hacer:!!!!!

# Escribir el nombre del documento cuando este listo 

#**********************************************************

#**********************************************************
# Librerías necesarias -------------------------------------
#**********************************************************

library(sf)
library(terra)
library(dplyr)
library(units)
library(tidyverse)
library(readxl)
library(nngeo)

#**********************************************************
# Directorios de trabajo ----------------------------------
#**********************************************************

setwd(file.path(this.path::this.path(),"..",".."))

# Definición de carpetas

# Carpeta con insumos originales
dir_datos <- file.path("Datos")
# Carpeta con resultados intermedios
dir_Intermedios <- file.path("Res_Intermedios")
# Carpeta con resultados finales
dir_Resultados <- file.path("Resultados")

#**********************************************************
# Carga de datos ------------------------------------------
#**********************************************************

# Capas vectoriales

rios <- read_sf(file.path(dir_datos, "caqueta/RedCaqueta/RedCaqueta/RedCaqueta.shp"))[c(3:4)] # Red hídrica

cuencas0 <- read_sf(file.path(dir_Intermedios, "caqueta/cuencas_niveles2.shp")) # microcuencas.
# la capa de la microcuencas tiene correciones respecto al archivo original. Estas están descritas en el documento xxxx. 

MEC24 <- st_read(file.path(dir_datos, "Mapa_Ecosistemas_Continentales_Costeros_Marinos_100K_2024/Mapa_Ecosistemas_Continentales_Costeros_Marinos_100K_2024/SHAPE/e_eccmc_100K_2024.shp")) # mec 2024
MEC17 <- st_read(file.path(dir_datos, "Mapa_ecosistemas_Continentales_Marinos_Costeros_100K_V2.1_2017/Mapa_ecosistemas_Continentales_Marinos_Costeros_100K_V2.1_2017/Shape_E_ECCMC_Ver21_100K/E_ECCMC_Ver21_100K.shp")) # mec 2017

# A la capa de ecorregiones se le hizo previamente ajustes de atributos y geométrico. Primeramente las ecorregiones fueron recortadas al área de estudio y luego se le asignó a los polígonos diminutos (<10 ha) del borde de la cuenca la clase dominante más cercana.
ecorreg0 <- read_sf(file.path(dir_Intermedios, "ecorregiones_caq93772.shp")) # ecorregiones



# Tabla KEA (Atributos claves de los ecosistemas)
ruta_archivo <- file.path(dir_Intermedios, "KEA_hidro_física.xlsx")
hojas <- excel_sheets(ruta_archivo)
KEA0 <- read_excel(ruta_archivo, sheet = 2)

#**********************************************************
# Parámetros globales -------------------------------------
#**********************************************************

sf::sf_use_s2(FALSE)  # Desactiva geometría esférica para operaciones más precisas
crs0 <- 9377  # EPSG:9377 usado en Colombia

#**********************************************************
# Preparación de datos ------------------------------------
#**********************************************************

## Ríos ####
rios <- st_transform(rios, crs0)

## Cuencas ####
# Corrección de nombres
nombres_cor <- unique(cuencas0$NOMSZH)
nombres_cor <- gsub("R�o", "Río", nombres_cor, fixed = TRUE)
nombres_cor <- gsub("Cu�are", "Cuñare" ,nombres_cor, fixed = TRUE)
nombres_cor <- gsub("Yar�", "Yarí" ,nombres_cor, fixed = TRUE)
nombres_cor <- gsub("� Macay�", "o Macayá",nombres_cor, fixed = TRUE)
nombres_cor <- gsub("Río Cueman�",  "Río Cuemaní"   ,nombres_cor, fixed = TRUE)
nombres_cor <- gsub("Río Cahuinar�"  ,"Río Cahuinarí"  ,nombres_cor, fixed = TRUE)
nombres_cor <- gsub("Río Mirit�-Paran�" ,"Río Mirití-Paraná"  ,nombres_cor, fixed = TRUE)

# Tabla de consulta para luego corregir los nombres
df_cor_nom <- data.frame(NOMSZH=unique(cuencas0$NOMSZH),NOMSZH2=nombres_cor)


# Aplicar correcciones
cuencas <- cuencas0 %>%
  mutate(Area_micro_ha = set_units(st_area(geometry),ha)) %>%
  left_join(df_cor_nom) %>%
  select(4, 14, 20, 15, 17, 19)

## Área de estudio (AOI) ####
# Crear un polígono de la Cuenca completa
aoi <- cuencas %>% st_union() %>% st_sf() %>% st_remove_holes() %>% mutate(Area_tot_ha = set_units(st_area(.), ha))

## Subcuencas y Zonas Hidrográficas ####
# Crear un poligonos para subcuencas y zons hidrográficas
aoi_sub <- cuencas %>% aggregate(by = list(cuencas$subName), FUN = first) %>% st_remove_holes() %>% mutate(Area_sub_ha = set_units(st_area(.), ha)) %>% select (3:6,9)
aoi_ZH <- cuencas %>% aggregate(by = list(cuencas$NOMSZH2), FUN = first) %>% st_remove_holes() %>% mutate(Area_zh_ha = set_units(st_area(.), ha)) %>% select (3:4,9)



save(cuencas,aoi,aoi_sub,aoi_ZH, file = file.path(dir_Resultados, "Cuencas_caq.RData"))

# Las líneas comentadas de abajo hacen lo mismo de arriba; la diferencia radica en el control que tengo sobre los atributos finales. En el código de abajo no tengo control.
# # Zonas  hidrográficas
# 
# SubCuencas <- cuencas %>% 
#   group_by(subName, subCode)%>%
#   summarise(Area_subcuenca_m2 = sum (Area_micro_m2) )

## Ecorregiones ####
# Reclasificar las clases originales para tener únicamente 3 Globales.
ecorreg <- ecorreg0 %>%
  mutate(Nom = factor(case_when(
    FEOW_ID %in% c(305, 306, 302, 301, 312) ~ "Alto en la Montaña",
    FEOW_ID == 313 ~ "Piedemonte",
    FEOW_ID == 316 ~ "Tierras bajas"
  ), levels = c("Alto en la Montaña", "Piedemonte", "Tierras bajas"))) %>%
  group_by(Nom) %>%
  summarise(geometry = st_union(geometry), .groups = "drop") %>%
  mutate(area_m2 = st_area(geometry))


# # Definir la ruta de salida para el nuevo shapefile
# ruta_salida <- "~/TNC/Res_Intermedios/ecorregion_rem.shp"
# 
# # Guardar el resultado en un nuevo shapefile
# st_write(ecorreg, ruta_salida, delete_dsn = TRUE)

## Tabla KEA ####
# Dar nombres a las categorías de los diferentes atributos
KEA <- KEA0 %>%
  mutate(
    Descripcion_caudal = case_when(
      Q < 10 ~ "Caudal bajo",
      Q < 100 ~ "Caudal medio",
      Q < 1000 ~ "Caudal alto",
      Q <= 10000 ~ "Caudal muy alto",
      TRUE ~ NA_character_
    ),
    CategoriaT = case_when(
      WaterQuality == 1 ~ "Blancas",
      WaterQuality == 2 ~ "Negras",
      WaterQuality %in% 3:4 ~ "Claras",
      TRUE ~ NA_character_
    ),
    `Limitado por transporte o suministro` = case_when(
      `Limitado TS` == "Capacity-limited" ~ "Limitado por transporte",
      `Limitado TS` == "Supply-limited" ~ "Limitado por suministro",
      TRUE ~ NA_character_
    ),
    Attribute_index = case_when(
      Attribute_index0 == "Unconfined" ~ "No confinado",
      Attribute_index0 == "Confined" ~ "Confinado",
      TRUE ~ NA_character_
    )
  )

#********************************************************** " '
# Tipologías de humedales y ríos --------------------------
#**********************************************************

## Humedales 2024 y 2017 ####

# Las capas se procesan seleccionando únicamente los ecosistemas acuáticos, Se revisa su geometría y transforma al sistema de referencia nacional.
# Seguidamente se intersecta con la subcuencas para tener la información de a que su cuenta pertenecen y se hace finalmente el cálculo de área.

acu24 <- MEC24 %>% select(2, 7, 8) %>% filter(tipo_ecos == "Acuatico") %>% st_make_valid() %>% st_transform(crs0)
sub_acu24 <- acu24[aoi, ] %>% st_intersection(aoi_sub) %>% mutate(Area_polygono_ha = set_units(st_area(.), ha))

acu17 <- MEC17 %>% select(2, 7, 8) %>% filter(TIPO_ECOSI == "Acuatico") %>% st_make_valid() %>% st_transform(crs0)
sub_acu17 <- acu17[aoi, ] %>% st_intersection(aoi_sub) %>% mutate(Area_polygono_ha = set_units(st_area(.), ha))


# Los datos del 2014 no están normalizados con los del 2017 por lo que se requirió hacer normalización de nombres

### Normalizar nombres ####

# Lista de conectores que deben mantenerse en minúsculas
conectores <- c("de", "del", "la", "las", "el", "los", "y", "en", "con", "por", "a")

# Función para capitalizar correctamente solo las palabras clave
capitalize_correctly <- function(text) {
  words <- str_split(text, " ")[[1]]  # Divide en palabras
  words <- ifelse(words %in% conectores, words, str_to_title(words))  # Capitaliza solo si no es conector
  str_c(words, collapse = " ")  # Une las palabras de nuevo
}


# Aplicar la función para capitalizar la primera letra y crear una lista de nombres que tenga todos los ecosistemas únicos en ambos años

# Normalizamos ambos vectores

ecos_general_norm <- sapply(unique(sub_acu24$ecos_gener), capitalize_correctly)
clas_acu_norm <- sapply(sort(unique(sub_acu17$ECOS_GENER)), capitalize_correctly) 

# Verificar si hay diferencias
diferencias_1 <- setdiff(ecos_general_norm, clas_acu_norm)  # En ecos_general pero no en clas_acu
diferencias_2 <- setdiff(clas_acu_norm, ecos_general_norm)  # En clas_acu pero no en ecos_general
diferencias_3 <- setdiff(ecos_general_norm, unique(sub_acu24$ecos_gener))  # Comparar 2024 conlos 2024 corregido, Idealmente deben ser iguales

diferencias_1
diferencias_2
diferencias_3

# Vector unificado
eco_general_def <- sort(union(ecos_general_norm, clas_acu_norm))

# Aplicar la función a La capa completa del 2017
sub_acu17 <- sub_acu17 %>%
  mutate(ecos_gener = sapply(ECOS_GENER, capitalize_correctly))%>% 
  mutate(ecos_gener = factor(ecos_gener, levels=eco_general_def))

save(sub_acu17,sub_acu24, file = file.path(dir_Resultados, "humedales_caq.RData"))

## Tipología de ríos ####
# La tipología es la combinacion de los 5 atributos claves de los ecosisitemas

# 1. Unir los datos de ríos con la tabla KEA
rios_u_color0 <- right_join(
  rios, 
  KEA, 
  by = join_by("Code_UTTL" == `Micro-basins code`)  # Unir por campo común entre rios y KEA
) 

# 2. Intersecar espacialmente los ríos resultantes con las ecorregiones (recorte espacial)
rios_u_color <- rios_u_color0 %>%
  st_intersection(ecorreg[c(1)]) %>%  # Mantener solo los tramos que están dentro de la ecorregión seleccionada
  
  # 3. Crear una columna 'cadena' que combina varios atributos en una sola cadena de texto
  mutate(cadena = paste(
    CategoriaT, 
    Descripcion_caudal, 
    Limitado.por.transporte.o.suministro, 
    Attribute_index, 
    Nom, 
    sep = "*"
  )) %>%
  
  # 4. Crear una columna categórica ordenada 'cadena4' con combinaciones específicas de caudal y limitación
  mutate(cadena4 = factor(
    paste(Descripcion_caudal, Limitado.por.transporte.o.suministro, sep = " * "), 
    levels = c(
      "Caudal bajo * Limitado por suministro",
      "Caudal bajo * Limitado por transporte",
      "Caudal medio * Limitado por suministro",
      "Caudal medio * Limitado por transporte",
      "Caudal alto * Limitado por suministro",
      "Caudal alto * Limitado por transporte",
      "Caudal muy alto * Limitado por suministro",
      "Caudal muy alto * Limitado por transporte"
    )
  )) %>%
  
  # 5. Unir con la tabla de cuencas (sin geometría), para agregar atributos adicionales
  left_join(st_drop_geometry(cuencas), by = "Code_UTTL")%>%
  
  left_join(
    st_drop_geometry(aoi_sub[,4:5]), 
    by = "subName"    # Unir por campo común entre rios y KEA
  ) %>% 
  st_cast("LINESTRING") %>% 
  
  # 6. Calcular la longitud de cada segmento de río en kilómetros
  mutate(longitud = set_units(st_length(geometry), km))


save(rios_u_color, file = file.path(dir_Resultados, "rios_u_color_caq.RData"))


