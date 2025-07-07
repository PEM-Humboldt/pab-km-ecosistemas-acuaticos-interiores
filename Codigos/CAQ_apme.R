# Título: Procesamiento de Áreas Protegidas y de Manejo Especial
#
# Autor(es): Alejandra Narváez Vallejo
#
# Descripción: 
# Este script prepara y organiza las capas espaciales de áreas protegidas y zonas de manejo especial 
# en Colombia (RUNAP, Resguardos Indígenas, Zonas de Reserva Campesina y OMEC) asociados a tres años clave (2012, 2018 y 2024).
# Y calcula áreas totales de protección y manejo especial evitando la doble contabilización 
# El código incluye transformación de proyecciones, intersección con AOI y cuencas, y 
# consolidación de áreas totales protegidas por año.

# Este script genera un objeto .RData con las capas por año (2012, 2018, 2024)
# de áreas protegidas y especiales, ya intersectadas con cuencas y AOI.
# Estas capas se utilizarán para análisis posteriores de indicadores de conservación.
# La documentación completa de los procesos se encuentra en el documento xxxxxxxxxxxxxxxxxxx

#
# Fuentes: 
#* RUNAP (Registro Único Nacional de Áreas Protegidas) - Parques Nacionales Naturales (2019, 2024)
#* Zonas de Reserva Campesina Constituidas y Resguardos Indígenas Formalizados - Agencia Nacional de Tierras (2025)
#* OMEC (Otras Medidas Efectivas de Conservación) - Ministerio de Ambiente (2025)
#
# Por hacer o corregir:
# - Quitar librerías que no se necesiten
# - Verificar columnas clave para fechas (e.g., ULTIMA_FEC en resguardos)
# - Confirmar AOI (`aoi`, `aoi_sub`) cargado previamente

#**********************************************************
# librerías o dependencias --------------------------------
#**********************************************************

# lectura de datos 

library (sf) 
library(terra)
library(dplyr)



#**********************************************************
# Definir directorio(s) de trabajo -----------------------
#**********************************************************

setwd(file.path(this.path::this.path(),"..",".."))

dir_datos<- file.path("Datos")
dir_Intermedios<- file.path ("Res_Intermedios")
dir_Resultados<- file.path ("Resultados")

#**********************************************************
# Cargar los datos necesarios ----------------------------
#**********************************************************

load(file=file.path (dir_Resultados,"Cuencas_caq.RData"))

# Cargar áreas de Manejo especial y protegidas

runap<- read_sf(file.path(dir_datos, "aresas_especiales/runap05-29-2019/runap2Polygon.shp")) 
runap_actual<- read_sf(file.path(dir_datos, "aresas_especiales/latest/runap.shp")) 

RIngdigenas<- read_sf(file.path(dir_datos,"aresas_especiales/Resguardo_Indigena_Formalizado/Resguardo_Indigena_Formalizado.shp"))

RCampesinas24<-st_read( "~/TNC/Datos/aresas_especiales/Zonas_de_Reserva_Campesina_Constituida/Zonas_de_Reserva_Campesina_Constituida.shp")
OMEC24 <- read_sf(file.path(dir_datos,"aresas_especiales/Capa entregada Ministerio_shape_omec_reportadas_55/shape_omec_reportadas_55/shape_omec_reportadas_55.shp")) # reportadas a 2025


#**********************************************************
# Parametros globales ----------------------------
#**********************************************************
sf::sf_use_s2(F) # desactivar el uso de la biblioteca s2 para las operaciones geométricas esféricas. Esto optimiza algunos analisis de sf.
crs0 <- 9377  # EPSG:9377 usado en Colombia


## Funciones ####

#' preparar capa espacial Ajustando proyección y zona estudio 
#'
#' @param x Área protegida de manejo especial objeto tipo SF
#' @param crs Código EPSG para la proyección deseada (ej. 9377)
#' @param aoi Objeto sf con el área de interés para recortar
#' @return Objeto `sf` reproyectado e intersectado con AOI
#' 
PrepararCapaAOI <- function(x, crs, aoi) {
  
    sf::st_transform(x, crs) %>%
    sf::st_intersection(aoi)
}



#**********************************************************
# Preparar datos ----------------------------
#**********************************************************

# preparar capa espacial Ajustando proyección y zona estudio 

OMEC24 <- PrepararCapaAOI(OMEC24, crs0, aoi)
runap_actual <- PrepararCapaAOI(runap_actual, crs0, aoi)
RIngdigenas <- PrepararCapaAOI(RIngdigenas, crs0, aoi)
RCampesinas24 <- PrepararCapaAOI(RCampesinas24, crs0, aoi)
runap <- PrepararCapaAOI(runap, crs0, aoi)


## Intersectar zonas y cuencas ####
# hacer las intersecciones para poder tener la informacion completa de a que cuenca pertenece cada zona

#OMEC

omec_n_cuencas <- OMEC24%>%
  select(NOMBRE_OME,ID_OMEC)%>% 
  st_intersection(aoi_sub)

#*runap

runap_n_cuencas <- runap%>%
  select(nombre,id_pnn,fecha_act)%>% 
  st_intersection(aoi_sub)


runap24_n_cuencas <- runap_actual%>%
  select(ap_nombre,ap_id,fecha_insc)%>% 
  st_intersection(aoi_sub)


#*rInd

rInd_n_cuencas <- RIngdigenas%>% 
  st_intersection(aoi_sub)

#*rZRC

rZRC_n_cuencas24 <- RCampesinas24%>% 
  select(NOMBRE_ZON,RESOLUCION) %>% 
  st_intersection(aoi_sub)
# Las zonas de reserva campesina son todas mayores al 2022


## Dividir Zonas en años 2012 y 2018 segun resolución ####

## definir las fechas para la division

fecha_limite12 <- as.Date("2012-12-31")
fecha_limite18 <- as.Date("2018-12-31")

## hacer las divisiones 

# runap

# Definir la fecha límite

# Filtrar las fechas
runap_2012 <- runap_n_cuencas %>% filter(fecha_act <= fecha_limite12)
runap_2018 <- runap_n_cuencas %>% filter(fecha_act <= fecha_limite18)
plot(runap_2018$geometry)

# RIndigenas 

# Filtrar las fechas
rInd_2012 <- rInd_n_cuencas %>% filter(ULTIMA_FEC<= fecha_limite12)
rInd_2018 <- rInd_n_cuencas %>% filter(ULTIMA_FEC <= fecha_limite18)
rInd_2024 <- rInd_n_cuencas



## Capas completas de proteccion por años ####
# Las interseccione no se cuentan doble

tot_2012 <-bind_rows(rInd_2012[1],runap_2012[1]) %>% 
  st_union() %>% 
  st_sf()


tot_2012 <- tot_2012%>% 
  st_intersection(aoi_sub)

tot_2018 <- bind_rows(rInd_2018[1],runap_2018) %>% 
  st_union() %>% 
  st_sf()

tot_2018 <- tot_2018%>% 
  st_intersection(aoi_sub)


tot_24 <- bind_rows(rInd_2024[1],runap_actual[4],RCampesinas24[3],OMEC24[2]) %>% 
  st_union() %>% 
  st_sf()


tot_24 <- tot_24%>% 
  st_intersection(aoi_sub)

save(runap_2012, rInd_2012,
    runap_2018, rInd_2018,
    runap24_n_cuencas, rInd_2024, rZRC_n_cuencas24, omec_n_cuencas, 
    tot_2012,tot_2018,tot_24,
    file = file.path(dir_Resultados,"caq_zonasEspecial_sf.RData"))




plot(tot_2012$geometry)
plot(tot_2018$geometry)
plot(tot_24$geometry)

