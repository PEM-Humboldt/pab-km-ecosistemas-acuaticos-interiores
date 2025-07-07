# Título: Procesamiento de Áreas Protegidas y de Manejo Especial
#
# Autor(es): Alejandra Narváez Vallejo
#
# Descripción:  Este codigo hace el cálculo de extensión The ecosistemas tipo humedal y ecosistemas de ríos según la metodología descrita en la ficha técnica XXXXX.
# Los resultados del indicador están a nivel subcuenca, zona hidrográfica, y cuenca total.
# Se elaboran gráficas y tablas que resumen los resultados

#
# Fuentes: 
# Depende de los objetos creados en el código CAQ_Tipologias

# 
# Por hacer o  corregir: 

# Nombre de la ficha técnica

#**********************************************************
# librerías o dependencias --------------------------------
#**********************************************************

# lectura de datos 

library (sf) 
library(terra)
library(dplyr)
library(units)


library(ggplot2)
library(tidyverse)

library(ggh4x)  # Para modificar los strips de las facetas
library(purrr)
library(writexl)


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

# Se cargan las cuencas y las tipologías de ríos y humedales
load(file=file.path (dir_Resultados,"Cuencas_caq.RData"))
load(file=file.path (dir_Resultados,"humedales_caq.RData"))
load(file=file.path (dir_Resultados,"rios_u_color_caq.RData"))

#**********************************************************
# Parametros globales ----------------------------
#**********************************************************
sf::sf_use_s2(F) # desactivar el uso de la biblioteca s2 para las operaciones geométricas esféricas. Esto optimiza algunos analisis de sf.

cat_caudal <- c("Caudal bajo",
                "Caudal medio",
                "Caudal alto",
                "Caudal muy alto")


#**********************************************************
# Procesos y estadísticas ---------------------------------
#**********************************************************

##  Extension humedales tipo -------------------------------------------

# Datos para probar la función

x <- sub_acu17
sbc <- "subName"
zh_nom <- "NOMSZH2"
nombre_fuente <- "sub_acu17"

# Esta función calcula estadísticas de extensión de ecosistemas de humedal 
# por subcuenca, zona hidrográfica, cuenca general, y cuenca total,
# a partir de una capa de entrada espacial (x), especificando los nombres de
# los campos de subcuenca (`sbc`), zona hidrográfica (`zh_nom`), 
# y una etiqueta de fuente (`nombre_fuente`) para identificar el origen de los datos.

ExtensionHumedales <- function(x, sbc, zh_nom, nombre_fuente) { 
  
  # Subcuencas: suma de área y frecuencia por subcuenca y ecosistema
  St_hum_n_cuencas <- x %>% 
    st_drop_geometry() %>% 
    group_by(.data[[sbc]], ecos_gener, Area_sub_ha) %>%
    summarise(
      tot_Area_ha = sum(Area_polygono_ha), 
      frecuencia = n(),
      Zona_hidro = first(.data[[zh_nom]]),
      .groups = "drop"
    ) %>% 
    group_by(.data[[sbc]]) %>% 
    mutate(
      prop_baseaño = prop.table(tot_Area_ha) * 100,        # proporción relativa entre clases ese año
      prop_cuenca = tot_Area_ha / Area_sub_ha * 100,       # proporción con respecto a área total de subcuenca
      año = nombre_fuente
    )
  
  # Zonas hidrográficas: suma de área y frecuencia por zona hidrográfica y ecosistema
  St_hum_n_Zhidro <- x %>% 
    st_drop_geometry() %>% 
    group_by(.data[[zh_nom]], ecos_gener) %>%
    summarise(
      tot_Area_ha = sum(Area_polygono_ha), 
      frecuencia = n(), 
      .groups = "drop"
    ) %>% 
    left_join(st_drop_geometry(aoi_ZH)) %>% 
    group_by(.data[[zh_nom]]) %>% 
    mutate(
      prop_baseaño = prop.table(tot_Area_ha) * 100,
      prop_cuenca = tot_Area_ha / Area_zh_ha * 100,
      año = nombre_fuente
    )
  
  # Cuenca : suma de área total por ecosistema
  St_hum_cuenca <- x %>%
    st_drop_geometry() %>%
    group_by(ecos_gener) %>% 
    summarise(
      tot_Area_ha = sum(Area_polygono_ha),
      No_cuencas = n(),
      .groups = "drop"
    ) %>% 
    mutate( 
      prop_baseaño = prop.table(tot_Area_ha) * 100,
      prop_cuenca = tot_Area_ha / aoi$Area_tot_ha * 100,
      año = nombre_fuente
    )
  
  # Cuenca total sin clases excluidas
  St_cuenca <- x %>%
    st_drop_geometry() %>%
    filter(!ecos_gener %in% c("Cuerpo de Agua Artificial", "Transicional Transformado")) %>% 
    summarise(
      tot_Area_ha = sum(Area_polygono_ha),
      No_cuencas = n(),
      .groups = "drop"
    ) %>% 
    mutate(
      prop_cuenca = tot_Area_ha / aoi$Area_tot_ha * 100,
      año = nombre_fuente
    )
  
  # Retornar una lista con las tablas
  contenedor <- list(
    subcuenca = St_hum_n_cuencas,
    zhidro = St_hum_n_Zhidro,
    cuenca = St_hum_cuenca,
    total = St_cuenca
  )
  
  return(contenedor)
}

## Aplicación a lista de capas 

# Lista de capas de entrada (objetos `sf`)
lista_clas <- list(sub_acu17, sub_acu24)
nombres_lista <- c("2012", "2018")

# Aplicar la función a cada capa de la lista
resultados <- mapply(
  ExtensionHumedales, 
  x = lista_clas, 
  sbc = "subName", 
  zh_nom = "NOMSZH2", 
  nombre_fuente = nombres_lista,
  SIMPLIFY = FALSE
)

# Combinar los resultados para cada tipo de análisis (por posición en las listas)
Resultados_DF <- map(seq_along(resultados[[1]]), function(i) {
  reduce(map(resultados, ~ .x[[i]]), bind_rows)
})

# Nombres para cada hoja en Excel
names(Resultados_DF) <- names(resultados[[1]])

length(unique(Resultados_DF$cuenca$ecos_gener))


# Guardar cada conjunto de valores únicos en una hoja separada
write_xlsx(Resultados_DF, file.path(dir_Resultados,"Ext_hum_caq.xlsx"))

### Gráficos extensión humedales####

# Barplot comparativo general - - - - - - - - - 

# Este gráfico muestra la extensión porcentual de humedales por tipo de ecosistema (ecos_gener)
# en toda la cuenca, comparando los diferentes años disponibles en los datos.

Resultados_DF$cuenca %>% 
  ggplot(aes(x = as.numeric(prop_cuenca), y = ecos_gener, fill = factor(año))) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(
    y = "",
    x = "Extensión (%)",
    fill = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.border = element_rect(color = "grey80", fill = NA, size = 0.5),
    axis.text = element_text(size = 8),
    axis.title.x = element_text(size = 8, margin = margin(t = 8)),
    panel.grid.major = element_line(color = "grey80", size = 0.3),  
    panel.grid.minor = element_line(color = "grey90", size = 0.2)
  ) +
  scale_x_continuous(breaks = seq(0, 100, by = 2)) +
  scale_y_discrete(drop = FALSE)

# Guardar el gráfico general
ggsave(file.path(dir_Resultados, "CAQ_EXT_general.png"), width = 17, height = 13.5, units = "cm", dpi = 400)


# Barplot comparativo por zona hidrográfica - - - - - - - - -

# Este gráfico muestra la extensión porcentual de los ecosistemas seleccionados
# en cada zona hidrográfica, para los diferentes años disponibles.
# Solo se visualizan los ecosistemas que tienen al menos un 0.5% de extensión
# en alguna zona hidrográfica.

# Filtrar ecosistemas relevantes con al menos 0.5% de cobertura en alguna zona hidrográfica
ecos_elegibles <- Resultados_DF$zhidro %>% 
  group_by(ecos_gener) %>% 
  summarise(maxi = max(prop_cuenca), .groups = "drop") %>% 
  filter(drop_units(maxi) >= 0.5)

# Generar barplot con facetas por ecosistema
Resultados_DF$zhidro %>% 
  filter(ecos_gener %in% ecos_elegibles$ecos_gener) %>%
  ggplot() +
  geom_bar(
    aes(y = .data[[zh_nom]], x = as.numeric(prop_cuenca), fill = factor(año)), 
    stat = "identity", 
    position = position_dodge(width = 0.8),
    size = 0.3
  ) +
  facet_wrap(vars(ecos_gener), nrow = 2) +
  labs(
    y = "",
    x = "Extensión (%)",
    fill = ""
  ) +
  theme_classic() +
  theme(
    panel.border = element_rect(color = "grey40", fill = NA, size = 0.5),
    axis.line = element_blank(),
    axis.ticks = element_line(color = "grey40", size = 0.3),
    axis.text = element_text(size = 8),
    axis.title.x = element_text(size = 8, margin = margin(t = 8)),
    strip.text = element_text(size = 7.5),
    strip.background = element_rect(fill = "grey95", color = "grey40", size = 0.2),
    panel.grid.major = element_line(color = "grey80", size = 0.3),
    panel.grid.minor = element_line(color = "grey90", size = 0.2),
    legend.position = "bottom"
  )

# Guardar el gráfico por zona hidrográfica
ggsave(file.path(dir_Resultados, "CAQ_EXT_zh.png"), width = 19, height = 20, units = "cm", dpi = 400)

##  Extension ríos tipo -------------------------------------------

# Datos para probar la función

rios_u_color$Descripcion_caudal <- factor( rios_u_color$Descripcion_caudal,levels=cat_caudal)


x <- rios_u_color
sbc <- "subName"
zh_nom <- "NOMSZH2"
nombre_fuente <- ""

# Esta función calcula estadísticas de extensión de ecosistemas de humedal 
# por subcuenca, zona hidrográfica, cuenca general, y cuenca total,
# a partir de una capa de entrada espacial (x), especificando los nombres de
# los campos de subcuenca (`sbc`), zona hidrográfica (`zh_nom`), 
# y una etiqueta de fuente (`nombre_fuente`) para identificar el origen de los datos.

ExtensionRios <- function(x, sbc, zh_nom) { 
  

  
  # cálculos nivel subcuencas
  St_rios_u_color <- x %>%
    st_drop_geometry() %>% 
      group_by(.data[[zh_nom]],.data[[sbc]], Nom,Limitado.por.transporte.o.suministro,Attribute_index,CategoriaT,Descripcion_caudal,cadena, cadena4,Area_sub_ha) %>% 
    summarise(long_sbc= sum(longitud))  %>% 
    mutate( Ext_m_km2= set_units(long_sbc,m)/units::set_units(Area_sub_ha,km2))
  
  
  # calculos nivel zHIDRO
  
  St_rios_zHidro <- x %>% 
    st_drop_geometry() %>% 
    group_by(.data[[zh_nom]],Descripcion_caudal,CategoriaT,cadena, cadena4,  Nom,Limitado.por.transporte.o.suministro,Attribute_index) %>% 
    summarise(long_zh= sum(longitud)
    ) %>% 
    ungroup %>% 
    left_join(st_drop_geometry(aoi_ZH))%>% 
  
    mutate( 
      prop_tot = prop.table(long_zh)*100,
      Ext_m_km2= set_units(long_zh,m)/units::set_units(Area_zh_ha, km2))
  
  # cálculos nivel orinoquía
  
  St_rios_cuenca <- x %>% 
    st_drop_geometry() %>% 
    group_by(Descripcion_caudal,CategoriaT,cadena, cadena4,  Nom,Limitado.por.transporte.o.suministro,Attribute_index) %>% 
    summarise(long_tipo_rios= sum(longitud)
    ) %>% 
    ungroup%>% 
    mutate( 
      prop_tot = prop.table(long_tipo_rios)*100,
      Ext_m_km2= set_units(long_tipo_rios,m)/units::set_units(aoi$Area_tot_ha, km2))
  
  # Cuenca total sin clases excluidas
  St_r_cuenca <- x %>%
    st_drop_geometry() %>%
    summarise(long_cuenca= sum(longitud)
    ) %>% 
    ungroup%>% 
    mutate( 
      
      Ext_m_km2= set_units(long_cuenca,m)/units::set_units(aoi$Area_tot_ha, km2))
  
  
  # Retornar una lista con las tablas
  contenedor <- list(
    subcuenca = St_rios_u_color,
    zhidro = St_rios_zHidro,
    cuenca = St_rios_cuenca,
    total = St_r_cuenca
  )
  
  return(contenedor)
}

# Aplicar la función 

Resultados_R <- ExtensionRios(x=rios_u_color,sbc = "subName", 
              zh_nom = "NOMSZH2" )


length(unique(Resultados_R$cuenca$cadena))


# Guardar cada conjunto de valores únicos en una hoja separada
write_xlsx(Resultados_R, file.path(dir_Resultados,"Ext_rios_caq.xlsx"))

### Gráficos extensión rios ####

#### Barras general ####
ggplot(Resultados_R$cuenca) +
  geom_bar(aes(x = as.numeric(Ext_m_km2), y =  fct_reorder(cadena, Ext_m_km2)), 
           stat = "identity",
           color = "black", linewidth = 0.3) +
  
  
  
  # Personalizar colores de los strips
  theme_minimal() +
  labs(y="",
       x= "Extensión (Densidad de drenaje)",
       fill="")+
  #scale_fill_manual(values= c("white","grey80","grey40"))+
  
  theme(  panel.border = element_rect(color = "grey80", fill = NA, size = 0.5),
          #axis.line = element_line(size = 0.3), # Grosor del eje igual en todas las facetas
          axis.text = element_text(size = 8),  # Tamaño de etiquetas
          axis.title.x = element_text(size = 9,  margin = margin(t= 8)), # Más legible
          strip.text =  element_text(size = 9, face="plain" ), # Más legible
          # panel.grid.major = element_line(color = "grey80", size = 0.3),  
          #panel.grid.minor = element_line(color = "grey90", size = 0.2)
  )


ggsave(file.path(dir_Resultados,"CAQ_Dlong_general.png"), width = 19, height =  25.4, units="cm",dpi=400)


#### Barras mutidimensional general ####

ggplot(Resultados_R$cuenca) +
  geom_bar(aes(x = as.numeric(Ext_m_km2), y = Descripcion_caudal, fill = Attribute_index, color = Attribute_index), 
           stat = "identity",
           , linewidth = 0.3) +
  
  # Usamos facet_nested para permitir colores diferentes en cada "Nom"
  facet_nested(Limitado.por.transporte.o.suministro ~ Nom + CategoriaT, 
               scales = "free_y", space = "free_y") +
  
  # Personalizar colores de los strips
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = "gray90", color = "black"),  # Base
    strip.text = element_text(size = 10, face = "bold"),
    #    strip.background.x = element_rect(fill = rep(colores, length.out = length(levels(St_rios_cuenca$Nom))))
    
  )+  labs(y="",
           x= "Extensión (Densidad de drenaje)",
           fill="")+
  #scale_fill_manual(values= c("white","grey80","grey40"))+
  
  theme(  panel.border = element_rect(color = "grey80", fill = NA, size = 0.5),
          #axis.line = element_line(size = 0.3), # Grosor del eje igual en todas las facetas
          axis.text = element_text(size = 8),  # Tamaño de etiquetas
          axis.title.x = element_text(size = 9,  margin = margin(t= 8)), # Más legible
          strip.text =  element_text(size = 9, face="plain" ), # Más legible
          # panel.grid.major = element_line(color = "grey80", size = 0.3),  
          #panel.grid.minor = element_line(color = "grey90", size = 0.2)
  )+
  guides(color="none")

ggsave(file.path(dir_Resultados,"CAQ_Dlong_general_disgregado.png"), width = 19, height =  12.5, units="cm",dpi=400)

#### Barras zh ####
# categorias frecuentes

Resultados_R$zhidro %>%
     # filter(!(cadena4 %in% c("Caudal alto * Limitado por suministro",
     #                         "Caudal medio * Limitado por suministro",
     #                         "Caudal muy alto * Limitado por suministro",
     #                         "Caudal muy alto * Limitado por transporte"))         
  #          #|
  #          #Zona_hidro %in% c("Inírida", "Vichada", "Tomo")) %>% 
  #   !(Zona_hidro %in% c("Inírida", "Vichada", "Tomo"))
 # ) %>% 
  
  ggplot(aes(y = .data[[zh_nom]] , x = as.numeric(Ext_m_km2),fill=Attribute_index)) +
  geom_bar( stat= "identity")+     # Transparencia de los outliers) +
  
  
  facet_nested( cadena4~ Nom + CategoriaT , 
               scales = "free_y", space = "free_y") +
  labs(y = "", x = "Extensión (Densidad de drenaje)", fill="") +
  
  #  scale_fill_manual(values= c("white","grey80","grey40"))+
  #  theme_minimal(base_family = "Arial")+
  theme_classic() +
  theme(  panel.border = element_rect(color = "grey40", fill = NA, size = 0.5),
          #axis.line = element_line(size = 0.3), # Grosor del eje igual en todas las facetas
          axis.line = element_blank(), # Grosor del eje igual en todas las facetas
          axis.ticks = element_line(color = "grey40", size = 0.3),
          axis.text.y = element_text(size = 8),  # Tamaño de etiquetas
          axis.text.x = element_text(size = 7),  # Tamaño de etiquetas
          axis.title.x = element_text(size = 8,  margin = margin(t= 8)), # Más legible
          strip.text = element_text(size = 7), # Facets más claros
          strip.background = element_rect(fill = "grey95", color = "grey40", size = 0.2),
          
          
          panel.grid.major = element_line(color = "grey80", size = 0.3),  
          panel.grid.minor = element_line(color = "grey90", size = 0.2))+
  
  
  
  theme(legend.position = "bottom")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

dev.size(units = "cm")

# dev.size()
# [1] 11.947917  7.427083

ggsave(file.path(dir_Resultados,"caq_big_Dlong_tipos_Zhidro.png"), width = 19.5, height =  25, units="cm",dpi=400) 


# categorias no frecuentes


Resultados_R$zhidro %>%
  filter(cadena4 %in% c("Caudal alto * Limitado por suministro",
                          "Caudal medio * Limitado por suministro",
                          "Caudal muy alto * Limitado por suministro",
                          "Caudal muy alto * Limitado por transporte")         
         #          #|
         #          #Zona_hidro %in% c("Inírida", "Vichada", "Tomo")) %>% 
         #   !(Zona_hidro %in% c("Inírida", "Vichada", "Tomo"))
  ) %>% 
  
  ggplot(aes(y = .data[[zh_nom]] , x = as.numeric(Ext_m_km2),fill=Attribute_index)) +
  geom_bar( stat= "identity")+     # Transparencia de los outliers) +
  
  
  facet_nested(cadena4 ~ Nom + CategoriaT, 
               scales = "free_y", space = "free_y",
               labeller = labeller(cadena4 = label_wrap_gen(width = 10), 
                                   Nom = label_value, 
                                   CategoriaT = label_value)) +
  labs(y = "", x = "Extensión (Densidad de drenaje)",fill="") +
  
  #  scale_fill_manual(values= c("white","grey80","grey40"))+
  #  theme_minimal(base_family = "Arial")+
  theme_classic() +
  theme(  panel.border = element_rect(color = "grey40", fill = NA, size = 0.5),
          #axis.line = element_line(size = 0.3), # Grosor del eje igual en todas las facetas
          axis.line = element_blank(), # Grosor del eje igual en todas las facetas
          axis.ticks = element_line(color = "grey40", size = 0.3),
          axis.text.y = element_text(size = 8),  # Tamaño de etiquetas
          axis.text.x = element_text(size = 7),  # Tamaño de etiquetas
          axis.title.x = element_text(size = 8,  margin = margin(t= 8)), # Más legible
          strip.text = element_text(size = 7), # Facets más claros
          strip.background = element_rect(fill = "grey95", color = "grey40", size = 0.2),
          
          
          panel.grid.major = element_line(color = "grey80", size = 0.3),  
          panel.grid.minor = element_line(color = "grey90", size = 0.2))+
  
  
  
  theme(legend.position = "bottom")
ggsave(file.path(dir_Resultados,"caq_short_Dlong_tipos_Zhidro.png"), width = 25, height =  19.5, units="cm",dpi=400) 

