# Título: Representatividad de tipos de rios
#
# Autor(es): Alejandra Narváez Vallejo
#
# Descripción:
# Este script calcula la representatividad de los Tipos de ríos a diferentes escalas 
# espaciales (zonas hidrográficas y la Orinoquia en general),
# considerandoáreas bajo diversas figuras de conservación o manejo especial según la metodología descrita en la ficha técnica XXXXX.
# Se elaboran gráficas y tablas que resumen los resultados
# Al final, guarda las estadísticas como archivos Excel.


#
# Fuentes: 
# Depende de los objetos creados en el código CAQ_Tipologias y CAQ_apme

# 
# Por hacer o  corregir: 

# Nombre de la ficha técnica


#**********************************************************
# librerías o dependencias --------------------------------
#**********************************************************

# lectura de datos 

library(sf)
library(terra)
library(dplyr)
library(units)
library(ggplot2)
library(ggh4x)       # Para modificar los strips de las facetas (gráficas)
library(writexl)     # Para exportar resultados a Excel
library(purrr)       # Para operaciones funcionales
    


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
load(file=file.path (dir_Resultados,"rios_u_color_caq.RData"))

load(file=file.path (dir_Resultados,"caq_zonasEspecial_sf.RData"))

#**********************************************************
# Parametros globales ----------------------------
#**********************************************************
sf::sf_use_s2(F) # desactivar el uso de la biblioteca s2 para las operaciones geométricas esféricas. Esto optimiza algunos analisis de sf.

zh_nom <- "NOMSZH2" # Definir el nombre que tiene la columna de las zonas hidrográficas En el objeto aoi_zh

cat_caudal <- c("Caudal bajo",
                "Caudal medio",
                "Caudal alto",
                "Caudal muy alto")

#**********************************************************
# Procesos y estadísticas ---------------------------------
#**********************************************************

rios_u_color$Descripcion_caudal <- factor( rios_u_color$Descripcion_caudal,levels=cat_caudal)



## Representatividad por tipo De protección ####
#**********************************************************
#*
# # Solo para probar la función
# clasificacion <- rios_u_color
# zona <- runap_2012
# nzona <- "RUNAP"
# c_año <- 2012
# AOI_sub <- aoi_sub


# # Solo para hacer pruebas
# # 
# AOI_sub <- aoi_sub
# clasificacion <- rios_u_color
# zona <- lista_zonas [[1]]
# nzona <- Zonas_nombres[[1]]
# c_año<- años[[1]]
# zh_nom <- "NOMSZH2"


# Función: StsRepresentatividad
# Calcula estadísticas de representatividad por tipo de rio
# a nivel de zonas hidrográficas y tipo de protección




StsRepresentatividad <-  function(clasificacion,zona,nzona,c_año,AOI_sub){
  
  
  # Area total por tipo en zonas hidrográficas 
  ZH_tipo_com <- AOI_sub %>% 
    st_intersection(clasificacion) %>% 
    mutate(long_tipo_sub=set_units(st_length(geometry), km)) %>% 
    st_drop_geometry()   %>% 
    group_by(.data[[zh_nom]],cadena) %>% 
    summarise(long_tipo_Zh=sum(long_tipo_sub)) %>% 
    mutate(Zona=nzona, 
           Año=c_año)
  
  cat("1 ", nzona,c_año)
  
  # long total humedales en zonas hidrográficas ####
  
  ZH_hum_com <- ZH_tipo_com %>% 
    group_by(.data[[zh_nom]]) %>% 
    summarise(long_Zh_hum=sum(long_tipo_Zh),
              Zona=first(Zona),
                            Año= first(c_año))
  
  
  
  # Areá tipo  total cuenca
  
  tipo_cuenca_com <- ZH_tipo_com %>% 
    group_by(cadena) %>% 
    summarise(long_tipo=sum(long_tipo_Zh),
              Zona=first(Zona),
              Año= first(c_año))
  
  
  # long total humedales en cuenca ####
  
  Cuenca_hum_com <- tipo_cuenca_com%>% 
    summarise(long_Zh_hum=sum(long_tipo),
              Zona=first(Zona),
              Año= first(c_año))
  
  
  
  # long por tipo en Zhidro y  en zona de manejo especial ####
  
  # Cálculo de actividad de las zonas hidrográficas ####
  
  a <- clasificacion %>% 
    st_intersection(zona) %>% 
    mutate(Zona=nzona, 
      Año=c_año,
      long_tipo_sub_z=units::set_units(st_length(geometry),km)
      
    )
  
  # Zhidro tipo
  rep_zh <- a %>% 
    st_drop_geometry() %>% 
    group_by(.data[[zh_nom]],cadena) %>% 
    summarise(long_tipo_ZH_z=sum(long_tipo_sub_z)) %>% 
    ungroup %>% 
    left_join(ZH_tipo_com,by = join_by(!!sym(zh_nom), cadena)) %>% 
    mutate(Representatividad=long_tipo_ZH_z/long_tipo_Zh*100)
  
  
  # cuenca tipo
  rep_Cuenca <- rep_zh %>% 
    group_by(cadena) %>% 
    summarise(long_tipo_Z=sum(long_tipo_ZH_z)) %>% 
    ungroup %>% 
    left_join(tipo_cuenca_com,by = join_by( cadena)) %>% 
    mutate(Representatividad=long_tipo_Z/long_tipo*100)
  
  
  
  # Zhidro total humedales
  
  rep_Zh_hum <- a %>% 
    st_drop_geometry() %>% 
    group_by(.data[[zh_nom]]) %>% 
    summarise(long_hum_ZH_z = sum(long_tipo_sub_z)) %>% 
    ungroup %>% 
    left_join(ZH_hum_com,by = join_by(!!sym(zh_nom))) %>% 
    mutate(Representatividad=long_hum_ZH_z/long_Zh_hum*100)
  
  
  # cuenca total humedales
  
  
  rep_O_hum <-  rep_zh %>% 
    summarise(
      Zona=first(Zona),
      Año= first(c_año),long_Ori_hum_z=sum(long_tipo_ZH_z)) %>% 
    mutate( long_Or_hum_ha=Cuenca_hum_com$long_Zh_hum,  # aqui era long_Or_ha
            Representatividad=  long_Ori_hum_z  /long_Or_hum_ha*100)
  
  contenedor <- list(tipo_zh=ZH_tipo_com,
                     zh=ZH_hum_com,
                     tipo=tipo_cuenca_com,
                     
                     
                     r_tipo_zh=rep_zh,
                     r_tipo=rep_Cuenca,
                     r_zh=rep_Zh_hum,
                     r=rep_O_hum
  )
  return(contenedor)
  
}

### Ejecutar función sobre varias zonas y años --------------
lista_zonas <- list(runap_2012, rInd_2012,
                    runap_2018, rInd_2018,
                    runap24_n_cuencas, rInd_2024, rZRC_n_cuencas24, omec_n_cuencas)
Zonas_nombres <- list("RUNAP","RIndígena",
                      "RUNAP","RIndígena",
                      "RUNAP","RIndígena", "RCampesinas","OMEC")
clasificaciones <- rios_u_color
años <- list(2012,2012,2018,2018,2024,2024,2024,2024)



# Aplicar la función

resultados <- mapply(StsRepresentatividad,
                     #clasificacion = clasificaciones,
                     zona = lista_zonas,
                     nzona = Zonas_nombres,
                     c_año = años,
                     MoreArgs = list(AOI_sub = aoi_sub, clasificacion=clasificaciones),
                     SIMPLIFY = FALSE)


# Unir elementos equivalentes de las listas dentro de "resultados"
Resultados_DF <- map(seq_along(resultados[[1]]), function(i) {
  reduce(map(resultados, ~ .x[[i]]), bind_rows)
})


# Nombres para cada hoja en Excel
names(Resultados_DF) <- names(resultados[[1]])

# Guardar cada conjunto de valores únicos en una hoja separada
write_xlsx(Resultados_DF, file.path(dir_Resultados,"CAQ_Representatividad_rios.xlsx"))
# save(Extension17, clas_acu, file=file.path(dir_Resultados,"obj_17.RData"))



## Representatividad Total ####
#**********************************************************

# Función: StsRe_total
# Calcula estadísticas de representatividad por tipo de ecosistema
# a nivel de zonas hidrográficas y total de áreas de conservación


StsRe_total <-  function(clasificacion,zona,c_año, AOI_sub){
  
  # long total por tipo en zonas hidrográficas 
  ZH_tipo_com <- AOI_sub %>% 
    st_intersection(clasificacion) %>% 
    mutate(long_tipo_sub=units::set_units(st_length(geometry),km)) %>% 
    st_drop_geometry() %>% 
    group_by(.data[[zh_nom]],cadena) %>% 
    summarise(long_tipo_Zh=sum(long_tipo_sub)) %>% 
    mutate(
      Año=c_año)
  
  cat("1 ", c_año)
  
  # long total humedales en zonas hidrográficas 
  
  ZH_hum_com <- ZH_tipo_com%>% 
    group_by(.data[[zh_nom]]) %>% 
    summarise(long_Zh_hum=sum(long_tipo_Zh),
              
              Año= first(c_año))
  
  
  # Areá tipo  total orinoquía
  
  tipo_Cuenca_com <- ZH_tipo_com %>% 
    group_by(cadena) %>% 
    summarise(long_tipo=sum(long_tipo_Zh),
              
              Año= first(c_año))
  
  
  # long total humedales en Orinoco 
  
  Cuenca_hum_com <- tipo_Cuenca_com%>% 
    summarise(long_Zh_hum=sum(long_tipo),
              
              Año= first(c_año))
  
  
  # long por tipo en Zhidro y  en zona de manejo especial 
  
  # Cálculo de actividad de las zonas hidrográficas 
  
  a <- clasificacion %>% 
    st_intersection(zona) %>% 
    mutate(
      Año=c_año,
      long_tipo_sub_z=units::set_units(st_length(geometry),km)
      
    )
  
  # Zhidro tipo
  rep_zh <- a %>% 
    st_drop_geometry() %>% 
    group_by(.data[[zh_nom]],cadena) %>% 
    summarise(long_tipo_ZH_z=sum(long_tipo_sub_z)) %>% 
    ungroup %>% 
    left_join(ZH_tipo_com,by = join_by(!!sym(zh_nom), cadena)) %>% 
    mutate(Representatividad=long_tipo_ZH_z/long_tipo_Zh*100)
  
  
  # Cuenca tipo
  rep_Cuenca <- rep_zh %>% 
    group_by(cadena) %>% 
    summarise(long_tipo_Z=sum(long_tipo_ZH_z)) %>% 
    ungroup %>% 
    left_join(tipo_Cuenca_com,by = join_by( cadena)) %>% 
    mutate(Representatividad=long_tipo_Z/long_tipo*100)
  
  # Zhidro total humedales
  
  rep_Zh_hum <- a %>% 
    st_drop_geometry() %>% 
    group_by(.data[[zh_nom]]) %>% 
    summarise(long_hum_ZH_z = sum(long_tipo_sub_z)) %>% 
    ungroup %>% 
    left_join(ZH_hum_com,by = join_by(!!sym(zh_nom))) %>% 
    mutate(Representatividad=long_hum_ZH_z/long_Zh_hum*100)
  
  
  # Cuenca total humedales
  
  
  rep_O_hum <-  rep_zh %>% 
    summarise(
      Año= first(c_año),long_Ori_hum_z=sum(long_tipo_ZH_z)) %>% 
    mutate( long_Or_hum_ha=Cuenca_hum_com$long_Zh_hum,  # aqui era long_Or_ha
            Representatividad=  long_Ori_hum_z  /long_Or_hum_ha*100)
  
  
  
  
  
  contenedor <- list(tipo_zh=ZH_tipo_com,
                     zh=ZH_hum_com,
                     tipo=tipo_Cuenca_com,
                     
                     
                     r_tipo_zh=rep_zh,
                     r_tipo=rep_Cuenca,
                     r_zh=rep_Zh_hum,
                     r=rep_O_hum
  )
  return(contenedor)
  
}


#Correr la función para las zonas y las clasificaciones

lista_zonas <- list(tot_2012,tot_2018, tot_24)
clasificaciones <- rios_u_color
años <- list(2012,2018,2024)

# # Solo para hacer pruebas
clasificacion <- rios_u_color
zona<- lista_zonas[[1]]
c_año <- años[[1]]
AOI_sub <- aoi_sub


# Aplicar la función
res_tot <- mapply(StsRe_total,
                  
                  zona = lista_zonas,
                  c_año = años,
                  MoreArgs = list(AOI_sub = aoi_sub,clasificacion = clasificaciones),
                  SIMPLIFY = FALSE)



# Unir elementos equivalentes de las listas dentro de "resultados"
Res_tot_DF <- map(seq_along(res_tot[[1]]), function(i) {
  reduce(map(res_tot, ~ .x[[i]]), bind_rows)
})


# Nombres para cada hoja en Excel
names(Res_tot_DF) <- names(res_tot[[1]])

# Guardar cada conjunto de valores únicos en una hoja separada
write_xlsx(Res_tot_DF, file.path(dir_Resultados,"CAQ_Representatividad_rios_tot.xlsx"))
# save(Extension17, clas_acu, file=file.path(dir_Resultados,"obj_17.RData"))

## Grafico de representatividad en areas protegidas por tipo ####

St_rios_orinoquia <-  Res_tot_DF[[5]] %>%
  mutate(cadena0=cadena) %>% 
  separate(cadena, into = c("color", "Caudal", "Limite", "Confinado", "region"), sep = "\\*", fill = "right") %>% 
  mutate(cadena2 = paste(Caudal,Limite,sep="*"),
         cadena3 = paste(color,Limite,sep="*"),
         region= factor(region, levels=c("Alto en la Montaña", "Piedemonte", "Tierras bajas")),
         Caudal= factor( Caudal,levels=cat_caudal),
         cadena2 = factor( cadena2,levels=c( "Caudal bajo*Limitado por suministro",
                                             "Caudal bajo*Limitado por transporte",
                                             "Caudal medio*Limitado por suministro", 
                                             "Caudal medio*Limitado por transporte",
                                             "Caudal alto*Limitado por suministro",
                                             "Caudal alto*Limitado por transporte",
                                             "Caudal muy alto*Limitado por suministro",
                                             "Caudal muy alto*Limitado por transporte"))
)


# Gráfico con strips de colores personalizados
St_rios_orinoquia %>% 
  ggplot() +
  geom_bar(aes(x = as.numeric(Representatividad), y = cadena2, fill=Confinado), 
           stat = "identity",
           color = "black", linewidth = 0.3) +
  
  # Usamos facet_nested para permitir colores diferentes en cada "Nom"
  facet_nested(Año ~ region +Confinado+ color, 
               scales = "free_y", space = "free_y") +
  
  # Personalizar colores de los strips
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = "gray90", color = "black"),  # Base
    strip.text = element_text(size = 10, face = "bold"),
    #    strip.background.x = element_rect(fill = rep(colores, length.out = length(levels(St_rios_orinoquia$Nom))))
    
  )+  labs(y="",
           x= " Representatividad (%)",
           fill="")+
  #scale_fill_manual(values= c("white","grey80","grey40"))+
  
  theme(  panel.border = element_rect(color = "grey80", fill = NA, size = 0.5),
          #axis.line = element_line(size = 0.3), # Grosor del eje igual en todas las facetas
          axis.text = element_text(size = 7),  # Tamaño de etiquetas
          axis.title.x = element_text(size = 9,  margin = margin(t= 8)), # Más legible
          strip.text =  element_text(size = 8, face="plain" ), # Más legible
          # panel.grid.major = element_line(color = "grey80", size = 0.3),  
          #panel.grid.minor = element_line(color = "grey90", size = 0.2)
  )+
  scale_x_continuous(breaks=seq(0,100,50),expand = expansion(mult = c(0.1, 0.1)))



ggsave(file.path(dir_Resultados,"caq_rep_rios.png"), width = 25.5, height =  19, units="cm",dpi=400)


## Grafico de representaividad en areas protegidas G ####

df_tot <- Res_tot_DF[[7]] %>% 
  mutate(g="Total")

ggplot(Resultados_DF[[7]]) +
  geom_line( aes(x = Año, y = as.numeric(Representatividad), color = Zona) ,linetype=2)+
  geom_point( aes(x = Año, y = as.numeric(Representatividad), color = Zona),size = 2)+
  geom_point(data=df_tot,mapping=aes(x = Año, y = as.numeric(Representatividad), color=factor(g)),size=2)+
  geom_line(data=df_tot,mapping=aes(x = Año, y = as.numeric(Representatividad), color=factor(g)),size=1)+
  #facet_wrap(~Zona, scales = "free_y") +  # Escalas independientes para cada zona si es necesario
  labs(y = "Representatividad (%)", x = "", color = "") +
  scale_color_manual(values = c("RUNAP" = "#C77CFF", "RIndígena" =  "#00BFC4", "RCampesinas" = "#7CAE00", "OMEC"="#F8766D"  ,"Total" = "black")) +
  theme_minimal() +
  theme(legend.position = "bottom",  
        legend.text = element_text(size = 9),
        panel.border = element_rect(color = "grey80", fill = NA, size = 0.5),
        axis.text = element_text(size = 8),  
        axis.title.y = element_text(size = 9, margin = margin(r = 8), face="plain", color="grey25"), 
        # panel.grid.major.x = element_blank(),
        #panel.grid.minor.x = element_blank(),
        #strip.text = element_text(size = 8), 
        #strip.background = element_rect(fill = "grey95", color = "grey40", size = 0.2)
  ) +
  scale_x_continuous(breaks = seq(2012, 2024, by = 6))+
  guides(color = guide_legend(nrow = 2))

ggsave(file.path(dir_Resultados,"CAQ_rep_zonasEspeciales_rios.png"), width = 11, height =  10, units="cm",dpi=400)


## Grafico representativdad por area protegida 2024  ####

St_rios_orinoquia_Z <-  Resultados_DF[[5]] %>%
  mutate(cadena0=cadena) %>% 
  separate(cadena, into = c("color", "Caudal", "Limite", "Confinado", "region"), sep = "\\*", fill = "right") %>% 
  mutate(cadena2 = paste(Caudal,Limite,sep="*"),
         cadena3 = paste(color,Limite,sep="*"),
         Caudal= factor( Caudal,levels=cat_caudal),
         region= factor(region, levels=c("Alto en la Montaña", "Piedemonte", "Tierras bajas")))

St_rios_orinoquia_Z %>% 
  filter(Año == 2024) %>% 
  complete(Zona, Limite, Confinado, fill = list(Caudal = 0))%>%  # Asegurar categorías completas
  ggplot() +
  geom_bar(aes(x = as.numeric(Representatividad), fill = factor(Zona), y = Caudal), 
           stat = "identity",
           linewidth = 0.3,
           width=.8,
           position = position_dodge2(preserve = "single", padding = 0))+   # Asegurar ancho igual
  
  facet_nested(Limite ~ region + Confinado + color, 
               scales = "free_y", space = "free_y") +
  labs(y="",
       x= " Representatividad (%)",
       fill="")+
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = "gray90", color = "black"),
    #strip.text = element_text(size = 10, face = "bold"),
    panel.border = element_rect(color = "grey80", fill = NA, size = 0.5),
    axis.text = element_text(size = 7),
    axis.title.x = element_text(size = 9, margin = margin(t = 8)),
    strip.text = element_text(size = 8, face = "plain")
  ) +
  
  scale_x_continuous(breaks = seq(0, 100, 50), expand = expansion(mult = c(0.1, 0.1)))

ggsave(file.path(dir_Resultados,"caq_rep_proteccion_2024.png"), width = 25, height =  15, units="cm",dpi=400)

# mapas de zonas ####
plot(runap_2012$geometry)
plot(runap_2018$geometry)
plot(runap24_n_cuencas$geometry)



