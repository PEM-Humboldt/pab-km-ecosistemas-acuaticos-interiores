# Título: Representatividad de humedales
#
# Autor(es): Alejandra Narváez Vallejo
#
# Descripción:
# Este script calcula la representatividad de los ecosistemas de humedal
# a diferentes escalas espaciales (zonas hidrográficas y la CUENCA en general),
# considerando áreas bajo figuras de conservación o manejo especial según la metodología descrita en la ficha técnica XXXXX.
# Se elaboran gráficas y tablas que resumen los resultados
# Al final, guarda las estadísticas como archivos Excel.


#
# Fuentes: 
# Depende de los objetos creados en el código CAQ_Tipologias y CAQ_apme

# 
# Por hacer o  corregir: 

# Nombre de la ficha técnica

# 
# Por hacer o  corregir: 

## Me sobran bibliotecas?


#**********************************************************
# librerías o dependencias --------------------------------
#**********************************************************

# lectura de datos 

library(sf)
library(terra)
library(dplyr)
library(units)
library(ggplot2)
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
load(file=file.path (dir_Resultados,"humedales_caq.RData"))


load(file=file.path (dir_Resultados,"caq_zonasEspecial_sf.RData"))

#**********************************************************
# Parametros globales ----------------------------
#**********************************************************
sf::sf_use_s2(F) # desactivar el uso de la biblioteca s2 para las operaciones geométricas esféricas. Esto optimiza algunos analisis de sf.

zh_nom <- "NOMSZH2" # Definir el nombre que tiene la columna de las zonas hidrográficas En el objeto aoi_zh

#**********************************************************
# Procesos y estadísticas ---------------------------------
#**********************************************************


## Representatividad por tipo De protección ####
#**********************************************************

# Función: StsRepresentatividad
# Calcula estadísticas de representatividad por tipo de ecosistema
# a nivel de zonas hidrográficas y tipo de protección



StsRepresentatividad <-  function(AOI_sub,clasificacion,zona,nzona,c_año){
  
  
  # Area total por tipo en zonas hidrográficas 
  ZH_tipo_com <- AOI_sub %>% 
    st_intersection(clasificacion) %>% 
    mutate(Area_tipo_sub=units::set_units(st_area(geometry),ha)) %>% 
    st_drop_geometry()   %>% 
    group_by(.data[[zh_nom]],ecos_gener) %>% 
    summarise(Area_tipo_Zh=sum(Area_tipo_sub)) %>% 
    mutate(Zona=nzona, 
           Año=c_año)
  
  cat("1 ", nzona,c_año)
  
  # Area total humedales en zonas hidrográficas ####
  
  ZH_hum_com <- ZH_tipo_com %>% 
    filter(!ecos_gener %in% c("Cuerpo de Agua Artificial", "Transicional Transformado")) %>% 
    group_by(.data[[zh_nom]]) %>% 
    summarise(Area_Zh_hum=sum(Area_tipo_Zh),
              Zona=first(Zona),
              Año= first(c_año))
  
  
  # Areá tipo  total cuenca
  
  tipo_Cuenca_com <- ZH_tipo_com %>% 
    group_by(ecos_gener) %>% 
    summarise(Area_tipo=sum(Area_tipo_Zh),
              Zona=first(Zona),
              Año= first(c_año))
  
  
  # Area total humedales en CUENCA ####
  
  Cuenca_hum_com <- tipo_Cuenca_com%>% 
    filter(!ecos_gener %in% c("Cuerpo de Agua Artificial", "Transicional Transformado")) %>% 
    summarise(Area_Zh_hum=sum(Area_tipo),
              Zona=first(Zona),
              Año= first(c_año))
  
  
  # Area por tipo en Zhidro y  en zona de manejo especial ####
  
  # Cálculo de Representatividad de las zonas hidrográficas ####
  
  a <- clasificacion %>% 
    st_intersection(zona) %>% 
    mutate(Zona=nzona, 
           Año=c_año,
           Area_tipo_sub_z=units::set_units(st_area(geometry),ha)
    )
  
  # Zhidro tipo
  rep_zh <- a %>% 
    st_drop_geometry() %>% 
    group_by(.data[[zh_nom]],ecos_gener)  %>% 
    summarise(Area_tipo_ZH_z=sum(Area_tipo_sub_z)) %>% 
    ungroup %>% 
    dplyr::left_join(ZH_tipo_com,by = join_by(!!sym(zh_nom), ecos_gener)) %>% 
    mutate(Representatividad=Area_tipo_ZH_z/Area_tipo_Zh*100)
  
  
  # CUENCA tipo
  rep_Cuenca <- rep_zh %>% 
    group_by(ecos_gener) %>% 
    summarise(Area_tipo_Z=sum(Area_tipo_ZH_z)) %>% 
    ungroup %>% 
    left_join(tipo_Cuenca_com,by = join_by( ecos_gener)) %>% 
    mutate(Representatividad=Area_tipo_Z/Area_tipo*100)
  
  
  
  # Zhidro total humedales
  
  rep_Zh_hum <- a %>% 
    st_drop_geometry() %>%
    filter(!ecos_gener %in% c("Cuerpo de Agua Artificial", "Transicional Transformado")) %>% 
    group_by(.data[[zh_nom]]) %>% 
    summarise(Area_hum_ZH_z = sum(Area_tipo_sub_z)) %>% 
    ungroup %>% 
    left_join(ZH_hum_com,by = join_by(!!sym(zh_nom))) %>% 
    mutate(Representatividad=Area_hum_ZH_z/Area_Zh_hum*100)
  
  
  # CUENCA total humedales
  
  
  rep_O_hum <-  rep_zh %>% 
    filter(!ecos_gener %in% c("Cuerpo de Agua Artificial", "Transicional Transformado")) %>% 
    summarise(
      Zona=first(Zona),
      Año= first(c_año),Area_Ori_hum_z=sum(Area_tipo_ZH_z)) %>% 
    mutate( Area_Or_hum_ha=Cuenca_hum_com$Area_Zh_hum, #Area_Or_ha antes
            Representatividad=  Area_Ori_hum_z  /Area_Or_hum_ha*100)
  
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

### Ejecutar función sobre varias zonas y años --------------
lista_zonas <- list(runap_2012, rInd_2012,
                    runap_2018, rInd_2018,
                    runap24_n_cuencas, rInd_2024, rZRC_n_cuencas24, omec_n_cuencas)
Zonas_nombres <- list("RUNAP","RIndígena",
                      "RUNAP","RIndígena",
                      "RUNAP","RIndígena", "RCampesinas","OMEC")
clasificaciones <- list(sub_acu17,sub_acu17,
                        sub_acu24, sub_acu24,
                        sub_acu24, sub_acu24, sub_acu24, sub_acu24)
años <- list(2012,2012,2018,2018,2024,2024,2024,2024)


# # Solo para hacer pruebas
# 
# AOI_sub <- aoi_sub
# clasificacion <- clasificaciones[[1]]
# 
# zona <- lista_zonas [[1]]
# nzona <- Zonas_nombres[[1]]
# c_año<- años[[1]]
# zh_nom <- "NOMSZH2"

# Aplicar la función

resultados <- mapply(StsRepresentatividad,
                     clasificacion = clasificaciones,
                     zona = lista_zonas,
                     nzona = Zonas_nombres,
                     c_año = años,
                     MoreArgs = list(AOI_sub = aoi_sub),
                     SIMPLIFY = FALSE)




# Unir elementos equivalentes de las listas dentro de "resultados"
Resultados_DF <- map(seq_along(resultados[[1]]), function(i) {
  reduce(map(resultados, ~ .x[[i]]), bind_rows)
})


# Nombres para cada hoja en Excel
names(Resultados_DF) <- names(resultados[[1]])

# Guardar cada conjunto de valores únicos en una hoja separada
write_xlsx(Resultados_DF, file.path(dir_Resultados,"CAQ_Representatividad.xlsx"))
# save(Extension17, clas_acu, file=file.path(dir_Resultados,"obj_17.RData"))



## Representatividad Total ####
#**********************************************************

# Función: StsRe_total
# Calcula estadísticas de representatividad por tipo de ecosistema
# a nivel de zonas hidrográficas y total de áreas de conservación


StsRe_total <-  function(clasificacion,zona,c_año, AOI_sub){
  
  
  # Area total por tipo en zonas hidrográficas 
  ZH_tipo_com <- AOI_sub %>% 
    st_intersection(clasificacion) %>% 
    mutate(Area_tipo_sub=units::set_units(st_area(geometry),ha)) %>% 
    st_drop_geometry() %>% 
    group_by(.data[[zh_nom]],ecos_gener) %>% 
    summarise(Area_tipo_Zh=sum(Area_tipo_sub)) %>% 
    mutate(
      Año=c_año)
  
  cat("1 ", c_año)
  
  # Area total humedales en zonas hidrográficas ####
  
  ZH_hum_com <- ZH_tipo_com%>% 
    filter(!ecos_gener %in% c("Cuerpo de Agua Artificial", "Transicional Transformado")) %>% 
    group_by(.data[[zh_nom]]) %>% 
    summarise(Area_Zh_hum=sum(Area_tipo_Zh),
              
              Año= first(c_año))
  
  
  # Areá tipo  total CUENCA
  
  tipo_Cuenca_com <- ZH_tipo_com %>% 
    group_by(ecos_gener) %>% 
    summarise(Area_tipo=sum(Area_tipo_Zh),
              
              Año= first(c_año))
  
  
  # Area total humedales en CUENCA ####
  
  Cuenca_hum_com <- tipo_Cuenca_com%>% 
    filter(!ecos_gener %in% c("Cuerpo de Agua Artificial", "Transicional Transformado")) %>% 
    summarise(Area_Zh_hum=sum(Area_tipo),
              
              Año= first(c_año))
  
  
  # Area por tipo en Zhidro y  en zona de manejo especial ####
  
  # Cálculo de actividad de las zonas hidrográficas ####
  
  a <- clasificacion %>% 
    st_intersection(zona) %>% 
    mutate(
      Año=c_año,
      Area_tipo_sub_z=units::set_units(st_area(geometry),ha)
      
    )
  
  # Zhidro tipo
  rep_zh <- a %>% 
    st_drop_geometry() %>% 
    group_by(.data[[zh_nom]],ecos_gener) %>% 
    summarise(Area_tipo_ZH_z=sum(Area_tipo_sub_z)) %>% 
    ungroup %>% 
    left_join(ZH_tipo_com,by = join_by(!!sym(zh_nom), ecos_gener)) %>% 
    mutate(Representatividad=Area_tipo_ZH_z/Area_tipo_Zh*100)
  
  
  # CUENCA tipo
  rep_Cuenca <- rep_zh %>% 
    group_by(ecos_gener) %>% 
    summarise(Area_tipo_Z=sum(Area_tipo_ZH_z)) %>% 
    ungroup %>% 
    left_join(tipo_Cuenca_com,by = join_by( ecos_gener)) %>% 
    mutate(Representatividad=Area_tipo_Z/Area_tipo*100)
  
  
  
  # Zhidro total humedales
  
  rep_Zh_hum <- a %>% 
    filter(!ecos_gener %in% c("Cuerpo de Agua Artificial", "Transicional Transformado")) %>% 
    st_drop_geometry() %>% 
    group_by(.data[[zh_nom]]) %>% 
    summarise(Area_hum_ZH_z = sum(Area_tipo_sub_z)) %>% 
    ungroup %>% 
    left_join(ZH_hum_com,by = join_by(!!sym(zh_nom))) %>% 
    mutate(Representatividad=Area_hum_ZH_z/Area_Zh_hum*100)
  
  
  # CUENCA total humedales
  
  
  rep_O_hum <-  rep_zh %>% 
    filter(!ecos_gener %in% c("Cuerpo de Agua Artificial", "Transicional Transformado")) %>% 
    summarise(
      Año= first(c_año),Area_Ori_hum_z=sum(Area_tipo_ZH_z)) %>% 
    mutate( Area_Or_hum_ha=Cuenca_hum_com$Area_Zh_hum,  # aqui era Area_Or_ha
            Representatividad=  Area_Ori_hum_z  /Area_Or_hum_ha*100)
  
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
clasificaciones <- list(sub_acu17,sub_acu24, sub_acu24)
años <- list(2012,2018,2024)

# # Solo para hacer pruebas
# clasificacion <- clasificaciones[[1]]
# zona<- lista_zonas[[1]]
# c_año <- años[[1]]
# AOI_sub <- aoi_sub


# Aplicar la función
res_tot <- mapply(StsRe_total,
                     clasificacion = clasificaciones,
                     zona = lista_zonas,
                     c_año = años,
                     MoreArgs = list(AOI_sub = aoi_sub),
                     SIMPLIFY = FALSE)






# Unir elementos equivalentes de las listas dentro de "resultados"
Res_tot_DF <- map(seq_along(res_tot[[1]]), function(i) {
  reduce(map(res_tot, ~ .x[[i]]), bind_rows)
})


# Nombres para cada hoja en Excel
names(Res_tot_DF) <- names(res_tot[[1]])

# Guardar cada conjunto de valores únicos en una hoja separada
write_xlsx(Res_tot_DF, file.path(dir_Resultados,"CAQ_Representatividad_tot.xlsx"))
# save(Extension17, clas_acu, file=file.path(dir_Resultados,"obj_17.RData"))


# Gráficas de humedales separadas. ####
## Gráfica general CUENCA ####

Resultados_DF[[5]] %>% 
  select(ecos_gener,Año,Zona,Representatividad) %>% 
  complete(ecos_gener,Año,Zona) %>% 
  
  
  #filter(Año==2024) %>% 
  ggplot()+
  geom_bar(aes(x= as.numeric( Representatividad), y= ecos_gener, fill=factor(Año)), stat= "identity",
           position = position_dodge(width = .8)
           #position="dodge"
  )+
  #facet_grid(cols=vars(Zona))+
  facet_grid(cols=vars(Zona))+
  labs(y="",
       x= "Representatividad (%)",
       fill="")+
  theme_minimal()+
  theme(legend.position = "bottom",  
        panel.border = element_rect(color = "grey80", fill = NA, size = 0.5),
        #axis.line = element_line(size = 0.3), # Grosor del eje igual en todas las facetas
        axis.text = element_text(size = 8),  # Tamaño de etiquetas
        axis.title.x = element_text(size = 8,  margin = margin(t= 8)), # Más legible
        panel.grid.major = element_line(color = "grey80", size = 0.3),  
        panel.grid.minor = element_line(color = "grey90", size = 0.2),
        strip.text = element_text(size = 8), # Facets más claros
        strip.background = element_rect(fill = "grey95", color = "grey40", size = 0.2))+
  scale_x_continuous(breaks = seq(0, 100, by = 20)) +
  scale_y_discrete(drop = FALSE)

ggsave(file.path(dir_Resultados,"caq_rep_tipos_zonas1.png"), width = 19, height =  12, units="cm",dpi=400)

## tipo_zh_zp_2024 ####

Resultados_DF[[4]] %>% 
  select(.data[[zh_nom]], ecos_gener, Año, Zona, Representatividad) %>% 
  filter(Año == 2024) %>% 
  complete(.data[[zh_nom]], ecos_gener, Año, Zona) %>% 
  ggplot() +
  geom_bar(aes(y = ecos_gener, x = as.numeric(Representatividad), fill = factor(Zona)), 
           size = .3, stat = "identity", 
           position = position_dodge(width = .8)) +
  facet_wrap(
    vars(.data[[zh_nom]]),
    nrow = 2,
    labeller = as_labeller(label_wrap_gen(width = 15)) # 
  ) +
  labs(y = "", x = "Representatividad (%)", fill = "") +
  theme_classic() +
  theme(
    panel.border = element_rect(color = "grey40", fill = NA, size = 0.5),
    axis.line = element_blank(),
    axis.ticks = element_line(color = "grey40", size = 0.3),
    axis.text = element_text(size = 8),
    axis.title.x = element_text(size = 10, margin = margin(t = 8), color = "grey25"),
    strip.text = element_text(size = 7),
    strip.background = element_rect(fill = "grey95", color = "grey40", size = 0.2),
    panel.grid.major = element_line(color = "grey80", size = 0.3),
    panel.grid.minor = element_line(color = "grey90", size = 0.2),
    legend.position = "bottom"
#    axis.text.x = element_text(angle = 45, hjust = 1)
  )


ggsave(file.path(dir_Resultados, "caq_Zhidro_tipos_Rep_20241.png"), width = 30, height = 19, units = "cm", dpi = 400)


## tipo_zh_zp_2024_2 ####

Resultados_DF[[4]] %>% 
  select(.data[[zh_nom]],ecos_gener,Año,Zona,Representatividad) %>% 
  filter(Año==2024) %>% 
  complete(.data[[zh_nom]],ecos_gener,Año,Zona) %>% 
  
  ggplot() +
  geom_bar(aes(y = .data[[zh_nom]], x = as.numeric(Representatividad), fill =factor(Zona)), 
           size=.3,stat="identity", 
           position = position_dodge(width = .8)
           
           # ,position = "dodge"
           
  )+    # Transparencia de los outliers) +
  facet_wrap(vars(ecos_gener), nrow = 2,labeller = labeller(ecos_gener = label_wrap_gen(width = 20))) +

  labs(y = "", x = "Representatividad (%)",
       fill="") +
  
  #scale_fill_manual(values = colores_distintos) +
  #  scale_fill_manual(values = hue_colores)+
  #  theme_minimal(base_family = "Arial")+
  theme_classic()+
  theme(  panel.border = element_rect(color = "grey40", fill = NA, size = 0.5),
          #axis.line = element_line(size = 0.3), # Grosor del eje igual en todas las facetas
          axis.line = element_blank(), # Grosor del eje igual en todas las facetas
          axis.ticks = element_line(color = "grey40", size = 0.3),
          axis.text = element_text(size = 8),  # Tamaño de etiquetas
          axis.title.x = element_text(size = 10,  margin = margin(t= 8),color="grey25"), # Más legible
          strip.text = element_text(size = 8), # Facets más claros
          strip.background = element_rect(fill = "grey95", color = "grey40", size = 0.2),
          
          
          panel.grid.major = element_line(color = "grey80", size = 0.3),  
          panel.grid.minor = element_line(color = "grey90", size = 0.2))+
  
  
  
  theme(legend.position = "bottom")



ggsave(file.path(dir_Resultados, "caq_Zhidro_tipos_Rep_20242.png"), width = 25, height = 19, units = "cm", dpi = 400)


## general 2024 zh tipos ####

Res_tot_DF[[4]] %>% 
  select(.data[[zh_nom]],ecos_gener,Año,Representatividad) %>% 
  filter(Año==2024) %>% 
  complete(.data[[zh_nom]],ecos_gener,Año) %>% 
  
  ggplot() +
  geom_bar(aes(y = ecos_gener, x = as.numeric(Representatividad)), 
           size=.3,stat="identity", 
           position = position_dodge(width = .8)
           
           # ,position = "dodge"
           
  )+    # Transparencia de los outliers) +
  facet_wrap(vars(.data[[zh_nom]]), nrow = 2,
             labeller = as_labeller(label_wrap_gen(width = 15))) +
  labs(y = "", x = "Representatividad (%)",
       fill="") +
  
  #scale_fill_manual(values = colores_distintos) +
  #  scale_fill_manual(values = hue_colores)+
  #  theme_minimal(base_family = "Arial")+
  theme_classic()+
  theme(  panel.border = element_rect(color = "grey40", fill = NA, size = 0.5),
          #axis.line = element_line(size = 0.3), # Grosor del eje igual en todas las facetas
          axis.line = element_blank(), # Grosor del eje igual en todas las facetas
          axis.ticks = element_line(color = "grey40", size = 0.3),
          axis.text = element_text(size = 8),  # Tamaño de etiquetas
          axis.title.x = element_text(size = 10,  margin = margin(t= 8),color="grey25"), # Más legible
          strip.text = element_text(size = 8), # Facets más claros
          strip.background = element_rect(fill = "grey95", color = "grey40", size = 0.2),
          
          
          panel.grid.major = element_line(color = "grey80", size = 0.3),  
          panel.grid.minor = element_line(color = "grey90", size = 0.2))+
  
  
  
  theme(legend.position = "bottom")



ggsave(file.path(dir_Resultados, "caq_Zhidro_tipos_Rep_2024tot.png"), width = 30, height = 19, units = "cm", dpi = 400)


## general CUENCA en tiempo ####

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

ggsave(file.path(dir_Resultados,"Caq_rep_zonasEspeciales.png"), width = 11, height =  10, units="cm",dpi=400)





















