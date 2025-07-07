Indicadores de ecosistemas de agua dulce interiores
================

Este repositorio tiene los paso y datos para hacer el cálculo de la huella espacial humana a nivel nacional. 


## Organizar directorio de trabajo

Los datos para correr los códigos están almacenados
[aquí](https://drive.google.com/file/d/1YQjFb3u8uJ7UmWHlNncM_UXtJ_gJcOmz/view?usp=drive_link).
Una vez descargada y descomprimida la carpeta, reemplaze la carpeta “Datos” en el directorio Datos del proyecto.
El directorio del proyecto está organizado de la siguiente manera.

    Codigos
    │-  00_InsumosGenerales.R
    │-  01_Insumosxhuella.R
    │-  02_HuellaHumana_Diaz.R
    │-  03_HuellaHumana_adaptada.R
    │-  04_Comparar entre versiones.R
    │    
    └-Datos
    │ │
    │ └- replaze aquí los datos que  descargue 
    │ 
    |
    └- Res_Intermedios
    |
    └- Resultados

## Datos

- Microcuencas y  Subcuencas (TNC, 2024a)
- Mapas de ecosistemas continentales, marinos y costeros (MEC) a escala 1:100.000 del IDEAM para los años 2017 y 2024 (IDEAM, 2017 y 2024)
- Red hídrica (TNC, 2024a) derivada de un modelo de elevación digital corregido a una resolución de 90 m.
- Los atributos ecológicos clave (TNC, 2024a)
- [Mapa de ecorregiones, Abell et al. (2008)]( https://feow.org/ ) 
- Resguardos indígenas y zonas de reserva campesina (Agencia Nacional de Tierras, 2025)
- Otras Medidas de Conservación Basadas en Áreas, OMEC ( UNEP-WCMC, 2025).
- Áreas protegidas del Registro Único Nacional de Áreas Protegidas (RUNAP) correspondientes a los años 2019 y 2025 (Parques Nacionales Naturales, 2019,2025).

## Bibliografía
- Abell, R., Thieme, M. L., Revenga, C., Bryer, M., Kottelat, M., Bogutskaya, N., ... & Petry, P. 2008. Freshwater ecoregions of the world: A new map of biogeographic units for freshwater biodiversity conservation. BioScience, 58(5), 403-414.
- Agencia Nacional de Tierras. (2025). Resguardo Indígena formalizado y Zonas de Reserva Campesina constituidas. Recuperado de https://data-agenciadetierras.opendata.arcgis.com/datasets
- Instituto de Hidrología, Meteorología y Estudios Ambientales (IDEAM). 2018. Ecosistemas continentales, costeros y marinos de Colombia.
- Instituto de Hidrología, Meteorología y Estudios Ambientales (IDEAM). 2024. Ecosistemas continentales, costeros y marinos de Colombia.
- Instituto de Hidrología, Meteorología y Estudios Ambientales (IDEAM). (2022). Zonificación Hidrográfica 2022.
- Parques Nacionales Naturales de Colombia (RUNAP). 2019. Registro Único Nacional de Áreas Protegidas (RUNAP). Recuperado de https://runap.parquesnacionales.gov.co/ [29-05-2019 y 28-02-2025]
- The Nature Conservancy (TNC). (2024). Caracterización de atributos ecológicos clave e índice de sostenibilidad para la conservación de la cuenca del Orinoco en Colombia. Tomo I: Índice de sostenibilidad de cuencas y áreas prioritarias para la conservación. Accelerating Impact Funds.
- The Nature Conservancy (TNC). (2024a). Portafolio de conservación para ecosistemas de agua dulce en la cuenca del río caquetá-Colombia.  Direccionales Caquetá. 

