#### script para construir los mapas de densidad de huevos, larvas y biovolumen


if(!require(readxl)) install.packages("readxl")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(dplyr)) install.packages("dplyr")
if(!require(patchwork)) install.packages("patchwork")

data<-readxl::read_excel("./data/raw/maps/Mapas_PELAGDEMER_Christian.xlsx", sheet="mapas_last")


###Mapas####

#Carga de las capas para el mapa
CPC<-sf::st_read("./data/gis/Mapas/GeoLayers.gpkg", layer="cuenca_pacifica") # read shapefile CPC
Paises<-sf::st_read("./data/gis/Mapas/GeoLayers.gpkg", layer="Continente") # read shapefile Countries
MPA<-sf::st_read("./data/gis/Mapas/GeoLayers.gpkg", layer="mpa2023_cpc") # read shapefile Countries


options(scipen=9999)


#Función para la construcción del mapa
Mapas_graficas_biovolumen <- function(data, titulo, subtitulo, leyenda) {
  
  # Definir categorías discretas de biovolumen
  niveles <- c("1–10", "10–100", "100–1000", "1000–10000", "10000–100000")
  
  data$BIOV_ZOO_Class <- cut(
    data$BIOV_ZOO,
    breaks = c(0, 10, 100, 1000, 10000, 100000),
    labels = niveles,
    include.lowest = TRUE,
    right = FALSE
  )
  
  data$BIOV_ZOO_Class <- factor(data$BIOV_ZOO_Class, levels = niveles)
  
  # Crear un dataset dummy con todas las clases pero coordenadas vacías
  dummy <- data.frame(
    LONGITUD = NA,
    LATITUD = NA,
    BIOV_ZOO_Class = factor(niveles, levels = niveles)
  )
  
  # Unir datos reales con los dummy
  data_plot <- rbind(data[, c("LONGITUD", "LATITUD", "BIOV_ZOO_Class")], dummy)
  
  ggplot() +
    geom_sf(data = CPC, color = "blue", linetype = 2, linewidth = 0.5, fill = "lightblue") +
    geom_sf(data = Paises, colour = "black", fill = "lightgrey") +
    geom_sf(data = MPA, color = "darkgreen", linetype = 1, linewidth = 0.5, fill = "lightblue") +
    
    geom_point(
      data = data_plot,
      aes(x = LONGITUD, y = LATITUD, size = BIOV_ZOO_Class, color = BIOV_ZOO_Class)
    ) +
    
    coord_sf(xlim = c(-80, -77), ylim = c(1, 8), expand = FALSE) +
    labs(
      title = titulo,
      subtitle = subtitulo,
      x = "Longitude",
      y = "Latitude",
      size = leyenda,
      color = leyenda
    ) +
    scale_x_continuous(
      name = "Longitude",
      breaks = seq(-80, -77, by = 2)   # cada 2 grados
    ) +
    scale_y_continuous(
      name = "Latitude",
      breaks = seq(1, 8, by = 2)       # cada 2 grados
    ) +
    labs(
      title = titulo,
      subtitle = subtitulo,
      size = leyenda,
      color = leyenda
    )+
  
  
    
    scale_size_manual(
      values = c("1–10" = 1,
                 "10–100" = 2,
                 "100–1000" = 3,
                 "1000–10000" = 4,
                 "10000–100000" = 5),
      drop = FALSE,
      guide = guide_legend(override.aes = list(shape = 16))
    ) +
    
    scale_color_manual( 
      values = c("1–10" = "#fee5d9",
                 "10–100" = "#fc9272",
                 "100–1000" = "#fb6a4a",
                 "1000–10000" = "#ef3b2c",
                 
                 "10000–100000" = "#cb181d"),
      drop = FALSE,
      guide = guide_legend(override.aes = list(shape = 16))
    ) +
    
    theme_bw() +
    theme(
      plot.title = element_text(size = 12, face = "italic", color = "black"),
      axis.title = element_text(face = "bold", color = "black")
    )
}



mapas_individuales <- function(crucero_fecha, fecha) {
  
  # Filtra los datos por crucero
  subconjunto <- subset(data, CRUCERO == crucero_fecha)
  
  # Convierte fecha a texto por si viene como número o factor
  fecha <- as.character(fecha)
  
  # Usa bquote para incluir el valor real de 'fecha' en la expresión
  mapa <- Mapas_graficas_biovolumen(
    subconjunto,
    bquote(bold(.(fecha))),   # título con fecha en negrita
    bquote(bold(" ")),        # separador
    expression(paste("Biomasa Vol. [ml·1000m"^{-3},"]"))
  )
  
  return(mapa)
}

pelagicos_1991_02_BIOV_ZOO <- mapas_individuales("PELAG9102", '1991-02')
pelagicos_1991_09_BIOV_ZOO <- mapas_individuales("PELAG9109", '1991-09')
pelagicos_1991_12_BIOV_ZOO <- mapas_individuales("PELAG9112", '1991-12')
pelagicos_1993_01_BIOV_ZOO <- mapas_individuales("PELAG9301", '1993-01')
pelagicos_1993_11_BIOV_ZOO <- mapas_individuales("PELAG9311", '1993-11')
pelagicos_1994_04_BIOV_ZOO <- mapas_individuales("PELAG9404", '1994-04')


# Primero define tu matriz de diseño
layout_matrix1 <- "
AABB
AABB
CCDD
CCDD
EEFF
EEFF
"

final_plot2 <- (
  pelagicos_1991_02_BIOV_ZOO +
    pelagicos_1991_09_BIOV_ZOO +
    pelagicos_1991_12_BIOV_ZOO +
    pelagicos_1993_01_BIOV_ZOO +
    pelagicos_1993_11_BIOV_ZOO +
    pelagicos_1994_04_BIOV_ZOO
) +
  patchwork::plot_layout(
    design = layout_matrix1,
    guides = "collect"
  ) +
  patchwork::plot_annotation(tag_levels = 'A') &
  theme(legend.position = "right")

final_plot2

# Mapas totales
png(filename="./outputs/mapas/Pelagicos_biovolumen_log_10_01.png", height = 28, width =  15, units = "cm", res = 300, pointsize = 12)
final_plot2
dev.off()


pelagicos_1994_07_BIOV_ZOO <- mapas_individuales("PELAG9407", '1994-07')
pelagicos_1994_12_BIOV_ZOO <- mapas_individuales("PELAG9412", '1994-12')
pelagicos_1995_06_BIOV_ZOO <- mapas_individuales("PELAG950607", '1995-06')
pelagicos_2008_12_BIOV_ZOO <- mapas_individuales("PELAG0812", '2008-12')
pelagicos_2009_12_BIOV_ZOO <- mapas_individuales("PELAG0912", '2009-12')
pelagicos_1996_05_BIOV_ZOO <- mapas_individuales("DEMER 9605", '1996-05')
pelagicos_1996_11_BIOV_ZOO <- mapas_individuales("DEMER 9611", '1996-11')

layout_matrix2 <- "
AABB
AABB
CCDD
CCDD
EEFF
EEFF
GGHH
GGHH
"


final_plot3 <- (
    pelagicos_1994_07_BIOV_ZOO +
    pelagicos_1994_12_BIOV_ZOO +
    pelagicos_1995_06_BIOV_ZOO +
    pelagicos_1996_05_BIOV_ZOO +
    pelagicos_1996_11_BIOV_ZOO+
    pelagicos_2008_12_BIOV_ZOO +
    pelagicos_2009_12_BIOV_ZOO 
    
) +
  patchwork::plot_layout(
    design = layout_matrix2,
    guides = "collect"
  ) +
  patchwork::plot_annotation(tag_levels = 'A') &
  theme(legend.position = "right")

final_plot3

# Mapas totales
png(filename="./outputs/mapas/Pelagicos_biovolumen_log_10_02.png", height = 30, width =  15, units = "cm", res = 300, pointsize = 12)
final_plot3
dev.off()



### Huevos ####



#Función para la construcción del mapa
Mapas_graficas_huevos <- function(data, titulo, subtitulo, leyenda) {
  
  # Definir categorías discretas de Huevos
  niveles <- c("1–10", "10–100", "100–1000", "1000–10000", "10000–100000")
  
  data$Huevos_Class <- cut(
    data$HUEVOS,
    breaks = c(0, 10, 100, 1000, 10000, 100000),
    labels = niveles,
    include.lowest = TRUE,
    right = FALSE
  )
  
  data$Huevos_Class <- factor(data$Huevos_Class, levels = niveles)
  
  # Crear un dataset dummy con todas las clases pero coordenadas vacías
  dummy <- data.frame(
    LONGITUD = NA,
    LATITUD = NA,
    Huevos_Class = factor(niveles, levels = niveles)
  )
  
  # Unir datos reales con los dummy
  data_plot <- rbind(data[, c("LONGITUD", "LATITUD", "Huevos_Class")], dummy)
  
  ggplot() +
    geom_sf(data = CPC, color = "blue", linetype = 2, linewidth = 0.5, fill = "lightblue") +
    geom_sf(data = Paises, colour = "black", fill = "lightgrey") +
    geom_sf(data = MPA, color = "darkgreen", linetype = 1, linewidth = 0.5, fill = "lightblue") +
    
    geom_point(
      data = data_plot,
      aes(x = LONGITUD, y = LATITUD, size = Huevos_Class, color = Huevos_Class)
    ) +
    
    coord_sf(xlim = c(-80, -77), ylim = c(1, 8), expand = FALSE) +
    labs(
      title = titulo,
      subtitle = subtitulo,
      x = "Longitude",
      y = "Latitude",
      size = leyenda,
      color = leyenda
    ) +
    scale_x_continuous(
      name = "Longitude",
      breaks = seq(-80, -77, by = 2)   # cada 2 grados
    ) +
    scale_y_continuous(
      name = "Latitude",
      breaks = seq(1, 8, by = 2)       # cada 2 grados
    ) +
    labs(
      title = titulo,
      subtitle = subtitulo,
      size = leyenda,
      color = leyenda
    )+
    
    scale_size_manual(
      values = c("1–10" = 2,
                 "10–100" = 3,
                 "100–1000" = 4,
                 "1000–10000" = 5,
                 "10000–100000" = 6),
      drop = FALSE,
      guide = guide_legend(override.aes = list(shape = 16))
    ) +
    
    scale_color_manual( 
      values = c("1–10" = "#f6e8c3",
                 "10–100" = "#dfc27d",
                 "100–1000" = "#bf812d",
                 "1000–10000" = "#8c510a",
                 "10000–100000" = "#543005"),
      drop = FALSE,
      guide = guide_legend(override.aes = list(shape = 16))
    ) +
    
    theme_bw() +
    theme(
      plot.title = element_text(size = 12, face = "italic", color = "black"),
      axis.title = element_text(face = "bold", color = "black")
    )
}



mapas_individuales <- function(crucero_fecha, fecha) {
  
  # Filtra los datos por crucero
  subconjunto <- subset(data, CRUCERO == crucero_fecha)
  
  # Convierte fecha a texto por si viene como número o factor
  fecha <- as.character(fecha)
  
  # Usa bquote para incluir el valor real de 'fecha' en la expresión
  mapa <- Mapas_graficas_huevos(
    subconjunto,
    bquote(bold(.(fecha))),   # título con fecha en negrita
    bquote(bold(" ")),        # separador
    expression(paste("Huevos.10m"^{-2}))
  )
  
  return(mapa)
}




pelagicos_1991_02_Huevos <- mapas_individuales("PELAG9102", '1991-02')
pelagicos_1991_09_Huevos <- mapas_individuales("PELAG9109", '1991-09')
pelagicos_1991_12_Huevos <- mapas_individuales("PELAG9112", '1991-12')
pelagicos_1993_01_Huevos <- mapas_individuales("PELAG9301", '1993-01')
pelagicos_1993_11_Huevos <- mapas_individuales("PELAG9311", '1993-11')
pelagicos_1994_04_Huevos <- mapas_individuales("PELAG9404", '1994-04')


# Primero define tu matriz de diseño
layout_matrix1 <- "
AABB
AABB
CCDD
CCDD
EEFF
EEFF
"


final_plot2 <- (
  pelagicos_1991_02_Huevos +
    pelagicos_1991_09_Huevos +
    pelagicos_1991_12_Huevos +
    pelagicos_1993_01_Huevos +
    pelagicos_1993_11_Huevos +
    pelagicos_1994_04_Huevos
) +
  patchwork::plot_layout(
    design = layout_matrix1,
    guides = "collect"
  ) +
  patchwork::plot_annotation(tag_levels = 'A') &
  theme(legend.position = "right")

final_plot2



# Mapas totales
png(filename="./outputs/mapas/Pelagicos_Huevos_log_10_01.png", height = 28, width =  15, units = "cm", res = 300, pointsize = 12)
final_plot2
dev.off()


pelagicos_1994_07_Huevos <- mapas_individuales("PELAG9407", '1994-07')
pelagicos_1994_12_Huevos <- mapas_individuales("PELAG9412", '1994-12')
pelagicos_1995_06_Huevos <- mapas_individuales("PELAG950607", '1995-06')
pelagicos_1996_05_Huevos <- mapas_individuales("DEMER 9605", '1996-05')
pelagicos_1996_11_Huevos <- mapas_individuales("DEMER 9611", '1996-11')
pelagicos_2008_12_Huevos <- mapas_individuales("PELAG0812", '2008-12')
pelagicos_2009_12_Huevos <- mapas_individuales("PELAG0912", '2009-12')


layout_matrix2 <- "
AABB
AABB
CCDD
CCDD
EEFF
EEFF
GGHH
GGHH
"

final_plot2 <- (
  pelagicos_1994_07_Huevos +
    pelagicos_1994_12_Huevos +
    pelagicos_1995_06_Huevos +
    pelagicos_1996_05_Huevos +
    pelagicos_1996_11_Huevos +
    pelagicos_2008_12_Huevos +
    pelagicos_2009_12_Huevos
) +
  patchwork::plot_layout(
    design = layout_matrix2,
    guides = "collect"
  ) +
  patchwork::plot_annotation(tag_levels = 'A') &
  theme(legend.position = "right")

final_plot2

# Mapas totales
png(filename="./outputs/mapas/Pelagicos_Huevos_log_10_02.png", height = 28, width =  15, units = "cm", res = 300, pointsize = 12)
final_plot2
dev.off()



#LARVAS####



#Función para la construcción del mapa
Mapas_graficas_larvas <- function(data, titulo, subtitulo, leyenda) {
  
  # Definir categorías discretas de larvas
  niveles <- c("1–10", "10–100", "100–1000", "1000–10000")
  
  data$larvas_Class <- cut(
    data$LARVAS,
    breaks = c(0, 10, 100, 1000, 10000),
    labels = niveles,
    include.lowest = TRUE,
    right = FALSE
  )
  
  data$larvas_Class <- factor(data$larvas_Class, levels = niveles)
  
  # Crear un dataset dummy con todas las clases pero coordenadas vacías
  dummy <- data.frame(
    LONGITUD = NA,
    LATITUD = NA,
    larvas_Class = factor(niveles, levels = niveles)
  )
  
  # Unir datos reales con los dummy
  data_plot <- rbind(data[, c("LONGITUD", "LATITUD", "larvas_Class")], dummy)
  
  ggplot() +
    geom_sf(data = CPC, color = "blue", linetype = 2, linewidth = 0.5, fill = "lightblue") +
    geom_sf(data = Paises, colour = "black", fill = "lightgrey") +
    geom_sf(data = MPA, color = "darkgreen", linetype = 1, linewidth = 0.5, fill = "lightblue") +
    
    geom_point(
      data = data_plot,
      aes(x = LONGITUD, y = LATITUD, size = larvas_Class, color = larvas_Class)
    ) +
    
    coord_sf(xlim = c(-80, -77), ylim = c(1, 8), expand = FALSE) +
    labs(
      title = titulo,
      subtitle = subtitulo,
      x = "Longitude",
      y = "Latitude",
      size = leyenda,
      color = leyenda
    ) +
    scale_x_continuous(
      name = "Longitude",
      breaks = seq(-80, -77, by = 2)   # cada 2 grados
    ) +
    scale_y_continuous(
      name = "Latitude",
      breaks = seq(1, 8, by = 2)       # cada 2 grados
    ) +
    labs(
      title = titulo,
      subtitle = subtitulo,
      size = leyenda,
      color = leyenda
    )+
    
    scale_size_manual(
      values = c("1–10" = 2,
                 "10–100" = 3,
                 "100–1000" = 4,
                 "1000–10000" = 5),
      drop = FALSE,
      guide = guide_legend(override.aes = list(shape = 16))
    ) +
    
    scale_color_manual( 
      values = c("1–10" = "#41b6c4",
                 "10–100" = "#1d91c0",
                 "100–1000" = "#225ea8",
                 "1000–10000" = "#253494"),
      drop = FALSE,
      guide = guide_legend(override.aes = list(shape = 16))
    ) +
    
    theme_bw() +
    theme(
      plot.title = element_text(size = 12, face = "italic", color = "black"),
      axis.title = element_text(face = "bold", color = "black")
    )
}



mapas_individuales <- function(crucero_fecha, fecha) {
  
  # Filtra los datos por crucero
  subconjunto <- subset(data, CRUCERO == crucero_fecha)
  
  # Convierte fecha a texto por si viene como número o factor
  fecha <- as.character(fecha)
  
  # Usa bquote para incluir el valor real de 'fecha' en la expresión
  mapa <- Mapas_graficas_larvas(
    subconjunto,
    bquote(bold(.(fecha))),   # título con fecha en negrita
    bquote(bold(" ")),        # separador
    expression(paste("Larvas.10m"^{-2}))
  )
  
  return(mapa)
}

pelagicos_1991_02_larvas <- mapas_individuales("PELAG9102", '1991-02')
pelagicos_1991_09_larvas <- mapas_individuales("PELAG9109", '1991-09')
pelagicos_1991_12_larvas <- mapas_individuales("PELAG9112", '1991-12')
pelagicos_1993_01_larvas <- mapas_individuales("PELAG9301", '1993-01')
pelagicos_1993_11_larvas <- mapas_individuales("PELAG9311", '1993-11')
pelagicos_1994_04_larvas <- mapas_individuales("PELAG9404", '1994-04')


# Primero define tu matriz de diseño
layout_matrix1 <- "
AABB
AABB
CCDD
CCDD
EEFF
EEFF
"


final_plot2 <- (
  pelagicos_1991_02_larvas +
    pelagicos_1991_09_larvas +
    pelagicos_1991_12_larvas +
    pelagicos_1993_01_larvas +
    pelagicos_1993_11_larvas +
    pelagicos_1994_04_larvas
) +
  patchwork::plot_layout(
    design = layout_matrix1,
    guides = "collect"
  ) +
  patchwork::plot_annotation(tag_levels = 'A') &
  theme(legend.position = "right")

final_plot2


# Mapas totales
png(filename="./outputs/mapas/Pelagicos_larvas_log_10_01.png", height = 28, width =  15, units = "cm", res = 300, pointsize = 12)
final_plot2
dev.off()


pelagicos_1994_07_larvas <- mapas_individuales("PELAG9407", '1994-07')
pelagicos_1994_12_larvas <- mapas_individuales("PELAG9412", '1994-12')
pelagicos_1995_06_larvas <- mapas_individuales("PELAG950607", '1995-06')
pelagicos_1996_05_larvas <- mapas_individuales("DEMER 9605", '1996-05')
pelagicos_1996_11_larvas <- mapas_individuales("DEMER 9611", '1996-11')
pelagicos_2008_12_larvas <- mapas_individuales("PELAG0812", '2008-12')
pelagicos_2009_12_larvas <- mapas_individuales("PELAG0912", '2009-12')


layout_matrix2 <- "
AABB
AABB
CCDD
CCDD
EEFF
EEFF
GGHH
GGHH
"

final_plot3 <- (
  pelagicos_1994_07_larvas +
    pelagicos_1994_12_larvas +
    pelagicos_1995_06_larvas +
    pelagicos_1996_05_larvas +
    pelagicos_1996_11_larvas +
    pelagicos_2008_12_larvas +
    pelagicos_2009_12_larvas
) +
  patchwork::plot_layout(
    design = layout_matrix2,
    guides = "collect"
  ) +
  patchwork::plot_annotation(tag_levels = 'A') &
  theme(legend.position = "right")

final_plot3

# Mapas totales
png(filename="./outputs/mapas/Pelagicos_larvas_log_10_02.png", height = 28, width =  15, units = "cm", res = 300, pointsize = 12)
final_plot3
dev.off()






