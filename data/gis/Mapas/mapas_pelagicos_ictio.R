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
                 "10–100" = 1,
                 "100–1000" = 1,
                 "1000–10000" = 1,
                 "10000–100000" = 2),
      drop = FALSE,
      guide = guide_legend(override.aes = list(shape = 16))
    ) +
    
    scale_color_manual( 
      values = c("1–10" = "#2d004b",
                 "10–100" = "#8073ac",
                 "100–1000" = "#fee08b",
                 "1000–10000" = "#f46d43",
                 
                 "10000–100000" = "#9e0142"),
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

#Biomasa####

pelagicos_1991_02_BIOV_ZOO <- mapas_individuales("PELAG9102", '1991-02')
pelagicos_1991_09_BIOV_ZOO <- mapas_individuales("PELAG9109", '1991-09')
pelagicos_1991_12_BIOV_ZOO <- mapas_individuales("PELAG9112", '1991-12')

pelagicos_1993_01_BIOV_ZOO <- mapas_individuales("PELAG9301", '1993-01')
pelagicos_1993_11_BIOV_ZOO <- mapas_individuales("PELAG9311", '1993-11')

pelagicos_1994_04_BIOV_ZOO <- mapas_individuales("PELAG9404", '1994-04')
pelagicos_1994_07_BIOV_ZOO <- mapas_individuales("PELAG9407", '1994-07')
pelagicos_1994_12_BIOV_ZOO <- mapas_individuales("PELAG9412", '1994-12')

pelagicos_1995_06_BIOV_ZOO <- mapas_individuales("PELAG950607", '1995-06')

pelagicos_1996_05_BIOV_ZOO <- mapas_individuales("DEMER 9605", '1996-05')
pelagicos_1996_11_BIOV_ZOO <- mapas_individuales("DEMER 9611", '1996-11')

pelagicos_2008_12_BIOV_ZOO <- mapas_individuales("PELAG0812", '2008-12')
pelagicos_2009_12_BIOV_ZOO <- mapas_individuales("PELAG0912", '2009-12')


# Biomasa 1991 


layout_matrix1991 <- "
AABB
AABB
CCDD
CCDD
"

plot1991<- (
  pelagicos_1991_02_BIOV_ZOO +
    pelagicos_1991_09_BIOV_ZOO +
    pelagicos_1991_12_BIOV_ZOO 
) +
  patchwork::plot_layout(
    design = layout_matrix1991,
    guides = "collect"
  ) +
  patchwork::plot_annotation(tag_levels = 'A') &
  theme(legend.position = "right")

png(filename="./outputs/mapas/Pelagicos_biovolumen_plot1991.png", height = 18, width =  15, units = "cm", res = 150, pointsize = 12)
plot1991
dev.off()

# Biomasa 1993

layout_matrix1993 <- "
AABB
AABB
"

plot1993<- (
  pelagicos_1993_01_BIOV_ZOO +
    pelagicos_1993_11_BIOV_ZOO 
) +
  patchwork::plot_layout(
    design = layout_matrix1993,
    guides = "collect"
  ) +
  patchwork::plot_annotation(tag_levels = 'A') &
  theme(legend.position = "right")

png(filename="./outputs/mapas/Pelagicos_biovolumen_plot1993.png", height = 10, width =  15, units = "cm", res = 150, pointsize = 12)
plot1993
dev.off()

# Biomasa 1994

layout_matrix1994 <- "
AABB
AABB
CCDD
CCDD
"

plot1994<- (
  pelagicos_1994_04_BIOV_ZOO +
    pelagicos_1994_07_BIOV_ZOO +
    pelagicos_1994_12_BIOV_ZOO
) +
  patchwork::plot_layout(
    design = layout_matrix1994,
    guides = "collect"
  ) +
  patchwork::plot_annotation(tag_levels = 'A') &
  theme(legend.position = "right")

png(filename="./outputs/mapas/Pelagicos_biovolumen_plot1994.png", height = 18, width =  15, units = "cm", res = 150, pointsize = 12)
plot1994
dev.off()

# Biomasa 1995


png(filename="./outputs/mapas/Pelagicos_biovolumen_plot1995.png", height = 10, width =  12, units = "cm", res = 150, pointsize = 12)
pelagicos_1995_06_BIOV_ZOO
dev.off()


# Biomasa 1996

layout_matrix1996 <- "
AABB
AABB
"

plot1996<- (
  pelagicos_1996_05_BIOV_ZOO +
    pelagicos_1996_11_BIOV_ZOO 
) +
  patchwork::plot_layout(
    design = layout_matrix1996,
    guides = "collect"
  ) +
  patchwork::plot_annotation(tag_levels = 'A') &
  theme(legend.position = "right")

png(filename="./outputs/mapas/Pelagicos_biovolumen_plot1996.png", height = 10, width =  15, units = "cm", res = 150, pointsize = 12)
plot1996
dev.off()

# Biomasa 2008-2009

layout_matrix2008_2009 <- "
AABB
AABB
"

plot2008_2009<- (
  pelagicos_2008_12_BIOV_ZOO +
    pelagicos_2009_12_BIOV_ZOO 
) +
  patchwork::plot_layout(
    design = layout_matrix2008_2009,
    guides = "collect"
  ) +
  patchwork::plot_annotation(tag_levels = 'A') &
  theme(legend.position = "right")

png(filename="./outputs/mapas/Pelagicos_biovolumen_plot2008_2009.png", height = 10, width =  15, units = "cm", res = 150, pointsize = 12)
plot2008_2009
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
      values = c("1–10" = 1,
                 "10–100" = 1,
                 "100–1000" = 1,
                 "1000–10000" = 1,
                 "10000–100000" = 2),
      drop = FALSE,
      guide = guide_legend(override.aes = list(shape = 16))
    ) +
    
    scale_color_manual( 
      values = c("1–10" = "#1a1a1a",
                 "10–100" = "#878787",
                 "100–1000" = "#e0e0e0",
                 "1000–10000" = "#f4a582",
                 "10000–100000" = "#b2182b"),
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
pelagicos_1994_07_Huevos <- mapas_individuales("PELAG9407", '1994-07')
pelagicos_1994_12_Huevos <- mapas_individuales("PELAG9412", '1994-12')

pelagicos_1995_06_Huevos <- mapas_individuales("PELAG950607", '1995-06')

pelagicos_1996_05_Huevos <- mapas_individuales("DEMER 9605", '1996-05')
pelagicos_1996_11_Huevos <- mapas_individuales("DEMER 9611", '1996-11')

pelagicos_2008_12_Huevos <- mapas_individuales("PELAG0812", '2008-12')
pelagicos_2009_12_Huevos <- mapas_individuales("PELAG0912", '2009-12')


# Huevos 1991 


layout_matrix1991 <- "
AABB
AABB
CCDD
CCDD
"

plot1991<- (
  pelagicos_1991_02_Huevos +
    pelagicos_1991_09_Huevos +
    pelagicos_1991_12_Huevos 
) +
  patchwork::plot_layout(
    design = layout_matrix1991,
    guides = "collect"
  ) +
  patchwork::plot_annotation(tag_levels = 'A') &
  theme(legend.position = "right")

png(filename="./outputs/mapas/Pelagicos_Huevos_plot1991.png", height = 18, width =  15, units = "cm", res = 150, pointsize = 12)
plot1991
dev.off()

# Huevos 1993

layout_matrix1993 <- "
AABB
AABB
"

plot1993<- (
  pelagicos_1993_01_Huevos +
    pelagicos_1993_11_Huevos 
) +
  patchwork::plot_layout(
    design = layout_matrix1993,
    guides = "collect"
  ) +
  patchwork::plot_annotation(tag_levels = 'A') &
  theme(legend.position = "right")

png(filename="./outputs/mapas/Pelagicos_Huevos_plot1993.png", height = 10, width =  15, units = "cm", res = 150, pointsize = 12)
plot1993
dev.off()

# Huevos 1994

layout_matrix1994 <- "
AABB
AABB
CCDD
CCDD
"

plot1994<- (
  pelagicos_1994_04_Huevos +
    pelagicos_1994_07_Huevos +
    pelagicos_1994_12_Huevos
) +
  patchwork::plot_layout(
    design = layout_matrix1994,
    guides = "collect"
  ) +
  patchwork::plot_annotation(tag_levels = 'A') &
  theme(legend.position = "right")

png(filename="./outputs/mapas/Pelagicos_Huevos_plot1994.png", height = 18, width =  15, units = "cm", res = 150, pointsize = 12)
plot1994
dev.off()

# Huevos 1995


png(filename="./outputs/mapas/Pelagicos_Huevos_plot1995.png", height = 10, width =  12, units = "cm", res = 150, pointsize = 12)
pelagicos_1995_06_Huevos
dev.off()


# Huevos 1996

layout_matrix1996 <- "
AABB
AABB
"

plot1996<- (
  pelagicos_1996_05_Huevos +
    pelagicos_1996_11_Huevos 
) +
  patchwork::plot_layout(
    design = layout_matrix1996,
    guides = "collect"
  ) +
  patchwork::plot_annotation(tag_levels = 'A') &
  theme(legend.position = "right")

png(filename="./outputs/mapas/Pelagicos_Huevos_plot1996.png", height = 10, width =  15, units = "cm", res = 150, pointsize = 12)
plot1996
dev.off()

# Huevos 2008-2009

layout_matrix2008_2009 <- "
AABB
AABB
"

plot2008_2009<- (
  pelagicos_2008_12_Huevos +
    pelagicos_2009_12_Huevos 
) +
  patchwork::plot_layout(
    design = layout_matrix2008_2009,
    guides = "collect"
  ) +
  patchwork::plot_annotation(tag_levels = 'A') &
  theme(legend.position = "right")

png(filename="./outputs/mapas/Pelagicos_Huevos_plot2008_2009.png", height = 10, width =  15, units = "cm", res = 150, pointsize = 12)
plot2008_2009
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
      values = c("1–10" = 1,
                 "10–100" = 1,
                 "100–1000" = 1,
                 "1000–10000" = 2),
      drop = FALSE,
      guide = guide_legend(override.aes = list(shape = 16))
    ) +
    
    scale_color_manual( 
      values = c("1–10" = "#8c510a",
                 "10–100" = "#dfc27d",
                 "100–1000" = "#fdae61",
                 "1000–10000" = "#d53e4f"),
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
pelagicos_1994_07_larvas <- mapas_individuales("PELAG9407", '1994-07')
pelagicos_1994_12_larvas <- mapas_individuales("PELAG9412", '1994-12')

pelagicos_1995_06_larvas <- mapas_individuales("PELAG950607", '1995-06')

pelagicos_1996_05_larvas <- mapas_individuales("DEMER 9605", '1996-05')
pelagicos_1996_11_larvas <- mapas_individuales("DEMER 9611", '1996-11')

pelagicos_2008_12_larvas <- mapas_individuales("PELAG0812", '2008-12')
pelagicos_2009_12_larvas <- mapas_individuales("PELAG0912", '2009-12')

# larvas 1991 


layout_matrix1991 <- "
AABB
AABB
CCDD
CCDD
"

plot1991<- (
  pelagicos_1991_02_larvas +
    pelagicos_1991_09_larvas +
    pelagicos_1991_12_larvas 
) +
  patchwork::plot_layout(
    design = layout_matrix1991,
    guides = "collect"
  ) +
  patchwork::plot_annotation(tag_levels = 'A') &
  theme(legend.position = "right")

png(filename="./outputs/mapas/Pelagicos_larvas_plot1991.png", height = 18, width =  15, units = "cm", res = 150, pointsize = 12)
plot1991
dev.off()

# larvas 1993

layout_matrix1993 <- "
AABB
AABB
"

plot1993<- (
  pelagicos_1993_01_larvas +
    pelagicos_1993_11_larvas 
) +
  patchwork::plot_layout(
    design = layout_matrix1993,
    guides = "collect"
  ) +
  patchwork::plot_annotation(tag_levels = 'A') &
  theme(legend.position = "right")

png(filename="./outputs/mapas/Pelagicos_larvas_plot1993.png", height = 10, width =  15, units = "cm", res = 150, pointsize = 12)
plot1993
dev.off()

# larvas 1994

layout_matrix1994 <- "
AABB
AABB
CCDD
CCDD
"

plot1994<- (
  pelagicos_1994_04_larvas +
    pelagicos_1994_07_larvas +
    pelagicos_1994_12_larvas
) +
  patchwork::plot_layout(
    design = layout_matrix1994,
    guides = "collect"
  ) +
  patchwork::plot_annotation(tag_levels = 'A') &
  theme(legend.position = "right")

png(filename="./outputs/mapas/Pelagicos_larvas_plot1994.png", height = 18, width =  15, units = "cm", res = 150, pointsize = 12)
plot1994
dev.off()

# larvas 1995


png(filename="./outputs/mapas/Pelagicos_larvas_plot1995.png", height = 10, width =  12, units = "cm", res = 150, pointsize = 12)
pelagicos_1995_06_larvas
dev.off()


# larvas 1996

layout_matrix1996 <- "
AABB
AABB
"

plot1996<- (
  pelagicos_1996_05_larvas +
    pelagicos_1996_11_larvas 
) +
  patchwork::plot_layout(
    design = layout_matrix1996,
    guides = "collect"
  ) +
  patchwork::plot_annotation(tag_levels = 'A') &
  theme(legend.position = "right")

png(filename="./outputs/mapas/Pelagicos_larvas_plot1996.png", height = 10, width =  15, units = "cm", res = 150, pointsize = 12)
plot1996
dev.off()

# larvas 2008-2009

layout_matrix2008_2009 <- "
AABB
AABB
"

plot2008_2009<- (
  pelagicos_2008_12_larvas +
    pelagicos_2009_12_larvas 
) +
  patchwork::plot_layout(
    design = layout_matrix2008_2009,
    guides = "collect"
  ) +
  patchwork::plot_annotation(tag_levels = 'A') &
  theme(legend.position = "right")

png(filename="./outputs/mapas/Pelagicos_larvas_plot2008_2009.png", height = 10, width =  15, units = "cm", res = 150, pointsize = 12)
plot2008_2009
dev.off()