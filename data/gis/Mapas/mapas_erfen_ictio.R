#### script para construir los mapas de densidad de huevos, larvas y biovolumen


if(!require(readxl)) install.packages("readxl")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(dplyr)) install.packages("dplyr")
if(!require(patchwork)) install.packages("patchwork")

data<-readxl::read_excel("./data/raw/maps/Coordenadas_Corregido_mapa_Icito_Sin2019.xlsx", sheet="Mapas_Christian")


###Mapas####

#Carga de las capas para el mapa
CPC<-sf::st_read("./data/gis/Mapas/GeoLayers.gpkg", layer="cuenca_pacifica") # read shapefile CPC
Paises<-sf::st_read("./data/gis/Mapas/GeoLayers.gpkg", layer="Continente") # read shapefile Countries
MPA<-sf::st_read("./data/gis/Mapas/GeoLayers.gpkg", layer="mpa2023_cpc") # read shapefile Countries


options(scipen=9999)


#Función para la construcción del mapa
Mapas_graficas_biovolumen <- function(data, titulo, subtitulo, leyenda) {
  
  # Definir categorías discretas de biovolumen
  niveles <- c("1–10", "10–100", "100–1000", "1000–10000")
  
  data$BIOV_ZOO_Class <- cut(
    data$BIOV_ZOO,
    breaks = c(0, 10, 100, 1000, 10000),
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
    
    coord_sf(xlim = c(-87, -77), ylim = c(1, 8), expand = FALSE) +
    labs(
      title = titulo,
      subtitle = subtitulo,
      x = "Longitude",
      y = "Latitude",
      size = leyenda,
      color = leyenda
    ) +
    
    scale_size_manual(
      values = c("1–10" = 2,
                 "10–100" = 3,
                 "100–1000" = 4,
                 "1000–10000" = 5),
      drop = FALSE,
      guide = guide_legend(override.aes = list(shape = 16))
    ) +
    
    scale_color_manual(
      values = c("1–10" = "#fc9272",
                 "10–100" = "#fb6a4a",
                 "100–1000" = "#ef3b2c",
                 "1000–10000" = "#cb181d"),
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


erfen_1993_04_BIOV_ZOO <- mapas_individuales("ERFEN9304", '1993-04')
erfen_1993_10_BIOV_ZOO <- mapas_individuales("ERFEN9310", "1993-10")
erfen_2004_09_BIOV_ZOO <- mapas_individuales("ERFEN0409", "2004-09")
erfen_2005_09_BIOV_ZOO <- mapas_individuales("ERFEN0509", "2005-09")
erfen_2006_03_BIOV_ZOO <- mapas_individuales("ERFEN0603", "2006-03")
erfen_2006_09_BIOV_ZOO <- mapas_individuales("ERFEN0609", "2006-09")


final_plot <- (
  (erfen_1993_04_BIOV_ZOO | erfen_1993_10_BIOV_ZOO) /
    (erfen_2004_09_BIOV_ZOO | erfen_2005_09_BIOV_ZOO) /
    (erfen_2006_03_BIOV_ZOO | erfen_2006_09_BIOV_ZOO)
) +
  patchwork::plot_layout(guides = "collect") +
  patchwork::plot_annotation(tag_levels = 'A') &
  theme(legend.position = "right")

final_plot

# Mapas totales
png(filename="./outputs/mapas/ERFEN_biovolumen_log_10.png", height = 20, width =  20, units = "cm", res = 300, pointsize = 12)
final_plot
dev.off()


  ### Huevos ####


#Función para la construcción del mapa
Mapas_graficas_huevos <- function(data, titulo, subtitulo, leyenda) {
  
  # Definir categorías discretas de biovolumen
  niveles <- c("1–10", "10–100", "100–1000", "1000–10000", "10000-100000")
  
  data$huevos_ZOO_Class <- cut(
    data$HUEVOS,
    breaks = c(0, 10, 100, 1000, 10000, 102000),
    labels = niveles,
    include.lowest = TRUE,
    right = FALSE
  )
  
  data$huevos_ZOO_Class <- factor(data$huevos_ZOO_Class, levels = niveles)
  
  # Crear un dataset dummy con todas las clases pero coordenadas vacías
  dummy <- data.frame(
    LONGITUD = NA,
    LATITUD = NA,
    huevos_ZOO_Class = factor(niveles, levels = niveles)
  )
  
  # Unir datos reales con los dummy
  data_plot <- rbind(data[, c("LONGITUD", "LATITUD", "huevos_ZOO_Class")], dummy)
  
  ggplot() +
    geom_sf(data = CPC, color = "blue", linetype = 2, linewidth = 0.5, fill = "lightblue") +
    geom_sf(data = Paises, colour = "black", fill = "lightgrey") +
    geom_sf(data = MPA, color = "darkgreen", linetype = 1, linewidth = 0.5, fill = "lightblue") +
    
    geom_point(
      data = data_plot,
      aes(x = LONGITUD, y = LATITUD, size = huevos_ZOO_Class, color = huevos_ZOO_Class)
    ) +
    
    coord_sf(xlim = c(-87, -77), ylim = c(1, 8), expand = FALSE) +
    labs(
      title = titulo,
      subtitle = subtitulo,
      x = "Longitude",
      y = "Latitude",
      size = leyenda,
      color = leyenda
    ) +
    
    scale_size_manual(
      values = c("1–10" = 2,
                 "10–100" = 3,
                 "100–1000" = 4,
                 "1000–10000" = 5,
                 "10000-100000" = 6),
      drop = FALSE,
      guide = guide_legend(override.aes = list(shape = 16))
    ) +
    
    scale_color_manual(
      values = c("1–10" = "#f6e8c3",
                 "10–100" = "#dfc27d",
                 "100–1000" = "#bf812d",
                 "1000–10000" = "#8c510a",
                 "10000-100000" = "#543005"),
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




erfen_1993_04_HUEVOS <- mapas_individuales("ERFEN9304", '1993-04')
erfen_1993_10_HUEVOS <- mapas_individuales("ERFEN9310", "1993-10")
erfen_2004_09_HUEVOS <- mapas_individuales("ERFEN0409", "2004-09")
erfen_2005_09_HUEVOS <- mapas_individuales("ERFEN0509", "2005-09")
erfen_2006_03_HUEVOS <- mapas_individuales("ERFEN0603", "2006-03")
erfen_2006_09_HUEVOS <- mapas_individuales("ERFEN0609", "2006-09")


final_plot <- (
  (erfen_1993_04_HUEVOS | erfen_1993_10_HUEVOS) /
    (erfen_2004_09_HUEVOS | erfen_2005_09_HUEVOS) /
    (erfen_2006_03_HUEVOS | erfen_2006_09_HUEVOS)
) +
  patchwork::plot_layout(guides = "collect") +
  patchwork::plot_annotation(tag_levels = 'A') &
  theme(legend.position = "right")

final_plot

png(filename="./outputs/mapas/ERFEN_huevos_log_10.png", height = 20, width =  20, units = "cm", res = 300, pointsize = 12)
final_plot
dev.off()

#Larvas####
#Función para la construcción del mapa
Mapas_graficas_larvas <- function(data, titulo, subtitulo, leyenda) {
  
  # Definir categorías discretas de biovolumen
  niveles <- c("1–10", "10–100", "100–1000", "1000–10000")
  
  data$larvas_ZOO_Class <- cut(
    data$LARVAS,
    breaks = c(0, 10, 100, 1000, 10000),
    labels = niveles,
    include.lowest = TRUE,
    right = FALSE
  )
  
  data$larvas_ZOO_Class <- factor(data$larvas_ZOO_Class, levels = niveles)
  
  # Crear un dataset dummy con todas las clases pero coordenadas vacías
  dummy <- data.frame(
    LONGITUD = NA,
    LATITUD = NA,
    larvas_ZOO_Class = factor(niveles, levels = niveles)
  )
  
  # Unir datos reales con los dummy
  data_plot <- rbind(data[, c("LONGITUD", "LATITUD", "larvas_ZOO_Class")], dummy)
  
  ggplot() +
    geom_sf(data = CPC, color = "blue", linetype = 2, linewidth = 0.5, fill = "lightblue") +
    geom_sf(data = Paises, colour = "black", fill = "lightgrey") +
    geom_sf(data = MPA, color = "darkgreen", linetype = 1, linewidth = 0.5, fill = "lightblue") +
    
    geom_point(
      data = data_plot,
      aes(x = LONGITUD, y = LATITUD, size = larvas_ZOO_Class, color = larvas_ZOO_Class)
    ) +
    
    coord_sf(xlim = c(-87, -77), ylim = c(1, 8), expand = FALSE) +
    labs(
      title = titulo,
      subtitle = subtitulo,
      x = "Longitude",
      y = "Latitude",
      size = leyenda,
      color = leyenda
    ) +
    
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




erfen_1993_04_larvas <- mapas_individuales("ERFEN9304", '1993-04')
erfen_1993_10_larvas <- mapas_individuales("ERFEN9310", "1993-10")
erfen_2004_09_larvas <- mapas_individuales("ERFEN0409", "2004-09")
erfen_2005_09_larvas <- mapas_individuales("ERFEN0509", "2005-09")
erfen_2006_03_larvas <- mapas_individuales("ERFEN0603", "2006-03")
erfen_2006_09_larvas <- mapas_individuales("ERFEN0609", "2006-09")


final_plot <- (
  (erfen_1993_04_larvas | erfen_1993_10_larvas) /
    (erfen_2004_09_larvas | erfen_2005_09_larvas) /
    (erfen_2006_03_larvas | erfen_2006_09_larvas)
) +
  patchwork::plot_layout(guides = "collect") +
  patchwork::plot_annotation(tag_levels = 'A') &
  theme(legend.position = "right")

final_plot

png(filename="./outputs/mapas/ERFEN_larvas_log_10.png", height = 20, width =  20, units = "cm", res = 300, pointsize = 12)
final_plot
dev.off()
