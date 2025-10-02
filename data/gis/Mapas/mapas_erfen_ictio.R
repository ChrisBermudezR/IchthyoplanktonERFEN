#### script para construir los mapas de densidad de huevos, larvas y biovolumen

if(!require(readxl)) install.packages("readxl")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(dplyr)) install.packages("dplyr")

data<-readxl::read_excel("../data/raw/maps/Coordenadas_Corregido_mapa_Icito_Sin2019.xlsx", sheet="Mapas_Christian")


###Mapas####

#Carga de las capas para el mapa
CPC<-sf::st_read("../data/gis/Mapas/GeoLayers.gpkg", layer="cuenca_pacifica") # read shapefile CPC
Paises<-sf::st_read("../data/gis/Mapas/GeoLayers.gpkg", layer="Continente") # read shapefile Countries
MPA<-sf::st_read("../data/gis/Mapas/GeoLayers.gpkg", layer="mpa2023_cpc") # read shapefile Countries


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
  
  subconjunto <- subset(data, data$CRUCERO == crucero_fecha)
  
  mapa <- Mapas_graficas_biovolumen(
    subconjunto, 
    expression(paste(bold(fecha))),         
    expression(paste(bold(" "))),           
    expression(paste("Biomasa Vol. [ml.1000m"^{-3},"]"))  
  )
  
  return(mapa)
}

erfen_1993_04_BIOV_ZOO <- mapas_individuales("ERFEN9304", "1993-04")
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
png(filename="./outputs/mapas/biovolumen_log_10.png", height = 20, width =  20, units = "cm", res = 300, pointsize = 12)
final_plot
dev.off()


### Huevos


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



Mapas_graficas_Huevos<-function(data, variable, titulo, subtitulo, leyenda) {
  ggplot() +
    
    geom_sf(data = CPC, color = "blue", linetype = 2, linewidth = 0.5, fill = "lightblue") +
    geom_sf(data = Paises, colour = "black", fill = "lightgrey") +
    geom_sf(data = MPA, color = "darkgreen", linetype = 1, linewidth = 0.5, , fill = "lightblue") +
    geom_point(data = data, aes(x = LONGITUD, y = LATITUD, size = variable), colour = "red") +
    coord_sf(xlim = c(-87, -77), ylim = c(1, 8), expand = FALSE) +
    labs(
      title = titulo,
      subtitle = subtitulo,
      x = "Longitude",
      y = "Latitude",
      size = leyenda
    ) +
    scale_size_manual(values = c(`0` = 1, `10` = 2, `100` = 3, `1000` = 4, `10000` = 5, `100000` = 8)) +
    # Ajusta los tamaños según las categorías
    #scale_size_continuous(range = c(1, 5),breaks = c(10, 100, 1000, 10000), labels = scales::label_number(accuracy = 1)) +
    theme_bw() +
    theme(
      plot.title = element_text(size = 12, face = "italic", color = "black"),
      axis.title = element_text(face = "bold", color = "black")
    )
}

options(scipen=9999)


erfen_1993_04<-erfen_1993_04%>%filter( HUEVOS > 0)

erfen_1993_04_HUEVOS<- Mapas_graficas_Huevos(erfen_1993_04, 
                                                   erfen_1993_04$HUEVOS_Log, 
                                                   expression(paste(bold("1993-04"))),
                                                   expression(paste(bold(" "))), 
                                                  expression(paste("[huevos.10m"^{-2}, "]")))
png(filename="./outputs/mapas/erfen_1993_04_HUEVOS.png", height = 15, width =  25, units = "cm", res = 300, pointsize = 12)
erfen_1993_04_HUEVOS
dev.off()

erfen_1993_10<-erfen_1993_10%>%filter( HUEVOS > 0)
erfen_1993_10_HUEVOS<- Mapas_graficas_Huevos(erfen_1993_10, 
                                                   erfen_1993_10$HUEVOS_Log, 
                                                   expression(paste(bold("1993-10"))),
                                                   expression(paste(bold(" "))), 
                                            expression(paste("[huevos.10m"^{-2}, "]")))
png(filename="./outputs/mapas/erfen_1993_10_HUEVOS.png", height = 15, width =  25, units = "cm", res = 300, pointsize = 12)
erfen_1993_10_HUEVOS
dev.off()

erfen_2004_09<-erfen_2004_09%>%filter( HUEVOS > 0)
erfen_2004_09_HUEVOS<- Mapas_graficas_Huevos(erfen_2004_09, 
                                                   erfen_2004_09$HUEVOS_Log, 
                                                   expression(paste(bold("2004-09"))),
                                                   expression(paste(bold(" "))), 
                                            expression(paste("[huevos.10m"^{-2}, "]")))
png(filename="./outputs/mapas/erfen_2004_09_HUEVOS.png", height = 15, width =  25, units = "cm", res = 300, pointsize = 12)
erfen_2004_09_HUEVOS
dev.off()

erfen_2005_09<-erfen_2005_09%>%filter( HUEVOS > 0)
erfen_2005_09_HUEVOS<- Mapas_graficas_Huevos(erfen_2005_09, 
                                                   erfen_2005_09$HUEVOS_Log, 
                                                   expression(paste(bold("2005-09"))),
                                                   expression(paste(bold(" "))), 
                                            expression(paste("[huevos.10m"^{-2}, "]")))
png(filename="./outputs/mapas/erfen_2005_09_HUEVOS.png", height = 15, width =  25, units = "cm", res = 300, pointsize = 12)
erfen_2005_09_HUEVOS
dev.off()


erfen_2006_03<-erfen_2006_03%>%filter( HUEVOS > 0)
erfen_2006_03_HUEVOS<- Mapas_graficas_Huevos(erfen_2006_03, 
                                                   erfen_2006_03$HUEVOS_Log, 
                                                   expression(paste(bold("2006-03"))),
                                                   expression(paste(bold(" "))), 
                                            expression(paste("[huevos.10m"^{-2}, "]")))
png(filename="./outputs/mapas/erfen_2006_03_HUEVOS.png", height = 15, width =  25, units = "cm", res = 300, pointsize = 12)
erfen_2006_03_HUEVOS
dev.off()


erfen_2006_09<-erfen_2006_09%>%filter( HUEVOS > 0)
erfen_2006_09_HUEVOS<- Mapas_graficas_Huevos(erfen_2006_09, 
                                                   erfen_2006_09$HUEVOS_Log, 
                                                   expression(paste(bold("2006-09"))),
                                                   expression(paste(bold(" "))), 
                                            expression(paste("[huevos.10m"^{-2}, "]")))
png(filename="./outputs/mapas/erfen_2006_09_HUEVOS.png", height = 15, width =  25, units = "cm", res = 300, pointsize = 12)
erfen_2006_09_HUEVOS
dev.off()

erfen_2019_03<-erfen_2019_03%>%filter( HUEVOS > 0)
erfen_2019_03_HUEVOS<- Mapas_graficas_Huevos(erfen_2019_03, 
                                                   erfen_2019_03$HUEVOS_Log, 
                                                   expression(paste(bold("2019-03"))),
                                                   expression(paste(bold(" "))), 
                                            expression(paste("[huevos.10m"^{-2}, "]")))
png(filename="./outputs/mapas/erfen_2019_03_HUEVOS.png", height = 15, width =  25, units = "cm", res = 300, pointsize = 12)
erfen_2019_03_HUEVOS
dev.off()

erfen_2019_09<-erfen_2019_09%>%filter( HUEVOS > 0)
erfen_2019_09_HUEVOS<- Mapas_graficas_Huevos(erfen_2019_09, 
                                                   erfen_2019_09$HUEVOS_Log, 
                                                   expression(paste(bold("2019-09"))),
                                                   expression(paste(bold(" "))), 
                                            expression(paste("[huevos.10m"^{-2}, "]")))
png(filename="./outputs/mapas/erfen_2019_09_HUEVOS.png", height = 15, width =  25, units = "cm", res = 300, pointsize = 12)
erfen_2019_09_HUEVOS
dev.off()



png(filename="./outputs/mapas/huevos_log_10.png", height = 28, width =  25, units = "cm", res = 300, pointsize = 12)
cowplot::plot_grid(labels=c("A","B", "C", "D", "E", "F", "G", "H"),
                   erfen_1993_04_HUEVOS,
                   erfen_1993_10_HUEVOS,
                   erfen_2004_09_HUEVOS,
                   erfen_2005_09_HUEVOS,
                   erfen_2006_03_HUEVOS,
                   erfen_2006_09_HUEVOS,
                   erfen_2019_03_HUEVOS,
                   erfen_2019_09_HUEVOS,
                   nrow=4,
                   ncol=2)
dev.off()

####Larvas####


Mapas_graficas_LARVAS<-function(data, variable, titulo, subtitulo, leyenda) {
  ggplot() +
    
    geom_sf(data = CPC, color = "blue", linetype = 2, linewidth = 0.5, fill = "lightblue") +
    geom_sf(data = Paises, colour = "black", fill = "lightgrey") +
    geom_sf(data = MPA, color = "darkgreen", linetype = 1, linewidth = 0.5, , fill = "lightblue") +
    geom_point(data = data, aes(x = LONGITUD, y = LATITUD,  size = variable), colour="red") +
    coord_sf(xlim = c(-87, -77), ylim = c(1, 8), expand = FALSE) +
    labs(
      title = titulo,
      subtitle = subtitulo,
      x = "Longitude",
      y = "Latitude",
      size = leyenda
    ) +
    scale_size_manual(values = c( `10` = 1, `100` = 2, `1000` = 4, `10000` = 6, `100000` = 10)) +
    # Ajusta los tamaños según las categorías
    #scale_size_continuous(range = c(1, 5),breaks = c(10, 100, 1000, 10000), labels = scales::label_number(accuracy = 1)) +
    theme_bw() +
    theme(
      plot.title = element_text(size = 12, face = "italic", color = "black"),
      axis.title = element_text(face = "bold", color = "black")
    )
}


options(scipen=9999)


erfen_1993_04<-erfen_1993_04%>%filter( LARVAS > 0)
erfen_1993_04_LARVAS<- Mapas_graficas_LARVAS(erfen_1993_04, 
                                             erfen_1993_04$LARVAS_log, 
                                             expression(paste(bold("1993-04"))),
                                             expression(paste(bold(" "))), 
                                              expression(paste("[larvas.10m"^{-2}, "]")))

png(filename="./outputs/mapas/erfen_1993_04_LARVAS_log_10.png", height = 10, width =  15, units = "cm", res = 300, pointsize = 12)
erfen_1993_04_LARVAS
dev.off()

erfen_1993_10<-erfen_1993_10%>%filter( LARVAS > 0)
erfen_1993_10_LARVAS<- Mapas_graficas_LARVAS(erfen_1993_10, 
                                             erfen_1993_10$LARVAS_log, 
                                             expression(paste(bold("1993-10"))),
                                             expression(paste(bold(" "))), 
                                              expression(paste("[larvas.10m"^{-2}, "]")))

png(filename="./outputs/mapas/erfen_1993_10_LARVAS.png", height = 10, width =  15, units = "cm", res = 300, pointsize = 12)
erfen_1993_10_LARVAS
dev.off()

erfen_2004_09<-erfen_2004_09%>%filter( LARVAS > 0)
erfen_2004_09_LARVAS<- Mapas_graficas_LARVAS(erfen_2004_09, 
                                             erfen_2004_09$LARVAS_log, 
                                             expression(paste(bold("2004-09"))),
                                             expression(paste(bold(" "))), 
                                              expression(paste("[larvas.10m"^{-2}, "]")))

png(filename="./outputs/mapas/erfen_2004_09_LARVAS.png", height = 10, width =  15, units = "cm", res = 300, pointsize = 12)
erfen_2004_09_LARVAS
dev.off()

erfen_2005_09<-erfen_2005_09%>%filter( LARVAS > 0)
erfen_2005_09_LARVAS<- Mapas_graficas_LARVAS(erfen_2005_09, 
                                             erfen_2005_09$LARVAS_log, 
                                             expression(paste(bold("2005-09"))),
                                             expression(paste(bold(" "))), 
                                              expression(paste("[larvas.10m"^{-2}, "]")))

png(filename="./outputs/mapas/erfen_2005_09_LARVAS.png", height = 10, width =  15, units = "cm", res = 300, pointsize = 12)
erfen_2005_09_LARVAS
dev.off()

erfen_2006_03<-erfen_2006_03%>%filter( LARVAS > 0)
erfen_2006_03_LARVAS<- Mapas_graficas_LARVAS(erfen_2006_03, 
                                             erfen_2006_03$LARVAS_log, 
                                             expression(paste(bold("2006-03"))),
                                             expression(paste(bold(" "))), 
                                              expression(paste("[larvas.10m"^{-2}, "]")))

png(filename="./outputs/mapas/erfen_2006_03_LARVAS.png", height = 10, width =  15, units = "cm", res = 300, pointsize = 12)
erfen_2006_03_LARVAS
dev.off()

erfen_2006_09<-erfen_2006_09%>%filter( LARVAS > 0)
erfen_2006_09_LARVAS<- Mapas_graficas_LARVAS(erfen_2006_09, 
                                             erfen_2006_09$LARVAS_log, 
                                             expression(paste(bold("2006-09"))),
                                             expression(paste(bold(" "))), 
                                              expression(paste("[larvas.10m"^{-2}, "]")))

png(filename="./outputs/mapas/erfen_2006_09_LARVAS.png", height = 10, width =  15, units = "cm", res = 300, pointsize = 12)
erfen_2006_09_LARVAS
dev.off()

erfen_2019_03<-erfen_2019_03%>%filter( LARVAS > 0)
erfen_2019_03_LARVAS<- Mapas_graficas_LARVAS(erfen_2019_03, 
                                             erfen_2019_03$LARVAS_log, 
                                             expression(paste(bold("2019-03"))),
                                             expression(paste(bold(" "))), 
                                              expression(paste("[larvas.10m"^{-2}, "]")))

png(filename="./outputs/mapas/erfen_2019_03_LARVAS.png", height = 10, width =  15, units = "cm", res = 300, pointsize = 12)
erfen_2019_03_LARVAS
dev.off()

erfen_2019_09<-erfen_2019_09%>%filter( LARVAS > 0)
erfen_2019_09_LARVAS<- Mapas_graficas_LARVAS(erfen_2019_09, 
                                             erfen_2019_09$LARVAS_log, 
                                             expression(paste(bold("2019-09"))),
                                             expression(paste(bold(" "))), 
                                              expression(paste("[larvas.10m"^{-2}, "]")))

png(filename="./outputs/mapas/erfen_2019_09_LARVAS.png", height = 10, width =  15, units = "cm", res = 300, pointsize = 12)
erfen_2019_09_LARVAS
dev.off()

png(filename="./outputs/mapas/LARVAS_log_10.png", height = 28, width =  25, units = "cm", res = 300, pointsize = 12)
cowplot::plot_grid(labels=c("A","B", "C", "D", "E", "F", "G", "H"),
                   erfen_1993_04_LARVAS,
                   erfen_1993_10_LARVAS,
                   erfen_2004_09_LARVAS,
                   erfen_2005_09_LARVAS,
                   erfen_2006_03_LARVAS,
                   erfen_2006_09_LARVAS,
                   erfen_2019_03_LARVAS,
                   erfen_2019_09_LARVAS,
                   nrow=4,
                   ncol=2)
dev.off()

