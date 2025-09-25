#### script para construir los mapas de densidad de huevos, larvas y biovolumen


data<-readxl::read_excel("Coordenadas_Corregido_mapa_Icito.xlsx", sheet="Mapas_Christian")
breaks <- c(10, 100, 1000, 10000)
data$BIOV_ZOO_Log <- cut(data$BIOV_ZOO,
                               breaks = c(-Inf, breaks, Inf), # Incluye -Inf y Inf para abarcar todos los valores posibles
                               labels = c("10", "100", "1000", "10000", "100000"),
                               right = FALSE)  # right = FALSE significa que los intervalos son cerrados por la izquierda, abiertos por la derecha

data$BIOV_ZOO_Log<-as.character(data$BIOV_ZOO_Log )
data$BIOV_ZOO_Log<-as.integer(data$BIOV_ZOO_Log )
data$BIOV_ZOO_Log<-as.factor(data$BIOV_ZOO_Log )
class(data$BIOV_ZOO_Log)

breaks_huevos <- c(1, 10, 100, 1000, 10000)
data$HUEVOS_Log <- cut(data$HUEVOS,
                         breaks = c(-Inf, breaks_huevos, Inf), # Incluye -Inf y Inf para abarcar todos los valores posibles
                         labels = c("0", "10", "100", "1000", "10000", "100000"),
                         right = FALSE)  # right = FALSE significa que los intervalos son cerrados por la izquierda, abiertos por la derecha

data$HUEVOS_Log<-as.character(data$HUEVOS_Log )
data$HUEVOS_Log<-as.integer(data$HUEVOS_Log )
data$HUEVOS_Log<-as.factor(data$HUEVOS_Log )
class(data$HUEVOS_Log)


breaks_LARVAS <- c(1, 10, 100, 1000, 10000)
data$LARVAS_log <- cut(data$LARVAS,
                       breaks = c(-Inf, breaks_huevos, Inf), # Incluye -Inf y Inf para abarcar todos los valores posibles
                       labels = c("1", "10", "100", "1000", "10000", "100000"),
                       right = FALSE)  # right = FALSE significa que los intervalos son cerrados por la izquierda, abiertos por la derecha

data$LARVAS_log<-as.character(data$LARVAS_log )
data$LARVAS_log<-as.integer(data$LARVAS_log )
data$LARVAS_log<-as.factor(data$LARVAS_log )
class(data$LARVAS_log)



###Mapas####

#Carga de las capas para el mapa
CPC<-sf::st_read("./GeoLayers.gpkg", layer="cuenca_pacifica") # read shapefile CPC
Paises<-sf::st_read("./GeoLayers.gpkg", layer="Continente") # read shapefile Countries
MPA<-sf::st_read("./GeoLayers.gpkg", layer="mpa2023_cpc") # read shapefile Countries


plot(Paises)


#Función para la construcción del mapa
Mapas_graficas_biovolumen <- function(data, variable, titulo, subtitulo, leyenda) {
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
    scale_size_manual(values = c(`1` = 1, `10` = 2, `100` = 3, `1000` = 4, `10000` = 5, `100000` = 8)) +
    # Ajusta los tamaños según las categorías
    #scale_size_continuous(range = c(1, 5),breaks = c(10, 100, 1000, 10000), labels = scales::label_number(accuracy = 1)) +
    theme_bw() +
    theme(
      plot.title = element_text(size = 12, face = "italic", color = "black"),
      axis.title = element_text(face = "bold", color = "black")
    )
}

options(scipen=9999)
erfen_1993_04<-subset(data,data$CRUCERO == "ERFEN9304")

erfen_1993_04_BIOV_ZOO<- Mapas_graficas_biovolumen(erfen_1993_04, 
                                                  erfen_1993_04$BIOV_ZOO_Log, 
                                                  expression(paste(bold("1993-04"))),
                                                  expression(paste(bold(" "))), 
                                                  expression(paste("Biomasa Vol. [ml.1000m"^{-3},"]")))
png(filename="erfen_1993_04_BIOV_ZOO.png", height = 15, width =  25, units = "cm", res = 300, pointsize = 12)
erfen_1993_04_BIOV_ZOO
dev.off()

erfen_1993_10<-subset(data,data$CRUCERO == "ERFEN9310")
erfen_1993_10_BIOV_ZOO<- Mapas_graficas_biovolumen(erfen_1993_10, 
                                                   erfen_1993_10$BIOV_ZOO_Log, 
                                                   expression(paste(bold("1993-10"))),
                                                   expression(paste(bold(" "))), 
                                                   expression(paste("Biomasa Vol. [ml.1000m"^{-3},"]")))
png(filename="erfen_1993_10_BIOV_ZOO.png", height = 15, width =  25, units = "cm", res = 300, pointsize = 12)
erfen_1993_10_BIOV_ZOO
dev.off()


erfen_2004_09<-subset(data,data$CRUCERO == "ERFEN0409")
erfen_2004_09_BIOV_ZOO<- Mapas_graficas_biovolumen(erfen_2004_09, 
                                                   erfen_2004_09$BIOV_ZOO_Log, 
                                                   expression(paste(bold("2004-09"))),
                                                   expression(paste(bold(" "))), 
                                                   expression(paste("Biomasa Vol. [ml.1000m"^{-3},"]")))
png(filename="erfen_2004_09_BIOV_ZOO.png", height = 15, width =  25, units = "cm", res = 300, pointsize = 12)
erfen_2004_09_BIOV_ZOO
dev.off()

erfen_2005_09<-subset(data,data$CRUCERO == "ERFEN0509")
erfen_2005_09_BIOV_ZOO<- Mapas_graficas_biovolumen(erfen_2005_09, 
                                                  erfen_2005_09$BIOV_ZOO_Log, 
                                                   expression(paste(bold("2005-09"))),
                                                   expression(paste(bold(" "))), 
                                                  expression(paste("Biomasa Vol. [ml.1000m"^{-3},"]")))
png(filename="erfen_2005_09_BIOV_ZOO.png", height = 15, width =  25, units = "cm", res = 300, pointsize = 12)
erfen_2005_09_BIOV_ZOO
dev.off()

erfen_2006_03<-subset(data,data$CRUCERO == "ERFEN0603")
erfen_2006_03_BIOV_ZOO<- Mapas_graficas_biovolumen(erfen_2006_03, 
                                                   erfen_2006_03$BIOV_ZOO_Log, 
                                                   expression(paste(bold("2006-03"))),
                                                   expression(paste(bold(" "))), 
                                                   expression(paste("Biomasa Vol. [ml.1000m"^{-3},"]")))
png(filename="erfen_2006_03_BIOV_ZOO.png", height = 15, width =  25, units = "cm", res = 300, pointsize = 12)
erfen_2006_03_BIOV_ZOO
dev.off()

erfen_2006_09<-subset(data,data$CRUCERO == "ERFEN0609")
erfen_2006_09_BIOV_ZOO<- Mapas_graficas_biovolumen(erfen_2006_09, 
                                                   erfen_2006_09$BIOV_ZOO_Log, 
                                                   expression(paste(bold("2006-09"))),
                                                   expression(paste(bold(" "))), 
                                                   expression(paste("Biomasa Vol. [ml.1000m"^{-3},"]")))
png(filename="erfen_2006_09_BIOV_ZOO.png", height = 15, width =  25, units = "cm", res = 300, pointsize = 12)
erfen_2006_09_BIOV_ZOO
dev.off()

erfen_2019_03<-subset(data,data$CRUCERO == "ERFEN1903")
erfen_2019_03_BIOV_ZOO<- Mapas_graficas_biovolumen(erfen_2019_03, 
                                                   erfen_2019_03$BIOV_ZOO_Log, 
                                                   expression(paste(bold("2019-03"))),
                                                   expression(paste(bold(" "))), 
                                                   expression(paste("Biomasa Vol. [ml.1000m"^{-3},"]")))
png(filename="erfen_2019_03_BIOV_ZOO.png", height = 15, width =  25, units = "cm", res = 300, pointsize = 12)
erfen_2019_03_BIOV_ZOO
dev.off()

erfen_2019_09<-subset(data,data$CRUCERO == "ERFEN1909")
erfen_2019_09_BIOV_ZOO<- Mapas_graficas_biovolumen(erfen_2019_09, 
                                                   erfen_2019_09$BIOV_ZOO_Log, 
                                                   expression(paste(bold("2019-09"))),
                                                   expression(paste(bold(" "))), 
                                                   expression(paste("Biomasa Vol. [ml.1000m"^{-3},"]")))
png(filename="erfen_2019_09_BIOV_ZOO.png", height = 15, width =  25, units = "cm", res = 300, pointsize = 12)
erfen_2019_09_BIOV_ZOO
dev.off()

# MApas totales
png(filename="biovolumen_log_10.png", height = 28, width =  25, units = "cm", res = 300, pointsize = 12)
cowplot::plot_grid(labels=c("A","B", "C", "D", "E", "F", "G", "H"),
                   erfen_1993_04_BIOV_ZOO,
                   erfen_1993_10_BIOV_ZOO,
                   erfen_2004_09_BIOV_ZOO,
                   erfen_2005_09_BIOV_ZOO,
                   erfen_2006_03_BIOV_ZOO,
                   erfen_2006_09_BIOV_ZOO,
                   erfen_2019_03_BIOV_ZOO,
                   erfen_2019_09_BIOV_ZOO,
                   nrow=4,
                   ncol=2)
dev.off()


### Huevos



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
png(filename="erfen_1993_04_HUEVOS.png", height = 15, width =  25, units = "cm", res = 300, pointsize = 12)
erfen_1993_04_HUEVOS
dev.off()

erfen_1993_10<-erfen_1993_10%>%filter( HUEVOS > 0)
erfen_1993_10_HUEVOS<- Mapas_graficas_Huevos(erfen_1993_10, 
                                                   erfen_1993_10$HUEVOS_Log, 
                                                   expression(paste(bold("1993-10"))),
                                                   expression(paste(bold(" "))), 
                                            expression(paste("[huevos.10m"^{-2}, "]")))
png(filename="erfen_1993_10_HUEVOS.png", height = 15, width =  25, units = "cm", res = 300, pointsize = 12)
erfen_1993_10_HUEVOS
dev.off()

erfen_2004_09<-erfen_2004_09%>%filter( HUEVOS > 0)
erfen_2004_09_HUEVOS<- Mapas_graficas_Huevos(erfen_2004_09, 
                                                   erfen_2004_09$HUEVOS_Log, 
                                                   expression(paste(bold("2004-09"))),
                                                   expression(paste(bold(" "))), 
                                            expression(paste("[huevos.10m"^{-2}, "]")))
png(filename="erfen_2004_09_HUEVOS.png", height = 15, width =  25, units = "cm", res = 300, pointsize = 12)
erfen_2004_09_HUEVOS
dev.off()

erfen_2005_09<-erfen_2005_09%>%filter( HUEVOS > 0)
erfen_2005_09_HUEVOS<- Mapas_graficas_Huevos(erfen_2005_09, 
                                                   erfen_2005_09$HUEVOS_Log, 
                                                   expression(paste(bold("2005-09"))),
                                                   expression(paste(bold(" "))), 
                                            expression(paste("[huevos.10m"^{-2}, "]")))
png(filename="erfen_2005_09_HUEVOS.png", height = 15, width =  25, units = "cm", res = 300, pointsize = 12)
erfen_2005_09_HUEVOS
dev.off()


erfen_2006_03<-erfen_2006_03%>%filter( HUEVOS > 0)
erfen_2006_03_HUEVOS<- Mapas_graficas_Huevos(erfen_2006_03, 
                                                   erfen_2006_03$HUEVOS_Log, 
                                                   expression(paste(bold("2006-03"))),
                                                   expression(paste(bold(" "))), 
                                            expression(paste("[huevos.10m"^{-2}, "]")))
png(filename="erfen_2006_03_HUEVOS.png", height = 15, width =  25, units = "cm", res = 300, pointsize = 12)
erfen_2006_03_HUEVOS
dev.off()


erfen_2006_09<-erfen_2006_09%>%filter( HUEVOS > 0)
erfen_2006_09_HUEVOS<- Mapas_graficas_Huevos(erfen_2006_09, 
                                                   erfen_2006_09$HUEVOS_Log, 
                                                   expression(paste(bold("2006-09"))),
                                                   expression(paste(bold(" "))), 
                                            expression(paste("[huevos.10m"^{-2}, "]")))
png(filename="erfen_2006_09_HUEVOS.png", height = 15, width =  25, units = "cm", res = 300, pointsize = 12)
erfen_2006_09_HUEVOS
dev.off()

erfen_2019_03<-erfen_2019_03%>%filter( HUEVOS > 0)
erfen_2019_03_HUEVOS<- Mapas_graficas_Huevos(erfen_2019_03, 
                                                   erfen_2019_03$HUEVOS_Log, 
                                                   expression(paste(bold("2019-03"))),
                                                   expression(paste(bold(" "))), 
                                            expression(paste("[huevos.10m"^{-2}, "]")))
png(filename="erfen_2019_03_HUEVOS.png", height = 15, width =  25, units = "cm", res = 300, pointsize = 12)
erfen_2019_03_HUEVOS
dev.off()

erfen_2019_09<-erfen_2019_09%>%filter( HUEVOS > 0)
erfen_2019_09_HUEVOS<- Mapas_graficas_Huevos(erfen_2019_09, 
                                                   erfen_2019_09$HUEVOS_Log, 
                                                   expression(paste(bold("2019-09"))),
                                                   expression(paste(bold(" "))), 
                                            expression(paste("[huevos.10m"^{-2}, "]")))
png(filename="erfen_2019_09_HUEVOS.png", height = 15, width =  25, units = "cm", res = 300, pointsize = 12)
erfen_2019_09_HUEVOS
dev.off()



png(filename="huevos_log_10.png", height = 28, width =  25, units = "cm", res = 300, pointsize = 12)
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

png(filename="erfen_1993_04_LARVAS_log_10.png", height = 10, width =  15, units = "cm", res = 300, pointsize = 12)
erfen_1993_04_LARVAS
dev.off()

erfen_1993_10<-erfen_1993_10%>%filter( LARVAS > 0)
erfen_1993_10_LARVAS<- Mapas_graficas_LARVAS(erfen_1993_10, 
                                             erfen_1993_10$LARVAS_log, 
                                             expression(paste(bold("1993-10"))),
                                             expression(paste(bold(" "))), 
                                              expression(paste("[larvas.10m"^{-2}, "]")))

png(filename="erfen_1993_10_LARVAS.png", height = 10, width =  15, units = "cm", res = 300, pointsize = 12)
erfen_1993_10_LARVAS
dev.off()

erfen_2004_09<-erfen_2004_09%>%filter( LARVAS > 0)
erfen_2004_09_LARVAS<- Mapas_graficas_LARVAS(erfen_2004_09, 
                                             erfen_2004_09$LARVAS_log, 
                                             expression(paste(bold("2004-09"))),
                                             expression(paste(bold(" "))), 
                                              expression(paste("[larvas.10m"^{-2}, "]")))

png(filename="erfen_2004_09_LARVAS.png", height = 10, width =  15, units = "cm", res = 300, pointsize = 12)
erfen_2004_09_LARVAS
dev.off()

erfen_2005_09<-erfen_2005_09%>%filter( LARVAS > 0)
erfen_2005_09_LARVAS<- Mapas_graficas_LARVAS(erfen_2005_09, 
                                             erfen_2005_09$LARVAS_log, 
                                             expression(paste(bold("2005-09"))),
                                             expression(paste(bold(" "))), 
                                              expression(paste("[larvas.10m"^{-2}, "]")))

png(filename="erfen_2005_09_LARVAS.png", height = 10, width =  15, units = "cm", res = 300, pointsize = 12)
erfen_2005_09_LARVAS
dev.off()

erfen_2006_03<-erfen_2006_03%>%filter( LARVAS > 0)
erfen_2006_03_LARVAS<- Mapas_graficas_LARVAS(erfen_2006_03, 
                                             erfen_2006_03$LARVAS_log, 
                                             expression(paste(bold("2006-03"))),
                                             expression(paste(bold(" "))), 
                                              expression(paste("[larvas.10m"^{-2}, "]")))

png(filename="erfen_2006_03_LARVAS.png", height = 10, width =  15, units = "cm", res = 300, pointsize = 12)
erfen_2006_03_LARVAS
dev.off()

erfen_2006_09<-erfen_2006_09%>%filter( LARVAS > 0)
erfen_2006_09_LARVAS<- Mapas_graficas_LARVAS(erfen_2006_09, 
                                             erfen_2006_09$LARVAS_log, 
                                             expression(paste(bold("2006-09"))),
                                             expression(paste(bold(" "))), 
                                              expression(paste("[larvas.10m"^{-2}, "]")))

png(filename="erfen_2006_09_LARVAS.png", height = 10, width =  15, units = "cm", res = 300, pointsize = 12)
erfen_2006_09_LARVAS
dev.off()

erfen_2019_03<-erfen_2019_03%>%filter( LARVAS > 0)
erfen_2019_03_LARVAS<- Mapas_graficas_LARVAS(erfen_2019_03, 
                                             erfen_2019_03$LARVAS_log, 
                                             expression(paste(bold("2019-03"))),
                                             expression(paste(bold(" "))), 
                                              expression(paste("[larvas.10m"^{-2}, "]")))

png(filename="erfen_2019_03_LARVAS.png", height = 10, width =  15, units = "cm", res = 300, pointsize = 12)
erfen_2019_03_LARVAS
dev.off()

erfen_2019_09<-erfen_2019_09%>%filter( LARVAS > 0)
erfen_2019_09_LARVAS<- Mapas_graficas_LARVAS(erfen_2019_09, 
                                             erfen_2019_09$LARVAS_log, 
                                             expression(paste(bold("2019-09"))),
                                             expression(paste(bold(" "))), 
                                              expression(paste("[larvas.10m"^{-2}, "]")))

png(filename="erfen_2019_09_LARVAS.png", height = 10, width =  15, units = "cm", res = 300, pointsize = 12)
erfen_2019_09_LARVAS
dev.off()

png(filename="LARVAS_log_10.png", height = 28, width =  25, units = "cm", res = 300, pointsize = 12)
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

