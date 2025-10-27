

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
    expression(paste("larvas.m"^{-2}))
  )
  
  return(mapa)
}

pelagicos_1991_02_larvas <- mapas_individuales("PELAG9102", '1991-02')
pelagicos_1991_03_larvas <- mapas_individuales("PELAG9103", '1991-03')
pelagicos_1991_09_larvas <- mapas_individuales("PELAG9109", '1991-09')
pelagicos_1991_12_larvas <- mapas_individuales("PELAG9112", '1991-12')
pelagicos_1993_01_larvas <- mapas_individuales("PELAG9301", '1993-01')
pelagicos_1993_11_larvas <- mapas_individuales("PELAG9311", '1993-11')




final_plot2 <- (
  (pelagicos_1991_02_larvas | pelagicos_1991_03_larvas) /
    (pelagicos_1991_09_larvas | pelagicos_1991_12_larvas) /
    (pelagicos_1993_01_larvas | pelagicos_1993_11_larvas)
) +
  patchwork::plot_layout(guides = "collect") +
  patchwork::plot_annotation(tag_levels = 'A') &
  theme(legend.position = "right")

final_plot2

# Mapas totales
png(filename="./outputs/mapas/Pelagicos_larvas_log_10_01.png", height = 30, width =  20, units = "cm", res = 300, pointsize = 12)
final_plot2
dev.off()

pelagicos_1994_04_larvas <- mapas_individuales("PELAG9404", '1994-04')
pelagicos_1994_07_larvas <- mapas_individuales("PELAG9407", '1994-07')
pelagicos_1994_12_larvas <- mapas_individuales("PELAG9412", '1994-12')
pelagicos_1995_06_larvas <- mapas_individuales("PELAG950607", '1995-06')
pelagicos_2008_12_larvas <- mapas_individuales("PELAG0812", '2008-12')
pelagicos_2009_12_larvas <- mapas_individuales("PELAG0912", '2009-12')



final_plot3 <- (
  (pelagicos_1994_04_larvas | pelagicos_1994_07_larvas) /
    (pelagicos_1994_12_larvas | pelagicos_1995_06_larvas) /
    (pelagicos_2008_12_larvas | pelagicos_2009_12_larvas)
) +
  patchwork::plot_layout(guides = "collect") +
  patchwork::plot_annotation(tag_levels = 'A') &
  theme(legend.position = "right")

final_plot3

# Mapas totales
png(filename="./outputs/mapas/Pelagicos_larvas_log_10_02.png", height = 30, width =  20, units = "cm", res = 300, pointsize = 12)
final_plot3
dev.off()

pelagicos_1996_05_larvas <- mapas_individuales("DEMER 9605", '1996-05')
pelagicos_1996_11_larvas <- mapas_individuales("DEMER 9611", '1996-11')


final_plot4 <- (
  (pelagicos_1996_05_larvas | pelagicos_1996_11_larvas) 
) +
  patchwork::plot_layout(guides = "collect") +
  patchwork::plot_annotation(tag_levels = 'A') &
  theme(legend.position = "right")

final_plot4

# Mapas totales
png(filename="./outputs/mapas/Pelagicos_larvas_log_10_03.png", height = 15, width =  20, units = "cm", res = 300, pointsize = 12)
final_plot4
dev.off()

