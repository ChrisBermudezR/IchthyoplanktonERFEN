# Cargar las librerías necesarias
library(ggplot2)
library(patchwork)

# Crear gráficos con ggplot2

graficas_descriptivas_variables_fecha<-function(Temperatura,
                                          Salinidad,
                                          Alturas,
                                          capaMezcla,
                                          Corrientes,
                                          fecha){
# Boxplots
  
assign(paste("g1"),  
 ggplot(data.frame(Temperatura), aes(x = Temperatura)) +
  geom_boxplot(fill = "lightgrey", color = "black") +
  ggtitle(paste("Temperatura", fecha)) +
  xlab(NULL)+
  theme_minimal()+
  theme(
    axis.text.y = element_blank(), 
    axis.text.x = element_blank(),# Elimina los textos del eje y
    axis.ticks.y = element_blank(),      # Elimina las marcas del eje y
    axis.line.y = element_blank(),       # Elimina la línea del eje y
    axis.title.y = element_blank()       # Elimina el título del eje y
  ), 
envir = .GlobalEnv)

assign(paste("g2"), 
  ggplot(data.frame(Salinidad), aes(x = Salinidad)) +
  geom_boxplot(fill = "lightgrey", color = "black") +
  ggtitle(paste("Salinidad", fecha)) +
  theme_minimal()+
  xlab(NULL)+
  theme(
    axis.text.y = element_blank(), 
    axis.text.x = element_blank(),# Elimina los textos del eje y
    axis.ticks.y = element_blank(),      # Elimina las marcas del eje y
    axis.line.y = element_blank(),       # Elimina la línea del eje y
    axis.title.y = element_blank()       # Elimina el título del eje y
  ),
  envir = .GlobalEnv)

assign(paste("g3"),
  ggplot(data.frame(Alturas), aes(x = Alturas)) +
  geom_boxplot(fill = "lightgrey", color = "black") +
  ggtitle(paste("Altura de la superficie del mar", fecha)) +
  xlab(NULL)+
  theme_minimal()+
  theme(
    axis.text.y = element_blank(), 
    axis.text.x = element_blank(),# Elimina los textos del eje y
    axis.ticks.y = element_blank(),      # Elimina las marcas del eje y
    axis.line.y = element_blank(),       # Elimina la línea del eje y
    axis.title.y = element_blank()       # Elimina el título del eje y
  ),
  envir = .GlobalEnv)

assign(paste("g4"),
  ggplot(data.frame(capaMezcla), aes(x = capaMezcla)) +
  geom_boxplot(fill = "lightgrey", color = "black") +
  xlab(NULL)+
  ggtitle(paste("Capa de mezcla", fecha)) +
  theme_minimal()+
  theme(
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),# Elimina los textos del eje y
    axis.ticks.y = element_blank(),      # Elimina las marcas del eje y
    axis.line.y = element_blank(),       # Elimina la línea del eje y
    axis.title.y = element_blank()       # Elimina el título del eje y
  ),
envir = .GlobalEnv)

assign(paste("g5"),
  ggplot(data.frame(Corrientes), aes(x = Corrientes)) +
  geom_boxplot(fill = "lightgrey", color = "black") +
  xlab(NULL)+
  ggtitle(paste("Corrientes", fecha)) +
  theme_minimal()+
  theme(
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),# Elimina los textos del eje y
    axis.ticks.y = element_blank(),      # Elimina las marcas del eje y
    axis.line.y = element_blank(),       # Elimina la línea del eje y
    axis.title.y = element_blank()       # Elimina el título del eje y
  ),
envir = .GlobalEnv)

# Histogramas

assign(paste("h1"),
  ggplot(data.frame(Temperatura), aes(x = Temperatura)) +
  geom_histogram(bins = 30, fill = "lightgrey", color = "black") +
  labs(x = "°C", y = "Frecuencia") +
  theme_minimal(),
envir = .GlobalEnv)

assign(paste("h2"),
  ggplot(data.frame(Salinidad), aes(x = Salinidad)) +
  geom_histogram(bins = 30, fill = "lightgrey", color = "black") +
  labs(x = "PSU", y = "Frecuencia") +
  theme_minimal(),
envir = .GlobalEnv)

assign(paste("h3"),
  ggplot(data.frame(Alturas), aes(x = Alturas)) +
  geom_histogram(fill = "lightgrey", color = "black") +
  labs(x = "m", y = "Frecuencia") +
  theme_minimal(),
envir = .GlobalEnv)

assign(paste("h4"),
  ggplot(data.frame(capaMezcla), aes(x = capaMezcla)) +
  geom_histogram(fill = "lightgrey", color = "black") +
  labs(x = "m", y = "Frecuencia") +
  theme_minimal(),
envir = .GlobalEnv)

assign(paste("h5"),
  ggplot(data.frame(Corrientes), aes(x = Corrientes)) +
  geom_histogram(fill = "lightgrey", color = "black") +
  labs(x = expression(m.s^-1), y = "Frecuencia") +
  theme_minimal(),
envir = .GlobalEnv)



# Crear el layout de gráficos usando patchwork


layout <- (g1) / (h1) / (g2) / (h2) / (g3) / (h3)/ (g4) / (h4) /(g5) / (h5)

# Guardar la imagen
ggsave(paste0("Histogramas_", fecha,".png"), layout, width = 25, height = 35, units = "cm", dpi = 300)
}


graficas_descriptivas_variables_unica<-function(nombre_variable,
                                                xlab,
                                                variable01,
                                                variable02,
                                                variable03,
                                                variable04,
                                                variable05,
                                                variable06,
                                                variable07,
                                                variable08,
                                                fecha01,
                                                fecha02,
                                                fecha03,
                                                fecha04,
                                                fecha05,
                                                fecha06,
                                                fecha07,
                                                fecha08
                                                ){
  # Boxplots
  
  assign(paste("g1"),  
         ggplot(data.frame(variable01), aes(x = variable01)) +
           geom_boxplot(fill = "lightgrey", color = "black") +
           ggtitle(paste(nombre_variable, fecha01)) +
           xlab(NULL)+
           theme_minimal()+
           theme(
             axis.text.y = element_blank(), 
             axis.text.x = element_blank(),# Elimina los textos del eje y
             axis.ticks.y = element_blank(),      # Elimina las marcas del eje y
             axis.line.y = element_blank(),       # Elimina la línea del eje y
             axis.title.y = element_blank()       # Elimina el título del eje y
           ), 
         envir = .GlobalEnv)
  
  assign(paste("g2"), 
         ggplot(data.frame(variable02), aes(x = variable02)) +
           geom_boxplot(fill = "lightgrey", color = "black") +
           ggtitle(paste(nombre_variable, fecha02)) +
           theme_minimal()+
           xlab(NULL)+
           theme(
             axis.text.y = element_blank(), 
             axis.text.x = element_blank(),# Elimina los textos del eje y
             axis.ticks.y = element_blank(),      # Elimina las marcas del eje y
             axis.line.y = element_blank(),       # Elimina la línea del eje y
             axis.title.y = element_blank()       # Elimina el título del eje y
           ),
         envir = .GlobalEnv)
  
  assign(paste("g3"),
         ggplot(data.frame(variable03), aes(x = variable03)) +
           geom_boxplot(fill = "lightgrey", color = "black") +
           ggtitle(paste(nombre_variable, fecha03)) +
           xlab(NULL)+
           theme_minimal()+
           theme(
             axis.text.y = element_blank(), 
             axis.text.x = element_blank(),# Elimina los textos del eje y
             axis.ticks.y = element_blank(),      # Elimina las marcas del eje y
             axis.line.y = element_blank(),       # Elimina la línea del eje y
             axis.title.y = element_blank()       # Elimina el título del eje y
           ),
         envir = .GlobalEnv)
  
  assign(paste("g4"),
         ggplot(data.frame(variable04), aes(x = variable04)) +
           geom_boxplot(fill = "lightgrey", color = "black") +
           xlab(NULL)+
           ggtitle(paste(nombre_variable, fecha04)) +
           theme_minimal()+
           theme(
             axis.text.y = element_blank(),
             axis.text.x = element_blank(),# Elimina los textos del eje y
             axis.ticks.y = element_blank(),      # Elimina las marcas del eje y
             axis.line.y = element_blank(),       # Elimina la línea del eje y
             axis.title.y = element_blank()       # Elimina el título del eje y
           ),
         envir = .GlobalEnv)
  
  assign(paste("g5"),
         ggplot(data.frame(variable05), aes(x = variable06)) +
           geom_boxplot(fill = "lightgrey", color = "black") +
           xlab(NULL)+
           ggtitle(paste(nombre_variable, fecha05)) +
           theme_minimal()+
           theme(
             axis.text.y = element_blank(),
             axis.text.x = element_blank(),# Elimina los textos del eje y
             axis.ticks.y = element_blank(),      # Elimina las marcas del eje y
             axis.line.y = element_blank(),       # Elimina la línea del eje y
             axis.title.y = element_blank()       # Elimina el título del eje y
           ),
         envir = .GlobalEnv)
  
  assign(paste("g6"),
         ggplot(data.frame(variable06), aes(x = variable06)) +
           geom_boxplot(fill = "lightgrey", color = "black") +
           xlab(NULL)+
           ggtitle(paste(nombre_variable, fecha06)) +
           theme_minimal()+
           theme(
             axis.text.y = element_blank(),
             axis.text.x = element_blank(),# Elimina los textos del eje y
             axis.ticks.y = element_blank(),      # Elimina las marcas del eje y
             axis.line.y = element_blank(),       # Elimina la línea del eje y
             axis.title.y = element_blank()       # Elimina el título del eje y
           ),
         envir = .GlobalEnv)
  
  assign(paste("g7"),
         ggplot(data.frame(variable07), aes(x = variable07)) +
           geom_boxplot(fill = "lightgrey", color = "black") +
           xlab(NULL)+
           ggtitle(paste(nombre_variable, fecha07)) +
           theme_minimal()+
           theme(
             axis.text.y = element_blank(),
             axis.text.x = element_blank(),# Elimina los textos del eje y
             axis.ticks.y = element_blank(),      # Elimina las marcas del eje y
             axis.line.y = element_blank(),       # Elimina la línea del eje y
             axis.title.y = element_blank()       # Elimina el título del eje y
           ),
         envir = .GlobalEnv)
  
  assign(paste("g8"),
         ggplot(data.frame(variable08), aes(x = variable08)) +
           geom_boxplot(fill = "lightgrey", color = "black") +
           xlab(NULL)+
           ggtitle(paste(nombre_variable, fecha08)) +
           theme_minimal()+
           theme(
             axis.text.y = element_blank(),
             axis.text.x = element_blank(),# Elimina los textos del eje y
             axis.ticks.y = element_blank(),      # Elimina las marcas del eje y
             axis.line.y = element_blank(),       # Elimina la línea del eje y
             axis.title.y = element_blank()       # Elimina el título del eje y
           ),
         envir = .GlobalEnv)
  
  
  # Histogramas
  
  assign(paste("h1"),
         ggplot(data.frame(variable01), aes(x = variable01)) +
           geom_histogram(bins = 30, fill = "lightgrey", color = "black") +
           labs(x = xlab, y = "Frecuencia") +
           theme_minimal(),
         envir = .GlobalEnv)
  
  assign(paste("h2"),
         ggplot(data.frame(variable02), aes(x = variable02)) +
           geom_histogram(bins = 30, fill = "lightgrey", color = "black") +
           labs(x = xlab, y = "Frecuencia") +
           theme_minimal(),
         envir = .GlobalEnv)
  
  assign(paste("h3"),
         ggplot(data.frame(variable03), aes(x = variable03)) +
           geom_histogram(fill = "lightgrey", color = "black") +
           labs(x = xlab, y = "Frecuencia") +
           theme_minimal(),
         envir = .GlobalEnv)
  
  assign(paste("h4"),
         ggplot(data.frame(variable04), aes(x = variable04)) +
           geom_histogram(fill = "lightgrey", color = "black") +
           labs(x = xlab, y = "Frecuencia") +
           theme_minimal(),
         envir = .GlobalEnv)
  
  assign(paste("h5"),
         ggplot(data.frame(variable05), aes(x = variable05)) +
           geom_histogram(fill = "lightgrey", color = "black") +
           labs(x = xlab, y = "Frecuencia") +
           theme_minimal(),
         envir = .GlobalEnv)
  
  assign(paste("h6"),
         ggplot(data.frame(variable06), aes(x = variable06)) +
           geom_histogram(fill = "lightgrey", color = "black") +
           labs(x = xlab, y = "Frecuencia") +
           theme_minimal(),
         envir = .GlobalEnv)
  
  
  assign(paste("h7"),
         ggplot(data.frame(variable07), aes(x = variable07)) +
           geom_histogram(fill = "lightgrey", color = "black") +
           labs(x = xlab, y = "Frecuencia") +
           theme_minimal(),
         envir = .GlobalEnv)
  
  assign(paste("h8"),
         ggplot(data.frame(variable08), aes(x = variable08)) +
           geom_histogram(fill = "lightgrey", color = "black") +
           labs(x = xlab, y = "Frecuencia") +
           theme_minimal(),
         envir = .GlobalEnv)
  
  
  
  # Crear el layout de gráficos usando patchwork
  
  
  layout <- (g1 | g2) / (h1 | h2) / (g3 | g4) / (h3 | h4) / (g5 | g6) / (h5 | h6)/ (g7 | g8) / (h7 | h8) 
  
  # Guardar la imagen
  ggsave(paste0("Histogramas_", nombre_variable,".png"), layout, width = 25, height = 35, units = "cm", dpi = 300)
}
