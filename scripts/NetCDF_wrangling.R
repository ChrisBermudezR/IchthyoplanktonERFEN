if(!require(raster)){install.packages("raster")}


# Especifica la ruta al archivo NetCDF



modelo <- raster::raster(nrow=121, 
                 ncol=109, 
                 xmn= -85.04167, 
                 xmx= -75.95833, 
                 ymn= -0.04166667, 
                 ymx= 10.04167,
                 crs="+proj=longlat +datum=WGS84 +no_defs",
                 resolution=c(0.08333333, 0.08333333 )) 

Color_Temperatura<-c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c")
Color_Salinidad<-c("#018571", "#80cdc1", "#f5f5f5", "#dfc27d", "#a6611a")
Color_alturas<-c("#eff3ff", "#bdd7e7", "#6baed6", "#3182bd", "#08519c")
Color_capaMezcla<-c("#f0f9e8", "#bae4bc", "#7bccc4", "#43a2ca", "#0868ac")
Color_velocidad<-c("#ffffcc", "#a1dab4", "#41b6c4", "#2c7fb8", "#253494")



#1993-04####

temperatura_1993_04 <- raster::brick("data_1993_04_sea_surface_temperature.nc")
salinidad_1993_04 <-  raster::brick("data_1993_04_salinity.nc")
alturas_1993_04 <-  raster::brick("data_1993_04_sea_surface_height.nc")
capaMezcla_1993_04 <-  raster::brick("data_1993_04_mixed_layer.nc")

uo_1993_04 <-  raster::brick("data_1993_04_eastward_sea_water_velocity.nc")
vo_1993_04 <-  raster::brick("data_1993_04_northward_sea_water_velocity.nc")
corrientes_1993_04<-sqrt(uo_1993_04^2 + vo_1993_04^2)


vector_abril <- paste("Abril", 1:30)



# Lista de datos y parámetros para las gráficas
datos_parametros_1993_04 <- list(
  list(datos = temperatura_1993_04, colores = Color_Temperatura, vector_epoca= vector_abril,unidad = "°C", titulo = "Temperatura - 1993-04"),
  list(datos = salinidad_1993_04, colores = Color_Salinidad, vector_epoca= vector_abril,unidad = "PSU", titulo = "Salinidad - 1993-04"),
  list(datos = alturas_1993_04, colores = Color_alturas, vector_epoca= vector_abril,unidad = "m", titulo = "Altura de la superficie del mar - 1993-04"),
  list(datos = capaMezcla_1993_04, colores = Color_capaMezcla, vector_epoca= vector_abril,unidad = "m", titulo = "Capa de mezcla - 1993-04"),
  list(datos = corrientes_1993_04, colores = Color_velocidad, vector_epoca= vector_abril,unidad = expression(m.s^-1), titulo = "Corrientes - 1993-04")
)

# Generar y guardar las gráficas
for (i in 1:length(datos_parametros)) {
  mapa <- mapeoVariables(datos_parametros_1993_04[[i]]$datos, datos_parametros_1993_04[[i]]$colores, datos_parametros_1993_04[[i]]$vector_epoca, datos_parametros_1993_04[[i]]$unidad, datos_parametros_1993_04[[i]]$titulo)
  nombre_archivo <- paste0("Mapa_", gsub(" ", "_", datos_parametros_1993_04[[i]]$titulo), ".png")
  generar_y_guardar_grafica(mapa, nombre_archivo)
}

Matrix_temperatura_1993_04 <- as.vector(getValues(temperatura_1993_04))
Matrix_salinidad_1993_04 <- as.vector(getValues(salinidad_1993_04))
Matrix_alturas_1993_04 <- as.vector(getValues(alturas_1993_04))
Matrix_capaMezcla_1993_04 <- as.vector(getValues(capaMezcla_1993_04))
Matrix_corrientes_1993_04 <- as.vector(getValues(corrientes_1993_04))

graficas_descriptivas_variables_fecha(Matrix_temperatura_1993_04,
                                      Matrix_salinidad_1993_04,
                                      Matrix_alturas_1993_04,
                                      Matrix_capaMezcla_1993_04,
                                      Matrix_corrientes_1993_04,
                                      "1993-04")

# 1993-10

temperatura_1993_10 <- raster::brick("data_1993_10_sea_surface_temperature.nc")
salinidad_1993_10 <-  raster::brick("data_1993_10_salinity.nc")
alturas_1993_10 <-  raster::brick("data_1993_10_sea_surface_height.nc")
capaMezcla_1993_10 <-  raster::brick("data_1993_10_mixed_layer.nc")

uo_1993_10 <-  raster::brick("data_1993_10_eastward_sea_water_velocity.nc")
vo_1993_10 <-  raster::brick("data_1993_10_northward_sea_water_velocity.nc")
corrientes_1993_10<-sqrt(uo_1993_10^2 + vo_1993_10^2)
vector_octubre <- paste("Octubre", 1:31)

# Lista de datos y parámetros para las gráficas
datos_parametros_1993_10 <- list(
  list(datos = temperatura_1993_10, colores = Color_Temperatura, vector_epoca= vector_octubre,unidad = "°C", titulo = "Temperatura - 1993-10"),
  list(datos = salinidad_1993_10, colores = Color_Salinidad, vector_epoca= vector_octubre,unidad = "PSU", titulo = "Salinidad - 1993-10"),
  list(datos = alturas_1993_10, colores = Color_alturas, vector_epoca= vector_octubre,unidad = "m", titulo = "Altura de la superficie del mar - 1993-10"),
  list(datos = capaMezcla_1993_10, colores = Color_capaMezcla, vector_epoca= vector_octubre,unidad = "m", titulo = "Capa de mezcla - 1993-10"),
  list(datos = corrientes_1993_10, colores = Color_velocidad, vector_epoca= vector_octubre,unidad = expression(m.s^-1), titulo = "Corrientes - 1993-10")
)

# Generar y guardar las gráficas
for (i in 1:length(datos_parametros)) {
  mapa <- mapeoVariables(datos_parametros_1993_10[[i]]$datos, datos_parametros_1993_10[[i]]$colores, datos_parametros_1993_10[[i]]$vector_epoca, datos_parametros_1993_10[[i]]$unidad, datos_parametros_1993_10[[i]]$titulo)
  nombre_archivo <- paste0("Mapa_", gsub(" ", "_", datos_parametros_1993_10[[i]]$titulo), ".png")
  generar_y_guardar_grafica(mapa, nombre_archivo)
}


Matrix_temperatura_1993_10 <- as.vector(getValues(temperatura_1993_10))
Matrix_salinidad_1993_10 <- as.vector(getValues(salinidad_1993_10))
Matrix_alturas_1993_10 <- as.vector(getValues(alturas_1993_10))
Matrix_capaMezcla_1993_10 <- as.vector(getValues(capaMezcla_1993_10))
Matrix_corrientes_1993_10 <- as.vector(getValues(corrientes_1993_10))


graficas_descriptivas_variables_fecha(Matrix_temperatura_1993_10,
                                Matrix_salinidad_1993_10,
                                Matrix_alturas_1993_10,
                                Matrix_capaMezcla_1993_10,
                                Matrix_corrientes_1993_10,
                                "1993-10")

# 2004-09

temperatura_2004_09 <- raster::brick("data_2004_09_sea_surface_temperature.nc")
salinidad_2004_09 <-  raster::brick("data_2004_09_salinity.nc")
alturas_2004_09 <-  raster::brick("data_2004_09_sea_surface_height.nc")
capaMezcla_2004_09 <-  raster::brick("data_2004_09_mixed_layer.nc")

uo_2004_09 <-  raster::brick("data_2004_09_eastward_sea_water_velocity.nc")
vo_2004_09 <-  raster::brick("data_2004_09_northward_sea_water_velocity.nc")
corrientes_2004_09<-sqrt(uo_2004_09^2 + vo_2004_09^2)

vector_septiembre <- paste("Septiembre", 1:30)



# Lista de datos y parámetros para las gráficas
datos_parametros_2004_09 <- list(
  list(datos = temperatura_2004_09, colores = Color_Temperatura, vector_epoca= vector_septiembre,unidad = "°C", titulo = "Temperatura - 2004-09"),
  list(datos = salinidad_2004_09, colores = Color_Salinidad, vector_epoca= vector_septiembre,unidad = "PSU", titulo = "Salinidad - 2004-09"),
  list(datos = alturas_2004_09, colores = Color_alturas, vector_epoca= vector_septiembre,unidad = "m", titulo = "Altura de la superficie del mar - 2004-09"),
  list(datos = capaMezcla_2004_09, colores = Color_capaMezcla, vector_epoca= vector_septiembre,unidad = "m", titulo = "Capa de mezcla - 2004-09"),
  list(datos = corrientes_2004_09, colores = Color_velocidad, vector_epoca= vector_septiembre,unidad = expression(m.s^-1), titulo = "Corrientes - 2004-09")
)

# Generar y guardar las gráficas
for (i in 1:length(datos_parametros)) {
  mapa <- mapeoVariables(datos_parametros_2004_09[[i]]$datos, datos_parametros_2004_09[[i]]$colores, datos_parametros_2004_09[[i]]$vector_epoca, datos_parametros_2004_09[[i]]$unidad, datos_parametros_2004_09[[i]]$titulo)
  nombre_archivo <- paste0("Mapa_", gsub(" ", "_", datos_parametros_2004_09[[i]]$titulo), ".png")
  generar_y_guardar_grafica(mapa, nombre_archivo)
}


Matrix_temperatura_2004_09 <- as.vector(getValues(temperatura_2004_09))
Matrix_salinidad_2004_09 <- as.vector(getValues(salinidad_2004_09))
Matrix_alturas_2004_09 <- as.vector(getValues(alturas_2004_09))
Matrix_capaMezcla_2004_09 <- as.vector(getValues(capaMezcla_2004_09))
Matrix_corrientes_2004_09 <- as.vector(getValues(corrientes_2004_09))

graficas_descriptivas_variables_fecha(Matrix_temperatura_2004_09,
                                      Matrix_salinidad_2004_09,
                                      Matrix_alturas_2004_09,
                                      Matrix_capaMezcla_2004_09,
                                      Matrix_corrientes_2004_09,
                                      "2004-09")


# 2005-09

temperatura_2005_09 <- raster::brick("data_2005_09_sea_surface_temperature.nc")
salinidad_2005_09 <-  raster::brick("data_2005_09_salinity.nc")
alturas_2005_09 <-  raster::brick("data_2005_09_sea_surface_height.nc")
capaMezcla_2005_09 <-  raster::brick("data_2005_09_mixed_layer.nc")

uo_2005_09 <-  raster::brick("data_2005_09_eastward_sea_water_velocity.nc")
vo_2005_09 <-  raster::brick("data_2005_09_northward_sea_water_velocity.nc")
corrientes_2005_09<-sqrt(uo_2005_09^2 + vo_2005_09^2)

vector_septiembre <- paste("Septiembre", 1:30)



# Lista de datos y parámetros para las gráficas
datos_parametros_2005_09 <- list(
  list(datos = temperatura_2005_09, colores = Color_Temperatura, vector_epoca= vector_septiembre,unidad = "°C", titulo = "Temperatura - 2005-09"),
  list(datos = salinidad_2005_09, colores = Color_Salinidad, vector_epoca= vector_septiembre,unidad = "PSU", titulo = "Salinidad - 2005-09"),
  list(datos = alturas_2005_09, colores = Color_alturas, vector_epoca= vector_septiembre,unidad = "m", titulo = "Altura de la superficie del mar - 2005-09"),
  list(datos = capaMezcla_2005_09, colores = Color_capaMezcla, vector_epoca= vector_septiembre,unidad = "m", titulo = "Capa de mezcla - 2005-09"),
  list(datos = corrientes_2005_09, colores = Color_velocidad, vector_epoca= vector_septiembre,unidad = expression(m.s^-1), titulo = "Corrientes - 2005-09")
)

# Generar y guardar las gráficas
for (i in 1:length(datos_parametros)) {
  mapa <- mapeoVariables(datos_parametros_2005_09[[i]]$datos, datos_parametros_2005_09[[i]]$colores, datos_parametros_2005_09[[i]]$vector_epoca, datos_parametros_2005_09[[i]]$unidad, datos_parametros_2005_09[[i]]$titulo)
  nombre_archivo <- paste0("Mapa_", gsub(" ", "_", datos_parametros_2005_09[[i]]$titulo), ".png")
  generar_y_guardar_grafica(mapa, nombre_archivo)
}


Matrix_temperatura_2005_09 <- as.vector(getValues(temperatura_2005_09))
Matrix_salinidad_2005_09 <- as.vector(getValues(salinidad_2005_09))
Matrix_alturas_2005_09 <- as.vector(getValues(alturas_2005_09))
Matrix_capaMezcla_2005_09 <- as.vector(getValues(capaMezcla_2005_09))
Matrix_corrientes_2005_09 <- as.vector(getValues(corrientes_2005_09))



graficas_descriptivas_variables_fecha(Matrix_temperatura_2005_09,
                                      Matrix_salinidad_2005_09,
                                      Matrix_alturas_2005_09,
                                      Matrix_capaMezcla_2005_09,
                                      Matrix_corrientes_2005_09,
                                      "2005-09")

# 2006-03

temperatura_2006_03 <- raster::brick("data_2006_03_sea_surface_temperature.nc")
salinidad_2006_03 <-  raster::brick("data_2006_03_salinity.nc")
alturas_2006_03 <-  raster::brick("data_2006_03_sea_surface_height.nc")
capaMezcla_2006_03 <-  raster::brick("data_2006_03_mixed_layer.nc")

uo_2006_03 <-  raster::brick("data_2006_03_eastward_sea_water_velocity.nc")
vo_2006_03 <-  raster::brick("data_2006_03_northward_sea_water_velocity.nc")
corrientes_2006_03<-sqrt(uo_2006_03^2 + vo_2006_03^2)

vector_marzo <- paste("Marzo", 1:30)



# Lista de datos y parámetros para las gráficas
datos_parametros_2006_03 <- list(
  list(datos = temperatura_2006_03, colores = Color_Temperatura, vector_epoca= vector_marzo,unidad = "°C", titulo = "Temperatura - 2006-03"),
  list(datos = salinidad_2006_03, colores = Color_Salinidad, vector_epoca= vector_marzo,unidad = "PSU", titulo = "Salinidad - 2006-03"),
  list(datos = alturas_2006_03, colores = Color_alturas, vector_epoca= vector_marzo,unidad = "m", titulo = "Altura de la superficie del mar - 2006-03"),
  list(datos = capaMezcla_2006_03, colores = Color_capaMezcla, vector_epoca= vector_marzo,unidad = "m", titulo = "Capa de mezcla - 2006-03"),
  list(datos = corrientes_2006_03, colores = Color_velocidad, vector_epoca= vector_marzo,unidad = expression(m.s^-1), titulo = "Corrientes - 2006-03")
)

# Generar y guardar las gráficas
for (i in 1:length(datos_parametros)) {
  mapa <- mapeoVariables(datos_parametros_2006_03[[i]]$datos, datos_parametros_2006_03[[i]]$colores, datos_parametros_2006_03[[i]]$vector_epoca, datos_parametros_2006_03[[i]]$unidad, datos_parametros_2006_03[[i]]$titulo)
  nombre_archivo <- paste0("Mapa_", gsub(" ", "_", datos_parametros_2006_03[[i]]$titulo), ".png")
  generar_y_guardar_grafica(mapa, nombre_archivo)
}


Matrix_temperatura_2006_03 <- as.vector(getValues(temperatura_2006_03))
Matrix_salinidad_2006_03 <- as.vector(getValues(salinidad_2006_03))
Matrix_alturas_2006_03 <- as.vector(getValues(alturas_2006_03))
Matrix_capaMezcla_2006_03 <- as.vector(getValues(capaMezcla_2006_03))
Matrix_corrientes_2006_03 <- as.vector(getValues(corrientes_2006_03))



graficas_descriptivas_variables_fecha(Matrix_temperatura_2006_03,
                                      Matrix_salinidad_2006_03,
                                      Matrix_alturas_2006_03,
                                      Matrix_capaMezcla_2006_03,
                                      Matrix_corrientes_2006_03,
                                      "2006-03")



# 2006-09

temperatura_2006_09 <- raster::brick("data_2006_09_sea_surface_temperature.nc")
salinidad_2006_09 <-  raster::brick("data_2006_09_salinity.nc")
alturas_2006_09 <-  raster::brick("data_2006_09_sea_surface_height.nc")
capaMezcla_2006_09 <-  raster::brick("data_2006_09_mixed_layer.nc")

uo_2006_09 <-  raster::brick("data_2006_09_eastward_sea_water_velocity.nc")
vo_2006_09 <-  raster::brick("data_2006_09_northward_sea_water_velocity.nc")
corrientes_2006_09<-sqrt(uo_2006_09^2 + vo_2006_09^2)


vector_septiembre <- paste("Septiembre", 1:30)



# Lista de datos y parámetros para las gráficas
datos_parametros_2006_09 <- list(
  list(datos = temperatura_2006_09, colores = Color_Temperatura, vector_epoca= vector_septiembre,unidad = "°C", titulo = "Temperatura - 2006-09"),
  list(datos = salinidad_2006_09, colores = Color_Salinidad, vector_epoca= vector_septiembre,unidad = "PSU", titulo = "Salinidad - 2006-09"),
  list(datos = alturas_2006_09, colores = Color_alturas, vector_epoca= vector_septiembre,unidad = "m", titulo = "Altura de la superficie del mar - 2006-09"),
  list(datos = capaMezcla_2006_09, colores = Color_capaMezcla, vector_epoca= vector_septiembre,unidad = "m", titulo = "Capa de mezcla - 2006-09"),
  list(datos = corrientes_2006_09, colores = Color_velocidad, vector_epoca= vector_septiembre,unidad = expression(m.s^-1), titulo = "Corrientes - 2006-09")
)

# Generar y guardar las gráficas
for (i in 1:length(datos_parametros)) {
  mapa <- mapeoVariables(datos_parametros_2006_09[[i]]$datos, datos_parametros_2006_09[[i]]$colores, datos_parametros_2006_09[[i]]$vector_epoca, datos_parametros_2006_09[[i]]$unidad, datos_parametros_2006_09[[i]]$titulo)
  nombre_archivo <- paste0("Mapa_", gsub(" ", "_", datos_parametros_2006_09[[i]]$titulo), ".png")
  generar_y_guardar_grafica(mapa, nombre_archivo)
}

Matrix_temperatura_2006_09 <- as.vector(getValues(temperatura_2006_09))
Matrix_salinidad_2006_09 <- as.vector(getValues(salinidad_2006_09))
Matrix_alturas_2006_09 <- as.vector(getValues(alturas_2006_09))
Matrix_capaMezcla_2006_09 <- as.vector(getValues(capaMezcla_2006_09))
Matrix_corrientes_2006_09 <- as.vector(getValues(corrientes_2006_09))


graficas_descriptivas_variables_fecha(Matrix_temperatura_2006_09,
                                      Matrix_salinidad_2006_09,
                                      Matrix_alturas_2006_09,
                                      Matrix_capaMezcla_2006_09,
                                      Matrix_corrientes_2006_09,
                                      "2006-09")


# 2019-03

temperatura_2019_03 <- raster::brick("data_2019_03_sea_surface_temperature.nc")
salinidad_2019_03 <-  raster::brick("data_2019_03_salinity.nc")
alturas_2019_03 <-  raster::brick("data_2019_03_sea_surface_height.nc")
capaMezcla_2019_03 <-  raster::brick("data_2019_03_mixed_layer.nc")

uo_2019_03 <-  raster::brick("data_2019_03_eastward_sea_water_velocity.nc")
vo_2019_03 <-  raster::brick("data_2019_03_northward_sea_water_velocity.nc")
corrientes_2019_03<-sqrt(uo_2019_03^2 + vo_2019_03^2)

vector_marzo <- paste("Marzo", 1:30)



# Lista de datos y parámetros para las gráficas
datos_parametros_2019_03 <- list(
  list(datos = temperatura_2019_03, colores = Color_Temperatura, vector_epoca= vector_marzo,unidad = "°C", titulo = "Temperatura - 2019-03"),
  list(datos = salinidad_2019_03, colores = Color_Salinidad, vector_epoca= vector_marzo,unidad = "PSU", titulo = "Salinidad - 2019-03"),
  list(datos = alturas_2019_03, colores = Color_alturas, vector_epoca= vector_marzo,unidad = "m", titulo = "Altura de la superficie del mar - 2019-03"),
  list(datos = capaMezcla_2019_03, colores = Color_capaMezcla, vector_epoca= vector_marzo,unidad = "m", titulo = "Capa de mezcla - 2019-03"),
  list(datos = corrientes_2019_03, colores = Color_velocidad, vector_epoca= vector_marzo,unidad = expression(m.s^-1), titulo = "Corrientes - 2019-03")
)

# Generar y guardar las gráficas
for (i in 1:length(datos_parametros)) {
  mapa <- mapeoVariables(datos_parametros_2019_03[[i]]$datos, datos_parametros_2019_03[[i]]$colores, datos_parametros_2019_03[[i]]$vector_epoca, datos_parametros_2019_03[[i]]$unidad, datos_parametros_2019_03[[i]]$titulo)
  nombre_archivo <- paste0("Mapa_", gsub(" ", "_", datos_parametros_2019_03[[i]]$titulo), ".png")
  generar_y_guardar_grafica(mapa, nombre_archivo)
}


Matrix_temperatura_2019_03 <- as.vector(getValues(temperatura_2019_03))
Matrix_salinidad_2019_03 <- as.vector(getValues(salinidad_2019_03))
Matrix_alturas_2019_03 <- as.vector(getValues(alturas_2019_03))
Matrix_capaMezcla_2019_03 <- as.vector(getValues(capaMezcla_2019_03))
Matrix_corrientes_2019_03 <- as.vector(getValues(corrientes_2019_03))

graficas_descriptivas_variables_fecha(Matrix_temperatura_2019_03,
                                      Matrix_salinidad_2019_03,
                                      Matrix_alturas_2019_03,
                                      Matrix_capaMezcla_2019_03,
                                      Matrix_corrientes_2019_03,
                                      "2019-03")

# 2019-09

temperatura_2019_09 <- raster::brick("data_2019_09_sea_surface_temperature.nc")
salinidad_2019_09 <-  raster::brick("data_2019_09_salinity.nc")
alturas_2019_09 <-  raster::brick("data_2019_09_sea_surface_height.nc")
capaMezcla_2019_09 <-  raster::brick("data_2019_09_mixed_layer.nc")

uo_2019_09 <-  raster::brick("data_2019_09_eastward_sea_water_velocity.nc")
vo_2019_09 <-  raster::brick("data_2019_09_northward_sea_water_velocity.nc")
corrientes_2019_09<-sqrt(uo_2019_09^2 + vo_2019_09^2)

vector_septiembre <- paste("Septiembre", 1:30)



# Lista de datos y parámetros para las gráficas
datos_parametros_2019_09 <- list(
  list(datos = temperatura_2019_09, colores = Color_Temperatura, vector_epoca= vector_septiembre,unidad = "°C", titulo = "Temperatura - 2019-09"),
  list(datos = salinidad_2019_09, colores = Color_Salinidad, vector_epoca= vector_septiembre,unidad = "PSU", titulo = "Salinidad - 2019-09"),
  list(datos = alturas_2019_09, colores = Color_alturas, vector_epoca= vector_septiembre,unidad = "m", titulo = "Altura de la superficie del mar - 2019-09"),
  list(datos = capaMezcla_2019_09, colores = Color_capaMezcla, vector_epoca= vector_septiembre,unidad = "m", titulo = "Capa de mezcla - 2019-09"),
  list(datos = corrientes_2019_09, colores = Color_velocidad, vector_epoca= vector_septiembre,unidad = expression(m.s^-1), titulo = "Corrientes - 2019-09")
)

# Generar y guardar las gráficas
for (i in 1:length(datos_parametros)) {
  mapa <- mapeoVariables(datos_parametros_2019_09[[i]]$datos, datos_parametros_2019_09[[i]]$colores, datos_parametros_2019_09[[i]]$vector_epoca, datos_parametros_2019_09[[i]]$unidad, datos_parametros_2019_09[[i]]$titulo)
  nombre_archivo <- paste0("Mapa_", gsub(" ", "_", datos_parametros_2019_09[[i]]$titulo), ".png")
  generar_y_guardar_grafica(mapa, nombre_archivo)
}

Matrix_temperatura_2019_09 <- as.vector(getValues(temperatura_2019_09))
Matrix_salinidad_2019_09 <- as.vector(getValues(salinidad_2019_09))
Matrix_alturas_2019_09 <- as.vector(getValues(alturas_2019_09))
Matrix_capaMezcla_2019_09 <- as.vector(getValues(capaMezcla_2019_09))
Matrix_corrientes_2019_09 <- as.vector(getValues(corrientes_2019_09))


graficas_descriptivas_variables_fecha(Matrix_temperatura_2019_09,
                                      Matrix_salinidad_2019_09,
                                      Matrix_alturas_2019_09,
                                      Matrix_capaMezcla_2019_09,
                                      Matrix_corrientes_2019_09,
                                      "2019-09")





graficas_descriptivas_variables_unica("Temperatura",
                                      "°C",
                                      Matrix_temperatura_1993_04,
                                      Matrix_temperatura_1993_10,
                                      Matrix_temperatura_2004_09,
                                      Matrix_temperatura_2005_09,
                                      Matrix_temperatura_2006_03,
                                      Matrix_temperatura_2006_09,
                                      Matrix_temperatura_2019_03,
                                      Matrix_temperatura_2019_09,
                                      "1993-04",
                                      "1993-10",
                                      "2004-09",
                                      "2005-09",
                                      "2006-03",
                                      "2006-09",
                                      "2019-03",
                                      "2019-09"
)

graficas_descriptivas_variables_unica("Salinidad",
                                      "PSU",
                                      Matrix_salinidad_1993_04,
                                      Matrix_salinidad_1993_10,
                                      Matrix_salinidad_2004_09,
                                      Matrix_salinidad_2005_09,
                                      Matrix_salinidad_2006_03,
                                      Matrix_salinidad_2006_09,
                                      Matrix_salinidad_2019_03,
                                      Matrix_salinidad_2019_09,
                                      "1993-04",
                                      "1993-10",
                                      "2004-09",
                                      "2005-09",
                                      "2006-03",
                                      "2006-09",
                                      "2019-03",
                                      "2019-09"
)

graficas_descriptivas_variables_unica("Salinidad",
                                      "PSU",
                                      Matrix_salinidad_1993_04,
                                      Matrix_salinidad_1993_10,
                                      Matrix_salinidad_2004_09,
                                      Matrix_salinidad_2005_09,
                                      Matrix_salinidad_2006_03,
                                      Matrix_salinidad_2006_09,
                                      Matrix_salinidad_2019_03,
                                      Matrix_salinidad_2019_09,
                                      "1993-04",
                                      "1993-10",
                                      "2004-09",
                                      "2005-09",
                                      "2006-03",
                                      "2006-09",
                                      "2019-03",
                                      "2019-09"
)

graficas_descriptivas_variables_unica("Capa de mezcla",
                                      "m",
                                      Matrix_capaMezcla_1993_04,
                                      Matrix_capaMezcla_1993_10,
                                      Matrix_capaMezcla_2004_09,
                                      Matrix_capaMezcla_2005_09,
                                      Matrix_capaMezcla_2006_03,
                                      Matrix_capaMezcla_2006_09,
                                      Matrix_capaMezcla_2019_03,
                                      Matrix_capaMezcla_2019_09,
                                      "1993-04",
                                      "1993-10",
                                      "2004-09",
                                      "2005-09",
                                      "2006-03",
                                      "2006-09",
                                      "2019-03",
                                      "2019-09"
)

graficas_descriptivas_variables_unica("Altura de la superficie del mar",
                                      "m",
                                      Matrix_alturas_1993_04,
                                      Matrix_alturas_1993_10,
                                      Matrix_alturas_2004_09,
                                      Matrix_alturas_2005_09,
                                      Matrix_alturas_2006_03,
                                      Matrix_alturas_2006_09,
                                      Matrix_alturas_2019_03,
                                      Matrix_alturas_2019_09,
                                      "1993-04",
                                      "1993-10",
                                      "2004-09",
                                      "2005-09",
                                      "2006-03",
                                      "2006-09",
                                      "2019-03",
                                      "2019-09"
)

graficas_descriptivas_variables_unica("Corrientes",
                                      expression(paste("m s"^{-1})),
                                      Matrix_corrientes_1993_04,
                                      Matrix_corrientes_1993_10,
                                      Matrix_corrientes_2004_09,
                                      Matrix_corrientes_2005_09,
                                      Matrix_corrientes_2006_03,
                                      Matrix_corrientes_2006_09,
                                      Matrix_corrientes_2019_03,
                                      Matrix_corrientes_2019_09,
                                      "1993-04",
                                      "1993-10",
                                      "2004-09",
                                      "2005-09",
                                      "2006-03",
                                      "2006-09",
                                      "2019-03",
                                      "2019-09"
)




# Calcula el promedio a lo largo del tiempo

library(raster)

# Define la función para calcular y guardar las imágenes
generar_imagenes <- function(raster, nombre_prefix, Gradiente_Color) {
  promedio <- calc(raster, mean, na.rm = TRUE)
  median <- calc(raster, median, na.rm = TRUE)
  sd <- calc(raster, sd, na.rm = TRUE)
  IQR <- calc(raster, IQR, na.rm = TRUE)
  
  nombres <- c("Promedio", "Mediana", "Desviacion_Estandar", "Rango_InterCuartilico")
  rasters <- list(promedio, median, sd, IQR)
  
  for (i in seq_along(nombres)) {
    nombre_archivo <- paste0(nombre_prefix, "_", nombres[i], ".png")
    png(nombre_archivo, width = 20, height = 20, units = "m", pointsize = 12, bg = "white", res = 300, symbolfamily = "default")
    mapa<-ggplot(data=rasters, aes(longitud, latitud, fill = value)) + 
      geom_raster() +
      scale_fill_gradientn(colours = Gradiente_Color) +
      coord_equal()+
      labs(x = "Longitud", y = "Latitud", fill="vTitulo", title = "TituloP") + 
      theme_classic()+
      theme(legend.position="right", 
            axis.text.y =  element_text(size = 8),
            axis.title =  element_text(size = 8),
            plot.title = element_text(size = 10),
            plot.subtitle = element_text(size = 8),
            plot.caption= element_text(size = 8),
            plot.tag = element_text(size=10),
            strip.text = element_text(color = "black", size = 10),  # Cambia el color y tamaño del texto del título de la faceta
            strip.background = element_blank())
    dev.off()
  }
}

# Llama a la función para generar imágenes para temperatura_Panama
generar_imagenes(temperatura_Panama, "temperatura_Panama", Color_Temperatura)

# Llama a la función para generar imágenes para temperatura_Choco
generar_imagenes(temperatura_Choco, "temperatura_Choco")




###Exportar capas

exportLayer(temperatura_Panama, median, "Panama_Temperatura_median")
exportLayer(temperatura_Panama, IQR, "Panama_Temperatura_IQR")

exportLayer(temperatura_Choco, median, "Choco_Temperatura_median")
exportLayer(temperatura_Choco, IQR, "Choco_Temperatura_IQR")


exportLayer(salinidad_Panama, median, "Panama_salinidad_median")
exportLayer(salinidad_Panama, IQR, "Panama_salinidad_IQR")

exportLayer(salinidad_Choco, median, "Choco_salinidad_median")
exportLayer(salinidad_Choco, IQR, "Choco_salinidad_IQR")


exportLayer(alturas_Panama, median, "Panama_alturas_median")
exportLayer(alturas_Panama, IQR, "Panama_alturas_IQR")

exportLayer(alturas_Choco, median, "Choco_alturas_median")
exportLayer(alturas_Choco, IQR, "Choco_alturas_IQR")

exportLayer(capaMezcla_Panama, median, "Panama_capaMezcla_median")
exportLayer(capaMezcla_Panama, IQR, "Panama_capaMezcla_IQR")

exportLayer(capaMezcla_Choco, median, "Choco_capaMezcla_median")
exportLayer(capaMezcla_Choco, IQR, "Choco_capaMezcla_IQR")

exportLayer(CarbonZoo_Panama, median, "Panama_CarbonZoo_median")
exportLayer(CarbonZoo_Panama, IQR, "Panama_CarbonZoo_IQR")

exportLayer(CarbonZoo_Choco, median, "Choco_CarbonZoo_median")
exportLayer(CarbonZoo_Choco, IQR, "Choco_CarbonZoo_IQR")

exportLayer(eufotica_Panama, median, "Panama_eufotica_median")
exportLayer(eufotica_Panama, IQR, "Panama_eufotica_IQR")

exportLayer(eufotica_Choco, median, "Choco_eufotica_median")
exportLayer(eufotica_Choco, IQR, "Choco_eufotica_IQR")

exportLayer(pp_Panama, median, "Panama_pp_median")
exportLayer(pp_Panama, IQR, "Panama_pp_IQR")

exportLayer(pp_Choco, median, "Choco_pp_median")
exportLayer(pp_Choco, IQR, "Choco_pp_IQR")

exportLayer(corrientes_panama, median, "Panama_corrientes_median")
exportLayer(corrientes_panama, IQR, "Panama_corrientes_IQR")

exportLayer(corrientes_Choco, median, "Choco_corrientes_median")
exportLayer(corrientes_Choco, IQR, "Choco_corrientes_IQR")


writeRaster(temperatura_Panama, 
            filename = "temperatura_Panama.gpkg", 
            format = "GPKG", 
            overwrite=TRUE)



#Alineamiento


pred_rem_Choco<-list.files(path="./layers/Choco",pattern = ".gpkg$", full.names = TRUE)
predictors_names<-list.files(path="./layers/Choco", pattern = ".gpkg$", full.names = FALSE)
evaluacion_Cor_Choco<-raster::stack(pred_rem_Choco)
names(evaluacion_Cor_Choco)<-c(
  "alturas_IQR",      "alturas_median" ,   
  "capaMezcla_IQR",     "capaMezcla_median", 
  "CarbonZoo_IQR",      "CarbonZoo_median",  
  "Corrientes_IQR",     "Corrientes_median", 
  "eufotica_IQR",       "eufotica_median",   
  "pp_IQR",             "pp_median",         
  "salinidad_IQR",      "salinidad_median",  
  "Temperatura_IQR",    "Temperatura_median"
  
)
raster::layerStats(evaluacion_Cor_Choco, 'pearson', na.rm=T)
  
  
correlacion_Choco=raster::layerStats(evaluacion_Cor_Choco, 'pearson', na.rm=T)
corr_matrix_Choco=as.data.frame(correlacion_Choco$'pearson correlation coefficient')
write.table(corr_matrix_Choco,file="./layers/Choco/Corr_matrix_Choco.csv", col.names = TRUE, row.names=TRUE, sep=",")

corr_matrix2_Choco<-as.matrix(corr_matrix_Choco)
col <- grDevices::colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
title <- "Correlaciones de los predictores para la epoca del Jet del Chochó"
png(filename = "./layers/Choco/corr_Matrix_Choco.png", res = 300, width = 3500, height = 3000, units = "px", pointsize = 12, type = c("cairo"))
corrplot::corrplot(corr_matrix2_Choco, type="upper", title=title,  sig.level = 0.01, insig = "blank", col=RColorBrewer::brewer.pal(n=8, name="RdYlBu"), tl.col="black", tl.srt=45, mar=c(0,0,1,0) )
dev.off()

#Covariation
covariation_Choco=layerStats(evaluacion_Cor_Choco, 'cov', na.rm=T)
covariation_matrix_Choco=as.data.frame(covariation_Choco$'covariance')
write.table(covariation_matrix_Choco,file="./layers/Choco/covariation_matrix_Choco.csv", col.names = TRUE, row.names=TRUE, sep=",")


#Panama

pred_rem_Panama<-list.files(path="./layers/Panama",pattern = ".gpkg$", full.names = TRUE)
predictors_names<-list.files(path="./layers/Panama", pattern = ".gpkg$", full.names = FALSE)
evaluacion_Cor_Panama<-raster::stack(pred_rem_Panama)
  
correlacion_Panama=raster::layerStats(evaluacion_Cor_Panama, 'pearson', na.rm=T)
corr_matrix_Panama=as.data.frame(correlacion_Panama$'pearson correlation coefficient')
write.table(corr_matrix_Panama,file="./layers/Panama/Corr_matrix_Panama.csv", col.names = TRUE, row.names=TRUE, sep=",")

corr_matrix2_Panama<-as.matrix(corr_matrix_Panama)
col <- grDevices::colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
title <- "Correlaciones de los predictores para la epoca del Jet del Panamá"
png(filename = "./layers/Panama/corr_Matrix_Panama.png", res = 300, width = 3500, height = 3000, units = "px", pointsize = 12, type = c("cairo"))
corrplot::corrplot(corr_matrix2_Panama, type="upper", title=title,  sig.level = 0.01, insig = "blank", col=RColorBrewer::brewer.pal(n=8, name="RdYlBu"), tl.col="black", tl.srt=45, mar=c(0,0,1,0) )
dev.off()

#Covariation
covariation_Panama=layerStats(evaluacion_Cor_Panama, 'cov', na.rm=T)
covariation_matrix_Panama=as.data.frame(covariation_Panama$'covariance')
write.table(covariation_matrix_Panama,file="./layers/Panama/covariation_matrix_Panama.csv", col.names = TRUE, row.names=TRUE, sep=",")




#Mapas####



Panama_Temperatura_median<-as.data.frame(raster("./layers/Panama/Panama_Temperatura_median.gpkg"), xy=TRUE)
Panama_Temperatura_IQR<-as.data.frame(raster("./layers/Panama/Panama_Temperatura_IQR.gpkg"), xy=TRUE)
Panama_Salinidad_median<-as.data.frame(raster("./layers/Panama/Panama_salinidad_median.gpkg"), xy=TRUE)
Panama_Salinidad_IQR<-as.data.frame(raster("./layers/Panama/Panama_salinidad_IQR.gpkg"), xy=TRUE)
Panama_alturas_median<-as.data.frame(raster("./layers/Panama/Panama_alturas_median.gpkg"), xy=TRUE)
Panama_alturas_IQR<-as.data.frame(raster("./layers/Panama/Panama_alturas_IQR.gpkg"), xy=TRUE)
Panama_capaMezcla_median<-as.data.frame(raster("./layers/Panama/Panama_capaMezcla_median.gpkg"), xy=TRUE)
Panama_capaMezcla_IQR<-as.data.frame(raster("./layers/Panama/Panama_capaMezcla_IQR.gpkg"), xy=TRUE)
Panama_eufotica_median<-as.data.frame(raster("./layers/Panama/Panama_eufotica_median.gpkg"), xy=TRUE)
Panama_eufotica_IQR<-as.data.frame(raster("./layers/Panama/Panama_eufotica_IQR.gpkg"), xy=TRUE)
Panama_Carbonzoo_median<-as.data.frame(raster("./layers/Panama/Panama_CarbonZoo_median.gpkg"), xy=TRUE)
Panama_Carbonzoo_IQR<-as.data.frame(raster("./layers/Panama/Panama_Carbonzoo_IQR.gpkg"), xy=TRUE)
Panama_pp_median<-as.data.frame(raster("./layers/Panama/Panama_pp_median.gpkg"), xy=TRUE)
Panama_pp_IQR<-as.data.frame(raster("./layers/Panama/Panama_pp_IQR.gpkg"), xy=TRUE)
Panama_Corrientes_median<-as.data.frame(raster("./layers/Panama/Panama_Corrientes_median.gpkg"), xy=TRUE)
Panama_Corrientes_IQR<-as.data.frame(raster("./layers/Panama/Panama_Corrientes_IQR.gpkg"), xy=TRUE)

Choco_Temperatura_median<-as.data.frame(raster("./layers/Choco/Choco_Temperatura_median.gpkg"), xy=TRUE)
Choco_Temperatura_IQR<-as.data.frame(raster("./layers/Choco/Choco_Temperatura_IQR.gpkg"), xy=TRUE)
Choco_Salinidad_median<-as.data.frame(raster("./layers/Choco/Choco_Salinidad_median.gpkg"), xy=TRUE)
Choco_Salinidad_IQR<-as.data.frame(raster("./layers/Choco/Choco_Salinidad_IQR.gpkg"), xy=TRUE)
Choco_alturas_median<-as.data.frame(raster("./layers/Choco/Choco_alturas_median.gpkg"), xy=TRUE)
Choco_alturas_IQR<-as.data.frame(raster("./layers/Choco/Choco_alturas_IQR.gpkg"), xy=TRUE)
Choco_capaMezcla_median<-as.data.frame(raster("./layers/Choco/Choco_capaMezcla_median.gpkg"), xy=TRUE)
Choco_capaMezcla_IQR<-as.data.frame(raster("./layers/Choco/Choco_capaMezcla_IQR.gpkg"), xy=TRUE)
Choco_eufotica_median<-as.data.frame(raster("./layers/Choco/Choco_eufotica_median.gpkg"), xy=TRUE)
Choco_eufotica_IQR<-as.data.frame(raster("./layers/Choco/Choco_eufotica_IQR.gpkg"), xy=TRUE)
Choco_Carbonzoo_median<-as.data.frame(raster("./layers/Choco/Choco_Carbonzoo_median.gpkg"), xy=TRUE)
Choco_Carbonzoo_IQR<-as.data.frame(raster("./layers/Choco/Choco_Carbonzoo_IQR.gpkg"), xy=TRUE)
Choco_pp_median<-as.data.frame(raster("./layers/Choco/Choco_pp_median.gpkg"), xy=TRUE)
Choco_pp_IQR<-as.data.frame(raster("./layers/Choco/Choco_pp_IQR.gpkg"), xy=TRUE)
Choco_Corrientes_median<-as.data.frame(raster("./layers/Choco/Choco_Corrientes_median.gpkg"), xy=TRUE)
Choco_Corrientes_IQR<-as.data.frame(raster("./layers/Choco/Choco_Corrientes_IQR.gpkg"), xy=TRUE)

generar_Mapa_Estadistico<- function(datos_df, colores, titulo_mapa, titulo_variable, nombre_objeto) {
  assign(nombre_objeto,ggplot(data=datos_df, aes(x, y, fill = Height)) + 
           geom_raster() +
           scale_fill_gradientn(colours = colores) +
           coord_equal()+
           labs(x = "Longitud", y = "Latitud", fill=titulo_variable, title = titulo_mapa) + 
           theme_classic()+
           theme(legend.position="right", 
                 text = element_text(size = 8),
                 strip.background = element_blank()),
         envir = .GlobalEnv)
  
}

generar_Mapa_Estadistico(Panama_Temperatura_median, Color_Temperatura, "Mediana de la Temperatura", "°C", "Panama_Mediana_Temp")
generar_Mapa_Estadistico(Panama_Temperatura_IQR, Color_Temperatura, "RIQ de la Temperatura", "°C", "Panama_RIQ_Temp")
generar_Mapa_Estadistico(Panama_Salinidad_median, Color_Salinidad, "Mediana de la Salinidad", "PSU", "Panama_Mediana_Sal")
generar_Mapa_Estadistico(Panama_Salinidad_IQR, Color_Salinidad, "RIQ de la Salinidad", "PSU", "Panama_RIQ_Sal")
generar_Mapa_Estadistico(Panama_alturas_median, Color_alturas, "Mediana de la alturas de la superficie", "m", "Panama_Mediana_alt")
generar_Mapa_Estadistico(Panama_alturas_IQR, Color_alturas, "RIQ de la alturas de la superficie", "m", "Panama_RIQ_alt")
generar_Mapa_Estadistico(Panama_capaMezcla_median, Color_capaMezcla, "Mediana de la capa mezcla", "m", "Panama_Mediana_Mezcla")
generar_Mapa_Estadistico(Panama_capaMezcla_IQR, Color_capaMezcla, "RIQ de la capa mezcla", "m", "Panama_RIQ_Mezcla")
generar_Mapa_Estadistico(Panama_eufotica_median, Color_eufotica, "Mediana de la prof. eufotica", "m", "Panama_Mediana_euf")
generar_Mapa_Estadistico(Panama_eufotica_IQR, Color_eufotica, "RIQ de la prof. eufotica", "m", "Panama_RIQ_euf")
generar_Mapa_Estadistico(Panama_Carbonzoo_median, Color_Carbonzoo, "Mediana de la carbono zooplancton", expression(g %.% m^-2), "Panama_Mediana_Carb")
generar_Mapa_Estadistico(Panama_Carbonzoo_IQR, Color_Carbonzoo, "RIQ de la Carbonozoo", expression(g %.% m^-2), "Panama_RIQ_Carb")
generar_Mapa_Estadistico(Panama_pp_median, Color_pp, "Mediana de la productividad primaria", expression(mg %.% m^-2 %.% day^-1), "Panama_Mediana_pp")
generar_Mapa_Estadistico(Panama_pp_IQR, Color_pp, "RIQ de la productividad primaria", expression(mg %.% m^-2 %.% day^-1), "Panama_RIQ_pp")
generar_Mapa_Estadistico(Panama_Corrientes_median, Color_velocidad, "Mediana de la Corrientes", expression(m.s^-1), "Panama_Mediana_Corr")
generar_Mapa_Estadistico(Panama_Corrientes_IQR, Color_velocidad, "RIQ de la Corrientes", expression(m.s^-1), "Panama_RIQ_Corr")

png("Panama_Estadisticos_Mapa_01.png", width = 18, height = 40, units = "m", pointsize = 12, bg = "white", res = 300, symbolfamily = "default")
gridExtra::grid.arrange(ncol=2,
                        
                        Panama_Mediana_Temp,
                        Panama_RIQ_Temp,
                        Panama_Mediana_Sal,
                        Panama_RIQ_Sal,
                        Panama_Mediana_alt,
                        Panama_RIQ_alt, 
                        Panama_Mediana_Mezcla, 
                        Panama_RIQ_Mezcla,
                        Panama_Mediana_euf,
                        Panama_RIQ_euf,
                        Panama_Mediana_Carb,
                        Panama_RIQ_Carb,
                        Panama_Mediana_pp,
                        Panama_RIQ_pp,
                        Panama_Mediana_Corr,
                        Panama_RIQ_Corr
)
dev.off()




generar_Mapa_Estadistico(Choco_Temperatura_median, Color_Temperatura, "Mediana de la Temperatura", "°C", "Choco_Mediana_Temp")
generar_Mapa_Estadistico(Choco_Temperatura_IQR, Color_Temperatura, "RIQ de la Temperatura", "°C", "Choco_RIQ_Temp")
generar_Mapa_Estadistico(Choco_Salinidad_median, Color_Salinidad, "Mediana de la Salinidad", "PSU", "Choco_Mediana_Sal")
generar_Mapa_Estadistico(Choco_Salinidad_IQR, Color_Salinidad, "RIQ de la Salinidad", "PSU", "Choco_RIQ_Sal")
generar_Mapa_Estadistico(Choco_alturas_median, Color_alturas, "Mediana de la alturas de la superficie", "m", "Choco_Mediana_alt")
generar_Mapa_Estadistico(Choco_alturas_IQR, Color_alturas, "RIQ de la alturas de la superficie", "m", "Choco_RIQ_alt")
generar_Mapa_Estadistico(Choco_capaMezcla_median, Color_capaMezcla, "Mediana de la capa mezcla", "m", "Choco_Mediana_Mezcla")
generar_Mapa_Estadistico(Choco_capaMezcla_IQR, Color_capaMezcla, "RIQ de la capa mezcla", "m", "Choco_RIQ_Mezcla")
generar_Mapa_Estadistico(Choco_eufotica_median, Color_eufotica, "Mediana de la prof. eufotica", "m", "Choco_Mediana_euf")
generar_Mapa_Estadistico(Choco_eufotica_IQR, Color_eufotica, "RIQ de la prof. eufotica", "m", "Choco_RIQ_euf")
generar_Mapa_Estadistico(Choco_Carbonzoo_median, Color_Carbonzoo, "Mediana de la carbono zooplancton", expression(g %.% m^-2), "Choco_Mediana_Carb")
generar_Mapa_Estadistico(Choco_Carbonzoo_IQR, Color_Carbonzoo, "RIQ de la Carbonozoo", expression(g %.% m^-2), "Choco_RIQ_Carb")
generar_Mapa_Estadistico(Choco_pp_median, Color_pp, "Mediana de la productividad primaria", expression(mg %.% m^-2 %.% day^-1), "Choco_Mediana_pp")
generar_Mapa_Estadistico(Choco_pp_IQR, Color_pp, "RIQ de la productividad primaria", expression(mg %.% m^-2 %.% day^-1), "Choco_RIQ_pp")
generar_Mapa_Estadistico(Choco_Corrientes_median, Color_velocidad, "Mediana de la Corrientes", expression(m.s^-1), "Choco_Mediana_Corr")
generar_Mapa_Estadistico(Choco_Corrientes_IQR, Color_velocidad, "RIQ de la Corrientes", expression(m.s^-1), "Choco_RIQ_Corr")

png("Choco_Estadisticos_Mapa_01.png", width = 18, height = 40, units = "m", pointsize = 12, bg = "white", res = 300, symbolfamily = "default")
gridExtra::grid.arrange(ncol=2,
                        
                        Choco_Mediana_Temp,
                        Choco_RIQ_Temp,
                        Choco_Mediana_Sal,
                        Choco_RIQ_Sal,
                        Choco_Mediana_alt,
                        Choco_RIQ_alt, 
                        Choco_Mediana_Mezcla, 
                        Choco_RIQ_Mezcla,
                        Choco_Mediana_euf,
                        Choco_RIQ_euf,
                        Choco_Mediana_Carb,
                        Choco_RIQ_Carb,
                        Choco_Mediana_pp,
                        Choco_RIQ_pp,
                        Choco_Mediana_Corr,
                        Choco_RIQ_Corr
)
dev.off()


###Análisis descriptivos####

Panama_Data_Estadisticas<-cbind(Panama_Temperatura_median,
                                Panama_Temperatura_IQR$Height,
                                Panama_Salinidad_median$Height,
                                Panama_Salinidad_IQR$Height,
                                Panama_alturas_median$Height,
                                Panama_alturas_IQR$Height,
                                Panama_capaMezcla_median$Height,
                                Panama_capaMezcla_IQR$Height,
                                Panama_eufotica_median$Height,
                                Panama_eufotica_IQR$Height,
                                Panama_Carbonzoo_median$Height,
                                Panama_Carbonzoo_IQR$Height,
                                Panama_pp_median$Height,
                                Panama_pp_IQR$Height,
                                Panama_Corrientes_median$Height,
                                Panama_Corrientes_IQR$Height)




colnames(Panama_Data_Estadisticas)<-c("longitud","latitud", "Temperatura_median",
                                      "Temperatura_IQR",
                                      "Salinidad_median",
                                      "Salinidad_IQR",
                                      "alturas_median",
                                      "alturas_IQR",
                                      "capaMezcla_median",
                                      "capaMezcla_IQR",
                                      "eufotica_median",
                                      "eufotica_IQR",
                                      "Carbonzoo_median",
                                      "Carbonzoo_IQR",
                                      "pp_median",
                                      "pp_IQR",
                                      "Corrientes_median",
                                      "Corrientes_IQR")

Panama_Data_Estadisticas$Epoca<-"Jet Panama"
Panama_Data_Estadisticas$test<-0




Choco_Data_Estadisticas<-cbind(Choco_Temperatura_median,
                               Choco_Temperatura_IQR$Height,
                               Choco_Salinidad_median$Height,
                               Choco_Salinidad_IQR$Height,
                               Choco_alturas_median$Height,
                               Choco_alturas_IQR$Height,
                               Choco_capaMezcla_median$Height,
                               Choco_capaMezcla_IQR$Height,
                               Choco_eufotica_median$Height,
                               Choco_eufotica_IQR$Height,
                               Choco_Carbonzoo_median$Height,
                               Choco_Carbonzoo_IQR$Height,
                               Choco_pp_median$Height,
                               Choco_pp_IQR$Height,
                               Choco_Corrientes_median$Height,
                               Choco_Corrientes_IQR$Height)




colnames(Choco_Data_Estadisticas)<-c("longitud",
                                     "latitud", 
                                     "Temperatura_median",
                                     "Temperatura_IQR",
                                     "Salinidad_median",
                                     "Salinidad_IQR",
                                     "alturas_median",
                                     "alturas_IQR",
                                     "capaMezcla_median",
                                     "capaMezcla_IQR",
                                     "eufotica_median",
                                     "eufotica_IQR",
                                     "Carbonzoo_median",
                                     "Carbonzoo_IQR",
                                     "pp_median",
                                     "pp_IQR",
                                     "Corrientes_median",
                                     "Corrientes_IQR")


Choco_Data_Estadisticas$Epoca<-"Jet Choco"
Choco_Data_Estadisticas$test<-1


variables_estadistica<-rbind(Choco_Data_Estadisticas, Panama_Data_Estadisticas)





library(readr)
library(dplyr)
library(Rtsne)
library(ggplot2)

# Carga de datos
variables_estadistica<-na.omit(variables_estadistica)

unique(variables_estadistica[,c(3:4)])

tsne <- Rtsne(X = unique(variables_estadistica[c(1:100,24612:24712),c(3, 5,7,9,11,13,15,17)]), is_distance = FALSE, dims = 2, 
              perplexity = 5,
              theta = 0.5, max_iter = 500)

# El objeto devuelto por Rtsne() almacena los valores de las dimensiones en el 
# elemento Y. Como en este caso se ha especificado que la reducción se haga
# a dos dimensiones (k=2), Y tiene solo dos columnas.
head(tsne$Y)
resultados <- as.data.frame(tsne$Y)
colnames(resultados) <- c("dim_1", "dim_2")
transitoria<-unique(variables_estadistica[c(1:100,8612:8712),c(3, 5,7,9,11,13,15,17,19)])
resultados$Epoca <- as.character(transitoria$Epoca)
ggplot(data = resultados, aes(x = dim_1, y = dim_2)) +
  geom_point(aes(color = Epoca)) + 
  theme_bw()




variables_estadistica
wilcox.test(variables_estadistica$Temperatura_median~ variables_estadistica$Epoca
)
variables_estadistica
wilcox.test(variables_estadistica$Salinidad_median~ variables_estadistica$Epoca
)

variables_estadistica
wilcox.test(variables_estadistica$alturas_median~ variables_estadistica$Epoca
)

variables_estadistica
wilcox.test(variables_estadistica$capaMezcla_median~ variables_estadistica$Epoca
)

variables_estadistica
wilcox.test(variables_estadistica$eufotica_median~ variables_estadistica$Epoca
)

variables_estadistica
wilcox.test(variables_estadistica$Carbonzoo_median~ variables_estadistica$Epoca
)

variables_estadistica
wilcox.test(variables_estadistica$pp_median~ variables_estadistica$Epoca
)

variables_estadistica
wilcox.test(variables_estadistica$Corrientes_median~ variables_estadistica$Epoca
)



sesion_info <- devtools::session_info()
dplyr::select(
  tibble::as_tibble(sesion_info$packages),
  c(package, loadedversion, source)
)

library(vegan)


library(ggplot2)
library(GGally)

# Seleccionar las columnas de interés
data <- variables_estadistica[, 3:18]

# Crear el plot de matriz de correlación
ggpairs(data, title = "Matriz de Correlaciones de Variables Estadísticas")


Prueba_MRPP<-mrpp(variables_estadistica[,3:18], 
                  variables_estadistica$Epoca, 
                  permutations = 999, 
                  distance = "euclidean",
                  weight.type = 1, 
                  parallel = getOption("mc.cores"))
