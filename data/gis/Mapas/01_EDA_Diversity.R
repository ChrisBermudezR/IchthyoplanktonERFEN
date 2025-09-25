# Análisis Exploratorio de Datos de Abundancia y Diversidad de la Familia Pontellidae en el Pacífico Colombiano en el año 2019.####



#Carga de los paquetes necesarios para los análisis####
if(!require(vegan))install.packages("vegan")
if(!require(tidyverse))install.packages("tidyverse")
if(!require(hilldiv))install.packages("hilldiv")
if(!require(ggplot2))install.packages("ggplot2")
if(!require(iNEXT))install.packages("iNEXT")
if(!require(tidyr))install.packages("tidyr")
if(!require(sf))install.packages("sf")

#Preparación de Datos####

#Carga de datos de DwC####

direccion_Datos="../Datos/DwC/Occurrence_Pontellidae.csv"

Occ_Dwc<-read.table(direccion_Datos, header = TRUE, sep = ",")

colnames(Occ_Dwc)
Occ_Dwc$datasetID
Occ_Dwc$scientificName
Occ_Dwc$eventID
Occ_Dwc$organismQuantity

#Creación del Data Frame de Análisis####
biolData_lon<-as.data.frame(cbind(Occ_Dwc$recordNumber, Occ_Dwc$decimalLatitude, Occ_Dwc$decimalLongitude, Occ_Dwc$scientificName,Occ_Dwc$organismQuantity))
colnames(biolData_lon)<-c("recordNumber", "decimalLatitude", "decimalLongitude", "scientificName", "organismQuantity")
biolData_lon$recordNumber<-as.factor(biolData_lon$recordNumber)
biolData_lon$scientificName<-as.factor(biolData_lon$scientificName)
biolData_lon$scientificName<-gsub("\\s+", "_", biolData_lon$scientificName)
biolData_lon$organismQuantity<-as.numeric(biolData_lon$organismQuantity)



biolData_lon$Codigo<-gsub("ENFEN-XXII-0", "Choco", biolData_lon$recordNumber) 
biolData_lon$Codigo<-gsub("ENFEN-XXI-0", "Panama", biolData_lon$Codigo)

biolData_lon$Temporada<-gsub("^(Choco).*", "Choco", biolData_lon$Codigo) 
biolData_lon$Temporada<-gsub("^(Panama).*", "Panama", biolData_lon$Temporada)

biolData_lon$Red<-gsub(".*-(\\d+)-.*", "\\1", biolData_lon$Codigo) 


biolData_lon$Estacion<-gsub(".*-(\\d+)", "\\1", biolData_lon$Codigo) 



#Creación de la matrix wide de los datos del densidad####
biolData_wide<-biolData_lon%>%
  tidyr::pivot_wider(names_from = scientificName,
              values_from = organismQuantity)

dim(biolData_wide)

#Datos totales
abundancia_Total<-sort(apply(biolData_wide[,8:29], 2, sum), decreasing = TRUE)

#Datos por temporada####
Choco_long<-biolData_lon%>%subset(Temporada == "Choco")
Panama_long<-biolData_lon%>%subset(Temporada == "Panama")


Jet_Choco<-tapply(Choco_long$organismQuantity, Choco_long$scientificName, sum)
Jet_Panama<-tapply(Panama_long$organismQuantity, Panama_long$scientificName, sum)

Temporadas_total<-as.data.frame(t(rbind(Jet_Panama, Jet_Choco)))

#Datos por estación####

Choco_wide<-biolData_wide%>%subset(Temporada == "Choco")
Panama_wide<-biolData_wide%>%subset(Temporada == "Panama")


Choco_Estaciones<-aggregate(Choco_wide[,8:29],list(Choco_wide$Estacion), sum)
Panama_Estaciones<-aggregate(Panama_wide[,8:29],list(Panama_wide$Estacion), sum)

Choco_Estaciones<-Choco_Estaciones%>%rename(Estaciones =Group.1)
Panama_Estaciones<-Panama_Estaciones%>%rename(Estaciones =Group.1)

Choco_Estaciones$Estaciones<-as.integer(Choco_Estaciones$Estaciones)
Panama_Estaciones$Estaciones<-as.integer(Panama_Estaciones$Estaciones)

Choco_Estaciones$Estaciones<-gsub("(\\d+)", "E\\1_Choco", Choco_Estaciones$Estaciones)
Panama_Estaciones$Estaciones<-gsub("(\\d+)", "E\\1_Panama", Panama_Estaciones$Estaciones)

rownames(Choco_Estaciones)<-Choco_Estaciones$Estaciones
rownames(Panama_Estaciones)<-Panama_Estaciones$Estaciones

Choco_Estaciones<-as.data.frame(t(Choco_Estaciones[,2:23]))
Panama_Estaciones<-as.data.frame(t(Panama_Estaciones[,2:23]))


#Código para suprimir la notación científica
options(scipen=999)

#Carga de los datos para la estimación de los números de Hill####
Hill_temporadas<-read.table("Hill_temporadas.csv", sep=",",header=TRUE)
abundancia_Total<-read.table("Abundancias_2019.csv", sep=",", header=TRUE)
abundancia_Total<-as.data.frame(t(abundancia_Total))

#Gráficas de rango abundancia####
#Sumatoria de todas especies en el muestreo

Total_Rank_data<- rad.lognormal(abundancia_Total$V1)
Choco_Rank_data<- rad.lognormal(t(Temporadas_total$Jet_Choco))
Panama_Rank_data<- rad.lognormal(t(Temporadas_total$Jet_Panama))

Total_Rank_radfit<- radfit(abundancia_Total$V1)
summary(Total_Rank_radfit)
plot(Total_Rank_radfit, ylab="Abundancia", xlab="Rango", main="Chocó")

AIC.rad(Total_Rank_radfit)


Choco_Rank_radfit<- radfit(t(Temporadas_total$Jet_Choco))
Panama_Rank_radfit<- radfit(t(Temporadas_total$Jet_Panama))

plot(Choco_Rank_radfit, ylab="Abundancia", xlab="Rango", main="Chocó")
plot(Panama_Rank_radfit, ylab="Abundancia", xlab="Rango", main="Panamá")

#Gráficas rango-abundancia
Rango_Plot_total<-ggplot()+
  geom_point(aes(y=Total_Rank_data[["y"]], x=seq(1:22)))+
  scale_y_continuous(trans='log10', limits = function(x){c(min(x), ceiling(1000000))})+
  labs(colour = "", subtitle = "Rango - Abundancia", title="Abundancia Total",  x ="Rango", y =expression(paste("log(abundancia) [inv 1000 ", m^-3, "]")),tag="A.")+
  theme_bw()+
  theme(legend.position="bottom", 
        axis.text.y =  element_text(size = 8),
        axis.title =  element_text(size = 8),
        plot.title = element_text(size = 8),
        plot.subtitle = element_text(size = 8),
        plot.caption= element_text(size = 8),
        plot.tag = element_text(size=8))

total_plot<-ggplot()+
  geom_point(aes(y=Total_Rank_radfit[["y"]], x=seq(1:22)))+
  geom_line(aes(y= Total_Rank_radfit[["models"]][["Null"]][["fitted.values"]],x=seq(1:22),color="Null" ))+
  geom_line(aes(y= Total_Rank_radfit[["models"]][["Preemption"]][["fitted.values"]],x=seq(1:22),color="Preemption"))+
  geom_line(aes(y= Total_Rank_radfit[["models"]][["Lognormal"]][["fitted.values"]],x=seq(1:22),color="Lognormal" ))+
  geom_line(aes(y= Total_Rank_radfit[["models"]][["Zipf"]][["fitted.values"]],x=seq(1:22),color="Zipf"))+
  geom_line(aes(y= Total_Rank_radfit[["models"]][["Mandelbrot"]][["fitted.values"]],x=seq(1:22),color="Mandelbrot" ))+
  scale_y_continuous(trans='log10')+
  scale_color_manual(values = c( "#d7191c","#fdae61", "#a6dba0", "#008837", "#2b83ba"),
                     #breaks=c("Jet Chocó", "Null", "Preemption", "Lognormal", "Zipf", "Mandelbrot"),
                     name = " ",
                     labels=c(  "Mandelbrot", "Zipf","Lognormal","Preemption", "Null")
  )+
labs(colour = "", subtitle = "Rango - Densidad", title="Densidad total",  x ="Rango", y =expression(paste("log(Densidad) [inv 1000 ", m^-3, "]")),tag="B.")+
  theme_bw()+
  theme(legend.position="bottom", 
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        plot.title = element_text(size = 8),
        plot.subtitle = element_text(size = 8),
        plot.caption= element_text(size = 8),
        plot.tag = element_text(size=8))


Rango_Plot<-ggplot()+
  geom_point(aes(y=Choco_Rank_data[["y"]], x=seq(1:7), color="Jet Chocó"))+
  #geom_line(aes(y= Sizigia_Rank_fitplot[["models"]][["Lognormal"]][["fitted.values"]],x=seq(1:124) ),color="red")+
  geom_point(aes(y=Panama_Rank_data[["y"]], x=seq(1:21),color="Jet Panamá"))+
  #geom_line(aes(y= Cuadratura_Rank_fitplot[["models"]][["Lognormal"]][["fitted.values"]],x=seq(1:84) ),color="black")+
  scale_y_continuous(trans='log10', limits = function(x){c(min(x), ceiling(1000000))})+
  scale_color_manual(values = c("#2b83ba", "#fdae61"))+
  labs(colour = "", subtitle = "Rango - Densidad", title="Temporadas",  x ="Rango", y =expression(paste("log(Densidad) [inv 1000 ", m^-3, "]")),tag="A.")+
   theme_bw()+
  theme(legend.position="bottom", 
        axis.text.y =  element_text(size = 8),
        axis.title =  element_text(size = 8),
        plot.title = element_text(size = 8),
        plot.subtitle = element_text(size = 8),
        plot.caption= element_text(size = 8),
        plot.tag = element_text(size=8))


choco_plot<-ggplot()+
  geom_point(aes(y=Choco_Rank_radfit[["y"]], x=seq(1:7)))+
  geom_line(aes(y= Choco_Rank_radfit[["models"]][["Null"]][["fitted.values"]],x=seq(1:7),color="Null" ))+
  geom_line(aes(y= Choco_Rank_radfit[["models"]][["Preemption"]][["fitted.values"]],x=seq(1:7),color="Preemption"))+
  geom_line(aes(y= Choco_Rank_radfit[["models"]][["Lognormal"]][["fitted.values"]],x=seq(1:7),color="Lognormal" ))+
  geom_line(aes(y= Choco_Rank_radfit[["models"]][["Zipf"]][["fitted.values"]],x=seq(1:7),color="Zipf"))+
  geom_line(aes(y= Choco_Rank_radfit[["models"]][["Mandelbrot"]][["fitted.values"]],x=seq(1:7),color="Mandelbrot" ))+
  scale_y_continuous(trans='log10')+
  scale_color_manual(values = c( "#d7191c","#fdae61", "#a6dba0", "#008837", "#2b83ba"),
                     #breaks=c("Jet Chocó", "Null", "Preemption", "Lognormal", "Zipf", "Mandelbrot"),
                     name = " ",
                     labels=c(  "Mandelbrot", "Zipf","Lognormal","Preemption", "Null")
  )+
  
  labs(colour = "", subtitle = "Rango - Densidad", title="Jet Chocó",  x ="Rango", y =expression(paste("log(Densidad) [inv 1000 ", m^-3, "]")),tag="B.")+
   theme_bw()+
  theme(legend.position="bottom", 
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        plot.title = element_text(size = 8),
        plot.subtitle = element_text(size = 8),
        plot.caption= element_text(size = 8),
        plot.tag = element_text(size=8))


panama_plot<-ggplot()+
  geom_point(aes(y=Panama_Rank_radfit[["y"]], x=seq(1:21)))+
  geom_line(aes(y= Panama_Rank_radfit[["models"]][["Null"]][["fitted.values"]],x=seq(1:21),color="Null" ))+
  geom_line(aes(y= Panama_Rank_radfit[["models"]][["Preemption"]][["fitted.values"]],x=seq(1:21),color="Preemption"))+
  geom_line(aes(y= Panama_Rank_radfit[["models"]][["Lognormal"]][["fitted.values"]],x=seq(1:21),color="Lognormal" ))+
  geom_line(aes(y= Panama_Rank_radfit[["models"]][["Zipf"]][["fitted.values"]],x=seq(1:21),color="Zipf"))+
  geom_line(aes(y= Panama_Rank_radfit[["models"]][["Mandelbrot"]][["fitted.values"]],x=seq(1:21),color="Mandelbrot" ))+
  scale_y_continuous(trans='log10')+
  scale_color_manual(values = c( "#d7191c","#fdae61", "#a6dba0", "#008837", "#2b83ba"),
                     #breaks=c("Jet Chocó", "Null", "Preemption", "Lognormal", "Zipf", "Mandelbrot"),
                     name = " ",
                     labels=c(  "Mandelbrot", "Zipf","Lognormal","Preemption", "Null")
  )+
  
  labs(colour = "", subtitle = "Rango - Densidad", title="Jet Panamá",  x ="Rango", y =expression(paste("log(Densidad) [inv 1000 ", m^-3, "]")),tag="C.")+
  theme_bw()+
  theme(legend.position="bottom", 
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        plot.title = element_text(size = 8),
        plot.subtitle = element_text(size = 8),
        plot.caption= element_text(size = 8),
        plot.tag = element_text(size=8))

g1<-gridExtra::arrangeGrob(Rango_Plot)
g2<-gridExtra::arrangeGrob(choco_plot,
                           panama_plot,
                           ncol=2)

#Exportación de las gráficas
png("Curvas_RangoAbundancia_Temporadas.png", width = 2000, height = 2000, units = "px", res="300", pointsize = 11)
gridExtra::grid.arrange(g1,
                        g2,
                        ncol=1)
dev.off()


 

#Calculo de los números de hill sin extrapolación####

#Calculo de los números de Hill Observados por temporada

 D0_Temporadas_total<-hilldiv::hill_div(Temporadas_total, qvalue = 0)
 D1_Temporadas_total<-hilldiv::hill_div(Temporadas_total, qvalue = 1)
 D2_Temporadas_total<-hilldiv::hill_div(Temporadas_total, qvalue = 2)
 
 Div_hill_Total<-base::list(D0_Temporadas_total,D1_Temporadas_total,D2_Temporadas_total)
 Div_hill_Total_df<-base::as.data.frame(Div_hill_Total)
 base::colnames(Div_hill_Total_df)<-c("q0", "q1","q2")
 
 Div_hill_Total_df_trans<-as.data.frame(t(Div_hill_Total_df))
 Div_hill_Total_df_trans$Orden<-c(0,1,2)
 
 
 Rango_DivPlot <- ggplot2::ggplot() +
   geom_line(data = Div_hill_Total_df_trans, aes(x = Orden, y = Jet_Choco ), color = "blue", lwd = 0.5, linetype = 1) +
   geom_point(data = Div_hill_Total_df_trans, aes(x = Orden, y = Jet_Choco ), color = "blue", size =3) +
    geom_line(data = Div_hill_Total_df_trans, aes(x = Orden, y = Jet_Panama ), color = "red", lwd = 0.5, linetype = 1) +
   geom_point(data = Div_hill_Total_df_trans, aes(x = Orden, y = Jet_Panama ), color = "red", size =3) +
    labs(
     title = "Perfiles de diversidad",
     x = "Orden",
     y = "Diversidad",
     color = "Variable") +
   scale_color_manual(values = c("Jet_Choco" = "blue", "Jet_Panama" = "red")) +
   annotate("text", x = 0.1, y = 8, label = "Jet Chocó", size = 5, color = "black") +
   annotate("text", x = 0.1, y = 22, label = "Jet Panamá", size = 5, color = "black") +
   theme_minimal()
 
 
 
 png("Perfil_Diversidad_Temporadas.png", width = 2000, height = 2000, units = "px", res="300", pointsize = 11)
 Rango_DivPlot
 dev.off()
 
 
 #Calculo diversidad estaciones
 
 #Panamá

 
 D0_Panama_Estaciones<-hilldiv::hill_div(Panama_Estaciones, qvalue = 0)
 D1_Panama_Estaciones<-hilldiv::hill_div(Panama_Estaciones, qvalue = 1)
 D2_Panama_Estaciones<-hilldiv::hill_div(Panama_Estaciones, qvalue = 2)
 
 Div_Panama_Estaciones<-base::list(D0_Panama_Estaciones,D1_Panama_Estaciones,D2_Panama_Estaciones)
 Div_Panama_Estaciones_df<-base::as.data.frame(Div_Panama_Estaciones)
 base::colnames(Div_Panama_Estaciones_df)<-c("q0", "q1","q2")
 
 Div_Panama_Estaciones_df_trans<-as.data.frame(t(Div_Panama_Estaciones_df))

 
 lista_Panama<-vector("list", length = 3)
 
 panmQ0<-as.vector(Div_Panama_Estaciones_df_trans[1,])
 panmQ1<-as.vector(Div_Panama_Estaciones_df_trans[2,])
 panmQ2<-as.vector(Div_Panama_Estaciones_df_trans[3,])
 
 lista_Panama[[1]]<-panmQ0
 lista_Panama[[2]]<-panmQ1
 lista_Panama[[3]]<-panmQ2
 
 
 
 
 for(i in 1:length(lista_Panama[[1]])){
   if(is.na(lista_Panama[[1]][[i]]) == "TRUE" | lista_Panama[[1]][[i]] == 22){
     lista_Panama[[1]][[i]]<-0
   }
   
 }
 
 
 for(i in 1:length(lista_Panama[[2]])){
   if(is.na(lista_Panama[[2]][[i]]) == "TRUE" | lista_Panama[[2]][[i]] == 22){
     lista_Panama[[2]][[i]]<-0
   }
   
 }
 
 for(i in 1:length(lista_Panama[[3]])){
   if(is.na(lista_Panama[[3]][[i]]) == "TRUE" | lista_Panama[[3]][[i]] == 22){
     lista_Panama[[3]][[i]]<-0
   }
   
 }
 
 panmQ0<-as.data.frame(lista_Panama[1])
 panmQ1<-as.data.frame(lista_Panama[2])
 panmQ2<-as.data.frame(lista_Panama[2])
 
 
 Div_Panama_Estaciones_df_trans<-rbind(panmQ0, panmQ1, panmQ2)
 
 
 datos_Diversidad_Panama<-as.data.frame(t(Div_Panama_Estaciones_df_trans))
 colnames(datos_Diversidad_Panama)<-c("q0", "q1", "q2")
 datos_Diversidad_Panama <- tibble::rownames_to_column(datos_Diversidad_Panama, var = "Panama")

 coordenadas<-read.table("Coordenadas_Estaciones.csv", sep=",", header=TRUE)
 
 datos_Diversidad_Panama<-merge(datos_Diversidad_Panama, coordenadas, by="Panama")
 datos_Diversidad_Panama<-datos_Diversidad_Panama[,1:6]
 
 #Panamá

 
 D0_Choco_Estaciones<-hilldiv::hill_div(Choco_Estaciones, qvalue = 0)
 D1_Choco_Estaciones<-hilldiv::hill_div(Choco_Estaciones, qvalue = 1)
 D2_Choco_Estaciones<-hilldiv::hill_div(Choco_Estaciones, qvalue = 2)
 
 Div_Choco_Estaciones<-base::list(D0_Choco_Estaciones,D1_Choco_Estaciones,D2_Choco_Estaciones)
 Div_Choco_Estaciones_df<-base::as.data.frame(Div_Choco_Estaciones)
 base::colnames(Div_Choco_Estaciones_df)<-c("q0", "q1","q2")
 
 Div_Choco_Estaciones_df_trans<-as.data.frame(t(Div_Choco_Estaciones_df))
 
 
 lista_Choco<-vector("list", length = 3)
 
 chocQ0<-as.vector(Div_Choco_Estaciones_df_trans[1,])
 chocQ1<-as.vector(Div_Choco_Estaciones_df_trans[2,])
 chocQ2<-as.vector(Div_Choco_Estaciones_df_trans[3,])
 
 lista_Choco[[1]]<-chocQ0
 lista_Choco[[2]]<-chocQ1
 lista_Choco[[3]]<-chocQ2
 
 
 
 
 for(i in 1:length(lista_Choco[[1]])){
   if(is.na(lista_Choco[[1]][[i]]) == "TRUE" | lista_Choco[[1]][[i]] == 22){
     lista_Choco[[1]][[i]]<-0
   }
   
 }
 
 
 for(i in 1:length(lista_Choco[[2]])){
   if(is.na(lista_Choco[[2]][[i]]) == "TRUE" | lista_Choco[[2]][[i]] == 22){
     lista_Choco[[2]][[i]]<-0
   }
   
 }
 
 for(i in 1:length(lista_Choco[[3]])){
   if(is.na(lista_Choco[[3]][[i]]) == "TRUE" | lista_Choco[[3]][[i]] == 22){
     lista_Choco[[3]][[i]]<-0
   }
   
 }
 
 chocQ0<-as.data.frame(lista_Choco[1])
 chocQ1<-as.data.frame(lista_Choco[2])
 chocQ2<-as.data.frame(lista_Choco[2])
 
 
 Div_Choco_Estaciones_df_trans<-rbind(chocQ0, chocQ0, chocQ2)
 
 
 datos_Diversidad_Choco<-as.data.frame(t(Div_Choco_Estaciones_df_trans))
 colnames(datos_Diversidad_Choco)<-c("q0", "q1", "q2")
 datos_Diversidad_Choco <- tibble::rownames_to_column(datos_Diversidad_Choco, var = "Choco")
 
 coordenadas<-read.table("Coordenadas_Estaciones.csv", sep=",", header=TRUE)
 
 datos_Diversidad_Choco<-merge(datos_Diversidad_Choco, coordenadas, by="Choco")
 datos_Diversidad_Choco<-datos_Diversidad_Choco[,1:6]
 
 
 ### Unión de datos de diversidad####
 

 
 colnames(datos_Diversidad_Panama)<-c("Codigo",    
                                     "q0",
                                     "q1",   
                                     "q2",
                                     "Latitud",
                                     "Longitud")
 
 
 #Panama
 datos_Diversidad_Panama_sf<-st_as_sf(datos_Diversidad_Panama, coords=c("Longitud", "Latitud"))
 
 pred_rem_Panama<-list.files(path="../Datos/Datos_Externos/layers/Panama",pattern = ".gpkg$", full.names = TRUE)
 predictors_names<-list.files(path="../Datos/Datos_Externos/layers/Panama", pattern = ".gpkg$", full.names = FALSE)
 panamaLayers<-raster::stack(pred_rem_Panama)
 names(panamaLayers)<-c(
   "alturas_IQR",      "alturas_median" ,   
   "capaMezcla_IQR",     "capaMezcla_median", 
   "CarbonZoo_IQR",      "CarbonZoo_median",  
   "Corrientes_IQR",     "Corrientes_median", 
   "eufotica_IQR",       "eufotica_median",   
   "pp_IQR",             "pp_median",         
   "salinidad_IQR",      "salinidad_median",  
   "Temperatura_IQR",    "Temperatura_median"
   
 )
Diversidad_Panama<- raster::extract(panamaLayers, datos_Diversidad_Panama_sf) %>% as.data.frame() 


colnames(datos_Diversidad_Choco)<-c("Codigo",    
                                    "q0",
                                    "q1",   
                                    "q2",
                                    "Latitud",
                                    "Longitud")

pred_rem_Choco<-list.files(path="../Datos/Datos_Externos/layers/Choco",pattern = ".gpkg$", full.names = TRUE)
predictors_names<-list.files(path="../Datos/Datos_Externos/layers/Choco", pattern = ".gpkg$", full.names = FALSE)
ChocoLayers<-raster::stack(pred_rem_Choco)

names(ChocoLayers)<-c(
  "alturas_IQR",      "alturas_median" ,   
  "capaMezcla_IQR",     "capaMezcla_median", 
  "CarbonZoo_IQR",      "CarbonZoo_median",  
  "Corrientes_IQR",     "Corrientes_median", 
  "eufotica_IQR",       "eufotica_median",   
  "pp_IQR",             "pp_median",         
  "salinidad_IQR",      "salinidad_median",  
  "Temperatura_IQR",    "Temperatura_median"
  
)

datos_Diversidad_Choco_sf<-st_as_sf(datos_Diversidad_Choco, coords=c("Longitud", "Latitud"))

Diversidad_Choco<- raster::extract(ChocoLayers, datos_Diversidad_Choco_sf) %>% as.data.frame() 


Diversidad_Total_df<-rbind(cbind(datos_Diversidad_Choco,Diversidad_Choco),cbind(datos_Diversidad_Panama,Diversidad_Panama))

write.csv(Diversidad_Total_df, "../Modelacion/Diversidad_Total_df.csv", row.names = FALSE)

#dataTotal<-dataTotal[,5:13]
Diversidad_Total_df$q0<-as.integer(Diversidad_Total_df$q0)
Diversidad_Total_df$q1<-as.numeric(Diversidad_Total_df$q1)
Diversidad_Total_df$q2<-as.numeric(Diversidad_Total_df$q2)

###Autocorrelacion

library(spdep)

nb <- dnearneigh(cbind(Diversidad_Total_df$Longitud[1:32], Diversidad_Total_df$Latitud[1:32]), 0, 2) # 0 indica que no se incluya el punto en sí mismo como vecino, 2 es la distancia máxima

w <- nb2listw(nb)

sp.correlogram(nb, Diversidad_Total_df$q0[1:32], order = 8, method = "corr",
               style = "W", randomisation = TRUE, zero.policy = NULL, spChk=NULL)


moran.test(datos$variable, sp = spdep::spgeom(datos))




coordenadas_Choco<-as.data.frame(cbind(datos_Diversidad_Choco$Longitud, datos_Diversidad_Choco$Latitud))
colnames(coordenadas_Choco)<-c("Longitud","Latitud")
distmat_Choco <- as.matrix(dist(coordenadas_Choco))
maxdist_Choco <- 2/3 * max(distmat_Choco)

library(pgirmess)
#correlog from pgirmess
correlog.pgirmess <- pgirmess::correlog(coordenadas_Choco, Diversidad_Total_df[1:32,2], method =
                                          "Moran", nbclass = 10, alternative = "two.sided")
#summary
head(round(correlog.pgirmess, 2))

plot(correlog.pgirmess)
abline(h = 0)


coordenadas_Panama<-as.data.frame(cbind(datos_Diversidad_Panama$Longitud, datos_Diversidad_Panama$Latitud))
colnames(coordenadas_Panama)<-c("Longitud","Latitud")
distmat_Panama <- as.matrix(dist(coordenadas_Panama))
maxdist_Panama <- 2/3 * max(distmat_Panama)

library(pgirmess)
#correlog from pgirmess
correlog.pgirmess <- pgirmess::correlog(coordenadas_Panama, Diversidad_Total_df[33:55,2], method =
                                          "Moran", nbclass = 30, alternative = "two.sided")
#summary
head(round(correlog.pgirmess, 2))

plot(correlog.pgirmess)
abline(h = 0)


 ###Mapas####
 
 CPC<-sf::st_read("./Capas_Sig/CPC.shp") # read shapefile CPC
 Colombia<-sf::st_read("./Capas_Sig/Colombia.shp") # read shapefile Countries
 Paises<-sf::st_read("./Capas_Sig/Paises.shp") # read shapefile Countries
 
 
 
 
 plot(Colombia)
 

 

 
 Mapas_graficas <- function(data, variable, titulo, subtitulo, leyenda) {
   ggplot() +
     geom_sf(data = CPC, color = "blue", linetype = 2, linewidth = 0.5) +
     geom_sf(data = Paises, colour = "black", fill = "lightgrey") +
     geom_point(data = data, aes(x = Longitud, y = Latitud, size = variable), colour = "red") +
     coord_sf(xlim = c(-87, -77), ylim = c(1, 8), expand = FALSE) +
     labs(
       title = titulo,
       subtitle = subtitulo,
       x = "Longitude",
       y = "Latitude",
       size = leyenda
     ) +
     scale_size_manual(values = c("1" = 1, 
                                  "2" = 2,
                                  "3" = 3,
                                  "4" = 4,
                                  "5" = 5,
                                  "6" = 6,
                                  "7" = 7,
                                  "8" = 8,
                                  "9" = 9,
                                  "10" = 10)) +  # Ajusta los tamaños según las categorías
     theme_bw() +
     theme(
       plot.title = element_text(size = 12, face = "italic", color = "black"),
       axis.title = element_text(face = "bold", color = "black")
     )
 }
 
 options(scipen=9999)
 
 Choco_q0_ss<-subset(datos_Diversidad_Choco,round(datos_Diversidad_Choco$q0)>0)
 Choco_q0<- Mapas_graficas(Choco_q0_ss, as.factor(round(Choco_q0_ss$q0)), 
                           expression(paste(bold("Jet de Chocó"))),
                           expression(paste(bold(" "))), 
                           expression(paste(bold("q0"))))
 
 Choco_q1_ss<-subset(datos_Diversidad_Choco,round(datos_Diversidad_Choco$q1)>0)
 Choco_q1<-Mapas_graficas(Choco_q1_ss,  as.factor(round(Choco_q1_ss$q1)), 
                expression(paste(bold(" "))),
                expression(paste(bold(" "))), 
                expression(paste(bold("q1"))))
 
 Choco_q2_ss<-subset(datos_Diversidad_Choco,round(datos_Diversidad_Choco$q2)>0)
 Choco_q2<-Mapas_graficas(Choco_q2_ss,  as.factor(round(Choco_q2_ss$q2)), 
                expression(paste(bold(" "))),
                expression(paste(bold(" "))), 
                expression(paste(bold("q2"))))

 Panama_q0<-Mapas_graficas(datos_Diversidad_Panama,  as.factor(round(datos_Diversidad_Panama$q0)), 
                expression(paste(bold("Jet de Panamá"))),
                expression(paste(bold(" "))), 
                expression(paste(bold("q0"))))
 Panama_q1<-Mapas_graficas(datos_Diversidad_Panama,  as.factor(round(datos_Diversidad_Panama$q1)), 
                expression(paste(bold(" "))),
                expression(paste(bold(" "))), 
                expression(paste(bold("q1"))))
 Panama_q2<-Mapas_graficas(datos_Diversidad_Panama,  as.factor(round(datos_Diversidad_Panama$q2)), 
                expression(paste(bold(" "))),
                expression(paste(bold(" "))), 
                expression(paste(bold("q2"))))
 
 

 png(filename="Diversidad_Pontellidae_Mapa.png", height = 25, width =  25, units = "cm", res = 300, pointsize = 12)
 cowplot::plot_grid(labels=c("A","B", "C", "D", "E", "F"),
                    Panama_q0,
                    Choco_q0,
                    Panama_q1,
                    Choco_q1,
                    Panama_q2,
                    Choco_q2,
                    nrow=3,
                    ncol=2)
 dev.off()
 
 ###Análisis espacial####
 

# Diversidad
 library(spdep)
 library(ape)
 
 
 
 coordinates_Choco <- cbind(datos_Diversidad_Choco$Longitud, datos_Diversidad_Choco$Latitud)
 nb_Choco <- spdep::dnearneigh(coordinates_Choco, 0, 5)  # Define vecinos dentro de un radio de 5 unidades
 listw_Choco <- spdep::nb2listw(nb_Choco, style = "W")
 
 
 
 # Calcular el índice de Moran para q0
 moran_test_Choco_q0<- spdep::moran.test(datos_Diversidad_Choco$q0, listw_Choco)
 moran_test_Choco_q1<- spdep::moran.test(datos_Diversidad_Choco$q1, listw_Choco)
 moran_test_Choco_q2<- spdep::moran.test(datos_Diversidad_Choco$q2, listw_Choco)
 

 

 coordinates_Panama <- cbind(datos_Diversidad_Panama$Longitud, datos_Diversidad_Panama$Latitud)
 nb_Panama <- spdep::dnearneigh(coordinates_Panama, 0, 5)  # Define vecinos dentro de un radio de 5 unidades
 listw_Panama <- spdep::nb2listw(nb_Panama, style = "W")
 
 # Calcular el índice de Moran para q0
 moran_test_Panama_q0<- spdep::moran.test(datos_Diversidad_Panama$q0, listw_Panama)
 moran_test_Panama_q1<- spdep::moran.test(datos_Diversidad_Panama$q1, listw_Panama)
 moran_test_Panama_q2<- spdep::moran.test(datos_Diversidad_Panama$q2, listw_Panama)
 
 ##MRPP Diversidad
 datos_Diversidad_Panama$temporada<-"Panama"
 datos_Diversidad_Choco$temporada<-"Choco"
 
 datos_Diversidad_Total<- rbind(datos_Diversidad_Panama, datos_Diversidad_Choco)
 
 
 vegan::mrpp(datos_Diversidad_Total[,2:4], datos_Diversidad_Total$temporada)
 
 
 
 ###Abundancia
 
 #Autocorrelación Abundancia Chocó####
 
 
 Mapas_graficas_Abundancia <- function(data, variable, titulo, subtitulo, leyenda) {
   ggplot() +
     geom_sf(data = CPC, color = "blue", linetype = 2, linewidth = 0.5) +
     geom_sf(data = Paises, colour = "black", fill = "lightgrey") +
     geom_point(data = data, aes(x = Longitud, y = Latitud, size = variable), colour = "red") +
     coord_sf(xlim = c(-87, -77), ylim = c(1, 8), expand = FALSE) +
     labs(
       title = titulo,
       subtitle = subtitulo,
       x = "Longitude",
       y = "Latitude",
       size = leyenda
     ) +
     
     theme_bw() +
     theme(
       plot.title = element_text(size = 12, face = "italic", color = "black"),
       axis.title = element_text(face = "bold", color = "black")
     )
 }
 
 
Densidad_Autocorrelacion<-readxl::read_excel("Densidad_pontellidae.xlsx", sheet="Estaciones_Total")


Choco_Densidad<-subset(Densidad_Autocorrelacion, Temporada=="Jet_Choco")
coordinates_Choco <- cbind(Choco_Densidad$Longitud, Choco_Densidad$Latitud)
nb_Choco <- spdep::dnearneigh(coordinates_Choco, 0, 5)  # Define vecinos dentro de un radio de 5 unidades
listw_Choco <- spdep::nb2listw(nb_Choco, style = "W")

colnames(Choco_Densidad)


moran_test_Choco_Labidocera_acuta<- spdep::moran.test(Choco_Densidad$`Labidocera acuta`, listw_Choco)
moran_test_Choco_Labidocera_detruncata<- spdep::moran.test(Choco_Densidad$`Labidocera detruncata`, listw_Choco) #Autocorrelacion Espacial
moran_test_Choco_Pontella_mimocerami<- spdep::moran.test(Choco_Densidad$`Pontella mimocerami`, listw_Choco)
moran_test_Choco_Pontella_valida<- spdep::moran.test(Choco_Densidad$`Pontella valida`, listw_Choco)
moran_test_Choco_Pontella_fera<- spdep::moran.test(Choco_Densidad$`Pontella fera`, listw_Choco)
moran_test_Choco_Pontellopsis_regalis<- spdep::moran.test(Choco_Densidad$`Pontellopsis regalis`, listw_Choco)
moran_test_Choco_Pontellina_morii<- spdep::moran.test(Choco_Densidad$`Pontellina morii`, listw_Choco)

Choco_total_densidad<-as.data.frame(rowSums(Choco_Densidad[,5:26]))
Choco_total_densidad<-cbind(Choco_Densidad[,1:4], Choco_total_densidad)
colnames(Choco_total_densidad)<-c("Temporada",
                                  "Latitud",
                                  "Longitud",   
                                  "Estaciones",
                                  "Densidad total"
)

moran_test_Choco_total_densidad<- spdep::moran.test(Choco_total_densidad$`Densidad total`, listw_Choco)



Choco_DenTotal<-subset(Choco_total_densidad,round(Choco_total_densidad$`Densidad total`)>0)
Choco_Densidad_total<- Mapas_graficas_Abundancia(Choco_DenTotal, round(Choco_DenTotal$`Densidad total`), 
                                                   expression(paste(bold("Jet de Chocó - Densidad total"))),
                                                   expression(paste(bold(" "))), 
                                                   expression(paste("[inv 1000 ", m^-3, "]")))

png(filename="Choco_Densidad_total.png", height = 15, width =  25, units = "cm", res = 300, pointsize = 12)
Choco_Densidad_total
dev.off()


Choco_Lacuta<-subset(Choco_Densidad,round(Choco_Densidad$`Labidocera acuta`)>0)
Choco_Labidocera_acuta<- Mapas_graficas_Abundancia(Choco_Lacuta, round(Choco_Lacuta$`Labidocera acuta`), 
                                                   expression(paste(bold("Jet de Chocó"), " - ",italic("Labidocera acuta"))),
                          expression(paste(bold(" "))), 
                          expression(paste("[inv 1000 ", m^-3, "]")))


Choco_Ldetruncata<-subset(Choco_Densidad,round(Choco_Densidad$`Labidocera detruncata`)>0)
Choco_Labidocera_detruncata<- Mapas_graficas_Abundancia(Choco_Ldetruncata, round(Choco_Ldetruncata$`Labidocera detruncata`), 
                                                        expression(paste(bold("Jet de Chocó")," - ", italic("Labidocera detruncata"))),
                                                   expression(paste(bold(" "))), 
                                                   expression(paste("[inv 1000 ", m^-3, "]")))

Choco_Pmimocerami<-subset(Choco_Densidad,round(Choco_Densidad$`Pontella mimocerami`)>0)
Choco_Pontella_mimocerami<- Mapas_graficas_Abundancia(Choco_Pmimocerami, round(Choco_Pmimocerami$`Pontella mimocerami`), 
                                                      expression(paste(bold("Jet de Chocó")," - ", italic("Pontella mimocerami"))),
                                                      expression(paste(bold(" "))), 
                                                      expression(paste("[inv 1000 ", m^-3, "]")))

Choco_Pregalis<-subset(Choco_Densidad,round(Choco_Densidad$`Pontellopsis regalis`)>0)
Choco_Pontellopsis_regalis<- Mapas_graficas_Abundancia(Choco_Pregalis, round(Choco_Pregalis$`Pontellopsis regalis`), 
                                                      expression(paste(bold("Jet de Chocó")," - ", italic("Pontellopsis regalis"))),
                                                      expression(paste(bold(" "))), 
                                                      expression(paste("[inv 1000 ", m^-3, "]")))

Choco_Pmorii<-subset(Choco_Densidad,round(Choco_Densidad$`Pontellina morii`)>0)
Choco_Pontellina_morii<- Mapas_graficas_Abundancia(Choco_Pmorii, round(Choco_Pmorii$`Pontellina morii`), 
                                                       expression(paste(bold("Jet de Chocó")," - ", italic("Pontellina morii"))),
                                                       expression(paste(bold(" "))), 
                                                       expression(paste("[inv 1000 ", m^-3, "]")))

Choco_Pvalida<-subset(Choco_Densidad,round(Choco_Densidad$`Pontella valida`)>0)
Choco_Pontella_valida<- Mapas_graficas_Abundancia(Choco_Pvalida, round(Choco_Pvalida$`Pontella valida`), 
                                                   expression(paste(bold("Jet de Chocó")," - ", italic("Pontella valida"))),
                                                   expression(paste(bold(" "))), 
                                                   expression(paste("[inv 1000 ", m^-3, "]")))

Choco_Pfera<-subset(Choco_Densidad,round(Choco_Densidad$`Pontella fera`)>0)
Choco_Pontella_fera<- Mapas_graficas_Abundancia(Choco_Pfera, round(Choco_Pfera$`Pontella fera`), 
                                                  expression(paste(bold("Jet de Chocó")," - ", italic("Pontella fera"))),
                                                  expression(paste(bold(" "))), 
                                                  expression(paste("[inv 1000 ", m^-3, "]")))




png(filename="Abundancias_especies_Choco.png", height = 30, width =  25, units = "cm", res = 300, pointsize = 12)
cowplot::plot_grid(labels=c("A","B", "C", "D", "E", "F", "G"),
                   Choco_Labidocera_acuta,
                   Choco_Labidocera_detruncata,
                   Choco_Pontella_fera,
                   Choco_Pontella_mimocerami,
                   Choco_Pontella_valida,
                   Choco_Pontellina_morii,
                   Choco_Pontellopsis_regalis,
                 nrow=4,
                   ncol=2)
dev.off()






Densidad_Autocorrelacion<-readxl::read_excel("Densidad_pontellidae.xlsx", sheet="Estaciones_Total")


Panama_Densidad<-subset(Densidad_Autocorrelacion, Temporada=="Jet_Panama")
coordinates_Panama <- cbind(Panama_Densidad$Longitud, Panama_Densidad$Latitud)
nb_Panama <- spdep::dnearneigh(coordinates_Panama, 0, 5)  # Define vecinos dentro de un radio de 5 unidades
listw_Panama <- spdep::nb2listw(nb_Panama, style = "W")

colnames(Panama_Densidad)

moran_test_Panama_Calanopia_elliptica<- spdep::moran.test(Panama_Densidad$`Calanopia elliptica`, listw_Panama)
moran_test_Panama_Calanopia_minor<- spdep::moran.test(Panama_Densidad$`Calanopia minor`, listw_Panama)

moran_test_Panama_Pontella_atlantica<- spdep::moran.test(Panama_Densidad$`Pontella atlantica`, listw_Panama)
moran_test_Panama_Pontella_danae<- spdep::moran.test(Panama_Densidad$`Pontella danae`, listw_Panama)
moran_test_Panama_Pontella_fera<- spdep::moran.test(Panama_Densidad$`Pontella fera`, listw_Panama)
moran_test_Panama_Pontella_mimocerami<- spdep::moran.test(Panama_Densidad$`Pontella mimocerami`, listw_Panama) #Autocorrelacion
moran_test_Panama_Pontella_spinicauda<- spdep::moran.test(Panama_Densidad$`Pontella spinicauda`, listw_Panama)
moran_test_Panama_Pontella_spinipes<- spdep::moran.test(Panama_Densidad$`Pontella spinipes`, listw_Panama)

moran_test_Panama_Pontellina_morii<- spdep::moran.test(Panama_Densidad$`Pontellina morii`, listw_Panama)
moran_test_Panama_Pontellina_plumata<- spdep::moran.test(Panama_Densidad$`Pontellina plumata`, listw_Panama)

moran_test_Panama_Pontellopsis_armata<- spdep::moran.test(Panama_Densidad$`Pontellopsis armata`, listw_Panama)
moran_test_Panama_Pontellopsis_perspicax<- spdep::moran.test(Panama_Densidad$`Pontellopsis perspicax`, listw_Panama)
moran_test_Panama_Pontellopsis_regalis<- spdep::moran.test(Panama_Densidad$`Pontellopsis regalis`, listw_Panama)
moran_test_Panama_Pontellopsis_lubbocki<- spdep::moran.test(Panama_Densidad$`Pontellopsis lubbocki`, listw_Panama)

moran_test_Panama_Labidocera_acuta<- spdep::moran.test(Panama_Densidad$`Labidocera acuta`, listw_Panama)
moran_test_Panama_Labidocera_acutifrons<- spdep::moran.test(Panama_Densidad$`Labidocera acutifrons`, listw_Panama)
moran_test_Panama_Labidocera_aestiva<- spdep::moran.test(Panama_Densidad$`Labidocera aestiva`, listw_Panama)
moran_test_Panama_Labidocera_detruncata<- spdep::moran.test(Panama_Densidad$`Labidocera detruncata`, listw_Panama) 
moran_test_Panama_Labidocera_nerii<- spdep::moran.test(Panama_Densidad$`Labidocera nerii`, listw_Panama)
moran_test_Panama_Labidocera_churaumi<- spdep::moran.test(Panama_Densidad$`Labidocera churaumi`, listw_Panama)
moran_test_Panama_Labidocera_sinilobata<- spdep::moran.test(Panama_Densidad$`Labidocera sinilobata`, listw_Panama)


Panama_total_densidad<-as.data.frame(rowSums(Panama_Densidad[,5:26]))
Panama_total_densidad<-cbind(Panama_Densidad[,1:4], Panama_total_densidad)
colnames(Panama_total_densidad)<-c("Temporada",
                                  "Latitud",
                                  "Longitud",   
                                  "Estaciones",
                                  "Densidad total"
)

moran_test_Panama_total_densidad<- spdep::moran.test(Panama_total_densidad$`Densidad total`, listw_Panama)



Panama_DenTotal<-subset(Panama_total_densidad,round(Panama_total_densidad$`Densidad total`)>0)
Panama_Densidad_total<- Mapas_graficas_Abundancia(Panama_DenTotal, round(Panama_DenTotal$`Densidad total`), 
                                                 expression(paste(bold("Jet de Panamá - Densidad total"))),
                                                 expression(paste(bold(" "))), 
                                                 expression(paste("[inv 1000 ", m^-3, "]")))

options(scipen = 10)

png(filename="Panama_Densidad_total.png", height = 15, width =  25, units = "cm", res = 300, pointsize = 12)
Panama_Densidad_total
dev.off()





Panama_Celliptica<-subset(Panama_Densidad,round(Panama_Densidad$`Calanopia elliptica`)>0)
Panama_Calanopia_elliptica<- Mapas_graficas_Abundancia(Panama_Celliptica, round(Panama_Celliptica$`Calanopia elliptica`), 
                                                    expression(paste(bold("Jet de Panamá"), " - ",italic("Calanopia elliptica"))),
                                                    expression(paste(bold(" "))), 
                                                    expression(paste("[inv 1000 ", m^-3, "]")))

Panama_Cminor<-subset(Panama_Densidad,round(Panama_Densidad$`Calanopia minor`)>0)
Panama_Calanopia_minor<- Mapas_graficas_Abundancia(Panama_Cminor, round(Panama_Cminor$`Calanopia minor`), 
                                                       expression(paste(bold("Jet de Panamá"), " - ",italic("Calanopia minor"))),
                                                       expression(paste(bold(" "))), 
                                                       expression(paste("[inv 1000 ", m^-3, "]")))


Panama_Patlantica<-subset(Panama_Densidad,round(Panama_Densidad$`Pontella atlantica`)>0)
Panama_Pontella_atlantica<- Mapas_graficas_Abundancia(Panama_Patlantica, round(Panama_Patlantica$`Pontella atlantica`), 
                                                       expression(paste(bold("Jet de Panamá")," - ", italic("Pontella atlantica"))),
                                                       expression(paste(bold(" "))), 
                                                       expression(paste("[inv 1000 ", m^-3, "]")))

Panama_Pdanae<-subset(Panama_Densidad,round(Panama_Densidad$`Pontella danae`)>0)
Panama_Pontella_danae<- Mapas_graficas_Abundancia(Panama_Pdanae, round(Panama_Pdanae$`Pontella danae`), 
                                                      expression(paste(bold("Jet de Panamá")," - ", italic("Pontella danae"))),
                                                      expression(paste(bold(" "))), 
                                                      expression(paste("[inv 1000 ", m^-3, "]")))

Panama_Pfera<-subset(Panama_Densidad,round(Panama_Densidad$`Pontella fera`)>0)
Panama_Pontella_fera<- Mapas_graficas_Abundancia(Panama_Pfera, round(Panama_Pfera$`Pontella fera`), 
                                                  expression(paste(bold("Jet de Panamá")," - ", italic("Pontella fera"))),
                                                  expression(paste(bold(" "))), 
                                                  expression(paste("[inv 1000 ", m^-3, "]")))

Panama_Pmimocerami<-subset(Panama_Densidad,round(Panama_Densidad$`Pontella mimocerami`)>0)
Panama_Pontella_mimocerami<- Mapas_graficas_Abundancia(Panama_Pmimocerami, round(Panama_Pmimocerami$`Pontella mimocerami`), 
                                                  expression(paste(bold("Jet de Panamá")," - ", italic("Pontella mimocerami"))),
                                                  expression(paste(bold(" "))), 
                                                  expression(paste("[inv 1000 ", m^-3, "]")))

Panama_Pspinicauda<-subset(Panama_Densidad,round(Panama_Densidad$`Pontella spinicauda`)>0)
Panama_Pontella_spinicauda<- Mapas_graficas_Abundancia(Panama_Pspinicauda, round(Panama_Pspinicauda$`Pontella spinicauda`), 
                                                       expression(paste(bold("Jet de Panamá")," - ", italic("Pontella spinicauda"))),
                                                       expression(paste(bold(" "))), 
                                                       expression(paste("[inv 1000 ", m^-3, "]")))

Panama_Pspinipes<-subset(Panama_Densidad,round(Panama_Densidad$`Pontella spinipes`)>0)
Panama_Pontella_spinipes<- Mapas_graficas_Abundancia(Panama_Pspinipes, round(Panama_Pspinipes$`Pontella spinipes`), 
                                                       expression(paste(bold("Jet de Panamá")," - ", italic("Pontella spinipes"))),
                                                       expression(paste(bold(" "))), 
                                                       expression(paste("[inv 1000 ", m^-3, "]")))




png(filename="Abundancias_especies_Panama_01.png", height = 30, width =  25, units = "cm", res = 300, pointsize = 12)
cowplot::plot_grid(labels=c("A","B", "C", "D", "E", "F", "G"),
                   Panama_Calanopia_elliptica,
                   Panama_Calanopia_minor,
                   Panama_Pontella_atlantica,
                   Panama_Pontella_danae,
                   Panama_Pontella_fera,
                   Panama_Pontella_mimocerami,
                   Panama_Pontella_spinicauda,
                   Panama_Pontella_spinipes,
                   nrow=4,
                   ncol=2)
dev.off()



Panama_Pmorii<-subset(Panama_Densidad,round(Panama_Densidad$`Pontellina morii`)>0)
Panama_Pontellina_morii<- Mapas_graficas_Abundancia(Panama_Pmorii, round(Panama_Pmorii$`Pontellina morii`), 
                                                     expression(paste(bold("Jet de Panamá")," - ", italic("Pontellina morii"))),
                                                     expression(paste(bold(" "))), 
                                                     expression(paste("[inv 1000 ", m^-3, "]")))

Panama_Pplumata<-subset(Panama_Densidad,round(Panama_Densidad$`Pontellina plumata`)>0)
Panama_Pontellina_plumata<- Mapas_graficas_Abundancia(Panama_Pplumata, round(Panama_Pplumata$`Pontellina plumata`), 
                                                    expression(paste(bold("Jet de Panamá")," - ", italic("Pontellina plumata"))),
                                                    expression(paste(bold(" "))), 
                                                    expression(paste("[inv 1000 ", m^-3, "]")))

Panama_Parmata<-subset(Panama_Densidad,round(Panama_Densidad$`Pontellopsis armata`)>0)
Panama_Pontellopsis_armata<- Mapas_graficas_Abundancia(Panama_Parmata, round(Panama_Parmata$`Pontellopsis armata`), 
                                                      expression(paste(bold("Jet de Panamá")," - ", italic("Pontellopsis armata"))),
                                                      expression(paste(bold(" "))), 
                                                      expression(paste("[inv 1000 ", m^-3, "]")))

Panama_Pperspicax<-subset(Panama_Densidad,round(Panama_Densidad$`Pontellopsis perspicax`)>0)
Panama_Pontellopsis_perspicax<- Mapas_graficas_Abundancia(Panama_Pperspicax, round(Panama_Pperspicax$`Pontellopsis perspicax`), 
                                                     expression(paste(bold("Jet de Panamá")," - ", italic("Pontellopsis perspicax"))),
                                                     expression(paste(bold(" "))), 
                                                     expression(paste("[inv 1000 ", m^-3, "]")))

Panama_Pregalis<-subset(Panama_Densidad,round(Panama_Densidad$`Pontellopsis regalis`)>0)
Panama_Pontellopsis_regalis<- Mapas_graficas_Abundancia(Panama_Pregalis, round(Panama_Pregalis$`Pontellopsis regalis`), 
                                                       expression(paste(bold("Jet de Panamá")," - ", italic("Pontellopsis regalis"))),
                                                       expression(paste(bold(" "))), 
                                                       expression(paste("[inv 1000 ", m^-3, "]")))

Panama_Plubbocki<-subset(Panama_Densidad,round(Panama_Densidad$`Pontellopsis lubbocki`)>0)
Panama_Pontellopsis_lubbocki<- Mapas_graficas_Abundancia(Panama_Plubbocki, round(Panama_Plubbocki$`Pontellopsis lubbocki`), 
                                                        expression(paste(bold("Jet de Panamá")," - ", italic("Pontellopsis lubbocki"))),
                                                        expression(paste(bold(" "))), 
                                                        expression(paste("[inv 1000 ", m^-3, "]")))



png(filename="Abundancias_especies_Panama_02.png", height = 25, width =  25, units = "cm", res = 300, pointsize = 12)
cowplot::plot_grid(labels=c("H","I", "J", "K", "L", "M"),
                   Panama_Pontellina_morii,
                   Panama_Pontellina_plumata,
                   Panama_Pontellopsis_armata,
                   Panama_Pontellopsis_perspicax,
                   Panama_Pontellopsis_regalis,
                   Panama_Pontellopsis_lubbocki,
                  
                   nrow=3,
                   ncol=2)
dev.off()


Panama_Lacuta<-subset(Panama_Densidad,round(Panama_Densidad$`Labidocera acuta`)>0)
Panama_Labidocera_acuta<- Mapas_graficas_Abundancia(Panama_Lacuta, round(Panama_Lacuta$`Labidocera acuta`), 
                                                         expression(paste(bold("Jet de Panamá")," - ", italic("Labidocera acuta"))),
                                                         expression(paste(bold(" "))), 
                                                         expression(paste("[inv 1000 ", m^-3, "]")))

Panama_Lacutifrons<-subset(Panama_Densidad,round(Panama_Densidad$`Labidocera acutifrons`)>0)
Panama_Labidocera_acutifrons<- Mapas_graficas_Abundancia(Panama_Lacutifrons, round(Panama_Lacutifrons$`Labidocera acutifrons`), 
                                                   expression(paste(bold("Jet de Panamá")," - ", italic("Labidocera acutifrons"))),
                                                   expression(paste(bold(" "))), 
                                                   expression(paste("[inv 1000 ", m^-3, "]")))

Panama_Laestiva<-subset(Panama_Densidad,round(Panama_Densidad$`Labidocera aestiva`)>0)
Panama_Labidocera_aestiva<- Mapas_graficas_Abundancia(Panama_Laestiva, round(Panama_Laestiva$`Labidocera aestiva`), 
                                                         expression(paste(bold("Jet de Panamá")," - ", italic("Labidocera aestiva"))),
                                                         expression(paste(bold(" "))), 
                                                         expression(paste("[inv 1000 ", m^-3, "]")))

Panama_Lchuraumi<-subset(Panama_Densidad,round(Panama_Densidad$`Labidocera churaumi`)>0)
Panama_Labidocera_churaumi<- Mapas_graficas_Abundancia(Panama_Lchuraumi, round(Panama_Lchuraumi$`Labidocera churaumi`), 
                                                      expression(paste(bold("Jet de Panamá")," - ", italic("Labidocera churaumi"))),
                                                      expression(paste(bold(" "))), 
                                                      expression(paste("[inv 1000 ", m^-3, "]")))

Panama_Ldetruncata<-subset(Panama_Densidad,round(Panama_Densidad$`Labidocera detruncata`)>0)
Panama_Labidocera_detruncata<- Mapas_graficas_Abundancia(Panama_Ldetruncata, round(Panama_Ldetruncata$`Labidocera detruncata`), 
                                                       expression(paste(bold("Jet de Panamá")," - ", italic("Labidocera detruncata"))),
                                                       expression(paste(bold(" "))), 
                                                       expression(paste("[inv 1000 ", m^-3, "]")))

Panama_Lnerii<-subset(Panama_Densidad,round(Panama_Densidad$`Labidocera nerii`)>0)
Panama_Labidocera_nerii<- Mapas_graficas_Abundancia(Panama_Lnerii, round(Panama_Lnerii$`Labidocera nerii`), 
                                                         expression(paste(bold("Jet de Panamá")," - ", italic("Labidocera nerii"))),
                                                         expression(paste(bold(" "))), 
                                                         expression(paste("[inv 1000 ", m^-3, "]")))


Panama_Lsinilobata<-subset(Panama_Densidad,round(Panama_Densidad$`Labidocera sinilobata`)>0)
Panama_Labidocera_sinilobata<- Mapas_graficas_Abundancia(Panama_Lsinilobata, round(Panama_Lsinilobata$`Labidocera sinilobata`), 
                                                    expression(paste(bold("Jet de Panamá")," - ", italic("Labidocera sinilobata"))),
                                                    expression(paste(bold(" "))), 
                                                    expression(paste("[inv 1000 ", m^-3, "]")))

png(filename="Abundancias_especies_Panama_03.png", height = 30, width =  25, units = "cm", res = 300, pointsize = 12)
cowplot::plot_grid(labels=c("N","O", "P", "Q", "R", "S", "T"),
                   Panama_Labidocera_acuta,
                   Panama_Labidocera_acutifrons,
                   Panama_Labidocera_aestiva,
                   Panama_Labidocera_churaumi,
                   Panama_Labidocera_detruncata,
                   Panama_Labidocera_nerii,
                   Panama_Labidocera_sinilobata,
                   
                   nrow=4,
                   ncol=2)
dev.off()




##Composición####

incidencia_total_ceros<-readxl::read_excel("Densidad_pontellidae.xlsx", sheet="Incidencia_Total_sinCeros")


CHoco_Incidencia<-subset(incidencia_total_ceros, Temporada=="Choco")
distancias_choco<-as.matrix(dist(CHoco_Incidencia[,3:4])*111)
bray_dist_choco <- vegan::vegdist(CHoco_Incidencia[5:26], method = "bray")
Mantel_Choco<-vegan::mantel(bray_dist_choco,distancias_choco, method = "spearman", permutations = 999 ) 
mantel.corr_choco<-vegan::mantel.correlog(bray_dist_choco, XY=CHoco_Incidencia[,3:4]*111, cutoff=T, r.type="spearman", nperm=99)


Panama_Incidencia<-subset(incidencia_total_ceros, Temporada=="Panama")
distancias_Panama<-as.matrix(dist(Panama_Incidencia[,3:4])*111)
bray_dist_Panama <- vegan::vegdist(Panama_Incidencia[5:26], method = "bray")
Mantel_Panama<-vegan::mantel(bray_dist_Panama,distancias_Panama, method = "spearman", permutations = 999 ) 
mantel.corr_panama<-mantel.corr_Panama<-vegan::mantel.correlog(bray_dist_Panama, XY=Panama_Incidencia[,3:4]*111, cutoff=T, r.type="spearman", nperm=99)

png(filename="Composicion_Autocorrelacion.png", height = 25, width =  18, units = "cm", res = 300, pointsize = 12)
par(mfrow = c(2, 1))
plot(mantel.corr_Panama)
plot(mantel.corr_choco)
mtext("Jet de Panamá", side = 3, line = -3, outer = TRUE)
mtext("Jet de Chocó", side = 3, line = -27, outer = TRUE)
dev.off()

##Perfiles de diversidad por estaciones####
 
 Div_Panama_Estaciones_df_trans$Orden<-c(0,1,2)
 Div_Choco_Estaciones_df_trans$Orden<-c(0,1,2)
 
 
 Perfiles_Diversidad<-function(data_Choco, variable_Choco, data_Panama, variable_Panama, titulo){
 
 ggplot2::ggplot() +
   geom_line(data = data_Choco, aes(x = Orden, y = variable_Choco   ), color = "blue", lwd = 0.5, linetype = 1) +
   geom_point(data = data_Choco, aes(x = Orden, y = variable_Choco   ), color = "blue", size =1.5) +
   geom_line(data = data_Panama, aes(x = Orden, y = variable_Panama), color = "red", lwd = 0.5, linetype = 1) +
   geom_point(data = data_Panama, aes(x = Orden, y = variable_Panama), color = "red", size =1.5) +
   
   labs(
     title = titulo,
     x = "Orden",
     y = "Diversidad",
     color = "Variable") +
   scale_color_manual(values = c("Jet_Choco" = "blue", "Jet_Panama" = "red")) +
   scale_y_continuous(limits = c(0,10), breaks = c(2,4,6,8,10))+
   theme_minimal()
 }
 
 
 
E1_perfiles<- Perfiles_Diversidad(Div_Choco_Estaciones_df_trans, Div_Choco_Estaciones_df_trans$E1_Choco, Div_Choco_Estaciones_df_trans, Div_Choco_Estaciones_df_trans$E1_Choco,"Estación 1")
E3_perfiles<- Perfiles_Diversidad(Div_Choco_Estaciones_df_trans, Div_Choco_Estaciones_df_trans$E3_Choco, Div_Choco_Estaciones_df_trans, Div_Choco_Estaciones_df_trans$E1_Choco,"Estación 3")
E7_perfiles<- Perfiles_Diversidad(Div_Choco_Estaciones_df_trans, Div_Choco_Estaciones_df_trans$E7_Choco, Div_Panama_Estaciones_df_trans, Div_Panama_Estaciones_df_trans$E7_Panama,"Estación 7")
E10_perfiles<- Perfiles_Diversidad(Div_Choco_Estaciones_df_trans, Div_Choco_Estaciones_df_trans$E10_Choco, Div_Choco_Estaciones_df_trans, Div_Choco_Estaciones_df_trans$E1_Choco,"Estación 10")
E12_perfiles<- Perfiles_Diversidad(Div_Choco_Estaciones_df_trans, Div_Choco_Estaciones_df_trans$E12_Choco, Div_Panama_Estaciones_df_trans, Div_Panama_Estaciones_df_trans$E12_Panama,"Estación 12")
E14_perfiles<- Perfiles_Diversidad(Div_Choco_Estaciones_df_trans, Div_Choco_Estaciones_df_trans$E14_Choco, Div_Panama_Estaciones_df_trans, Div_Panama_Estaciones_df_trans$E14_Panama,"Estación 14")
E16_perfiles<- Perfiles_Diversidad(Div_Choco_Estaciones_df_trans, Div_Choco_Estaciones_df_trans$E16_Choco, Div_Panama_Estaciones_df_trans, Div_Panama_Estaciones_df_trans$E16_Panama,"Estación 16")
E25_perfiles<- Perfiles_Diversidad(Div_Choco_Estaciones_df_trans, Div_Choco_Estaciones_df_trans$E25_Choco, Div_Choco_Estaciones_df_trans, Div_Choco_Estaciones_df_trans$E1_Choco,"Estación 25")
E27_perfiles<- Perfiles_Diversidad(Div_Choco_Estaciones_df_trans, Div_Choco_Estaciones_df_trans$E27_Choco, Div_Panama_Estaciones_df_trans, Div_Panama_Estaciones_df_trans$E27_Panama,"Estación 27")
E29_perfiles<- Perfiles_Diversidad(Div_Choco_Estaciones_df_trans, Div_Choco_Estaciones_df_trans$E29_Choco, Div_Panama_Estaciones_df_trans, Div_Panama_Estaciones_df_trans$E29_Panama,"Estación 29")
E31_perfiles<- Perfiles_Diversidad(Div_Choco_Estaciones_df_trans, Div_Choco_Estaciones_df_trans$E31_Choco, Div_Panama_Estaciones_df_trans, Div_Panama_Estaciones_df_trans$E31_Panama,"Estación 31")
E33_perfiles<- Perfiles_Diversidad(Div_Choco_Estaciones_df_trans, Div_Choco_Estaciones_df_trans$E33_Choco, Div_Panama_Estaciones_df_trans, Div_Panama_Estaciones_df_trans$E33_Panama,"Estación 33")
E43_perfiles<- Perfiles_Diversidad(Div_Choco_Estaciones_df_trans, Div_Choco_Estaciones_df_trans$E43_Choco, Div_Panama_Estaciones_df_trans, Div_Panama_Estaciones_df_trans$E43_Panama,"Estación 43")
E45_perfiles<- Perfiles_Diversidad(Div_Choco_Estaciones_df_trans, Div_Choco_Estaciones_df_trans$E45_Choco, Div_Panama_Estaciones_df_trans, Div_Panama_Estaciones_df_trans$E45_Panama,"Estación 45")
E47_perfiles<- Perfiles_Diversidad(Div_Choco_Estaciones_df_trans, Div_Choco_Estaciones_df_trans$E47_Choco, Div_Panama_Estaciones_df_trans, Div_Panama_Estaciones_df_trans$E47_Panama,"Estación 47")
E49_perfiles<- Perfiles_Diversidad(Div_Choco_Estaciones_df_trans, Div_Choco_Estaciones_df_trans$E49_Choco, Div_Panama_Estaciones_df_trans, Div_Panama_Estaciones_df_trans$E49_Panama,"Estación 49")
E59_perfiles<- Perfiles_Diversidad(Div_Choco_Estaciones_df_trans, Div_Choco_Estaciones_df_trans$E59_Choco, Div_Panama_Estaciones_df_trans, Div_Panama_Estaciones_df_trans$E59_Panama,"Estación 59")
E61_perfiles<- Perfiles_Diversidad(Div_Choco_Estaciones_df_trans, Div_Choco_Estaciones_df_trans$E61_Choco, Div_Panama_Estaciones_df_trans, Div_Panama_Estaciones_df_trans$E61_Panama,"Estación 61")
E63_perfiles<- Perfiles_Diversidad(Div_Choco_Estaciones_df_trans, Div_Choco_Estaciones_df_trans$E63_Choco, Div_Choco_Estaciones_df_trans, Div_Choco_Estaciones_df_trans$E1_Choco,"Estación 63")
E65_perfiles<- Perfiles_Diversidad(Div_Choco_Estaciones_df_trans, Div_Choco_Estaciones_df_trans$E65_Choco, Div_Choco_Estaciones_df_trans, Div_Choco_Estaciones_df_trans$E1_Choco,"Estación 65")
E75_perfiles<- Perfiles_Diversidad(Div_Choco_Estaciones_df_trans, Div_Choco_Estaciones_df_trans$E75_Choco, Div_Panama_Estaciones_df_trans, Div_Panama_Estaciones_df_trans$E75_Panama,"Estación 75")
E77_perfiles<- Perfiles_Diversidad(Div_Choco_Estaciones_df_trans, Div_Choco_Estaciones_df_trans$E77_Choco, Div_Panama_Estaciones_df_trans, Div_Panama_Estaciones_df_trans$E77_Panama,"Estación 77")
E79_perfiles<- Perfiles_Diversidad(Div_Choco_Estaciones_df_trans, Div_Choco_Estaciones_df_trans$E79_Choco, Div_Panama_Estaciones_df_trans, Div_Panama_Estaciones_df_trans$E79_Panama,"Estación 79")
E81_perfiles<- Perfiles_Diversidad(Div_Choco_Estaciones_df_trans, Div_Choco_Estaciones_df_trans$E81_Choco, Div_Choco_Estaciones_df_trans, Div_Choco_Estaciones_df_trans$E1_Choco,"Estación 81")
E91_perfiles<- Perfiles_Diversidad(Div_Choco_Estaciones_df_trans, Div_Choco_Estaciones_df_trans$E91_Choco, Div_Panama_Estaciones_df_trans, Div_Panama_Estaciones_df_trans$E91_Panama,"Estación 91")
E93_perfiles<- Perfiles_Diversidad(Div_Choco_Estaciones_df_trans, Div_Choco_Estaciones_df_trans$E93_Choco, Div_Panama_Estaciones_df_trans, Div_Panama_Estaciones_df_trans$E93_Panama,"Estación 93")
E95_perfiles<- Perfiles_Diversidad(Div_Choco_Estaciones_df_trans, Div_Choco_Estaciones_df_trans$E95_Choco, Div_Panama_Estaciones_df_trans, Div_Panama_Estaciones_df_trans$E95_Panama,"Estación 95")
E97_perfiles<- Perfiles_Diversidad(Div_Choco_Estaciones_df_trans, Div_Choco_Estaciones_df_trans$E97_Choco, Div_Choco_Estaciones_df_trans, Div_Choco_Estaciones_df_trans$E1_Choco,"Estación 97")
E107_perfiles<- Perfiles_Diversidad(Div_Choco_Estaciones_df_trans, Div_Choco_Estaciones_df_trans$E107_Choco, Div_Panama_Estaciones_df_trans, Div_Panama_Estaciones_df_trans$E107_Panama,"Estación 107")
E109_perfiles<- Perfiles_Diversidad(Div_Choco_Estaciones_df_trans, Div_Choco_Estaciones_df_trans$E109_Choco, Div_Panama_Estaciones_df_trans, Div_Panama_Estaciones_df_trans$E109_Panama,"Estación 109")
E111_perfiles<- Perfiles_Diversidad(Div_Choco_Estaciones_df_trans, Div_Choco_Estaciones_df_trans$E111_Choco, Div_Panama_Estaciones_df_trans, Div_Panama_Estaciones_df_trans$E111_Panama,"Estación 111")
E113_perfiles<- Perfiles_Diversidad(Div_Choco_Estaciones_df_trans, Div_Choco_Estaciones_df_trans$E113_Choco, Div_Choco_Estaciones_df_trans, Div_Choco_Estaciones_df_trans$E1_Choco,"Estación 113")

png(filename="Perfiles_Diversidad_Estaciones.png", height = 25, width =  25, units = "cm", res = 300, pointsize = 12)
cowplot::plot_grid(E1_perfiles,
                   E3_perfiles,
                   E7_perfiles,
                   E10_perfiles,
                   E12_perfiles,
                   E14_perfiles,
                   E16_perfiles,
                   E25_perfiles,
                   E27_perfiles,
                   E29_perfiles,
                   E31_perfiles,
                   E33_perfiles,
                   E43_perfiles,
                   E45_perfiles,
                   E47_perfiles,
                   E49_perfiles,
                   E59_perfiles,
                   E61_perfiles,
                   E63_perfiles,
                   E65_perfiles,
                   E75_perfiles,
                   E77_perfiles,
                   E79_perfiles,
                   E81_perfiles,
                   E91_perfiles,
                   E93_perfiles,
                   E95_perfiles,
                   E97_perfiles,
                   E107_perfiles,
                   E109_perfiles,
                   E111_perfiles,
                   E113_perfiles,
                   nrow=7,
                   ncol=5)
dev.off()



#Estimación de la diversdad
 
 Estaciones_Total<-read.table("Estaciones_Total.csv", header=TRUE, sep=",")
 
 
 Estaciones_Panama<-dplyr::filter(Estaciones_Total, Temporada=="Panama")
 Estaciones_Choco<-dplyr::filter(Estaciones_Total, Temporada=="Choco")
 
 Estaciones_Panama_sum<-as.data.frame(t(Estaciones_Panama[,3:24]%>% dplyr::summarise_all((sum))))
 Estaciones_Choco_sum<-as.data.frame(t(Estaciones_Choco[,3:24]%>% dplyr::summarise_all((sum))))
 
 Estaciones_Panama_Densidad<-dplyr::filter(Estaciones_Panama_sum, V1>0)
 Estaciones_Choco_Densidad<-dplyr::filter(Estaciones_Choco_sum, V1>0)
 
 Estaciones_Panama_Densidad_vector<-as.vector(Estaciones_Panama_Densidad$V1)
 Estaciones_Choco_Densidad_vector<-as.vector(Estaciones_Choco_Densidad$V1)
 
 Estaciones_total_Lista<-list(Estaciones_Panama_Densidad_vector, Estaciones_Choco_Densidad_vector)
 names(Estaciones_total_Lista)<-c("Panamá", "Chocó")
 
 Calculo_Inext_Temporadas<-iNEXT(Estaciones_total_Lista,
                                 q=c(0,1,2),
                                 datatype = "abundance",
                                 endpoint = 1000000)
 
 Temporada_plot_Hill<-ggiNEXT(Calculo_Inext_Temporadas, type=1, facet.var="Order.q", color.var = "Assemblage")+
   theme_bw(base_size = 6) +  
   labs(
     #title = "Estimaciones de diversidad",
     x = "Número de Individuos",
     y = "Diversidad de especies") +
   theme(legend.position="bottom",
         legend.title=element_blank(),
         legend.text=element_text(size = 9),
         axis.text = element_text(size = 9),
         axis.title = element_text(size = 9),
         strip.text= element_text(size = 9),
         plot.title = element_text(size = 9),
         plot.subtitle = element_text(size = 9),
         plot.caption= element_text(size = 9))
 
 png("Hill_extrapolation_plots.png", width = 3000, height = 1500, units = "px", res="300", pointsize = 3)
 Temporada_plot_Hill
 dev.off()
 
 
 
 
 #Compisición####
 
 Densidad_matriz_total<-readxl::read_excel("Densidad_pontellidae.xlsx", sheet="Densidad_Sin_Ceros")
 Inc_Total<-read.table("Incidencia_Total.csv", header=TRUE, sep=",")
 
 ##Anosim
 Densidad_matriz_total$Temporada_Estaciones <- paste(Densidad_matriz_total$Temporada, Densidad_matriz_total$Estaciones, sep = "_")
 Densidad_matrix_anosim<-as.data.frame(matriz_total[,5:26])
 rownames( Densidad_matrix_anosim)<-  Densidad_matriz_total$Temporada_Estaciones
 grupos<-factor(Densidad_matriz_total$Temporada)
 
 Densidad_resultado_anosim<- anosim(matrix_anosim, grupos, distance="bray", permutations = 999)


 Inc_Total$Temporada_Estaciones <- paste(Inc_Total$Temporada, Inc_Total$Estaciones, sep = "_")
 Incidendia_matrix_anosim<-Inc_Total[,3:24]
 rownames(Incidendia_matrix_anosim)<- Inc_Total$Temporada_Estaciones
 grupos<-factor(Inc_Total$Temporada)
 
 Incidencia_resultado_anosim <- anosim(Incidendia_matrix_anosim, grupos, distance="bray", permutations = 999)

 par(mfrow = c(1, 2))
 plot(Densidad_resultado_anosim, 
      main="ANOSIM basada en densidad",
      xlab="Clases",
      ylab="Rango de distancias")
 plot(Incidencia_resultado_anosim, 
      main="ANOSIM basada en incidencia",
      xlab="Clases",
      ylab="Rango de distancias")
 
 #Simper

 Densidad_resultado_simper<- simper(Densidad_matrix_anosim, grupos)
 print(Densidad_resultado_simper)
 summary(Densidad_resultado_simper)
 Incidencia_resultado_simper<- simper(Incidendia_matrix_anosim, grupos)
 print(Incidencia_resultado_simper)
 summary(Incidencia_resultado_simper)
 
 #NMDS####
 
 Inc_Total<-read.table("Incidencia_Total.csv", header=TRUE, sep=",")
 
 Data_spp_Only<-Inc_Total[,3:24]
 
 nombres_Grupos<-paste0(Inc_Total[,1],"_", Inc_Total[,2])
 row.names(Data_spp_Only)<-nombres_Grupos
 
 
 grupos_df<-Inc_Total[1:2]
 row.names(grupos_df)<-nombres_Grupos
 grupos_df$Temporada<-as.factor( grupos_df$Temporada)
 grupos_df$Estaciones<-as.factor( grupos_df$Estaciones)
 
 Incidencia_Relativa<-vegan::decostand(Data_spp_Only, method = "total")
 Incidencia_Relativa_MDist<-vegan::vegdist(Data_spp_Only, method = "bray")
 
 Incidencia_Relativa_MatrizDist<-as.matrix(Incidencia_Relativa_MDist, labels=TRUE)
 
 write.csv(Incidencia_Relativa_MatrizDist, "Incidencia_Relativa_MatrizDist.csv")
 
 Resultado_NMDS<- metaMDS(Incidencia_Relativa_MatrizDist,
                          distance ="bray",
                          k =3,
                          maxit = 999,
                          trymax=500,
                          wascores = TRUE)
 
 stressplot(Resultado_NMDS)
 
 Coordenadas_NMDS<-as.data.frame(scores(Resultado_NMDS[["points"]]))
 Coordenadas_NMDS$Estaciones<-Inc_Total$Estaciones
 Coordenadas_NMDS$Temporada<-as.factor(   Inc_Total$Temporada)
 colnames(Coordenadas_NMDS)<-c("NMDS1",
                               "NMDS2",
                               "NMDS3",
                               "Estaciones",
                               "Temporada")
   
Temporada_group <- Coordenadas_NMDS %>%
   ggplot(aes(x = NMDS1,
              y = NMDS2,
              color=Temporada,
              label=Estaciones))+
   geom_point()+geom_text(hjust=0, vjust=0)


hull_data_Temporada <- 
  Coordenadas_NMDS %>%
  tidyr::drop_na() %>%
  dplyr::group_by(Temporada) %>% dplyr::slice(chull(NMDS1, NMDS2))


Temporada_group<-Temporada_group +
  geom_polygon(data = hull_data_Temporada,
               aes(fill = Temporada,
                   colour = Temporada),
               alpha = 0.3,
               show.legend = FALSE)+
  theme_bw()+
  theme(legend.position="bottom",
        legend.title=element_blank(),
        legend.text=element_text(size = 9),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 9),
        strip.text= element_text(size = 9),
        plot.title = element_text(size = 9),
        plot.subtitle = element_text(size = 9),
        plot.caption= element_text(size = 9))
Temporada_group
png("NMDS_plots.png", width = 2000, height = 2000, units = "px", res="300", pointsize = 3)
Temporada_group
dev.off()



resultado_simper$Choco_Panama
###### perfiles de diversidad de cada red por temporada
 
 malla<-read.table("Q0_1_2Choco_Panama_300_500.csv", sep=",",header=TRUE)
 malla2<-malla[,2:5]
 row.names(malla2)<-malla$Especies
 
 D0_Hill_malla<-hilldiv::hill_div(malla2, qvalue = 0)
 D1_Hill_malla<-hilldiv::hill_div(malla2, qvalue = 1)
 D2_Hill_malla<-hilldiv::hill_div(malla2, qvalue = 2)
 
 Div_hill_Total_malla<-list(D0_Hill_malla,D1_Hill_malla,D2_Hill_malla)
 Div_hill_Total_malla_df<-as.data.frame(Div_hill_Total_malla)
 colnames(Div_hill_Total_malla_df)<-c("q0", "q1","q2")
 
 Div_hill_Total_malla_df_trans<-as.data.frame(t(Div_hill_Total_malla_df))
 Div_hill_Total_malla_df_trans$Orden<-c(0,1,2)
 
 
 Rango_DivPlot <- ggplot2::ggplot() +
   geom_line(data = Div_hill_Total_malla_df_trans, aes(x = Orden, y = Jet_Choco_300 ), color = "blue", lwd = 0.5, linetype = 1) +
   geom_point(data = Div_hill_Total_malla_df_trans, aes(x = Orden, y = Jet_Choco_300 ), color = "blue", size =3) +
   geom_line(data = Div_hill_Total_malla_df_trans, aes(x = Orden, y = Jet_Choco_500 ), color = "red", lwd = 0.5, linetype = 1) +
   geom_point(data = Div_hill_Total_malla_df_trans, aes(x = Orden, y = Jet_Choco_500 ), color = "red", size =3) +
   geom_line(data = Div_hill_Total_malla_df_trans, aes(x = Orden, y = Jet_Panama_300 ), color = "black", lwd = 0.5, linetype = 1) +
   geom_point(data = Div_hill_Total_malla_df_trans, aes(x = Orden, y = Jet_Panama_300 ), color = "black", size =3) +
   geom_line(data = Div_hill_Total_malla_df_trans, aes(x = Orden, y = Jet_Panama_500 ), color = "green", lwd = 0.5, linetype = 1) +
   geom_point(data = Div_hill_Total_malla_df_trans, aes(x = Orden, y = Jet_Panama_500 ), color = "green", size =3) +
   labs(
     title = "Perfiles de diversidad",
     x = "Orden",
     y = "Diversidad",
     color = "Variable") +
   #scale_color_manual(values = c("Jet_Choco" = "blue", "Jet_Panama" = "red")) +
   annotate("text", x = 0.15, y = 12, label = "Jet Panamá - 300 um", size = 3, color = "black") +
   annotate("text", x = 0.15, y = 20, label = "Jet Panamá - 300 um", size = 3, color = "black") +
   annotate("text", x = 0.15, y = 7, label = "Jet Chocó", size = 3, color = "black") +
   annotate("text", x = 0.15, y = 3, label = "Jet Panamá", size = 3, color = "black") +
   theme_minimal()
 
 
 
 #cURAVAS DE RANGO ABUNDANCIA
 
 
 
 
 Panama300<-read.table("q0_1_2_panama_300.csv", sep=",",header=TRUE)
 Temporadas_total<-Temporadas_total[,2:3]
 row.names(Temporadas_total)<-Temporadas_total$Especies
 
 D0_Temporadas_total<-hilldiv::hill_div(Temporadas_total, qvalue = 0)
 D1_Temporadas_total<-hilldiv::hill_div(Temporadas_total, qvalue = 1)
 D2_Temporadas_total<-hilldiv::hill_div(Temporadas_total, qvalue = 2)
 
 Div_hill_Total<-list(D0_Temporadas_total,D1_Temporadas_total,D2_Temporadas_total)
 Div_hill_Total_df<-as.data.frame(Div_hill_Total)
 colnames(Div_hill_Total_df)<-c("q0", "q1","q2")
 
 Div_hill_Total_df_trans<-as.data.frame(t(Div_hill_Total_df))
 Div_hill_Total_df_trans$Orden<-c(0,1,2)
 
 
 Rango_DivPlot <- ggplot2::ggplot() +
   geom_line(data = Div_hill_Total_df_trans, aes(x = Orden, y = Jet_Choco ), color = "blue", lwd = 0.5, linetype = 1) +
   geom_line(data = Div_hill_Total_df_trans, aes(x = Orden, y = Jet_Panama ), color = "red", lwd = 0.5, linetype = 1) +
   labs(
     title = "Perfiles de diversidad",
     x = "Orden",
     y = "Diversidad",
     color = "Variable") +
   scale_color_manual(values = c("Jet_Choco" = "blue", "Jet_Panama" = "red")) +
   annotate("text", x = 0.3, y = 8, label = "Jet Chocó", size = 5, color = "black") +
   annotate("text", x = 0.3, y = 22, label = "Jet Panamá", size = 5, color = "black") +
   theme_minimal()
 
#####Estimaciones

 Temporadas_total


 Jet_Choco_Densidad<-dplyr::filter(Temporadas_total, Jet_Choco>0)
 Jet_Panama_Densidad<-dplyr::filter(Temporadas_total, Jet_Panama>0)

 Jet_ChocoVec_Densidad<-as.vector(Jet_Choco_Densidad$Jet_Choco)
 Jet_PanamaVec_Densidad<-as.vector(Jet_Panama_Densidad$Jet_Panama)

Total_Densidad<-list(Jet_ChocoVec_Densidad, Jet_PanamaVec_Densidad)
names(Total_Densidad)<-c("Jet Chocó", "Jet Panamá")

Total_Plot_Densidad <- iNEXT(Total_Densidad, 
                                 q=c(0,1,2), 
                                 datatype = "abundance",
                                 endpoint = 100000) # q = 0 es la riqueza

Total_Plot_HILL<-ggiNEXT(Total_Plot_Densidad, type=1,facet.var = "Order.q")

#Hacer las gráficas finales####


#GAM####


GAM_q0 <- mgcv::gam(q0 ~
                      s(salinidad_median,bs="ts")+
                      s(Temperatura_median,bs="ts"),
                    data = Diversidad_Total_df,
                    family=poisson,
                    scale=-1,
                    gamma=1.4)

GAM_q0.int.res <- residuals(GAM_q0, type = "response")


summary(GAM_q0)
AIC(GAM_q0)
anova(GAM_q0)



plot(GAM_q0,pages=1, residuals=TRUE)
plot(GAM_q0, pages=1, seWithMean = TRUE)





RF_Choco_response_Curve<-as.data.frame(cbind(GAM_q0$Probabilidad,ChocoPredictores))
colnames(RF_Choco_response_Curve)[1]<-"probabilidad"
head(RF_Choco_response_Curve)

curvas_Respuesta2<-function(datos, variable, titulo){
  ggplot(datos, aes(x = datos[,variable], y = datos[,1])) +
    geom_smooth(mapping = aes(group = variable), method = "auto") +  # Añadir puntos
    
    labs(title=titulo,
         x = "Valores",
         y = "Probabilidad")+
    theme_bw()
  
}
curvas_Respuesta2(knn_Panama_response_Curve, 10, "Temperatura IQR")

GAM_q1 <- mgcv::gam(q1 ~
                      s(salinidad_median,bs="ts")+
                      s(Temperatura_median,bs="ts"),
                    data = Diversidad_Total_df,
                    family=poisson,
                    scale=-1,
                    gamma=1.4)

summary(GAM_q1)
anova(GAM_q1)


plot(GAM_q1,pages=1, residuals=TRUE)
plot(GAM_q1, pages=1, seWithMean = TRUE)


GAM_q2 <- mgcv::gam(q2 ~
                      s(salinidad_median,bs="ts")+
                      s(Temperatura_median,bs="ts"),
                    data = Diversidad_Total_df,
                    family=poisson,
                    scale=-1,
                    gamma=1.4)

summary(GAM_q2)
anova(GAM_q2)



plot(GAM_q2,pages=1, residuals=TRUE)
plot(GAM_q2, pages=1, seWithMean = TRUE)








