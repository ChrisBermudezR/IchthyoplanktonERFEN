

data_quantity<-readxl::read_excel("../data/raw/biodiversity/Listado_Especies_v3 revisada_BB.xlsx", sheet="organismQuantity")


densidad_total<-as.data.frame(colSums(data_quantity[, 12:272]))

colnames(densidad_total)<-c("Total_Density")

# Ordenar por la columna A de manera descendente
densidad_ordenado <- densidad_total %>%
  arrange(desc(Total_Density))

#densidad_ordenado$rango<-1:261

summary(densidad_total$Total_Density)

boxplot(densidad_total$Total_Density)

hist(densidad_total$Total_Density,
     freq = TRUE)

options(scipen=9999)

Rango_Plot_total<-ggplot()+
  geom_point(aes(y=densidad_ordenado[["Total_Density"]], x=seq(1:261)))+
  scale_y_continuous(trans='log10', limits = function(x){c(min(x), ceiling(100000))})+
  labs(colour = "", subtitle = "Rango - Abundancia", title="Abundancia Total",  x ="Rango", y =expression(paste("log(abundancia) [ind 1000 ", m^-3, "]")),tag="A.")+
  theme_bw()+
  theme(legend.position="bottom", 
        axis.text.y =  element_text(size = 8),
        axis.title =  element_text(size = 8),
        plot.title = element_text(size = 8),
        plot.subtitle = element_text(size = 8),
        plot.caption= element_text(size = 8),
        plot.tag = element_text(size=8))



Total_Rank_data<- vegan::rad.lognormal(densidad_ordenado$Total_Density)

Total_Rank_radfit<- vegan::radfit(densidad_ordenado$Total_Density)



total_plot<-ggplot()+
  geom_point(aes(y=Total_Rank_radfit[["y"]], x=seq(1:261)))+
  geom_line(aes(y= Total_Rank_radfit[["models"]][["Null"]][["fitted.values"]],x=seq(1:261),color="Null" ))+
  geom_line(aes(y= Total_Rank_radfit[["models"]][["Preemption"]][["fitted.values"]],x=seq(1:261),color="Preemption"))+
  geom_line(aes(y= Total_Rank_radfit[["models"]][["Lognormal"]][["fitted.values"]],x=seq(1:261),color="Lognormal" ))+
  geom_line(aes(y= Total_Rank_radfit[["models"]][["Zipf"]][["fitted.values"]],x=seq(1:261),color="Zipf"))+
  geom_line(aes(y= Total_Rank_radfit[["models"]][["Mandelbrot"]][["fitted.values"]],x=seq(1:261),color="Mandelbrot" ))+
  scale_y_continuous(trans='log10')+
  scale_color_manual(values = c( "#d7191c","#fdae61", "#a6dba0", "#008837", "#2b83ba"),
                     #breaks=c("Jet Chocó", "Null", "Preemption", "Lognormal", "Zipf", "Mandelbrot"),
                     name = " ",
                     labels=c(  "Mandelbrot", "Zipf","Lognormal","Preemption", "Null")
  )+
  labs(colour = "", subtitle = "Rango - Densidad", title="Densidad total",  x ="Rango", y =expression(paste("log(Densidad) [ind 1000 ", m^-3, "]")),tag="B.")+
  theme_bw()+
  theme(legend.position="bottom", 
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        plot.title = element_text(size = 8),
        plot.subtitle = element_text(size = 8),
        plot.caption= element_text(size = 8),
        plot.tag = element_text(size=8))

gridExtra::grid.arrange(Rango_Plot_total,
                        total_plot,
                        ncol=1)

#Diversidad####


D0_total<-hilldiv::hill_div(densidad_ordenado, qvalue = 0)
D1_total<-hilldiv::hill_div(densidad_ordenado, qvalue = 1)
D2_total<-hilldiv::hill_div(densidad_ordenado, qvalue = 2)



Div_hill_Total<-base::list(D0_total,D1_total,D2_total)
Div_hill_Total_df<-base::as.data.frame(Div_hill_Total)
base::colnames(Div_hill_Total_df)<-c("q0", "q1","q2")

Div_hill_Total_df_trans<-as.data.frame(t(Div_hill_Total_df))
Div_hill_Total_df_trans$Orden<-c(0,1,2)


Rango_DivPlot <- ggplot2::ggplot() +
  geom_line(data = Div_hill_Total_df_trans, aes(x = Orden, y = Total_Density ), color = "black", lwd = 0.5, linetype = 1) +
  geom_point(data = Div_hill_Total_df_trans, aes(x = Orden, y = Total_Density ), color = "black", size =3) +
   labs(
    title = "Perfiles de diversidad",
    x = "Orden",
    y = "Diversidad",
    color = "Variable") +
     theme_minimal()


#ENOS####

nino<-data_quantity%>%
        filter(ONI == "Niño")
nina<-data_quantity%>%
  filter(ONI == "Niña")
neutro<-data_quantity%>%
  filter(ONI == "Neutro")




densidad_nino<-as.data.frame(colSums(nino[, 12:272]))

colnames(densidad_nino)<-c("Total_Density")

# Ordenar por la columna A de manera descendente
densidad_nino_ord <- densidad_nino %>%
  arrange(desc(Total_Density))%>%
  filter(Total_Density != 0)


densidad_nina<-as.data.frame(colSums(nina[, 12:272]))

colnames(densidad_nina)<-c("Total_Density")

# Ordenar por la columna A de manera descendente
densidad_nina_ord <- densidad_nina %>%
  arrange(desc(Total_Density))%>%
  filter(Total_Density != 0)


densidad_neutro<-as.data.frame(colSums(neutro[, 12:272]))

colnames(densidad_neutro)<-c("Total_Density")

# Ordenar por la columna A de manera descendente
densidad_neutro_ord <- densidad_neutro %>%
  arrange(desc(Total_Density))%>%
  filter(Total_Density != 0)




Rango_ENOS_total<-ggplot()+
  geom_point(aes(y=densidad_nino_ord[["Total_Density"]], x=seq(1:145)), color ="red")+
  geom_point(aes(y=densidad_nina_ord[["Total_Density"]], x=seq(1:125)), color ="blue")+
  geom_point(aes(y=densidad_neutro_ord[["Total_Density"]], x=seq(1:202)), color ="grey")+
  scale_y_continuous(trans='log10', limits = function(x){c(min(x), ceiling(100000))})+
  labs(colour = "", subtitle = "Rango - Abundancia", title="Abundancia Total",  x ="Rango", y =expression(paste("log(abundancia) [ind 1000 ", m^-3, "]")),tag="A.")+
  theme_bw()+
  theme(legend.position="bottom", 
        axis.text.y =  element_text(size = 8),
        axis.title =  element_text(size = 8),
        plot.title = element_text(size = 8),
        plot.subtitle = element_text(size = 8),
        plot.caption= element_text(size = 8),
        plot.tag = element_text(size=8))




#Diversidad ENOS####

#Niño

D0_nino<-hilldiv::hill_div(densidad_nino_ord, qvalue = 0)
D1_nino<-hilldiv::hill_div(densidad_nino_ord, qvalue = 1)
D2_nino<-hilldiv::hill_div(densidad_nino_ord, qvalue = 2)



Div_hill_nino<-base::list(D0_nino,D1_nino,D2_nino)
Div_hill_nino_df<-base::as.data.frame(Div_hill_nino)
base::colnames(Div_hill_nino_df)<-c("q0", "q1","q2")

Div_hill_nino_df_trans<-as.data.frame(t(Div_hill_nino_df))
Div_hill_nino_df_trans$Orden<-c(0,1,2)

#Niña

D0_nina<-hilldiv::hill_div(densidad_nina_ord, qvalue = 0)
D1_nina<-hilldiv::hill_div(densidad_nina_ord, qvalue = 1)
D2_nina<-hilldiv::hill_div(densidad_nina_ord, qvalue = 2)



Div_hill_nina<-base::list(D0_nina,D1_nina,D2_nina)
Div_hill_nina_df<-base::as.data.frame(Div_hill_nina)
base::colnames(Div_hill_nina_df)<-c("q0", "q1","q2")

Div_hill_nina_df_trans<-as.data.frame(t(Div_hill_nina_df))
Div_hill_nina_df_trans$Orden<-c(0,1,2)


#Neutro

D0_neutro<-hilldiv::hill_div(densidad_neutro_ord, qvalue = 0)
D1_neutro<-hilldiv::hill_div(densidad_neutro_ord, qvalue = 1)
D2_neutro<-hilldiv::hill_div(densidad_neutro_ord, qvalue = 2)



Div_hill_neutro<-base::list(D0_neutro,D1_neutro,D2_neutro)
Div_hill_neutro_df<-base::as.data.frame(Div_hill_neutro)
base::colnames(Div_hill_neutro_df)<-c("q0", "q1","q2")

Div_hill_neutro_df_trans<-as.data.frame(t(Div_hill_neutro_df))
Div_hill_neutro_df_trans$Orden<-c(0,1,2)





Rango_DivPlot_oni <- ggplot2::ggplot() +
  geom_line(data = Div_hill_nino_df_trans, aes(x = Orden, y = Total_Density ), color = "red", lwd = 0.5, linetype = 1) +
  geom_point(data = Div_hill_nino_df_trans, aes(x = Orden, y = Total_Density ), color = "red", size =3) +
  geom_line(data = Div_hill_nina_df_trans, aes(x = Orden, y = Total_Density ), color = "blue", lwd = 0.5, linetype = 1) +
  geom_point(data = Div_hill_nina_df_trans, aes(x = Orden, y = Total_Density ), color = "blue", size =3) +
  geom_line(data = Div_hill_neutro_df_trans, aes(x = Orden, y = Total_Density ), color = "grey", lwd = 0.5, linetype = 1) +
  geom_point(data = Div_hill_neutro_df_trans, aes(x = Orden, y = Total_Density ), color = "grey", size =3) +
  labs(
    title = "Perfiles de diversidad ",
    x = "Orden",
    y = "Diversidad",
    color = "Variable") +
  theme_minimal()


#Estaciones####

C_1993_04<-data_quantity%>%
  filter(crucero == "1993-04")

C_1993_04_02<-C_1993_04[12:272]

C_1993_04_02<-as.data.frame(t(C_1993_04_02))


colnames(C_1993_04_02)<-c("E124",
                          "E140",
                          "E133",
                          "E133A",
                          "E1MND",
                          "E1MNN",
                          "E1MED",
                          "E1MEN",
                          "E1ME1D",
                          "E1ME1N",
                          "E1MWD",
                          "E1MWN",
                          "E1MW1D",
                          "E1MW1N",
                          "E1MSD",
                          "E1MSN") 

D0_C_1993_04_02<-hilldiv::hill_div(C_1993_04_02, qvalue = 0)
D1_C_1993_04_02<-hilldiv::hill_div(C_1993_04_02, qvalue = 1)
D2_C_1993_04_02<-hilldiv::hill_div(C_1993_04_02, qvalue = 2)

hill_C_1993_04_02<-base::list(D0_C_1993_04_02, D1_C_1993_04_02, D2_C_1993_04_02)
hill_C_1993_04_02<-base::as.data.frame(hill_C_1993_04_02)
colnames(hill_C_1993_04_02)<-c("q0", "q1","q2")



C_1993_10<-data_quantity%>%
  filter(crucero == "1993-10")

C_1993_10_02<-C_1993_10[12:272]

C_1993_10_02<-as.data.frame(t(C_1993_10_02))


colnames(C_1993_10_02)<-c("E224",
                          "E240",
                          "E248",
                          "E233",
                          "E2MND",
                          "E2MNN",
                          "E2MED",
                          "E2MEN",
                          "E2ME1D",
                          "E2ME1N",
                          "E2MWD",
                          "E2MWN",
                          "E2MW1D",
                          "E2MW1N",
                          "E2MSD",
                          "E2MSN" 
                          ) 

D0_C_1993_10_02<-hilldiv::hill_div(C_1993_10_02, qvalue = 0)
D1_C_1993_10_02<-hilldiv::hill_div(C_1993_10_02, qvalue = 1)
D2_C_1993_10_02<-hilldiv::hill_div(C_1993_10_02, qvalue = 2)

hill_C_1993_10_02<-base::list(D0_C_1993_10_02, D1_C_1993_10_02, D2_C_1993_10_02)
hill_C_1993_10_02<-base::as.data.frame(hill_C_1993_10_02)
colnames(hill_C_1993_10_02)<-c("q0", "q1","q2")



C_2004_09<-data_quantity%>%
  filter(crucero == "2004-09")

C_2004_09_02<-C_2004_09[12:272]

C_2004_09_02<-as.data.frame(t(C_2004_09_02))


colnames(C_2004_09_02)<-c( "E33",
                           "E35",
                           "E37",
                           "E310",
                           "E312",
                           "E314",
                           "E316",
                           "E325",
                           "E327",
                           "E329",
                           "E331",
                           "E333",
                           "E343",
                           "E345",
                           "E347",
                           "E349", 
                           "E361",
                           "E375",
                           "E377",
                           "E379",
                           "E381",
                           "E3107",
                           "E3109",
                           "E3111",
                           "E3113",
                           "E3ME1",
                           "E3MN",
                           "E3MS" ) 

D0_C_2004_09_02<-hilldiv::hill_div(C_2004_09_02, qvalue = 0)
D1_C_2004_09_02<-hilldiv::hill_div(C_2004_09_02, qvalue = 1)
D2_C_2004_09_02<-hilldiv::hill_div(C_2004_09_02, qvalue = 2)

hill_C_2004_09_02<-base::list(D0_C_2004_09_02, D1_C_2004_09_02, D2_C_2004_09_02)
hill_C_2004_09_02<-base::as.data.frame(hill_C_2004_09_02)
colnames(hill_C_2004_09_02)<-c("q0", "q1","q2")




C_2005_09<-data_quantity%>%
  filter(crucero == "2005-09")
C_2005_09_02<-C_2005_09[12:272]

C_2005_09_02<-as.data.frame(t(C_2005_09_02))


colnames(C_2005_09_02)<-c("E41",
                          "E43",
                          "E45",
                          "E47",
                          "E410",
                          "E412",
                          "E414",
                          "E425",
                          "E427",
                          "E429",
                          "E431",
                          "E433",
                          "E443",
                          "E447", 
                          "E449",
                          "E475", 
                          "E479",
                          "E481",
                          "E4107",
                          "E4109",
                          "E4111",
                          "E4G2",
                          "E4G5",
                          "E4G7",
                          "E4G11",
                          "E4G13",
                          "E4G15",
                          "E4G16",
                          "E4G17",
                          "E4G19",
                          "E4M3",
                          "E4M4", 
                          "E4M6",
                          "E4M9",
                          "E4M10",
                          "E4M12") 

D0_C_2005_09_02<-hilldiv::hill_div(C_2005_09_02, qvalue = 0)
D1_C_2005_09_02<-hilldiv::hill_div(C_2005_09_02, qvalue = 1)
D2_C_2005_09_02<-hilldiv::hill_div(C_2005_09_02, qvalue = 2)

hill_C_2005_09_02<-base::list(D0_C_2005_09_02, D1_C_2005_09_02, D2_C_2005_09_02)
hill_C_2005_09_02<-base::as.data.frame(hill_C_2005_09_02)
colnames(hill_C_2005_09_02)<-c("q0", "q1","q2")

C_2006_03<-data_quantity%>%
  filter(crucero == "2006-03")
C_2006_03_02<-C_2006_03[12:272]

C_2006_03_02<-as.data.frame(t(C_2006_03_02))


colnames(C_2006_03_02)<-c("E51",
                          "E53",
                          "E55",
                          "E510",
                          "E512",
                          "E514",
                          "E516",
                          "E525",
                          "E527",
                          "E529",
                          "E531",
                          "E533",
                          "E543",
                          "E545",
                          "E547",
                          "E549", 
                          "E577",
                          "E579",
                          "E581",
                          "E5107",
                          "E5109",
                          "E5111",
                          "E5113",
                          "E5G2",
                          "E5G4",
                          "E5G7",
                          "E5G9",
                          "E5G11",
                          "E5G13",
                          "E5G15",
                          "E5G17",
                          "E5M2",
                          "E5M3",
                          "E5M4",
                          "E5M5",
                          "E5M8",
                          "E5M9",
                          "E5M10",
                          "E5M11") 

D0_C_2006_03_02<-hilldiv::hill_div(C_2006_03_02, qvalue = 0)
D1_C_2006_03_02<-hilldiv::hill_div(C_2006_03_02, qvalue = 1)
D2_C_2006_03_02<-hilldiv::hill_div(C_2006_03_02, qvalue = 2)

hill_C_2006_03_02<-base::list(D0_C_2006_03_02, D1_C_2006_03_02, D2_C_2006_03_02)
hill_C_2006_03_02<-base::as.data.frame(hill_C_2006_03_02)
colnames(hill_C_2006_03_02)<-c("q0", "q1","q2")

C_2006_09<-data_quantity%>%
  filter(crucero == "2006-09")
C_2006_09_02<-C_2006_09[12:272]

C_2006_09_02<-as.data.frame(t(C_2006_09_02))


colnames(C_2006_09_02)<-c("E633",
                          "E649",
                          "E677",
                          "E679",
                          "E681",
                          "E6109",
                          "E6111",
                          "E6113",
                          "E6G4",
                          "E6G5",
                          "E6G15",
                          "E6G17",
                          "E6G19",
                          "E6M1",
                          "E6M4",
                          "E6M6",
                          "E6M7",
                          "E6M9",
                          "E6M12") 

D0_C_2006_09_02<-hilldiv::hill_div(C_2006_09_02, qvalue = 0)
D1_C_2006_09_02<-hilldiv::hill_div(C_2006_09_02, qvalue = 1)
D2_C_2006_09_02<-hilldiv::hill_div(C_2006_09_02, qvalue = 2)

hill_C_2006_09_02<-base::list(D0_C_2006_09_02, D1_C_2006_09_02, D2_C_2006_09_02)
hill_C_2006_09_02<-base::as.data.frame(hill_C_2006_09_02)
colnames(hill_C_2006_09_02)<-c("q0", "q1","q2")

C_2019_03<-data_quantity%>%
  filter(crucero == "2019-03")
C_2019_03_02<-C_2019_03[12:272]

C_2019_03_02<-as.data.frame(t(C_2019_03_02))


colnames(C_2019_03_02)<-c("E77",
                          "E712",
                          "E714",
                          "E7316",
                          "E727",
                          "E7329",
                          "E731",
                          "E733",
                          "E743",
                          "E745",
                          "E747",
                          "E749",
                          "E759",
                          "E761",
                          "E775",
                          "E777",
                          "E779",
                          "E791",
                          "E793",
                          "E795",
                          "E7107",
                          "E7109",
                          "E7111",
                          "E7113") 

D0_C_2019_03_02<-hilldiv::hill_div(C_2019_03_02, qvalue = 0)
D1_C_2019_03_02<-hilldiv::hill_div(C_2019_03_02, qvalue = 1)
D2_C_2019_03_02<-hilldiv::hill_div(C_2019_03_02, qvalue = 2)

hill_C_2019_03_02<-base::list(D0_C_2019_03_02, D1_C_2019_03_02, D2_C_2019_03_02)
hill_C_2019_03_02<-base::as.data.frame(hill_C_2019_03_02)
colnames(hill_C_2019_03_02)<-c("q0", "q1","q2")

C_2019_09<-data_quantity%>%
  filter(crucero == "2019-09")
C_2019_09_02<-C_2019_09[12:272]

C_2019_09_02<-as.data.frame(t(C_2019_09_02))


colnames(C_2019_09_02)<-c("E81",
                          "E83",
                          "E87",
                          "E810",
                          "E812",
                          "E814",
                          "E816",
                          "E825",
                          "E827",
                          "E829",
                          "E831",
                          "E833",
                          "E843",
                          "E845",
                          "E847",
                          "E849", 
                           "E859",
                          "E861",
                          "E863",
                          "E865",
                          "E875",
                          "E877",
                          "E879",
                          "E881",
                          "E891",
                          "E893",
                          "E895",
                          "E897",
                          "E8107",
                          "E8109",
                          "E8111",
                          "E8113") 

D0_C_2019_09_02<-hilldiv::hill_div(C_2019_09_02, qvalue = 0)
D1_C_2019_09_02<-hilldiv::hill_div(C_2019_09_02, qvalue = 1)
D2_C_2019_09_02<-hilldiv::hill_div(C_2019_09_02, qvalue = 2)

hill_C_2019_09_02<-base::list(D0_C_2019_09_02, D1_C_2019_09_02, D2_C_2019_09_02)
hill_C_2019_09_02<-base::as.data.frame(hill_C_2019_09_02)
colnames(hill_C_2019_09_02)<-c("q0", "q1","q2")
