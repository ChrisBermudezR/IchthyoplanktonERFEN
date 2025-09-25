



data<-readxl::read_excel("Especies_ERFEN.xlsx", sheet = "Hoja1")


####Worms
Especies<-as.character(as.factor(data$scientificName))

matchedTaxa1<-worrms::wm_records_taxamatch(Especies[1:50], 
                                          marine_only = TRUE, 
                                          kingdom = "animalia")

matchedTaxa2<-worrms::wm_records_taxamatch(Especies[51:100], 
                                           marine_only = TRUE, 
                                           kingdom = "animalia")
matchedTaxa3<-worrms::wm_records_taxamatch(Especies[101:150], 
                                           marine_only = TRUE, 
                                           kingdom = "animalia")
matchedTaxa4<-worrms::wm_records_taxamatch(Especies[151:200], 
                                           marine_only = TRUE, 
                                           kingdom = "animalia")
matchedTaxa5<-worrms::wm_records_taxamatch(Especies[201:250], 
                                           marine_only = TRUE, 
                                           kingdom = "animalia")
matchedTaxa6<-worrms::wm_records_taxamatch(Especies[251:262], 
                                           marine_only = TRUE, 
                                           kingdom = "animalia")

# Unir los dataframes en uno solo
df1 <- dplyr::bind_rows(matchedTaxa1)
df2 <- dplyr::bind_rows(matchedTaxa2)
df3 <- dplyr::bind_rows(matchedTaxa3)
df4 <- dplyr::bind_rows(matchedTaxa4)
df5 <- dplyr::bind_rows(matchedTaxa5)
df6 <- dplyr::bind_rows(matchedTaxa6)





df_final<-rbind(df1, df2, df3, df4, df5, df6)



writexl::write_xlsx(df_final,
                    path = "Worms_matched_Total_ver4.xlsx",
                    col_names = TRUE,
                    format_headers = TRUE,
                    use_zip64 = FALSE
)


