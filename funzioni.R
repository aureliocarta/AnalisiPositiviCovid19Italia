
# Situazione odierna
situazione_odierna <- function(da, data){
  oggi <- data
  dati_oggi <- da %>% dplyr::filter(data %in% c(oggi, oggi-1))
  
  ricov_sintomi_ultimo <- dati_oggi$`ricoverati_con_sintomi`[2] 
  isol_domic_ultimo <- dati_oggi$`isolamento_domiciliare`[2]
  terapia_intens_ultimo <- dati_oggi$`terapia_intensiva`[2]
  

  
  list_out <- list("Terapia intensiva" = terapia_intens_ultimo,
                   "Ricoverati con sintomi" = ricov_sintomi_ultimo, 
                   "Isolamento domiciliare" = isol_domic_ultimo)
  
  return(
    list_out
  )
}



