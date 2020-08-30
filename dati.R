# Dati italia
ita <-
  read.csv(
    "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-andamento-nazionale/dpc-covid19-ita-andamento-nazionale.csv"
    , stringsAsFactors =FALSE) %>%
  mutate(data = as_datetime(data)) %>%
  mutate(data = date(data)) %>%
  dplyr::select(data,
                stato,
                ricoverati_con_sintomi,
                terapia_intensiva,
                isolamento_domiciliare)  %>%
  mutate(stato = ifelse(stato == "ITA", "Italia")) %>%
  unique()

colnames(ita)[-c(1, 3, 4, 5)] <- c("denominazione_regione")



 
# Dati regioni
reg <-
  read.csv(
    "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv",
    stringsAsFactors = FALSE) %>%
  dplyr::select(
    data,
    denominazione_regione,
    ricoverati_con_sintomi,
    terapia_intensiva,
    isolamento_domiciliare
  ) %>%
  mutate(data = as_datetime(data)) %>%
  mutate(data = date(data)) %>%
  mutate(denominazione_regione = ifelse(denominazione_regione %in% c("P.A. Bolzano", "P.A. Trento"), "Trentino-Alto Adige",
                                        denominazione_regione))

reg <- reg %>%
  group_by(denominazione_regione, data) %>%
  summarize_if(is.numeric, sum, na.rm = TRUE)

reg <- as.data.frame(reg) %>%
  unique()




ita2 <- bind_rows(ita, reg)



regshp <- st_read("www/Reg01012020_g/Reg01012020_g_WGS84.shp")

