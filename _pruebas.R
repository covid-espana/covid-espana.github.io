library(dplyr)
library(lubridate)
library(plotly)

file_name <- "hospitales.csv"
file_url = "https://cnecovid.isciii.es/covid19/resources/casos_hosp_uci_def_sexo_edad_provres.csv"
if(!file.exists(file_name)) {download.file(file_url, destfile = file_name)}

# Leemos el csv y lo metemos en el marco de datos df_pcrs
df_hospitales = read.table(
  file = file_name,
  header = TRUE,
  sep = ",",
  dec = ".",
  na.strings = "",
  stringsAsFactors = FALSE
)

# Damos formato a df_hospitales
df_hospitales$fecha = ymd(df_hospitales$fecha) # Convertimos a tipo fecha
df_hospitales = df_hospitales %>%
  rename(provincia = provincia_iso, 
         nro_hospitalizaciones = num_hosp) %>%
  arrange(provincia, fecha)

print("Marco de datos básico")
print(Sys.time())

# Creamos el marco de datos df_hospitales_espana de datos de España
df_hospitales_espana = df_hospitales %>%
  group_by(fecha) %>%
  summarize(nro_hospitalizaciones = sum(nro_hospitalizaciones))

print("Marco de datos de España")
print(Sys.time())

# Creamos el marco de datos df_hospitales_7d de datos semanales
df_hospitales_7d = data.frame()
provincias = unique(df_hospitales$provincia)

for (i in 1:length(provincias)) {
  mi_provincia = provincias[i]
  mi_df = df_hospitales %>% filter(provincia == mi_provincia)
  mi_df_7d = mi_df 
  mi_df_7d$nro_hospitalizaciones = 0 
  for (i in 7:nrow(mi_df)) {
    mi_df_7d$nro_hospitalizaciones[i] = sum(mi_df$nro_hospitalizaciones[(i - 6):i])
  }
  mi_df_7d = mi_df_7d[-c(1,2,3,4,5,6),]
  df_hospitales_7d = rbind(df_hospitales_7d, mi_df_7d)
}

print("Marco de datos de semanas")
print(Sys.time())

# Creamos el marco de datos df_hospitales_espana_7d de datos semanales de España
df_hospitales_espana_7d = df_hospitales_7d %>%
  group_by(fecha) %>%
  summarize(nro_hospitalizaciones = sum(nro_hospitalizaciones))

print("Marco de datos de semanas de España")
print(Sys.time())
