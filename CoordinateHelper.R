library(readxl)
library(leaflet)
library(tidygeocoder)
library(writexl)

df <- read_excel("izmir-2023-2024-Kazalar.xlsx")
df <- na.omit(df) 

# exceldeki her bir satır için enlem ve boylam hesapla
df <- df %>%
geocode(address = ADRES_CADDE, method = "osm")  

head(df, 10)

# enlem ve boylam ile birlikte exceli kaydet
write_xlsx(df, "lokasyonlar_guncellenmis.xlsx")
