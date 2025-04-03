library(readxl)
library(dplyr)
library(ggplot2)
library(forecast)
library(leaflet)
library(tidygeocoder)
library(writexl)
library(leaflet.extras)

df <- read_excel("izmir-2023-2024-Kazalar.xlsx")
df <- na.omit(df) 
head(df, 10)
str(df)

#------------------ Cadde Isı haritası çizimi 
yoğunluk_veri <- df %>%
  group_by(CADDE_LAT, CADDE_LONG) %>%
  summarise(
    TOPLAM_KAZA = n(),
    TURLER = paste(unique(TUR), collapse = ", "),
    .groups = 'drop'
  )

yoğunluk_paleti <- colorNumeric(
  palette = c("green", "yellow", "red"),
  domain = yoğunluk_veri$TOPLAM_KAZA
)

leaflet(yoğunluk_veri) %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addTiles() %>%
  
  # Isı haritası çizimi
  addHeatmap(
    lng = ~CADDE_LONG, lat = ~CADDE_LAT,
    intensity = ~TOPLAM_KAZA,
    blur = 25,         
    max = 0.1,         
    radius = 20        
  ) %>%
  
  # Kümeleme (Marker Clustering)
  addMarkers(
    ~CADDE_LONG, ~CADDE_LAT,
    clusterOptions = markerClusterOptions(
      spiderfyOnMaxZoom = TRUE,
      showCoverageOnHover = FALSE,
      zoomToBoundsOnClick = TRUE
    ),
    label = ~paste("Toplam Kaza:", TOPLAM_KAZA),
    popup = ~paste(
      "<b>Kaza Yoğunluğu:</b>", TOPLAM_KAZA, "<br>",
      "<b>Türler:</b>", TURLER
    )
  ) %>%
  
  # Legend
  addLegend(
    position = "bottomright",
    pal = yoğunluk_paleti,
    values = ~TOPLAM_KAZA,
    title = "Kaza Yoğunluğu<br>(Nokta Başına)",
    bins = 5,
    opacity = 1
  ) %>%
  

  addCircleMarkers(
    ~CADDE_LONG, ~CADDE_LAT,
    radius = ~sqrt(TOPLAM_KAZA)*2,
    fillColor = ~yoğunluk_paleti(TOPLAM_KAZA),
    color = "black",
    weight = 1,
    fillOpacity = 0.7,
    stroke = TRUE
  )
