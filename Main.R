library(readxl)
library(dplyr)
library(ggplot2)
library(forecast)
library(writexl)
library(lubridate)

#--------------- Veri seti okuma ve ön işleme ---------------
df <- read_excel("izmir-2023-2024-Kazalar.xlsx")
df <- na.omit(df) 
head(df, 10)

# Eksik değer kontrolü
sum(is.na(df))

# İlk 10 satırı gösterme
head(df, 10)

# Veri seti hakkında bilgi
str(df)

# Satır ve sütun sayısı
dim(df)

# Veri setini inceleyelim
head(df)

# Veri seti özet istatistikleri
summary(df)

#--------------- Türlerine göre kaza sayıları ---------------
kaza_tur_sayilari <- df %>%
  group_by(TUR) %>%
  summarise(KAZA_SAYISI = n())
head(kaza_tur_sayilari, 50)

#--------------- İstikamete göre 100 den büyük kaza sayıları toplamı ---------------
kaza_istikamet_sayilari_500 <- df %>%
  group_by(ISTIKAMET) %>%
  summarise(KAZA_SAYISI = n())%>%
  filter(KAZA_SAYISI > 500) 
head(kaza_istikamet_sayilari_500, 50)

#--------------- Mevsimlere göre kaza sayıları ---------------
kaza_mevsim_sayilari <- df %>%
  group_by(MEVSIM) %>%
  summarise(KAZA_SAYISI = n())
head(kaza_mevsim_sayilari, 50)

#--------------- Kaza sayısı 100 den büyük cadde toplamı ---------------
kaza_cadde_sayilari_100 <- df %>%
  group_by(CADDE) %>%
  summarise(KAZA_SAYISI = n())%>%
  filter(KAZA_SAYISI > 100) 
head(kaza_cadde_sayilari_100, 50)

#--------------- Kaza sayısı 500 den büyük kaza türleri toplamı ---------------
kaza_tur_sayilari_500 <- df %>%
  group_by(TUR) %>%
  summarise(KAZA_SAYISI = n())%>%
  filter(KAZA_SAYISI > 500) 
head(kaza_sayilari, 50)

#--------------- Yıllık bazda kaza sayıları toplamı ---------------
yillik_kaza_sayilari <- df %>%
  mutate(YIL = year(TARIH)) %>%  
  group_by(YIL) %>%             
  summarise(KAZA_SAYISI = n())  
head(yillik_kaza_sayilari)

#--------------- Mevsim bazında kaza sayıları toplamı ---------------
mevsim_kaza_sayisi <- df %>%
  group_by(MEVSIM) %>%
  summarise(KAZA_SAYISI = n())
head(mevsim_kaza_sayisi)

# Mevsim bazlı kaza sayılarının grafiksel gösterimi
ggplot(mevsim_kaza_sayisi, aes(x = MEVSIM, y = KAZA_SAYISI, fill = MEVSIM)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Mevsim Bazlı Kaza Sayıları", x = "Mevsim", y = "Kaza Sayısı")

#--------------- İstikamete göre 500 den büyük kaza sayıları toplamı ---------------
Istikamet_kaza_sayisi_500 <- df %>%
  group_by(ISTIKAMET) %>%
  summarise(KAZA_SAYISI = n())%>%
  filter(KAZA_SAYISI > 500) 
  head(Istikamet_kaza_sayisi_500)
  
# istikamete göre kaza sayılarının toplamının grafiksel gösterimi
ggplot(Istikamet_kaza_sayisi_500, aes(x = ISTIKAMET, y = KAZA_SAYISI, fill = ISTIKAMET)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "İstikamet Bazlı Kaza Sayıları", x = "İstikamet", y = "Kaza Sayısı")

#df$TARIH <- as.Date(df$TARIH)

#--------------- Zaman İçinde Mevsimsel Kaza Dağılımı ---------------
ggplot(df, aes(x = TARIH, fill = MEVSIM)) +
  geom_histogram(bins = 50, alpha = 0.7) +
  theme_minimal() +
  labs(title = "Zaman İçinde Mevsimsel Kaza Dağılımı", x = "Tarih", y = "Kaza Sayısı")

#Mevsim Bazlı Kaza Türleri Grafiği
ggplot(df, aes(x = MEVSIM, fill = TUR)) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  labs(title = "Mevsim Bazlı Kaza Türleri Dağılımı",
       x = "Mevsim",
       y = "Kaza Sayısı",
       fill = "Kaza Türü") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#--------------- "Ölümlü" türündeki kazalar için analiz ---------------
olumlu_kazalar <- df[df$TUR == "Ölümlü", ]

# Tarih sütununu Date formatına çevir
olumlu_kazalar$TARIH <- as.Date(olumlu_kazalar$TARIH, format = "%Y-%m-%d")

head(olumlu_kazalar)

# Aylık bazda ölümlü kazaları grupla
olumlu_kaza_aylik <- aggregate(TUR ~ month(TARIH), data = olumlu_kazalar, FUN = length)
head(olumlu_kaza_aylik)

# Sütun isimlerini düzenle
colnames(olumlu_kaza_aylik) <- c("Ay", "Kaza_Sayisi")
head(olumlu_kaza_aylik)

# Ay isimleri
ay_isimleri <- c("Ocak", "Şubat", "Mart", "Nisan", "Mayıs", "Haziran", "Temmuz", "Ağustos", 
                 "Eylül", "Ekim", "Kasım", "Aralık")

olumlu_kaza_aylik$Ay <- factor(ay_isimleri[olumlu_kaza_aylik$Ay], levels = ay_isimleri)

# Aylara Göre Ölümlü Kaza Dağılımı grafiği
ggplot(olumlu_kaza_aylik, aes(x = Ay, y = Kaza_Sayisi, group = 1)) +
  geom_line(color = "red", size = 1) + 
  geom_point(size = 3, color = "red") +
  theme_minimal() +
  labs(title = "Aylara Göre Ölümlü Kaza Dağılımı",
       x = "Ay",
       y = "Ölümlü Kaza Sayısı") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#--------------- Kaza sayılarını zaman serisine çevirme ---------------
mevsim_kaza_sayisi_zaman_serisi<- ts(mevsim_kaza_sayisi$KAZA_SAYISI, frequency = 4) # 4 mevsim olduğu için
head(mevsim_kaza_sayisi_zaman_serisi)

#--------------- Yıl ve ay bazında kaza sayıları toplamı ---------------
yillik_aylik_kaza_sayilari <- df %>%
  mutate(YIL = year(TARIH), AY = month(TARIH, label = TRUE)) %>%
  group_by(YIL, AY) %>%
  summarise(KAZA_SAYISI = n(), .groups = "drop")
print(yillik_aylik_kaza_sayilari,n=24)

# Yıl ve aya göre kaza sayılar toplamı grafiği
ggplot(yillik_aylik_kaza_sayilari, aes(x = AY, y = KAZA_SAYISI, group = YIL, color = as.factor(YIL))) +
  geom_line(size = 1) +  
  geom_point(size = 3) +  
  theme_minimal() +
  labs(title = "Aylık Kaza Sayıları", x = "Ay", y = "Kaza Sayısı", color = "Yıl") +
  scale_x_discrete(limits = month.abb)  


#--------------- Arima modeli ile önümüzdeki 12 ayın kaza sayısı tahmini ---------------
aylik_kaza_sayilari_zaman_serisi <- ts(aylik_kaza_sayilari$KAZA_SAYISI, 
              start = c(min(aylik_kaza_sayilari$YIL), min(aylik_kaza_sayilari$AY)), 
              frequency = 12)  

model <- auto.arima(aylik_kaza_sayilari_zaman_serisi)  
forecast_data <- forecast(model, h = 12)  
# hata oranları
accuracy(arima_model)
# 12 aylık tahmin grafiği çiz
plot(forecast_data, 
     main = "Kazaların 12 Aylık ARIMA Tahmini", 
     ylab = "Kaza Sayısı", 
     xlab = "Yıl", 
     col = "blue", 
     fcol = "red", 
     shaded = TRUE)


#--------------- Ortalama, max, min hesaplama ---------------
df$TARIH <- as.Date(df$TARIH, format = "%Y-%m-%d")
# Yıl-Ay formatına dönüştür
df$YIL_AY <- format(df$TARIH, "%Y-%m")
aylik_kaza_sayisi <- df %>%
  group_by(YIL_AY) %>%
  summarise(KAZA_SAYISI = n())

head(aylik_kaza_sayisi)

ortalama_kaza_sayisi <- mean(aylik_kaza_sayisi$KAZA_SAYISI)
# Aylık ortalama kaza sayısını yazdır
print(paste("Aylık Ortalama Kaza Sayısı:", round(ortalama_kaza_sayisi, 2)))

# Aylık kaza sayıları grafiği çiz
ggplot(aylik_kaza_sayisi, aes(x = YIL_AY, y = KAZA_SAYISI)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  theme_minimal() +
  labs(title = "Aylık Kaza Sayıları", x = "Yıl-Ay", y = "Kaza Sayısı") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Günlük kaza sayıları
gunluk_kaza_sayisi <- df %>%
  group_by(TARIH) %>%
  summarise(KAZA_SAYISI = n())

head(gunluk_kaza_sayisi)

# Günlük ortalama kaza sayısını hesapla
ortalama_gunluk_kaza_sayisi <- mean(gunluk_kaza_sayisi$KAZA_SAYISI)

# Günlük ortalama kaza sayısını yazdır
print(paste("Günlük Ortalama Kaza Sayısı:", round(ortalama_gunluk_kaza_sayisi, 2)))


#--------------- Saatlik yoğunluk dağılımı ---------------
# Tarih ve saat bilgisini POSIXct formatına çevir
df$KAZA_ZAMANI_YENI <- as.POSIXct(df$KAZA_ZAMANI, format = "%Y-%m-%d %H:%M")

print(df$KAZA_ZAMANI_YENI)

# Saat bilgisini al
df$SAAT <- format(df$KAZA_ZAMANI_YENI, "%H")
print(df$SAAT)

# Saatlere göre kaza sayısını grupla
saat_kaza_sayisi <- df %>%
  group_by(SAAT) %>%
  summarise(KAZA_SAYISI = n())

print(saat_kaza_sayisi)

df$SAAT_DILIMI <- case_when(
  df$SAAT >= "06" & df$SAAT < "12" ~ "1-Sabah(06-12)",
  df$SAAT >= "12" & df$SAAT < "18" ~ "2-Öğle(12-18)",
  df$SAAT >= "18" & df$SAAT < "24" ~ "3-Akşam(18-24)",
  df$SAAT >= "00" & df$SAAT < "06" ~ "4-Gece(00-06)"
)

# Saat dilimlerine göre kaza sayısını grupla
saat_dilim_kaza_sayisi <- df %>%
  group_by(SAAT_DILIMI) %>%
  summarise(KAZA_SAYISI = n())
head(saat_dilim_kaza_sayisi)
# Saat dilimlerine göre kaza sayısını yazdır
print(saat_dilim_kaza_sayisi)

ggplot(saat_kaza_sayisi, aes(x = SAAT, y = KAZA_SAYISI)) +
  geom_bar(stat = "identity", fill = "blue") +
  theme_minimal() +
  labs(title = "Saat Bazlı Kaza Yoğunluğu", x = "Saat", y = "Kaza Sayısı") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Saat dilimlerine ve kaza türlerine göre kaza sayısını grupla
kaza_turu_dilimi <- df %>%
  group_by(SAAT_DILIMI, TUR) %>%
  summarise(KAZA_SAYISI = n())
print(kaza_turu_dilimi,n=45)


ggplot(kaza_turu_dilimi, aes(x = SAAT_DILIMI, y = KAZA_SAYISI, fill = TUR)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Saat Dilimlerine Göre Kaza Türleri", x = "Saat Dilimi", y = "Kaza Sayısı") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


df <- df[df$MUDAHALE_ZAMANI >= df$KAZA_ZAMANI, ]

df$KAZA_ZAMANI <- as.POSIXct(df$KAZA_ZAMANI, format = "%Y-%m-%d %H:%M")
df$MUDAHALE_ZAMANI <- as.POSIXct(df$MUDAHALE_ZAMANI, format = "%Y-%m-%d %H:%M")

# Kaza zamanı ile müdahale zamanı arasındaki farkı hesapla (dakika cinsinden)
df$MUDAHALE_SURESİ <- as.numeric(difftime(df$MUDAHALE_ZAMANI, df$KAZA_ZAMANI, units = "min"))

print(df$MUDAHALE_SURESİ)

# Ortalama müdahale süresini dakika cinsinden hesapla
ortalama_mudahale_suresi_dakika <- mean(df$MUDAHALE_SURESİ, na.rm = TRUE)
print(ortalama_mudahale_suresi_dakika)

# Max müdahale süresini saniye cinsinden hesapla
max_mudahale_suresi <- max(df$MUDAHALE_SURESİ, na.rm = TRUE)

print(max_mudahale_suresi)

min_mudahale_suresi <- min(df$MUDAHALE_SURESİ, na.rm = TRUE)

print(min_mudahale_suresi)

