#learning ggplot2 to visualize data 
library(ggplot2)
library(plotly)
library(tidyverse)
library(dplyr)
library(gridExtra)
library(treemapify)

## PERSIAPKAN DATANYA DULU DIBERSIHKAN DAN DIRAPIKAN

#Importing data 
internetdata <- read.csv("datainternet.csv", stringsAsFactors = FALSE)
#View data
View(internetdata)

#buang obs yang gak perlu (nama dan email)
internetdata <- internetdata %>%
  select(-c("Nama", "Email", "Asal")) %>%
  as.data.frame()


#Beberapa data dijadikan nominal 
data_nominal <- (c("Sex", "Fakultas", "Pendidikan", "Device", "WiFi", 
                   "Koneksi", "Alasan", "MedSos", "Lembur", "Layanan",
                   "Situs", "Waktu", "Tempat"))
internetdata[data_nominal] <- lapply(internetdata[data_nominal], function(x){factor(x)})

#Obs Durasi dijadikan ordinal aja biar bisa dianalisis 
internetdata$Durasi <- factor(internetdata$Durasi,
                              ordered = T,
                              levels = c("1- 3 Jam",
                                         "4 - 7 Jam",
                                         "> 7 Jam")) 

internetdata <- as_tibble(internetdata)

#cek dulu
glimpse(internetdata)


## SAATNYA VISUALISASI 

# Demografi responden kita 
jeniskelamin <- internetdata %>% 
  ggplot(aes(x = Sex, fill = Sex)) +
  geom_bar() +
  theme(legend.position = "none", title = element_text(size = 10))

jeniskelamin

fakultas <- internetdata %>%
  ggplot(aes(x = Fakultas, fill = Fakultas)) +
  geom_bar() +
  theme(legend.position = "none", title = element_text(size = 10))

#atau langsung digabung jadi satu chart aja 
sex_fac <- internetdata %>%
  ggplot(aes(x = Fakultas, fill = Sex)) +
  geom_bar(alpha = 3/5) +
  theme(title = element_text(size = 10))

sex_fac  

#dijadikan interaktif bisa? bisa dong bosq~ 
library(plotly)
ggplotly(sex_fac)

#di-Facet Wrap juga bisa? Bisa 
konek_sex_fac <- internetdata %>% 
  ggplot(aes(x = Koneksi, fill = Sex)) +
  theme_bw() +
  facet_wrap(~ Fakultas) +
  geom_bar(alpha = 3/5) +
  labs(title = "Karakteristik Responden Survey", 
       subtitle = "Dipecah dari Fakultas, menurut Jenis Kelamin, dan Koneksi.",
       y = "Frekuensi",
       X = "Fakultas") 

#Persentase 
#Berapa banyak persentase responden yang masuk kategori adiksi dan tidak 
internetdata %>%
  ggplot(aes(x = Addicted,
             y = ..count.. / sum(..count..),
             fill = Addicted)) +
  geom_bar(alpha = 3/5) +
  labs(title = "Persentase Kategori Adiksi",
       y = "Persen",
       x = "Kategori") +
  scale_y_continuous(labels = scales::percent)


#Angka persentase tepatnya 
prop.table(table(internetdata$Addicted)) 
##hasilnya: Addicted  Nonaddic 
##          0.4529148 0.5470852


#persentase responden adiksi perfakultas bagaimana? 
facet_fac_addi <- internetdata %>%
  ggplot(aes(x = Addicted, 
             y = ..count.. / sum(..count..),
             fill = Sex)) +
  theme_bw() +
  facet_wrap(~ Fakultas) +
  geom_bar(alpha = 3/5) + 
  labs(title = "Persentase Adiksi per Fakultas",
       x = "Kategori", 
       y = "Frekuensi") +
  scale_y_continuous(labels = scales::percent)

ggplotly(facet_fac_addi)

#Dibalik juga bisa (maksudnya, pake kategori adiksi tapi isinya fakultas )
internetdata %>%
  ggplot(aes(Addicted, fill = Fakultas)) +
  geom_bar(alpha = 3/5, position = "identity") +
  labs(y = "Responden Survey",
       x = "Kategori",
       title =  "Frekuensi Responden Kecanduan Internet")

#Bagaimana kategori adiksi dari segi koneksi?
ggplot(internetdata, aes(x = Koneksi, fill = Sex)) +
  theme_bw() + 
  facet_wrap(~ Addicted) +
  geom_bar(alpha = 4/5) + 
  labs(y = "Jumlah Responden", 
       title = "Kategori 'Adiksi' dan 'Tidak' Dipecah dari Koneksi",
       subtitle = "frekuensi jenis Kelamin dalam kategori addicted dan nonaddicted berdasarkan koneksi yang digunakan")

internetdata %>%
  ggplot(aes(x = Koneksi, fill = Koneksi)) +
  theme_bw() +
  geom_bar() +
  labs(y = "Jumlah Subjek", 
           title = "Frekuensi Addicted and Non-addicted by Koneksi") +
  theme(legend.position = "none")


