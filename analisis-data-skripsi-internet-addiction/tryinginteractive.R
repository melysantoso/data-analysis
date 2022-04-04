library(tidyverse)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(plotly)
library(psych)
library(corrr)
library(corrplot)

setwd("C://Users/User/Documents/Learning R S02/Internet/")
data <- read.csv("datainternet.csv", stringsAsFactors = FALSE, header = TRUE)
glimpse(data)

#remove some unused variables -c(Nama, Email, Asal)
data <- data %>% 
  select(-c("Nama", "Email", "Asal", "Addicted")) %>% 
  as.data.frame()

#beberapa data dijadikan nominal 
data.nominal <- c("Sex", "Fakultas", "Pendidikan", "Device", "WiFi", 
                    "Koneksi", "Alasan", "MedSos", "Lembur", "Layanan",
                    "Situs", "Waktu", "Tempat")
data[data.nominal] <- lapply(data[data.nominal], function(x){factor(x)})

#membuat data durasi menjadi ordinal 
data$Durasi <- factor(data$Durasi,
                      ordered = T,
                      levels = c("1- 3 Jam",
                                 "4 - 7 Jam",
                                 "> 7 Jam")) 

#membuat kategorisasi responden adiksi dengan mean skor IA 
data$Kategori <- ifelse(data$IA <= round(mean(data$IA)), "nonadiksi", "adiksi")
str(data)
data$Kategori <- as.factor(data$Kategori)

##VIZ

plot_ly(data = data, x = ~IA, y = ~Depresi, z = ~Stress,
        color = ~Fakultas, 
        type = "scatter3d",
        mode = "markers") %>% 
  layout(scene = list(xaxis = list(title = "Internet Addiction"),
                      yaxis = list(title = "Depression"),
                      zaxis = list(title = "Stress")))



##################################### 

data("iris")
plot_ly(
  # (1) data; (2) # Assign X, Y, and Z variables (put '~' before each variable).
  data = iris, x = ~Sepal.Length, y = ~Petal.Length, z = ~Petal.Width,
  color = ~Species,  # Separate variable by color. Put '~' before variable.
  type = "scatter3d",  # Makes a 3D scatterplot.
  mode = "markers"  # Use markers. 
) %>%  
  layout(scene = list(xaxis = list(title = 'Sepal length'), # Assign x, y, & z axes names. 
                      yaxis = list(title = 'Petal length'),
                      zaxis = list(title = 'Petal width')))
