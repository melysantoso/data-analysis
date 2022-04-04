setwd("C://Users/User/Documents/Learning R S02/Internet/")
data <- read.csv("datainternet.csv")
library(tidyverse)
library(dplyr)

data1 <- select(data, Sex, Fakultas, Device)
str(data1)
data1$Sex <- as.factor(data1$Sex)
data1$Fakultas <- as.factor(data1$Fakultas)
data1$Device <- as.factor(data1$Device)
str(data1)

data1 %>% ggplot(aes(y = Fakultas, fill = Sex)) +
  geom_bar(position = position_stack(reverse = TRUE)) +
  geom_text(stat='count', aes(label= round(..count.. / sum(..count..), 2))) +
  theme(legend.position = "top")


data2 <- dplyr::select_if(data, is.numeric)
glimpse(data2)
plot3d(data2$IA, data2$Depresi, data2$Anxiety)

disMat <- as.matrix(dist(data2[,2:5]))
heatmap(disMat)


