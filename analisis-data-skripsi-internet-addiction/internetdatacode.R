#learning ggplot2 to visualize data 
library(ggplot2)
library(tidyverse)
library(dplyr)
library(magrittr)
library(plotly)
#Importing data 
internetdata <- read.csv("datainternet.csv", stringsAsFactors = FALSE)
#View data
View(internetdata)

#attaching data
attach(internetdata)
#Access all 'variable'
names(internetdata)
detach(internetdata)

#set up factor
internetdata$Sex <- as.factor(internetdata$Sex)
internetdata$Usia <- as.factor(internetdata$Usia)
internetdata$Fakultas <- as.factor(internetdata$Fakultas)
internetdata$Pendidikan <- as.factor(internetdata$Pendidikan)
internetdata$Durasi <- as.factor(internetdata$Durasi)
internetdata$Device <- as.factor(internetdata$Device)
internetdata$WiFi <- as.factor(internetdata$WiFi)
internetdata$Koneksi <- as.factor(internetdata$Koneksi)
internetdata$Alasan <- as.factor(internetdata$Alasan)
internetdata$MedSos <- as.factor(internetdata$MedSos)
internetdata$Addicted <- as.factor(internetdata$Addicted)

#1st question - what is the frequency of whose addicted and not-addicted?
ggplot(internetdata, aes(x = Addicted)) +
  geom_bar()
#percentages 
prop.table(table(internetdata$Addicted))

#percent
ggplot(internetdata, 
       aes(x = Addicted,
           y = ..count.. / sum(..count..),
           fill = Addicted)) +
  geom_bar() +
  labs(x = "Kategori Adiksi",
       y = "Persentase",
       title = "Persentase Responden Adiksi") +
  scale_y_continuous(labels = scales::percent)

prop.table(table(internetdata$Addicted))

#or you can do it this way 
ggplot(data = internetdata) +
  geom_bar(mapping = aes(x = Addicted)) 

ggplot(internetdata, aes(x = Addicted, fill = Addicted)) +
  theme_bw() +
  geom_bar() +
  labs(y = "Responden Survey", 
       title = "Frekuensi Responden Addicted Internet")

####trying this way 
## divisualkan pake fill fakultas 
addi <- ggplot(internetdata, aes(Addicted, fill = Fakultas)) +
  geom_bar(alpha = 3/5, position = "identity") +
  labs(y = "Responden Survey",
       x = "Kategori",
       title =  "Frekuensi Responden Kecanduan Internet")
addi
#buat interactive panggil plotly
ggplotly(addi)

#2nd question -  Addicted with sex 

#responden by age 
ggplot(internetdata, aes(x = Usia)) +
  geom_histogram() +
  labs(title = "Research Participants by Age", x = "Age")

addicted_sex <- ggplot(internetdata, aes(x = Sex, fill = Addicted)) +
  theme_bw() + 
  geom_bar() + 
  labs(y = "Jumlah Subjek", 
       title = "Frekuensi Addicted and Non-addicted by Gender")
ggplotly(addicted_sex)
help("geom_text")

require(scales)
ggplot(internetdata, aes(x = Sex, fill = Addicted, position = 'fill')) +
  geom_bar() +
  scale_y_continuous(labels = percent_format())+
  stat_bin(aes(label = paste("n = ", scales::percent((..count..)/sum(..count..)))), vjust=1, geom="text")



#3rd question- what was the addicted rate by media social?

ggplot(internetdata, aes(x = Koneksi, fill = Addicted)) +
  theme_bw() + 
  geom_bar() + 
  labs(y = "Jumlah Subjek", 
       title = "Frekuensi Addicted and Non-addicted by Koneksi")

## trying postion = fill
ggplot(data = internetdata) +
  geom_bar(mapping = aes(x = Addicted, fill = Fakultas), alpha = 3/5, position = "fill") 


#VISUALIZING MAIN VARIABLE - Statistic Modelling 
ia_depres <- ggplot(data = internetdata, 
                    aes(x = IA, 
                        y = Depresi,
                        color = Sex)) +
  geom_point(alpha = .7,
             size = 3) +
  geom_smooth(method = "lm", 
              se = FALSE,
              size = 1.5) +
  labs(y = "Depresi",
       x = "Internet Addiction", 
       title = "Korelasi Internet Addiction & Depresi") 
ia_depres

ggplotly(ia_depres)


##trying tree map
library(treemapify)
plotdata <- internetdata %>%
  count(Alasan)

ggplot(plotdata, 
       aes(fill = Alasan,
           area = n)) +
  geom_treemap() +
  labs(title = "Alasan Menggunakan Smartphone")
  
##trying more useful version 
n <- ggplot(plotdata,
       aes(fill = Alasan, 
           area = n,
           label = Alasan)) +
  geom_treemap() +
  geom_treemap_text(colour = "white",
                    place = "centre") +
  labs("Alasan Penggunaan Internet") +
  theme(legend.position = "none")

n

#Frequency of most media social used by respondent 
plot1 <- internetdata %>%
  count(MedSos)

ggplot(plot1, 
       aes(fill = MedSos,
           area = n,
           label = MedSos)) +
  geom_treemap() +
  geom_treemap_text(colour = "white",
                    place = "centre") +
  labs(title = "Media Sosial yang Sering Digunakan") +
  theme(legend.position = "none")


# question - what was the addicted rate by connection and sex? 

ggplot(internetdata, aes(x = Sex, fill = Addicted)) +
  theme_bw() +
  facet_wrap(~ Koneksi) +
  geom_bar() + 
  labs(y = "Responden Count", 
       title = "Internet Addiction Rates by Connection and Sex")

ggplot(internetdata, aes(x = Sex, fill = Addicted)) +
  theme_bw() +
  facet_wrap(~ Alasan) +
  geom_bar() + 
  labs(y = "Responden Count", 
       title = "Internet Addiction Rates by Sex and Alasan ")

#5th question - what is the distribution of passenger ages?

ggplot(internetdata, aes(x = Usia)) +
  theme_bw() +
  geom_histogram(binwidth = 1) + 
  labs(y = "Responden Count", 
       x = "Age (binwidth = 1",
       title = "Internet Addiction Responden Age Distribution")

#what is internet addiction rates by age when segmented by gender and reason?

ggplot(internetdata, aes(x = Usia, fill = Addicted)) +
  theme_bw() +
  facet_wrap(Sex ~ Koneksi) +
  geom_density(alpha = 0.5) +
  labs(y = "Usia", 
       x = "Addiction Rates", 
       title = "Internet addiction rates by age, sex and connection")



###trying correlation 
newdata <- select(internetdata, IA, Depresi, Anxiety, Stress)
df <- dplyr::select_if(newdata, is.numeric)
r <- cor(df, use="complete.obs")
round(r, 2)
library(ggcorrplot)
rr <- ggcorrplot(r)
rr 

ggcorrplot(r, 
           hc.order = T,
           type = "lower",
           lab = T)

newdata1 <-  select(internetdata, Usia, IA, Depresi, Anxiety, Stress)
internet_lm <- lm(IA ~ Usia + Depresi + Anxiety + Stress,
                  data = newdata1)
internet_lm


library(visreg)
visreg(internet_lm, "Stress", gg = T)
visreg(internetdata$Stress, "internetdata$Depresi", gg = T)

library(vcd)

jj <- xtabs(~Kategori + Sex + Device)

mosaic(jj, main = "Internet Data")

mosaic(jj, 
       shade = T,
       legend = T,
       labeling_args = list(set_vernames = c(Sex = "Gender",
                                             Addicted = "Kategori Adiksi",
                                             Koneksi = "Tipe Koneksi")),
       set_labels = list(Addicted = c("Yes", "No"),
                         Sex = c("Pria", "Wanita"),
                         Koneksi = c("Data", "Wifi")),
       main = "Internet Data")




###Alluvial test
internetall <- internetdata %>%
  group_by(Addicted, Sex, Koneksi) %>%
  count()

head(internetall)

ggplot(internetall, 
       aes(axis1 = Koneksi,
           axis2 = Addicted,
           y = n)) +
  geom_alluvium(aes(fill = Sex)) +
  geom_stratum() +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Koneksi", "Addicted"),
                   expand = c(.1, .1)) +
  labs(title = "Internet Data",
       subtitle = "stratified by koneksi, sex, and addicted",
       y = "Frequency") +
  theme_minimal()

##the second alluv
internetall2 <- internetdata %>% 
  group_by(Addicted, Sex, Koneksi, Fakultas) %>%
  count()

ggplot(internetall2, 
       aes(axis1 = Addicted,
           axis2 = Koneksi,
           axis3 = Fakultas,
           y = n)) +
  geom_alluvium(aes(fill = Sex)) +
  geom_stratum() +
  geom_text(stat = "stratum", 
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Addicted", "Koneksi", "Fakultas"),
                   expand = c(.1, .1)) +
  scale_fill_viridis_d() +
  labs(title = "Internet Data",
       subtitle = "stratified by addicting category, koneksi, dan layanan",
       y = "Frequency",
       x = "Demographic") +
  theme_minimal() 



##trying heatmap 
resp <- select(internetdata, Usia, IA, Depresi, Anxiety, Stress)
superheat(resp, scale = T)
superheat(mtcars, scale = T)







###membuat kategorisasi adiksi dan nonadiksi 
# internetdata$kategori <- ifelse(internetdata$IA < 38, 0, 1)
# internetdata$kategori <- ifelse(internetdata$kategori==0, "nonadiksi", 
#                          ifelse(internetdata$kategori==1, "adiksi", NA))
