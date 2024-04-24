library(rstatix)
library(dplyr) 
library(ggplot2)
library("ggpubr")
library("dunn.test")
library("FSA")
library(pscl)
library("performance")
library(car)
library(lmerTest)
library(glmmTMB)
library(sjPlot)
library(dplyr)
library(olsrr)
library(ggplot2)
library("fitdistrplus")
library("tibble")
library("tribble")
library(tidyverse)
library(corrplot)
library(agricolae)
library(promises)
library(hrbrthemes)
library(ggplot2)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(agricolae)
library(ggbreak)

setwd("C:/Users/Mylka/Desktop/MAGISTERKA")


#####################################################
#owady
################################################################################
############udział
data=read.csv(file="Zapylacze_magisterka.csv", header=T, sep=",")
str(data)
data=data[,c(4,7)]
data=na.omit(data)
#podsumowanie
group_by(data, owad) %>%
  summarise(
    count = n(),
    mean = mean(visits, na.rm = TRUE),sd = sd(visits, na.rm = TRUE), median = median(visits, na.rm = TRUE), IQR = IQR(visits, na.rm = TRUE),se   = sd / sqrt(count) )
##porównanie
shapiro.test(data$visits) #nienormalny
x=kruskal.test(data$visits,data$owad, alpha = 0.05, p.adj="bonferroni")
print(x) #brak różńic


# WYKES KOŁOWY
data=read.csv(file="Zapylacze_magisterka.csv", header=T, sep=",") #ładujemy ponownie
data=data[,c(4,10)]
data=na.omit(data)

pie <- ggplot(data, aes(x="", y=share, fill=owad)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)

#LEGENDA + PODPISY OSI
pie + theme_minimal() + 
  labs(x="", y="")+
  font("xy.text", size = 12, color = "black") +
  font("legend.title", size = 15, color = "black")+
  font("legend.text", size = 15, color = "black")+
  scale_fill_manual(values=c("#F2C14E", "#5FAD56"), name = "Morfogrupy:", labels = c(~italic("Apis mellifera"),("Pszczoła samotna")))

ggsave("owady_udział.tiff", units="in", width=7, height=7, dpi=200,bg = "white", compression = 'lzw')


####################################################średni czas trwania odwiedzin
data=read.csv(file="Zapylacze_magisterka.csv", header=T, sep=",")
str(data)
data=data[,c(4,5)]
data=na.omit(data)
group_by(data, owad) %>%
  summarise(
    count = n(),
    mean = mean(time, na.rm = TRUE),sd = sd(time, na.rm = TRUE), median = median(time, na.rm = TRUE), IQR = IQR(time, na.rm = TRUE),se   = sd / sqrt(count) )
##porównanie
shapiro.test(data$time)
x=kruskal.test(data$time,data$owad, alpha = 0.05, p.adj="bonferroni")
print(x) 
#wykres 
# Plot ##dobrze bęzie zmienić wielkość podpisów n aosiach, nie mam czasu juz grzebać, zostawaim Tobie
plot=data %>%
  
  ggplot( aes(x=owad, y=time, fill=owad)) +
  geom_boxplot() +
  scale_fill_manual(values=c("#F2C14E", "#5FAD56")) +
  geom_jitter(color="black", size=1, alpha=2) +
  theme_classic()

plot + theme(legend.position='none') +
  xlab("Owady") +
  ylab("Długość trwania wizyt [s]") +
  theme(axis.title.y = element_text(size = 10))+theme(axis.title.x = element_text(size = 10)) +
  font("xy.text", size = 18, color = "black") +
  font("xlab", size = 18, color = "black") +
  font("ylab", size = 18, color = "black") +
  scale_x_discrete(labels=c(~italic("Apis mellifera"),("Pszczoła samotna")))
  

 

ggsave("długosć_wizyt.tiff", units="in", width=7, height=7, dpi=200,bg = "white", compression = 'lzw')



###############################################częstość na godzinę
data=read.csv(file="Zapylacze_magisterka.csv", header=T, sep=",")
str(data)
data=data[,c(4,9)]
data=na.omit(data)
group_by(data, owad) %>%
  summarise(
    count = n(),
    mean = mean(mean_hour, na.rm = TRUE),sd = sd(mean_hour, na.rm = TRUE), median = median(mean_hour, na.rm = TRUE), IQR = IQR(mean_hour, na.rm = TRUE),se   = sd / sqrt(count) )
shapiro.test(data$mean_hour)
x=kruskal.test(data$mean_hour,data$owad, alpha = 0.05, p.adj="bonferroni")
print(x) 

#wykres 
# Plot ##dobrze bęzie zmienić wielkość podpisów n aosiach, nie mam czasu juz grzebać, zostawaim Tobie

plot=data %>%
  
  ggplot( aes(x=owad, y=mean_hour, fill=owad)) +
  geom_boxplot() +
  scale_fill_manual(values=c("#F2C14E", "#5FAD56")) +
  geom_jitter(color="black", size=1, alpha=2) +
  theme_classic()

plot + theme(legend.position='none') +
  xlab("Owady") +
  ylab("Częstość wizyt owadów / h") +
  theme(axis.title.y = element_text(size = 10))+theme(axis.title.x = element_text(size = 10)) +
  font("xy.text", size = 18, color = "black") +
  font("xlab", size = 18, color = "black") +
  font("ylab", size = 18, color = "black") +
  scale_x_discrete(labels=c(~italic("Apis mellifera"),("Pszczoła samotna")))

ggsave("czestosc_wizyt.tiff", units="in", width=7, height=7, dpi=200,bg = "white", compression = 'lzw')
########################penetracja
data=read.csv(file="Zapylacze_magisterka.csv", header=T, sep=",")
str(data)
data=data[,c(4,6)]
data=na.omit(data)
group_by(data, owad) %>%
  summarise(
    count = n(),
    mean = mean(flowers, na.rm = TRUE),sd = sd(flowers, na.rm = TRUE), median = median(flowers, na.rm = TRUE), IQR = IQR(flowers, na.rm = TRUE),se   = sd / sqrt(count) )
shapiro.test(data$flowers)
x=kruskal.test(data$flowers,data$owad, alpha = 0.05, p.adj="bonferroni")
print(x) 

#wykres PENTRACJA
  
plot=data %>%
  ggplot( aes(x=owad, y=flowers, fill=owad)) +
  geom_boxplot() +
  scale_fill_manual(values=c("#F2C14E", "#5FAD56")) +
  geom_jitter(color="black", size=1, alpha=2) +
  theme_classic()

plot + theme(legend.position='none') +
  xlab("Owady") +
  ylab("Liczba odwiedzanych kwiatów") +
  theme(axis.title.y = element_text(size = 10))+theme(axis.title.x = element_text(size = 10)) +
  font("xy.text", size = 18, color = "black") +
  font("xlab", size = 18, color = "black") +
  font("ylab", size = 18, color = "black") +
  scale_x_discrete(labels=c(~italic("Apis mellifera"),("Pszczoła samotna")))


ggsave("penetracja.tiff", units="in", width=7, height=7, dpi=200,bg = "white", compression = 'lzw')

##################################################################################################################3
###########################Kwiaty
########################################################################################################################
##############jaka jest wielkość kwiatów białe vs ciemne na poszczególnych piętrach
#jasna
data=read.csv(file="kwiaty.csv", header=T, sep=",")
data$Nr_rośliny=as.character(data$Nr_rośliny)
data$Nr_kwiatu=as.character(data$Nr_kwiatu)
str(data)

data <- data %>%
  filter(Kolor == "jasna") %>%
  #select(1:6) %>%
  droplevels()
str(data)
#Tabelka do podglądu
data1 <- group_by(data, Nr_kwiatu) %>%
  summarise(
    count = n(),
    mean = mean(Średnica, na.rm = TRUE),sd = sd(Średnica, na.rm = TRUE), median = median(Średnica, na.rm = TRUE), IQR = IQR(Średnica, na.rm = TRUE),se   = sd / sqrt(count) )
shapiro.test(data$Średnica)
x=kruskal.test(data$Średnica,data$Nr_kwiatu, alpha = 0.05, p.adj="bonferroni")
print(x) 
str(data)
#ciemna
data=read.csv(file="kwiaty.csv", header=T, sep=",")
data$Nr_rośliny=as.character(data$Nr_rośliny)
data$Nr_kwiatu=as.character(data$Nr_kwiatu)
str(data)
data <- data %>%
  filter(Kolor == "ciemna") %>%
  #select(1:6) %>%
  droplevels()
str(data)
#Tabelka do podglądu
data2 <- group_by(data, Nr_kwiatu) %>%
  summarise(
    count = n(),
    mean = mean(Średnica, na.rm = TRUE),sd = sd(Średnica, na.rm = TRUE), median = median(Średnica, na.rm = TRUE), IQR = IQR(Średnica, na.rm = TRUE),se   = sd / sqrt(count) )


shapiro.test(data$Średnica) #normanlyn
a1 <- aov(data$Średnica ~ data$Nr_kwiatu)
summary(a1)

#porównianie na tym samym poziomie jasne vs ciemne
##na pewno jest na to jakaś fromuła, ale z braku czasu robię kwiaty po kolei
data=read.csv(file="kwiaty.csv", header=T, sep=",")
data$Nr_rośliny=as.character(data$Nr_rośliny)
data$Nr_kwiatu=as.character(data$Nr_kwiatu)
data <- data %>%
  filter(Nr_kwiatu == "1") %>%
  #select(1:6) %>%
  droplevels()
str(data)
shapiro.test(data$Średnica) #normanlyn
a1 <- aov(data$Średnica ~ data$Kolor)
summary(a1)

data1 <- group_by(data, Nr_kwiatu) %>%
  summarise(
    count = n(),
    mean = mean(Średnica, na.rm = TRUE),sd = sd(Średnica, na.rm = TRUE), median = median(Średnica, na.rm = TRUE), IQR = IQR(Średnica, na.rm = TRUE),se   = sd / sqrt(count) )


data=read.csv(file="kwiaty.csv", header=T, sep=",")
data$Nr_rośliny=as.character(data$Nr_rośliny)
data$Nr_kwiatu=as.character(data$Nr_kwiatu)
data <- data %>%
  filter(Nr_kwiatu == "2") %>%
  #select(1:6) %>%
  droplevels()
str(data)
shapiro.test(data$Średnica) #normanlyn
a2 <- aov(data$Średnica ~ data$Kolor)
summary(a2)

data2 <- group_by(data, Nr_kwiatu) %>%
  summarise(
    count = n(),
    mean = mean(Średnica, na.rm = TRUE),sd = sd(Średnica, na.rm = TRUE), median = median(Średnica, na.rm = TRUE), IQR = IQR(Średnica, na.rm = TRUE),se   = sd / sqrt(count) )

data=read.csv(file="kwiaty.csv", header=T, sep=",")
data$Nr_rośliny=as.character(data$Nr_rośliny)
data$Nr_kwiatu=as.character(data$Nr_kwiatu)
data <- data %>%
  filter(Nr_kwiatu == "3") %>%
  #select(1:6) %>%
  droplevels()
str(data)
shapiro.test(data$Średnica) #normanlyn
a3 <- aov(data$Średnica ~ data$Kolor)
summary(a3)

data3 <- group_by(data, Nr_kwiatu) %>%
  summarise(
    count = n(),
    mean = mean(Średnica, na.rm = TRUE),sd = sd(Średnica, na.rm = TRUE), median = median(Średnica, na.rm = TRUE), IQR = IQR(Średnica, na.rm = TRUE),se   = sd / sqrt(count) )


data=read.csv(file="kwiaty.csv", header=T, sep=",")
data$Nr_rośliny=as.character(data$Nr_rośliny)
data$Nr_kwiatu=as.character(data$Nr_kwiatu)
data <- data %>%
  filter(Nr_kwiatu == "25") %>%  
 # select(1:6) %>%
  droplevels()
str(data)
shapiro.test(data$Średnica) #normanlyn
a4 <- aov(data$Średnica ~ data$Kolor)
summary(a4)

data4 <- group_by(data, Nr_kwiatu) %>%
  summarise(
    count = n(),
    mean = mean(Średnica, na.rm = TRUE),sd = sd(Średnica, na.rm = TRUE), median = median(Średnica, na.rm = TRUE), IQR = IQR(Średnica, na.rm = TRUE),se   = sd / sqrt(count) )

#########################RESZTA ROBIONA ZMIENIAJĄC TYLKO CYFERKĘ (TAK SZYBCIEJ)####################################################

################wykres dla obydwu barw
data=read.csv(file="kwiaty.csv", header=T, sep=",")
data$Nr_rośliny=as.character(data$Nr_rośliny)
data$Nr_kwiatu=as.character(data$Nr_kwiatu)
data$Nr_kwiatu <- factor(data$Nr_kwiatu,                                    # Change ordering manually
                         levels = c("1", "2", "3", "4", "5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25")) #to robimy, żeby na wyklresie ustawiał je po kolei, a nie 1, 10, 11 itd

plot=data %>%
  ggplot( aes(x=Nr_kwiatu, names.arg=Nr_kwiatu, y=Średnica, fill=Kolor)) +
  geom_boxplot() +
  scale_fill_manual(values=c("plum3", "darkseagreen3"), name = "Kolory:", labels = c(("Ciemna"),("Jasna"))) + 
  geom_jitter(color="black", size=1, alpha=2) +
  theme_classic2()

plot + theme(legend.position='right') +
  xlab("Numery kwiatów w obrębie jednego kwiatostanu") +
  ylab("Średnica kwiatów") +
  theme(axis.title.y = element_text(size = 10))+theme(axis.title.x = element_text(size = 10)) +
  font("xy.text", size = 12, color = "black") +
  font("xlab", size = 15, color = "black") +
  font("ylab", size = 15, color = "black") +
  font("legend.title", size = 10, color = "black") +
  font("legend.text", size = 10, color = "black")

ggsave("Średniaca kwaitów.tiff", units="in", width=7, height=7, dpi=200,bg = "white", compression = 'lzw')


###############################################jaka jest aranżacja elementów na różnych piętrach?
### proponję korelację między odległościa krótkich/długich pylników i słupkiem a piętrem kwiatu
#najpierw z podziałem na kolory
data=read.csv(file="deposition.csv", header=T, sep=",")
data$Nr_rośliny=as.character(data$Nr_rośliny)
data$Nr_kwiatu=as.character(data$Nr_kwiatu)
#jasne
data <- data %>%
  filter(morph == "green") %>% #Nazwa w kolumnie
  #select(1:17) %>%
  droplevels()
#rozkład
shapiro.test(data$sty_l_ant) #nienormanlyn
kruskal.test(sty_l_ant ~ Nr_kwiatu, data = data) #skorelowane
#ciemne
data=read.csv(file="deposition.csv", header=T, sep=",")
data$Nr_rośliny=as.character(data$Nr_rośliny)
data$Nr_kwiatu=as.character(data$Nr_kwiatu)
data <- data %>%
  filter(morph == "purple") %>% #Nazwa w kolumnie
  #select(1:17) %>%
  droplevels()

#rozkład
shapiro.test(data$sty_l_ant) #nienormanlyn
kruskal.test(sty_l_ant ~ Nr_kwiatu, data = data) #skorelowane

###########wykres dla obydwu morf
data=read.csv(file="deposition.csv", header=T, sep=",")
data$Nr_rośliny=as.character(data$Nr_rośliny)
data$Nr_kwiatu=as.character(data$Nr_kwiatu)
data$Nr_kwiatu <- factor(data$Nr_kwiatu,                                    # Change ordering manually
                         levels = c("1", "2", "3", "4", "5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25"))


plot=data %>%
  ggplot( aes(x=Nr_kwiatu, names.arg=Nr_kwiatu, y=sty_l_ant, fill=morph)) +
  geom_boxplot() +
  scale_fill_manual(values=c("plum3", "darkseagreen3"), name = "Kolory:", labels = c(("Ciemna"),("Jasna"))) + 
  geom_jitter(color="black", size=1, alpha=2) +
  theme_classic2()

plot + theme(legend.position='right') +
  xlab("Numery kwiatów w obrębie jednego kwiatostanu") +
  ggtitle("Aranżacja w piętrach-długie") +
  ylab("Odległość między ( )") +
  theme(axis.title.y = element_text(size = 10))+theme(axis.title.x = element_text(size = 10)) +
  font("xy.text", size = 12, color = "black") +
  font("xlab", size = 15, color = "black") +
  font("ylab", size = 15, color = "black") +
  font("legend.title", size = 10, color = "black") +
  font("legend.text", size = 10, color = "black")

ggsave("Aranżacja w piętrach-długie", units="in", width=7, height=7, dpi=200,bg = "white", compression = 'lzw')

###krótki e pylniki 
data=read.csv(file="deposition.csv", header=T, sep=",")
data$Nr_rośliny=as.character(data$Nr_rośliny)
data$Nr_kwiatu=as.character(data$Nr_kwiatu)
#jasne
data <- data %>%
  filter(morph == "green") %>%
  #select(1:17) %>%
  droplevels()
#rozkład
shapiro.test(data$sty_s_ant) #nienormanlyn
kruskal.test(sty_s_ant ~ Nr_kwiatu, data = data) #skorelowane
#ciemne
data=read.csv(file="deposition.csv", header=T, sep=",")
data$Nr_rośliny=as.character(data$Nr_rośliny)
data$Nr_kwiatu=as.character(data$Nr_kwiatu)
data <- data %>%
  filter(morph == "purple") %>%
 # select(1:17) %>%
  droplevels()

#rozkład
shapiro.test(data$sty_s_ant) #nienormanlyn
kruskal.test(sty_s_ant ~ Nr_kwiatu, data = data) #skorelowane

###########wykres dla obydwu morf
data=read.csv(file="deposition.csv", header=T, sep=",")
data$Nr_rośliny=as.character(data$Nr_rośliny)
data$Nr_kwiatu=as.character(data$Nr_kwiatu)
data$Nr_kwiatu <- factor(data$Nr_kwiatu,                                    # Change ordering manually
                         levels = c("1", "2", "3", "4", "5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25"))


plot=data %>%
  ggplot( aes(x=Nr_kwiatu, names.arg=Nr_kwiatu, y=sty_s_ant, fill=morph)) +
  geom_boxplot() +
  scale_fill_manual(values=c("plum3", "darkseagreen3"), name = "Kolory:", labels = c(("Ciemna"),("Jasna"))) + 
  geom_jitter(color="black", size=1, alpha=2) +
  theme_classic2()

plot + theme(legend.position='right') +
  xlab("Numery kwiatów w obrębie jednego kwiatostanu") +
  ggtitle("Aranżacja w piętrach-krótkie") +
  ylab("Odległość między ( )") +
  theme(axis.title.y = element_text(size = 10))+theme(axis.title.x = element_text(size = 10)) +
  font("xy.text", size = 12, color = "black") +
  font("xlab", size = 15, color = "black") +
  font("ylab", size = 15, color = "black") +
  font("legend.title", size = 10, color = "black") +
  font("legend.text", size = 10, color = "black")


ggsave("Aranżacja w piętrach-krótkie.tiff", units="in", width=7, height=7, dpi=200,bg = "white", compression = 'lzw')


#############jeszcze sprawdzam to samo dla najdłuższych i najkrótkszych pylników 
###########max
#najpierw z podziałem na kolory
data=read.csv(file="deposition.csv", header=T, sep=",")
data$Nr_rośliny=as.character(data$Nr_rośliny)
data$Nr_kwiatu=as.character(data$Nr_kwiatu)
#jasne
data <- data %>%
  filter(morph == "green") %>%
  #select(1:17) %>%
  droplevels()
#rozkład
shapiro.test(data$max) #nienormanlyn
kruskal.test(max ~ Nr_kwiatu, data = data) #brak
#ciemne
data=read.csv(file="deposition.csv", header=T, sep=",")
data$Nr_rośliny=as.character(data$Nr_rośliny)
data$Nr_kwiatu=as.character(data$Nr_kwiatu)
data <- data %>%
  filter(morph == "purple") %>%
  #select(1:17) %>%
  droplevels()

#rozkład
shapiro.test(data$max) #nienormanlyn
kruskal.test(max ~ Nr_kwiatu, data = data) #brak

###########wykres dla obydwu morf
data=read.csv(file="deposition.csv", header=T, sep=",")
data$Nr_rośliny=as.character(data$Nr_rośliny)
data$Nr_kwiatu=as.character(data$Nr_kwiatu)
data$Nr_kwiatu <- factor(data$Nr_kwiatu,                                    # Change ordering manually
                         levels = c("1", "2", "3", "4", "5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25"))



plot=data %>%
  ggplot( aes(x=Nr_kwiatu, names.arg=Nr_kwiatu, y=max, fill=morph)) +
  geom_boxplot() +
  scale_fill_manual(values=c("plum3", "darkseagreen3"), name = "Kolory:", labels = c(("Ciemna"),("Jasna"))) + 
  geom_jitter(color="black", size=1, alpha=2) +
  theme_classic2()

plot + theme(legend.position='right') +
  xlab("Numery kwiatów w obrębie jednego kwiatostanu") +
  ylab("Odległość między ( )") +
  ggtitle("Aranżacja w piętrach-max_pręciki") +
  theme(axis.title.y = element_text(size = 10))+theme(axis.title.x = element_text(size = 10)) +
  font("xy.text", size = 12, color = "black") +
  font("xlab", size = 15, color = "black") +
  font("ylab", size = 15, color = "black") +
  font("legend.title", size = 10, color = "black") +
  font("legend.text", size = 10, color = "black")


ggsave("Aranżacja w piętrach-max_pręciki.tiff", units="in", width=7, height=7, dpi=200,bg = "white", compression = 'lzw')

#########min
#najpierw z podziałem na kolory
data=read.csv(file="deposition.csv", header=T, sep=",")
data$Nr_rośliny=as.character(data$Nr_rośliny)
data$Nr_kwiatu=as.character(data$Nr_kwiatu)
#jasne
data <- data %>%
  filter(morph == "green") %>%
 # select(1:17) %>%
  droplevels()
#rozkład
shapiro.test(data$min) #nienormanlyn
kruskal.test(min ~ Nr_kwiatu, data = data) #brak
#ciemne
data=read.csv(file="deposition.csv", header=T, sep=",")
data$Nr_rośliny=as.character(data$Nr_rośliny)
data$Nr_kwiatu=as.character(data$Nr_kwiatu)
data <- data %>%
  filter(morph == "purple") %>%
 # select(1:17) %>%
  droplevels()

#rozkład
shapiro.test(data$min) #nienormanlyn
kruskal.test(min ~ Nr_kwiatu, data = data) #brak

###########wykres dla obydwu morf
data=read.csv(file="deposition.csv", header=T, sep=",")
data$Nr_rośliny=as.character(data$Nr_rośliny)
data$Nr_kwiatu=as.character(data$Nr_kwiatu)
data$Nr_kwiatu <- factor(data$Nr_kwiatu,                                    # Change ordering manually
                         levels = c("1", "2", "3", "4", "5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25"))


plot=data %>%
  ggplot( aes(x=Nr_kwiatu, names.arg=Nr_kwiatu, y=min, fill=morph)) +
  geom_boxplot() +
  scale_fill_manual(values=c("plum3", "darkseagreen3"), name = "Kolory:", labels = c(("Ciemna"),("Jasna"))) + 
  geom_jitter(color="black", size=1, alpha=2) +
  theme_classic2()

plot + theme(legend.position='right') +
  xlab("Numery kwiatów w obrębie jednego kwiatostanu") +
  ylab("Odległość między ( )") +
  ggtitle("Aranżacja w piętrach-min_pręciki") +
  theme(axis.title.y = element_text(size = 10))+theme(axis.title.x = element_text(size = 10)) +
  font("xy.text", size = 12, color = "black") +
  font("xlab", size = 15, color = "black") +
  font("ylab", size = 15, color = "black") +
  font("legend.title", size = 10, color = "black") +
  font("legend.text", size = 10, color = "black")

ggsave("Aranżacja w piętrach-min_pręciki.tiff", units="in", width=7, height=7, dpi=200,bg = "white", compression = 'lzw')



###różnice między okółkami
data=read.csv(file="Persica_2023 - nectary.csv", header=T, sep=",")
str(data)
data=data[,(c(3:5,9))]
data=na.omit(data)
data$plant_no=as.factor(data$plant_no)
data$flower_no=as.factor(data$flower_no)
data$year=as.factor(data$year)
data$whorl=as.factor(data$whorl)
data= data %>%
  filter(year == "2020") %>%
  droplevels()

kruskal.test(nectar_v ~ whorl, data = data)
x=kruskal.test(data$nectar_v,data$whorl, alpha = 0.05, p.adj="bonferroni")
print(x)

data=read.csv(file="Persica_2023 - nectary.csv", header=T, sep=",")
str(data)
data=data[,(1:10)]
data=na.omit(data)
data$plant_no=as.factor(data$plant_no)
data$flower_no=as.factor(data$flower_no)
data$year=as.factor(data$year)
data$whorl=as.factor(data$whorl)
data= data %>%
  filter(year == "2023") %>%
  droplevels()

kruskal.test(nectar_v ~ whorl, data = data)
x=kruskal.test(data$nectar_v,data$whorl, alpha = 0.05, p.adj="bonferroni")
print(x)
data1=data[,5]
mean(data1)


#####Podpisy polskie BARTEK - kod ogarnięty, śmiało modyfikuj

datacor=data[,c(4:7)]
x=cor(datacor[,c(1:4)],use="pairwise.complete.obs",method="spearman")

colnames(x) <- c("objętość nektaru", "koncentracja nektaru", "długość", "szerokość")
rownames(x) <- c("objętość nektaru", "koncentracja nektaru", "długość", "szerokość")

corrplot::corrplot(x,method="number",
                   type = "upper",tl.col = "black",
                   cl.cex=1,tl.cex = 1,number.cex=1)

x=kruskal.test(data$nectar_v,data$length, alpha = 0.05, p.adj="bonferroni")
print(x)


#zewnętrzne
data= data %>%
  filter(whorl == "outer") %>%
  droplevels()

shapiro.test(data$nectar_v)
##########HISTOGRAM KONCENTRACJA NEKTARU###########
hist(data$nectar_con,
     col = 'wheat1',
     border = "blue",
     xlab = 'Koncentracja nektaru',
     ylab = '( )',
     main = 'Koncentracja nektaru w zewnętrznych nektarnikach',
     breaks = "scott",
     xlim = c(0, 100))
  
data$nectar_v_log=log(data$nectar_v+1)
hist(data$nectar_v,
     col = 'wheat1',
     border = "blue",
     xlab = '( )',
     ylab = 'Objętość [V]',
     main = 'Objętość nektaru w zewnętrznych nektarnikach',
     breaks = "scott",
     xlim = c(0, 20))
     
shapiro.test(data$nectar_con)

data$nectar_con_log=log(data$nectar_con)
hist(data$length)
shapiro.test(data$length)
#data$length_log=log(data$length)
shapiro.test(data$width)
data$width_log=log(data$width)
data$nectar_con_log=pmax(data$nectar_con_log,0)
data$nectar_v_log=pmax(data$nectar_v_log,0)

model_zip <- glmmTMB(nectar_con_log ~nectar_v_log + width_log+ length +(1|plant_no), data = data, family = poisson(link = "log"), ziformula = ~ 1)
tab_model(model_zip, transform = NULL)
check_collinearity(model_zip)
AIC(model_zip)
model_zip <- glmmTMB(nectar_con ~nectar_v + width+ length +(1|plant_no), data = data, family = poisson(link = "log"), ziformula = ~ 1)
tab_model(model_zip, transform = NULL)
check_collinearity(model_zip)
AIC(model_zip)

model_zinb <- glmmTMB(nectar_con ~nectar_v_log+ width+ length +(1|plant_no), data = data, family = nbinom2(link = "log"), ziformula = ~ 1)
tab_model(model_zinb, transform = NULL)
check_collinearity(model_zinb)
AIC(model_zinb)
model_zip <- glmmTMB(nectar_con ~nectar_v_log+ width+ length +(1|plant_no), data = data, family = poisson(link = "log"), ziformula = ~ 1)
tab_model(model_zip, transform = NULL)
check_collinearity(model_zip)
AIC(model_zip)


#wew
data=read.csv(file="Persica_2023 - nectary.csv", header=T, sep=",")
str(data)
data=data[,(1:10)]
data=na.omit(data)
data$plant_no=as.factor(data$plant_no)
data$flower_no=as.factor(data$flower_no)
data$year=as.factor(data$year)
data$whorl=as.factor(data$whorl)
data= data %>%
  filter(year == "2023") %>%
  droplevels()
data= data %>%
  filter(whorl == "inner") %>%
  droplevels()

shapiro.test(data$nectar_v)
hist(data$nectar_con)
data$nectar_v_log=log(data$nectar_v+1)
hist(data$nectar_v)
shapiro.test(data$nectar_con)
data$nectar_con_log=log(data$nectar_con)
hist(data$length)
shapiro.test(data$length)
#data$length_log=log(data$length)
shapiro.test(data$width)
data$width_log=log(data$width)
data$nectar_con_log=pmax(data$nectar_con_log,0)
data$nectar_v_log=pmax(data$nectar_v_log,0)

model_zip <- glmmTMB(nectar_con_log ~nectar_v_log + width_log+ length +(1|plant_no), data = data, family = poisson(link = "log"), ziformula = ~ 1)
tab_model(model_zip, transform = NULL)
check_collinearity(model_zip)
AIC(model_zip)
model_zinb <- glmmTMB(nectar_con_log ~nectar_v_log + width_log+ length +(1|plant_no), data = data, family = nbinom2(link = "log"), ziformula = ~ 1)
tab_model(model_zinb, transform = NULL)
check_collinearity(model_zinb)
AIC(model_zinb)


model_zinb <- glmmTMB(nectar_con ~nectar_v_log+ width+ length +(1|plant_no), data = data, family = nbinom2(link = "log"), ziformula = ~ 1)
tab_model(model_zinb, transform = NULL)
check_collinearity(model_zinb)
AIC(model_zinb)
model_zip <- glmmTMB(nectar_con ~nectar_v_log+ width+ length +(1|plant_no), data = data, family = poisson(link = "log"), ziformula = ~ 1)
tab_model(model_zip, transform = NULL)
check_collinearity(model_zip)
AIC(model_zip)

data=read.csv(file="Persica_2023 - nectary.csv", header=T, sep=",")
data=data[,c(3,4,9)]
str(data)
data=na.omit(data)
### Na dole nie zadziała to co z # -> nie ma takich kolumn
#data$plant_no=as.factor(data$plant_no)
#data$flower_no=as.factor(data$flower_no)

data$year=as.factor(data$year)
library(ggbreak)

###### Brak podpisów legendy, za duża legenda########## BARTEK

wyk <- ggplot(data, aes(fill=whorl, y=nectar_v, x=year)) + 
  geom_boxplot() +
  geom_jitter(color="black", size=2, alpha=0.5) +
  scale_y_break(c(16, 23)) +
  theme_classic() 
 
wyk +  labs(x='Rok', y='Objętość nektaru [µl]', title='Objętość nektaru w zewnętrzych i wewnętrznych nektarnikach') +
  theme(text = element_text(size = 20),plot.title = element_text(hjust=0.5, size=20, face='bold')) +
  scale_fill_manual(values=c("deeppink4","grey"),
                      font("xy.text", size = 12, color = "black") +
                      font("xlab", size = 15, color = "black") +
                      font("ylab", size = 15, color = "black") +
                      font("legend.title", size = 10, color = "black") +
                      font("legend.text", size = 10, color = "black"))

#ggsave("Fig.7.tiff", units="in", width=8, height=6, dpi=300, compression = 'lzw')
## TIME COMPARISON IN MORPHOGROUP



###deposition

data=read.csv(file="deposition.csv", header=T, sep=",")
str(data)
hist(data$Średnica)
hist(data$anters)
hist(data$style)
hist(data$pollen)
hist(data$sty_l_ant)
hist(data$sty_s_ant)
hist(data$max)
hist(data$min)
data=data[,(c(1:4,11:17))]


#######Edycja Corrplota (brak polskich podpisów, do poprawy)##### BARTEK - kod ogarnięty, śmiało modyfikuj


datacor=data[,c(4:11)]
x=cor(datacor[,c(1:4)],use="pairwise.complete.obs",method="spearman")

colnames(x) <- c("średnica", "pylniki", "słupki", "pyłek")
rownames(x) <- c("średnica", "pylniki", "słupki", "pyłek")

corrplot::corrplot(x,method="number",
                   type = "upper",tl.col = "black",
                   cl.cex=1,tl.cex = 1,number.cex=1)

x=kruskal.test(data$nectar_v,data$length, alpha = 0.05, p.adj="bonferroni")
print(x)






model_zinb <- glmmTMB(pollen ~Średnica+ anters+ style+sty_l_ant+ sty_s_ant+ max + min +morph +(1|Nr_rośliny)+(1|Nr_kwiatu), data = data, family = nbinom2(link = "log"), ziformula = ~ 1)
tab_model(model_zinb, transform = NULL)
check_collinearity(model_zinb)
AIC(model_zinb)
model_zip <- glmmTMB(pollen ~Średnica+ anters+ style+sty_l_ant+ sty_s_ant+ max + min +morph +(1|Nr_rośliny)+(1|Nr_kwiatu), data = data, family = poisson(link = "log"), ziformula = ~ 1)
tab_model(model_zip, transform = NULL)
check_collinearity(model_zip)
AIC(model_zip)


model_zinb <- glmmTMB(pollen ~Średnica+ anters+ style+morph +(1|Nr_rośliny), data = data, family = nbinom2(link = "log"), ziformula = ~ 1)
tab_model(model_zinb, transform = NULL)
check_collinearity(model_zinb)
AIC(model_zinb)
model_zip <- glmmTMB(pollen ~Średnica+ anters+ style+morph +(1|Nr_rośliny), data = data, family = poisson(link = "log"), ziformula = ~ 1)
tab_model(model_zip, transform = NULL)
check_collinearity(model_zip)
AIC(model_zip)


data= data %>%
  filter(morph == "purple") %>%
  droplevels()

model_zinb <- glmmTMB(pollen ~Średnica+ anters+ style +(1|Nr_rośliny), data = data, family = nbinom2(link = "log"), ziformula = ~ 1)
tab_model(model_zinb, transform = NULL)
check_collinearity(model_zinb)
AIC(model_zinb)


data=read.csv(file="deposition.csv", header=T, sep=",")
data= data %>%
  filter(morph == "green") %>%
  droplevels()
model_zinb <- glmmTMB(pollen ~Średnica+ anters+ style +(1|Nr_rośliny), data = data, family = nbinom2(link = "log"), ziformula = ~ 1)
tab_model(model_zinb, transform = NULL)
check_collinearity(model_zinb)
AIC(model_zinb)

#korelacja słupków i pyłku

cor(data$pollen, data$style,  method = "pearson", use = "complete.obs")
p=ggscatter(data, x = "pollen", y = "style", font.label = c(46, "bold"),
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          cor.coef.size = 8, #zmienia wielkość tego r na wykrsie
          xlab = "Ładunki pyłkowe", ylab = "Długość słupka [mm]")+
theme(text = element_text(size = 18))
p +font("xlab", size = 19, color = "black")+
  font("ylab", size = 19, color = "black")+
  font("xy.text", size = 19, color = "black")

####różnice między morfami w depozycji pyłku

data=read.csv(file="deposition.csv", header=T, sep=",")
x=kruskal.test(data$pollen,data$morph, alpha = 0.05, p.adj="bonferroni")
  print(x)
 
  
###Podpisy osi X nie działają####  BARTEK
   
ggplot(data, aes(fill=morph, y=pollen, x=morph)) + 
  geom_bar(position='dodge', stat='identity') +
  theme_classic() + 
  labs(x='Rok', y='Objętość nektaru [µl]', title='Objętość nektaru w zewnętrzych i wewnętrznych nektarnikach', axis.title = element_text(size = 20)) +
  theme(text = element_text(size = 20),plot.title = element_text(hjust=0.5, size=20, face='bold'), ) +
  scale_fill_manual('morph', values=c("darkseagreen3","plum3"), name = "Kolor:", labels = c(("Jasna"),("Ciemna"))) 
                     ##scale_x_discrete(labels=c(("Jasna"),("Ciemna"))))



sample_size = data %>% group_by(morph) %>% summarize(num=n())

# Plot volume #######NIE DZIAŁA I NIE WIEM CO TO BARTEK
data %>%
  left_join(sample_size) %>%
  mutate(myaxis = paste0(morph, "\n", "n=", num)) %>%
  ggplot( aes(x=myaxis, y=pollen, fill=morph)) +
  geom_violin(width=1.1) +
  geom_boxplot(width=0.1, color="black", alpha=0.2) +
  
  theme_ipsum(base_size = 19,  axis_title_size = 20) +
  scale_fill_manual(values = c("","deeppink4"))+
  theme(legend.position="none",
    plot.title = element_text(size=19)) +
  ggtitle("Pollen deposition") +
  xlab("Color morph")+
  ylab("Pollen grains desposition")

