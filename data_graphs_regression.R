
# ================================================================================================================================= #
# ============                                                LIBRAIRIES                                               ============ #
# ================================================================================================================================= #

setwd("C:/Users/cleme/Documents/Etudes/MASTER SANTE PUBLIQUE/M1/S8/STG201 - Stage d'insertion professionnel en santé publique/code")

library(lme4)
library(lattice)
library(dplyr)
library(tidyr)
library(ggplot2)
library(mfp)
library(gridExtra)
library(scales)
library(saemix)
library(stringr)
library(ggridges)


# ================================================================================================================================= #
# ============                                                 DONNÉES                                                 ============ #
# ================================================================================================================================= #

    # __________ Données générales (+ météo) __________ #
data_climate_NPI <- read.table(file="DatasetNPIClimate_060421.txt")

b_IDF <- read.delim("./ResultsKalman/b_1.data",sep=" ",header=F) 
b_Centre <- read.delim("./ResultsKalman/b_2.data",sep=" ",header=F) 
b_BFC <- read.delim("./ResultsKalman/b_3.data",sep=" ",header=F) 
b_Normandie <- read.delim("./ResultsKalman/b_4.data",sep=" ",header=F) 
b_HDF <- read.delim("./ResultsKalman/b_5.data",sep=" ",header=F) 
b_GrandEst <- read.delim("./ResultsKalman/b_6.data",sep=" ",header=F) 
b_PaysLoire <- read.delim("./ResultsKalman/b_7.data",sep=" ",header=F) 
b_Bretagne <- read.delim("./ResultsKalman/b_8.data",sep=" ",header=F) 
b_NAquitaine <- read.delim("./ResultsKalman/b_9.data",sep=" ",header=F) 
b_Occitanie <- read.delim("./ResultsKalman/b_10.data",sep=" ",header=F) 
b_AURA <- read.delim("./ResultsKalman/b_11.data",sep=" ",header=F) 
b_PACA <- read.delim("./ResultsKalman/b_12.data",sep=" ",header=F) 

    # __________ Données météo __________ #
weather_reg <- load("weather_data_reg.rdata")
weather_dep <- load("weather_data_dep.rdata")

    # __________ Merging Reff regional data __________ #
col <- c("time", "b","b_min","b_max")
colnames(b_IDF)<- col
colnames(b_Centre)<- col
colnames(b_BFC)<- col
colnames(b_Normandie)<- col
colnames(b_HDF)<- col
colnames(b_GrandEst)<- col
colnames(b_PaysLoire)<- col
colnames(b_Bretagne)<- col
colnames(b_NAquitaine)<- col
colnames(b_Occitanie)<- col
colnames(b_AURA)<- col
colnames(b_PACA)<- col

    # __________ Ajout de l'ID des régions __________ #
regions_order <-  c("IDF","Centre","BFC","Normandie","HDF","GrandEst","PaysLoire","Bretagne","NAquitaine","Occitanie","AURA","PACA")
b_IDF$nameid <- regions_order[1]
b_Centre$nameid <- regions_order[2]
b_BFC$nameid <- regions_order[3]
b_Normandie$nameid <-regions_order[4]
b_HDF$nameid <- regions_order[5]
b_GrandEst$nameid <- regions_order[6]
b_PaysLoire$nameid <- regions_order[7]
b_Bretagne$nameid <- regions_order[8]
b_NAquitaine$nameid <- regions_order[9]
b_Occitanie$nameid <- regions_order[10]
b_AURA$nameid <- regions_order[11]
b_PACA$nameid <- regions_order[12]

    # __________ Fusion des tables __________ #
b <- bind_rows(b_IDF, b_Centre, b_BFC,b_Normandie,b_HDF,b_GrandEst,b_PaysLoire,
               b_Bretagne,b_NAquitaine,b_Occitanie,b_AURA,b_PACA)

    # __________ Kalman start at March 2 and NPI etc at March 3 __________ #
b$time <- b$time+1

    # __________ Merge climate & NPI data with Reff __________ #
data <- data_climate_NPI %>%
  left_join(b, by = c("time"="time", "nameid"="nameid")) %>%
  na.omit(data) %>%
  select(- date_day) %>%
  relocate(date,time,nameid,label_insee,code_insee) %>%
  mutate_at(vars(-c("date","time","nameid","label_insee","code_insee")),as.numeric) %>%
  mutate(date = as.Date(date))

    # __________ Vaccin __________ #
data$bOnNonVaccine <- data$b/(1-data$dose1PercentPop/100) # correspond à B
data$logbOnNonVaccine <- log(data$bOnNonVaccine) # correspond à log(B)

    # __________ Moyenne logbOnNonVaccine __________ #
logbOnNonVaccine_mean <- aggregate(data[,42], list(data$date), mean) #lobgOnNonVaccine moyen (pour toutes les regions) pour chaque jour
colnames(logbOnNonVaccine_mean)<- c("date","logbOnNonVaccine")

    # __________ Moyenne b, bmin et bmax __________ #
b_mean <- aggregate(data[,38:40], list(data$date), mean) # b, bmin et bmax moyens (pour toutes les regions) pour chaque jour
colnames(b_mean)<- c("date", "b", "b_min", "b_max")


# ================================================================================================================================= #
# ============                                             GRAPHIQUES                                                  ============ #
# ================================================================================================================================= #

    # _______________ Figure : taux de transmission (régions) _______________ #
gg1 <- ggplot(data, aes(x=date, y=b, color=label_insee)) + geom_line() + labs(title="Transmission rate", x="Date", y="b") + facet_wrap( ~ label_insee, ncol=6) + scale_x_date(date_labels = "%B %Y") + theme(axis.text.x = element_text(angle=45, hjust = 1))
print(gg1)
ggsave("figure1.png",width=20,height=10)

    # _______________ Figure : taux de transmission _______________ #
gg1b <- ggplot(data, aes(x=date, y=b, color=label_insee)) + geom_line() + labs(title="Transmission rate", x="Date", y="b") + scale_x_date(date_labels = "%B %Y") + theme(axis.text.x = element_text(angle=45, hjust = 1))
print(gg1b)
ggsave("figure1b.png",width=10,height=10)

    # _______________ Figure : taux de transmission lissé  _______________ #
gg2 <- ggplot(data, aes(x=date, y=b, color=label_insee)) + geom_smooth(span=0.1) + labs(title="Transmission rate (smoothed)", x="Date", y="b") + facet_wrap( ~ label_insee, ncol=6) + scale_x_date(date_labels = "%B %Y") + theme(axis.text.x = element_text(angle=45, hjust = 1))
print(gg2)
ggsave("figure2.png",width=20,height=10)

    # _______________ Loess des données météorologiques (en cours) _______________ #
mod = loess(weather_data_reg$IPTCC.mean ~ weather_data_reg$date_day, span = 0.1)

    # _______________ Figure : IPTCC (régions et territoires d'outre-mer) _______________ #
gg3 <- ggplot(weather_data_reg, aes(x=date_day, y=IPTCC.mean, color=label_insee)) + geom_line() + labs(title="Evolution of IPTCC", x="Date", y="IPTCC") + facet_wrap( ~ label_insee, ncol=6) + scale_x_date(date_labels = "%B %Y") + theme(axis.text.x = element_text(angle=45, hjust = 1))
print(gg3)
ggsave("figure3.png",width=20,height=10)

    # _______________ Figure : IPTCC (régions de la France métropolitaine) _______________ #
gg3a <- weather_data_reg %>% filter(!(label_insee %in% c("La Réunion", "Martinique", "Guadeloupe", "Guyane","Mayotte","Corse"))) %>% ggplot() + aes(x = date_day, y = IPTCC.mean, colour = label_insee) + geom_line(size = 0.9) + scale_color_manual(values = c(`Auvergne-Rhône-Alpes` = "#F8766D", `Bourgogne-Franche-Comté` = "#E68339", Bretagne = "#D59006", `Centre-Val de Loire` = "#B89B00", Corse = "#9AA700", `Grand Est` = "#5FAF13", Guadeloupe = "#19B72E", Guyane = "#00BC56", `Hauts-de-France` = "#00BF86", `Île-de-France` = "#00BFAF", `La Réunion` = "#00BBCF", Martinique = "#11B3E7", Mayotte = "#3EA6F5", Normandie = "#6F97FE", `Nouvelle-Aquitaine` = "#A883FC", Occitanie = "#DD71F7", `Pays de la Loire` = "#EE69DD", `Provence-Alpes-Côte d'Azur` = "#FF61C3")) + labs(x = "date", y = "IPTCC", title = "IPTCC", subtitle = "France", color = "Régions") + theme_classic() + facet_wrap(vars(label_insee), scales = "free_x")
print(gg3a)
ggsave("figure3bis.png",width=20,height=10)

    # _______________ Figure : IPTCC lissé _______________ #
gg4 <- ggplot(weather_data_reg, aes(x=date_day, y=IPTCC.mean, color=label_insee)) + geom_smooth(span=0.1) + labs(title="Evolution of IPTCC (smoothed)", x="Date", y="IPTCC") + facet_wrap( ~ label_insee, ncol=6) + scale_x_date(date_labels = "%B %Y") + theme(axis.text.x = element_text(angle=45, hjust = 1))
print(gg4)
ggsave("figure4.png",width=20,height=10)

    # _______________  _______________ #
xyplot(lockdown1 + closedCafe + closedSchool + maskClosedPlaces + lockdown2 + curfew6pm ~ date | label_insee, data = data, type='l', lwd=2, groups = label_insee, main="Major NPI")
  # comment faire pour regrouper les infos sur un même graphique ?

gg5 <- 0
print(gg5)

    # _______________ Figure : Variants (depuis le 28/01/2021) _______________ #
data %>%
  filter(date >= "2021-01-29" & date <= "2021-04-01") %>%
  ggplot() +
  aes(x = date, y = AllVariantPop, colour = label_insee) +
  geom_line(size = 1) +
  scale_color_hue(direction = 1) +
  theme_minimal() +
  labs(title="Variants depuis le 28 janvier 2021", x="Date", y="Proportion") +
  facet_wrap(vars(label_insee)) + scale_x_date(date_labels = "%B %Y") + theme(axis.text.x = element_text(angle=45, hjust = 1))
ggsave("figure7.png",width=20,height=10)

    # _______________ Figure : Variants _______________ #
gg7 <- ggplot(data, aes(x=date, y=AllVariantPop, color=label_insee)) + geom_line() + labs(title="Variants", x="Date", y="Proportion") + facet_wrap( ~ label_insee, ncol=6) + scale_x_date(date_labels = "%B %Y") + theme(axis.text.x = element_text(angle=45, hjust = 1))
print(gg7)
ggsave("figure7bis.png",width=20,height=10)

    # _______________  _______________ #
gg8 <- 0
print(gg8)
#ggsave("figure8.png",width=20,height=10)


    # _______________  _______________ #
xyplot(logbOnNonVaccine ~ date | label_insee, data = data, type='l', lwd=2, groups = label_insee, main="logbOnNonVaccine")
    
    # _______________ Figure : logbOnNonVaccine (toutes les régions regroupées) _______________ #
gg6 <- ggplot(data, aes(x=date, y=logbOnNonVaccine, color=label_insee)) + geom_line() + labs(title="logbOnNonVaccine", x="Date", y="b") + scale_x_date(date_labels = "%B %Y") + theme(axis.text.x = element_text(angle=45, hjust = 1))
print(gg6)
ggsave("figure6.png",width=20,height=10)

    # _______________ Figure : logbOnNonVaccine moyen _______________ #
gg9 <- ggplot(logbOnNonVaccine_mean, aes(x=date, y=logbOnNonVaccine)) + geom_line() + labs(title="logbOnNonVaccine", x="Date", y="b") + scale_x_date(date_labels = "%B %Y") + theme(axis.text.x = element_text(angle=45, hjust = 1))
print(gg9)
ggsave("figure6bis.png",width=20,height=10)

    # _______________ Figure : logbOnNonVaccine (régions de la France métropolitaine) _______________ #
gg10 <- ggplot(data, aes(x=date, y=logbOnNonVaccine, color=label_insee)) + geom_line() + labs(title="logbOnNonVaccine", x="Date", y="b")+ facet_wrap( ~ label_insee, ncol=6)+ scale_x_date(date_labels = "%B %Y") + theme(axis.text.x = element_text(angle=45, hjust = 1))
print(gg10)
ggsave("figure6ter.png",width=20,height=10)


# ================================================================================================================================= #
# ============                                             REGRESSION                                                  ============ #
# ================================================================================================================================= #

    # INDICATIONS :
    # --- weatherSmooth = ITPCC.mean avec loess
    # --- variantsGompertz = AllVariantPop
    # --- maskAfterMay11 = maskClosedPlaces

    # __________ Moyenne de data en France __________ #
data_mean <- aggregate(data[,6:42], list(data$date), mean)


    # __________ Régression avec détail par région __________ #
reg_region <- lm(logbOnNonVaccine ~ 1 + lockdown1 + lockdown2 + curfew6pm + maskClosedPlaces + closedSchool + closedCafe + closedCafe:IPTCC.mean + IPTCC.mean + AllVariantPop + (1 + lockdown1 + lockdown2 + curfew6pm | code_insee), data = data)
summary(reg_region)
    
    # __________ Régression sans détail par région __________ #
reg_france <- lm(logbOnNonVaccine ~ 1 + lockdown1 + lockdown2 + curfew6pm + maskClosedPlaces + closedSchool + closedCafe + closedCafe:IPTCC.mean + IPTCC.mean + AllVariantPop + (1 + lockdown1 + lockdown2 + curfew6pm), data = data_mean)
summary(reg_france)

    # __________ Ajout des valeurs prédites dans le tableau des données __________ #
logbOnNonVaccine_mean$fitted_logbOnNonVaccine <- reg_france$fitted.values

    # __________ Figure : bOnNonVaccine VS fitted bOnNonVaccine __________ #
gg11 <- ggplot(logbOnNonVaccine_mean, aes(x = date)) + geom_line(aes(y = exp(logbOnNonVaccine), color = "blue"), size=1) + geom_smooth(aes(y = exp(fitted_logbOnNonVaccine), color = "red"), span = 0.1, size=0.5) + labs(title = "bOnNonVaccine vs fitted_bOnNonVaccine ", x = "Date", y = "Valeur") + scale_x_date(date_labels = "%B %Y") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(gg11)
ggsave("figure8.png",width=20,height=10)

