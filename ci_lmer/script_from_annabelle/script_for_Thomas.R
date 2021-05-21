#Linear mixed effects model, to test which NPI help reducing the transmission b
rm(list=ls())

# Charge the libraries
library(lme4)
library(nlme)
library(lattice)
library(dplyr)
library(tidyr)
library(ggplot2)
library(mfp)
library(gridExtra)
library(scales)
library(saemix)
library(stringr)


# Loading data
# Read the data
data_Weather_NPI <- readRDS("ci_lmer/script_from_annabelle/data/2021-05-03_Full_Covar_data.RDS")
data_Weather_NPI$time <- as.numeric(data_Weather_NPI$date) - min(as.numeric(data_Weather_NPI$date))

# Open effective ration 
b_IDF <- read.delim("ci_lmer/script_from_annabelle/data/ResultsKalman/b_1.data",sep=" ",header=F) 
b_Centre <- read.delim("ci_lmer/script_from_annabelle/data/ResultsKalman/b_2.data",sep=" ",header=F) 
b_BFC <- read.delim("ci_lmer/script_from_annabelle/data/ResultsKalman/b_3.data",sep=" ",header=F) 
b_Normandie <- read.delim("ci_lmer/script_from_annabelle/data/ResultsKalman/b_4.data",sep=" ",header=F) 
b_HDF <- read.delim("ci_lmer/script_from_annabelle/data/ResultsKalman/b_5.data",sep=" ",header=F) 
b_GrandEst <- read.delim("ci_lmer/script_from_annabelle/data/ResultsKalman/b_6.data",sep=" ",header=F) 
b_PaysLoire <- read.delim("ci_lmer/script_from_annabelle/data/ResultsKalman/b_7.data",sep=" ",header=F) 
b_Bretagne <- read.delim("ci_lmer/script_from_annabelle/data/ResultsKalman/b_8.data",sep=" ",header=F) 
b_NAquitaine <- read.delim("ci_lmer/script_from_annabelle/data/ResultsKalman/b_9.data",sep=" ",header=F) 
b_Occitanie <- read.delim("ci_lmer/script_from_annabelle/data/ResultsKalman/b_10.data",sep=" ",header=F) 
b_AURA <- read.delim("ci_lmer/script_from_annabelle/data/ResultsKalman/b_11.data",sep=" ",header=F) 
b_PACA <- read.delim("ci_lmer/script_from_annabelle/data/ResultsKalman/b_12.data",sep=" ",header=F) 



# Set column names
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

# Add region id
regions_order <-  c("IDF","Centre","BFC","Normandie","HDF","GrandEst","PaysLoire","Bretagne","NAquitaine","Occitanie","AURA","PACA")
b_IDF$region <- regions_order[1]
b_Centre$region <- regions_order[2]
b_BFC$region <- regions_order[3]
b_Normandie$region <-regions_order[4]
b_HDF$region <- regions_order[5]
b_GrandEst$region <- regions_order[6]
b_PaysLoire$region <- regions_order[7]
b_Bretagne$region <- regions_order[8]
b_NAquitaine$region <- regions_order[9]
b_Occitanie$region <- regions_order[10]
b_AURA$region <- regions_order[11]
b_PACA$region <- regions_order[12]


# Merge tables
b <- bind_rows(b_IDF, b_Centre, b_BFC,b_Normandie,b_HDF,b_GrandEst,b_PaysLoire,
               b_Bretagne,b_NAquitaine,b_Occitanie,b_AURA,b_PACA)


# Merge Weather & NPI data with Reff
data <- data_Weather_NPI %>%
  left_join(b, by = c("time"="time", "region"="region")) %>%
  na.omit(data) %>%
  relocate(date,time,region,label_insee,code_insee) %>%
  mutate_at(vars(-c("date","time","region","label_insee","code_insee")),as.numeric) %>%
  mutate(date = as.Date(date))


# Modify school 
data$closedSchool <- as.numeric(data$C1_School_closing == 5)
#data$midClosedSchoolMayJune2020 <- 0
data$closedSchool[(data$date >= "2020-05-11") &  (data$date <= "2020-07-04")] <- 0.7
plot=xyplot(closedSchool~date|region,data=data,type='l',col=c("black"),lwd=5,groups=region)
print(plot)
#plot=xyplot(midClosedSchoolMayJune2020~date|region,data=data,type='l',col=c("black"),lwd=5,groups=region)
#print(plot)


# Lockdowns
# (lock1, endlock1phase1, endlock1phase2)
data$lock1 <- as.numeric(data$Lockdown1)-1
plot=xyplot(lock1~date|region,data=data,type='l',col=c("black"),lwd=5,groups=region)
print(plot)
data$endlock1phase1 <- 0
data$endlock1phase1[data$date > "2020-05-10" & data$date < "2020-06-03"] <- 1
data$endlock1phase2 <- 0
data$endlock1phase2[data$date > "2020-06-02" & data$date < "2020-06-22"] <- 1
data$lockdown1delay7 <- data$lock1
data$lockdown1delay7[data$date < "2020-03-24"] <- 0
plot=xyplot(lockdown1delay7~date|region,data=data,type='l',col=c("black"),lwd=5,groups=region)
print(plot)
plot=xyplot(endlock1phase1~date|region,data=data,type='l',col=c("black"),lwd=5,groups=region)
print(plot)
plot=xyplot(endlock1phase2~date|region,data=data,type='l',col=c("black"),lwd=5,groups=region)
print(plot)

data$lock2full <- as.numeric(data$Lockdown2==3)
data$lock2reduced <- as.numeric(data$Lockdown2 == 2)
data$lockdown2delay7 <- data$lock2full
data$lockdown2delay7[data$date < "2020-11-06"] <- 0
plot=xyplot(lockdown2delay7~date|region,data=data,type='l',col=c("black"),lwd=5,groups=region)
print(plot)
plot=xyplot(lock2reduced~date|region,data=data,type='l',col=c("black"),lwd=5,groups=region)
print(plot)

data$lock3 <- as.numeric(data$Lockdown3 == 3)
data$lock3del7 <- 0
data$lock3del7[data$date > "2021-04-09"] <- 1
plot=xyplot(lock3del7~date|region,data=data,type='l',col=c("black"),lwd=5,groups=region)
print(plot)


# Barrier gestures
data$barriergestures <- as.numeric(data$Mandatory_mask_wearing_policies > 1.5)
plot=xyplot(barriergestures~date|region,data=data,type='l',col=c("black"),lwd=5,groups=region)
print(plot)


# Bars & Restaurants
data$closedbarresto <- as.numeric((data$Closed_bars_restaurants == 3) )
plot=xyplot(closedbarresto~date|region,data=data,type='l',col=c("black"),lwd=5,groups=region)
print(plot)

# Curfew - after January 10
#data$curf <- as.numeric(data$Curfew-min(data$Curfew))/(max(data$Curfew)-min(data$Curfew))
data$curfew6pm <- as.numeric(data$Curfew == 5)
plot=xyplot(Curfew~date|region,data=data,type='l',col=c("black"),lwd=5,groups=region)
print(plot)
data$curfew8pm <- as.numeric(data$Curfew == 3)
plot=xyplot(curfew8pm~date|region,data=data,type='l',col=c("black"),lwd=5,groups=region)
print(plot)

# Weather
# Normalize and Inverse
data$Weather <- 1-((data$IPTCC.mean-min(data$IPTCC.mean))/(max(data$IPTCC.mean)-min(data$IPTCC.mean)))
# Smooth
data$smoothWeather <- 0
for (i in unique(data$region)){
  data$smoothWeather[data$region == i] <- predict(loess(data$Weather[data$region == i]~
                                                          data$time[data$region == i], span = 0.2))
}
# data$smoothWeather <-(data$smoothWeather-min(data$smoothWeather))/(max(data$smoothWeather)-min(data$smoothWeather))
# Variants
data$variants <- as.numeric(data$perc_var_lin/100)
data$variantsThreshold <- as.numeric(data$perc_var_lin/100 > 0.8)
data$variantsOnlySup50 <- as.numeric(data$perc_var_lin/100 > 0.5)*as.numeric(data$perc_var_lin/100)
plot1=xyplot(variantsOnlySup50~date|region,data=data,type='l',col=c("black"),lwd=5,main="Variants then > 50%")
print(plot1)

# Vaccine
data$bOnUnVaccine <- data$b/(1-data$couv_dose1/100)
# Log(Vaccine)
data$logbOnUnVaccine <- log(data$bOnUnVaccine)

data <- data[data$date < "2021-03-28",]


# All individuals (regions) 
pdf("ci_lmer/script_from_annabelle/saved_plot/bOnUnVaccine.pdf",width=10,height=7)
plot=xyplot(bOnUnVaccine~date|region,data=data,type='l',col=c("black"),lwd=5,groups=region,main="t -> b(t)/(1-V(t)/N)",xlab=NULL,ylab=NULL,scales = list(x = list(format = "%b-%y")))
print(plot)
dev.off()
print(plot)


# All individuals (regions) together
pdf("ci_lmer/script_from_annabelle/saved_plot/smoothWeather.pdf",width=10,height=7)
plot1=xyplot(Weather+smoothWeather~date|region,data=data,type='l',col=c("black","red"),lwd=5,main="Weather variables",xlab=NULL,ylab=NULL,scales = list(x = list(format = "%b-%y")))
print(plot1)
dev.off()
print(plot1)

# All individuals (regions) together
pdf("ci_lmer/script_from_annabelle/saved_plot/varlin.pdf",width=10,height=7)
plot1=xyplot(perc_var_lin~date|region,data=data,type='l',col=c("black"),lwd=5,main="Non-historical variants",xlab=NULL,ylab=NULL,scales = list(x = list(format = "%b-%y")))
print(plot1)
dev.off()
print(plot1)




# All individuals (regions) together
pdf("ci_lmer/script_from_annabelle/saved_plot/dose1PercentPop.pdf",width=10,height=7)
plot1=xyplot(couv_dose1~date|region,data=data,type='l',col=c("black"),lwd=5,main="Percentage of people who had received first dose of vaccine",xlab=NULL,ylab=NULL,scales = list(x = list(format = "%b-%y")))
print(plot1)
print(plot1)
dev.off()
print(plot1)


pdf("ci_lmer/script_from_annabelle/saved_plot/NPI-not-partial.pdf",width=10,height=7)
plot=xyplot((lock1+9*1.2)+
              (endlock1phase1+8*1.2)+
              (endlock1phase2+7*1.2)+
              (lock2full+6*1.2)+
              (lock2reduced+5*1.2)+
              (closedbarresto+4*1.2)+
              (closedSchool+3*1.2)+
              (barriergestures+2*1.2)+
              (curfew8pm+1*1.2)+
              (curfew6pm+0*1.2)~date|region,lwd=2,data=data,type='l', xlab=NULL,ylab=NULL,scales = list(x = list(format = "%b-%y"), y = list(at = NULL)),
            col=c("red","blue","darkorchid4","orange","gold","darkcyan","cyan","green","coral1","darkslateblue"),
            key=list(space="right",
                     lines=list(col=c("red","blue","darkorchid4","orange","gold","darkcyan","cyan","green","coral1","darkslateblue"), lwd=4),
                     text=list(c("Lockdown1","Reopening1Lockdown1","Reopening2Lockdown1","Lockdown2","ReducedLockdown2","Closed Bars - Restaurants","Closed Schools","Barrier Geastures",
                                 "Curfew 8pm","Curfew 6pm"))))
print(plot)
dev.off()
print(plot)



full1 <- lmer(logbOnUnVaccine~1+lockdown1delay7+endlock1phase1+endlock1phase2+
                lockdown2delay7+lock2reduced+#lock3+
                closedSchool+
                closedbarresto+
                barriergestures+
                curfew6pm+curfew8pm+
                variants+
                smoothWeather+
                closedbarresto:smoothWeather+
                (1+lockdown1delay7+lockdown2delay7+curfew6pm
                 |region), data=data, na.action=na.exclude)

saveRDS(object = full1, file = "ci_lmer/lmer_b_npi.rds")

round(100*(1-exp(fixef(full1))),1)

data$fit <- predict(full1)

pdf("ci_lmer/script_from_annabelle/saved_plot/bpredict_Kalman_NPI_ind.pdf",width=10,height=7)
plot1=xyplot(logbOnUnVaccine+fit~date|region,data=data,type='l',lwd=5,main=NULL,xlab=NULL,ylab=NULL, col=c("black","red"), scales = list(x = list(format = "%b-%y")))
print(plot1)
dev.off()
print(plot1)



####Fits of the full model
plot(fitted(full1),residuals(full1))
data$prediction<-predict(full1)


effect1 = 100*(1-exp(fixef(full1)))
effectminus1 = 100*(1-exp(fixef(full1)+1.96*sqrt(diag(vcov(full1)))))
effectplus1 = 100*(1-exp(fixef(full1)-1.96*sqrt(diag(vcov(full1)))))

var <- c("lockdown1delay7","endlock1phase1","endlock1phase2","lockdown2delay7","lockdown2delay7","closedSchool","closedbarresto","barriergestures","curfew6pm","curfew8pm","variants","Weather","interaction Weather/bar")
for (i in 2:length(effect1))
{
  if ((i>1))
  {
    print(paste0(var[i-1]," & ",
                 round(effect1[i],0), "% [", round(effectminus1[i],0), "% - ", round(effectplus1[i],0), "%]  \\ " ))
  }
}
print(paste0("AIC &",  round(AIC(full1),0),  " \\"))
