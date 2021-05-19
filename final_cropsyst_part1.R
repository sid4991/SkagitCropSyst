################################################################################
# @Organization - Washington State University 
# @Project - Skagit Basin Irrigation Demand
# @Documentation- Gabe, Kirti and Siddharth
# @ Description- Reads the results from Cropsys and analyses the season data and provides useful for plots
# Documentation: https://.pdf
################################################################################

#################################################################################
# needed library
library(dplyr)
library(tidyverse)
library(reshape2)
library(ggplot2)
#library(ggthemes)
###################################################################################

###################################################################################
#Reading season files 

season <- rbind(season_historical,season_rcp45,rcp85)
season <- separate(data = season, col = YYYY.MM.DD.DOY., into = c("YYYY", "MM","DD"), sep = "\\-")
season <- separate(data = season, col = DD, into = c("DD", "DOY"), sep = "\\(")
season <- separate(data = season, col = DOY, into = c("DOY"), sep = "\\)")

# Reading HUC data
HUC_CropSys <- read.csv("~/sid/WSU/Skagit/HUC/HUC_CropSys.csv")
season <- merge(season,HUC_CropSys,by="site")

# Creating new variable TimeFrme
season$TimeFrame[season$YYYY < 2021] <- "TimeFrame0" 
season$TimeFrame[season$YYYY > 2020 &season$YYYY < 2050] <- "TimeFrame1" 
season$TimeFrame[season$YYYY > 2080] <- "TimeFrame3" 
season$TimeFrame[season$YYYY > 2049 & season$YYYY < 2081 ] <- "TimeFrame2"


################################################################################
# Input 2: summarizing the season results for different variables grouped by Year Crop HUC and Scenario
season_results <- season %>% group_by(YYYY,crop,TimeFrame,Huc,scenario) %>% 
  summarise(Planting_DOY = round(mean(planting_date)),
            harvest_DOY = round(mean(harvest_date)),
            ETpot_mm = round(mean(ET_max)*1000),
            ETact_mm = round(mean(ET_act)*1000),
            irrigation_mm = round(mean(irrig)*1000),
            rainfall_mm= round(mean(precip)*1000))

 

##Reshape the data frame
season_reshaped <- melt(season_results , id.vars = c('crop','Huc','TimeFrame','scenario'), variable.name = 'Var')          

Crop <- unique(season_reshaped$crop)
Var <- unique(season_reshaped$Var)
Scenario <- unique(season_reshaped$scenario)

for (i in 1:length(Crop)){
  season_reshaped1 <- season_reshaped[which(season_reshaped$crop==Crop[i]),]
  #jpeg(paste("Season_",Crop[i],".jpg",sep = ""), width = 3000, height = 1480))
  jpeg(paste("Season_", Crop[i],".jpeg", sep = ""), width=2000, height=1080) # start export
  p <- ggplot(season_reshaped1, aes(x=Huc  ,y=value, group = 1,fill=Huc))+
    geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size= 3,alpha=0.3,fill="#69b3a2") +  geom_jitter(aes(color= Huc), size=3, alpha=0.9)+ scale_alpha(guide = 'none') +
    theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+
    labs(title = Crop[i], y = "Variable Values",caption = "TimeFrame0:Historical, TimeFrame1:2021-2060, TimeFrame2:2061-2080, TimeFrame3:2081-2099")+  theme(text=element_text(size=25))+
    facet_grid(~season_reshaped1$Var ~season_reshaped1$TimeFrame, scales = "free")+scale_y_continuous(expand = c(0.1,0))+
    theme(panel.spacing = unit(.05, "lines"),
          panel.border = element_rect(color = "black", fill = NA, size = 1), 
          strip.background = element_rect(color = "black", size = 1))
  print(p)
  dev.off()
}


for (i in 1:length(Crop)){
  season_reshaped1 <- season_reshaped[which(season_reshaped$crop==Crop[i]),]
  for (j in 1:length(Var)) {
    season_reshaped2 <- season_reshaped1[which(season_reshaped1$Var==Var[j]),]
    jpeg(paste("Season_", Crop[i],Var[j],".jpeg", sep = ""), width=3000, height=1580) # start export
    p <- ggplot(season_reshaped2, aes(x=Huc  ,y=value, group = 1,fill=Huc))+
      geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size= 3,alpha=0.3,fill="#69b3a2") +  geom_jitter(aes(color= Huc), size=3, alpha=0.9)+ scale_alpha(guide = 'none') +
      theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+
      labs(title = Crop[i], y = "Variable Values",caption = "TimeFrame0:Historical, TimeFrame1:2021-2060, TimeFrame2:2061-2080, TimeFrame3:2081-2099")+  theme(text=element_text(size=25))+
      facet_grid(~season_reshaped2$scenario ~season_reshaped2$TimeFrame, scales = "free")+scale_y_continuous(expand = c(0.1,0))+
      theme(panel.spacing = unit(.05, "lines"),
            panel.border = element_rect(color = "black", fill = NA, size = 1), 
            strip.background = element_rect(color = "black", size = 1))
    print(p)
    dev.off()
  }
}

##### Creating Box plot for Irrigation Across Year and Model
season_irr <- season %>% group_by(YYYY,crop,TimeFrame,scenario) %>% 
  summarise(irrigation_feet = round(mean(irrig)*1000)*0.00328084)
Acc_season_irr <- melt(season_irr , id.vars = c('crop','TimeFrame','scenario'), variable.name = 'Var')

ggplot(Acc_season_irr, aes(x=crop, y=value, fill=TimeFrame)) + 
  geom_boxplot() +
  labs(title = "Skagit Basin Irrigation Across Year and Model", x= "Crop",y = "Irrigation (feet)",caption = "TimeFrame0:Historical, TimeFrame1:2021-2060, TimeFrame2:2061-2080, TimeFrame3:2081-2099")+  theme(text=element_text(size=25))+
  theme(axis.text.x = element_text(face = "bold", color = "black",size = 12, angle = 45,hjust = 1))+
  theme_economist() + 
  scale_color_economist()+ 
  theme(panel.spacing = unit(.05, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = 1), 
        strip.background = element_rect(color = "black", size = 1))

for (i in 1:length(Var)){
  season_reshaped1 <- season_reshaped[which(season_reshaped$Var==Var[i]),]
  for (j in 1:length(Scenario)) {
    Acc_season <- season_reshaped1[which(season_reshaped1$scenario=="historical"),]
    season_reshaped2 <- season_reshaped1[which(season_reshaped1$scenario==Scenario[j]),]
    jpeg(paste("BoxPlot_", Var[i],Scenario[j],".jpeg", sep = ""), width=3000, height=1580) # start export
    p <- ggplot(season_reshaped2, aes(x=crop, y=value, fill=TimeFrame)) + 
      geom_boxplot() +
      labs(title = "Skagit Basin", y = Var[i])+  theme(text=element_text(size=25))+
      theme(axis.text.x = element_text(face = "bold", color = "black",size = 12, angle = 45,hjust = 1))+
      facet_wrap(~Huc, scale="free")+
      theme(panel.spacing = unit(.05, "lines"),
            panel.border = element_rect(color = "black", fill = NA, size = 1), 
            strip.background = element_rect(color = "black", size = 1))
    print(p)
    dev.off()
  }
  
}

## month plot
ggplot(Acc_season_irr, aes(x=MM, y=value, fill=TimeFrame)) + 
  geom_boxplot() +
  labs(title = "Skagit Basin Irrigation", y = "Irrigation (feet)",caption = "TimeFrame0:Historical, TimeFrame1:2021-2060, TimeFrame2:2061-2080, TimeFrame3:2081-2099")+  theme(text=element_text(size=25))+
  theme_economist() + 
  scale_color_economist()+
  theme(axis.text.x = element_text(face = "bold", color = "black",size = 12, angle = 45,hjust = 1))+
  facet_wrap(~crop, scale="free")+
  theme(panel.spacing = unit(.05, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = 1), 
        strip.background = element_rect(color = "black", size = 1))


######
season <- season[,c(1,2,6,14,25,29,35,36,50,52,56,58,60)]
season <- season[,-c(3,4)]

season$ET_act = season$ET_act*1000
season$irrig = season$irrig*1000
season$yield = season$yield*10000
season$rainfall= season$precip*1000
season <- season %>% rename(duration = duration_season)

Acc_season <- melt(season , id.vars = c('YYYY','site','crop','Huc','TimeFrame'), variable.name = 'Var')

Acc_season1 <- Acc_season[which(Acc_season$crop=="spinach"),]
ggplot(Acc_season1, aes(x=Huc  ,y=value, group = 1,fill=Huc))+
  geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size= 3,alpha=0.3,fill="#69b3a2") +  geom_jitter(aes(color= Huc), size=1, alpha=0.9)+ scale_alpha(guide = 'none') +
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+
  labs(title = "Spinach", y = "Variable Values")+  theme(text=element_text(size=25))+
  facet_grid(~Acc_season1$Var ~Acc_season1$TimeFrame, scales = "free")+scale_y_continuous(expand = c(0.1,0))+
  theme(panel.spacing = unit(.05, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = 1), 
        strip.background = element_rect(color = "black", size = 1))

ggplot(data = season_reshaped,aes(x=crop, y=value, fill=Huc,group = 1))+
  geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size= 3,alpha=0.3,fill="#69b3a2") +  geom_jitter(aes(color= Huc), size=1, alpha=0.9)+ scale_alpha(guide = 'none') +
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+
  labs(title = "All crops irrigation", y = "Irrigation (mm)")+  theme(text=element_text(size=25))+
  facet_grid(~season_reshaped$Huc, scales = "free")+scale_y_continuous(expand = c(0.1,0))+
  theme(panel.spacing = unit(.05, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = 1), 
        strip.background = element_rect(color = "black", size = 1))


#######
season_irr <- season %>% group_by(crop,TimeFrame,scenario) %>% 
  summarise(irrigation_feet = round(mean(irrig)*1000)*0.00328084)

Acc_season_irr <- melt(season_irr , id.vars = c('crop','TimeFrame','scenario'), variable.name = 'Var')

ggplot(Acc_season_irr, aes(x=crop, y=value, fill=TimeFrame)) + 
  geom_boxplot() +
  labs(title = "Scenario", x= "Crop",y = "Irrigation (feet)",caption = "TimeFrame0:Historical, TimeFrame1:2021-2060, TimeFrame2:2061-2080, TimeFrame3:2081-2099")+  theme(text=element_text(size=25))+
  theme(axis.text.x = element_text(face = "bold", color = "black",size = 12, angle = 45,hjust = 1))+
  #facet_wrap(~scenario, scale="free")+
  theme_economist() + 
  scale_color_economist()+ 
  theme(panel.spacing = unit(.05, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = 1), 
        strip.background = element_rect(color = "black", size = 1))

###
season_irr <- season %>% group_by(YYYY,crop,TimeFrame,scenario) %>% 
  summarise(irrigation_feet = round(mean(irrig)*1000)*0.00328084)
season_irr <-season_irr[-c(1)]

Acc_season_irr <- melt(season_irr , id.vars = c('YYYY','crop','TimeFrame','scenario'), variable.name = 'Var')

ggplot(Acc_season_irr, aes(x=crop, y=value, fill=TimeFrame)) + 
  geom_boxplot() +
  labs(title = "Across All", x= "Crop",y = "Irrigation (feet)",caption = "TimeFrame0:Historical, TimeFrame1:2021-2060, TimeFrame2:2061-2080, TimeFrame3:2081-2099")+  theme(text=element_text(size=25))+
  theme(axis.text.x = element_text(face = "bold", color = "black",size = 12, angle = 45,hjust = 1))+
  #facet_wrap(~scenario, scale="free")+
  theme_economist() + 
  scale_color_economist()+ 
  theme(panel.spacing = unit(.05, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = 1), 
        strip.background = element_rect(color = "black", size = 1))


season_results <- season_results%>% mutate(across(is.numeric, ~ round(., 2))) 
ggplot(season_results, aes(x=crop, y=irrigation_mm, fill=TimeFrame)) + 
  geom_bar(stat="identity", position="dodge")+
  geom_text(aes(label = irrigation_mm), position = position_dodge(0.9),angle = 90)+
  labs(title = "Irrigation Demand", x= "Crop",y = "Irrigation (inch)",caption = "TimeFrame0:Historical, TimeFrame1:2021-2050, TimeFrame2:2051-2080, TimeFrame3:2081-2099")+  theme(text=element_text(size=25))+
  theme(axis.text.x = element_text(face = "bold", color = "black",size = 12, angle = 45,hjust = 1))+
  scale_y_continuous(breaks=seq(0,15,1))+
  theme_economist() + 
  scale_color_economist()+ 
  theme(panel.spacing = unit(.05, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = 1), 
        strip.background = element_rect(color = "black", size = 1))


blueberry$crop <- "blueberry"
brocoli$crop <- "brocoli"
corn$crop <- "corn"
cucumber$crop <- "cucumber"
hay$crop <- "hay"
potato$crop <- "potato"

##### daily is the dataframe having daily data for historical, rcp45& rcp85
daily <- separate(data = daily, col = YYYY.MM.DD.DOY., into = c("YYYY", "MM","DD"), sep = "\\-")
daily <- separate(data = daily, col = DD, into = c("DD", "DOY"), sep = "\\(")
daily <- separate(data = daily, col = DOY, into = c("DOY"), sep = "\\)")


#### Produce results for monthly 
daily_results <- dataset %>% group_by(YYYY,MM,Site,Crop) %>% 
  summarise(irrigation_in = (sum(irrig)*0.0394)/12)

daily_results <- dataset %>% group_by(YYYY,MM,Site,Crop) %>% 
  summarise(irrigation_in = (sum(irrig)*0.0394))


ggplot(daily_results, aes(x=MM, y=irrigation_in)) + 
  geom_boxplot() +
  labs(title = "Skagit Basin Irrigation",x = "Month", y = "Irrigation (inch)")+
  facet_wrap(~Site,scales="free")+
  labs(y = "Crop Irrigation Requirement (inch)")+
  scale_y_continuous(breaks = pretty_breaks(n = 10))+
  theme(legend.position="top", legend.direction="horizontal") +
  labs(fill="")+
  theme_economist_white(gray_bg = FALSE) +
  scale_colour_economist()+
  theme(panel.spacing = unit(.5, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = 1), 
        strip.background = element_rect(color = "black", size = 1))+
  theme(text=element_text(size=rel(2)))+ theme(axis.text.x = element_text(vjust = 0, hjust = 0.5)) + 
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))+theme(axis.title.x = element_blank())


##### Adding acreage for each crop and calculating Acreage Feet 
daily_results_feet <- dataset %>% group_by(MM,Crop) %>% 
  summarise(irrigation_in = round(mean(irrig)*0.0394))
daily_results_feet$Area[daily_results_feet$Crop == "blueberry"] <- 1139.57 
daily_results_feet$Area[daily_results_feet$Crop == "broccoli"] <- 256.14 
daily_results_feet$Area[daily_results_feet$Crop == "corn-field"] <- 3871.38 
daily_results_feet$Area[daily_results_feet$Crop == "cucumber"] <- 1177 
daily_results_feet$Area[daily_results_feet$Crop == "hay"] <- 2881.48 
daily_results_feet$Area[daily_results_feet$Crop == "potato"] <- 5286.61 
daily_results_feet$AcFt <- daily_results_feet$irrigation_in*daily_results_feet$Area*0.083

ggplot(daily_results_feet, aes(x=MM, y=AcFt, fill=Crop)) + 
  geom_bar(stat="identity", position="dodge")+
  #geom_text(aes(label = AcFt), position = position_dodge(0.9),angle = 90)+
  labs(title = "Acreage Feet", x= "Month",y = "Acreage Feet")+  theme(text=element_text(size=25))+
  theme(axis.text.x = element_text(face = "bold", color = "black",size = 12, angle = 45,hjust = 1))+
  scale_y_continuous(breaks=seq(0,6000,500))+
  theme_economist() + 
  scale_color_economist()+ 
  theme(panel.spacing = unit(.05, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = 1), 
        strip.background = element_rect(color = "black", size = 1))
