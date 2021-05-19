################################################################################
# @Organization - Washington State University 
# @Project - Skagit Basin Irrigation Demand
# @Documentation- Gabe, Kirti and Siddharth
# @ Description- Reads the results from Cropsys and analyses the season data and provides useful for plots
# Documentation: https://.pdf
################################################################################

require(lubridate)
library(dplyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(ggplot2)
library(readxl)
library(tidyr)
library(reshape2)
library(ggthemes)

## daily data from Cropsyst collate
load("daily.RData")
ls()

## acres into by crop-HUC (file attached in repository)
locationacres<-read.csv("Location_Huc_Crop_Area.csv")
locationacres[is.na(locationacres)] <- 0
head(locationacres)

## add pasture acres to the other category as we did not simulate it
locationacres$Other=locationacres$Pasture+locationacres$Other
colnames(locationacres)<-c("pointid","Site","Huc","WRIA","potato","pasture","corn-field","hay","blueberry",
                           "broccoli","cucumber","other")
nrow(locationacres)

## remove last two rows that were not simulated for now.
locationacres<-locationacres[c(1:29),]

## dataset is the dataframe in daily.RData
head(dataset)
unique(dataset$Crop)
listofgridtouse<-unique(dataset$Site)[c(12:29)]

##get just the relevant columns
cropirrigdemanddepth<-dataset[dataset$Site %in% listofgridtouse ,c(1,2,3,17,19,20)]
head(cropirrigdemanddepth)

## summarize irrigation demand by year, month, site, crop
yearlycropirrigdemad<- cropirrigdemanddepth %>%
  group_by(YYYY, Site,Crop) %>%
  summarize(  yearlyirr = sum(irrig*.0394))
monthlycropirrigdemad<- cropirrigdemanddepth %>%
  group_by(YYYY,MM,Site,Crop) %>%
  summarize(  monthlyirr = sum(irrig*.0394))
monthlycropirrigdemandMedian<- monthlycropirrigdemad %>%
  group_by(MM,Crop) %>%
  summarize(  monthlyirr = median(monthlyirr))

Year_month_cropirrigdemandMedianAcrossgrids<- monthlycropirrigdemad %>%
  group_by(YYYY,MM,Crop) %>%
  summarize(  monthlyirr = median(monthlyirr))
head(Year_month_cropirrigdemandMedianAcrossgrids)
head(monthlycropirrigdemandMedian)

monthlycropirrigdemandMedianWide<-dcast(monthlycropirrigdemandMedian, MM ~ Crop, value.var = "monthlyirr")

year_monthcropirrigdemandMedianWide<-dcast(Year_month_cropirrigdemandMedianAcrossgrids, YYYY+ MM ~ Crop, value.var = "monthlyirr")
View(monthlycropirrigdemandMedianWide)
View(year_monthcropirrigdemandMedianWide)
year_monthcropirrigdemandMedianWide$other<-rowMeans(year_monthcropirrigdemandMedianWide[3:8], na.rm=TRUE)

## acreage data from Sids's summary
potatoacres<-5286.61
cornacres<-3871.38
hayacres<-2881.48
blueberryacres<-1139.57
broccoliacres<-256.14
cucumberacres<-408.99
otheracres<-2456

## divide demand by 12 to convert to ft and multiple by acres.
year_monthcropirrigdemandMedianWide$otheracreft<-year_monthcropirrigdemandMedianWide$other/12*otheracres
year_monthcropirrigdemandMedianWide$cucumberacreft<-year_monthcropirrigdemandMedianWide$cucumber/12*cucumberacres
year_monthcropirrigdemandMedianWide$broccoliacreft<-year_monthcropirrigdemandMedianWide$broccoli/12*broccoliacres
year_monthcropirrigdemandMedianWide$blueberryacreft<-year_monthcropirrigdemandMedianWide$blueberry/12*blueberryacres
year_monthcropirrigdemandMedianWide$hayacreft<-year_monthcropirrigdemandMedianWide$hay/12*hayacres
year_monthcropirrigdemandMedianWide$cornacreft<-year_monthcropirrigdemandMedianWide$`corn-field`/12*cornacres
year_monthcropirrigdemandMedianWide$potatoacreft<-year_monthcropirrigdemandMedianWide$potato/12*potatoacres

year_monthcropirrigdemandMedianWide$TotalAcreFt<-rowSums(year_monthcropirrigdemandMedianWide[10:16], na.rm=TRUE)


head(year_monthcropirrigdemandMedianWide)

dataforpercentiles<-dcast(year_monthcropirrigdemandMedianWide[,c(1,2,17)],
                          YYYY ~ MM, value.var = "TotalAcreFt")
summary(dataforpercentiles)
suma<-data.frame(unclass(summary(dataforpercentiles)), check.names = FALSE, stringsAsFactors = FALSE)
write.csv(suma,"summary.csv")

head(monthlycropirrigdemad)
View(yearlycropirrigdemad)
plotannualirrig<- ggplot(yearlycropirrigdemad, aes(x= Site)) +
  geom_boxplot(aes(y=yearlyirr, fill = Site)) +
  facet_wrap(~Crop)
library(ggthemes)

#### Sid, I think this is the monthly plot. Please check.
p<- ggplot(year_monthcropirrigdemandMedianWide, aes(x= MM)) +
  geom_boxplot(aes(y=TotalAcreFt))  +
  labs(y = "Historical monthly irrigation demands (acre-ft / month)")+
  #scale_y_continuous(breaks = pretty_breaks(n = 10))+
  theme(legend.position="top", legend.direction="horizontal") +
  labs(fill="")+
  theme_economist_white(gray_bg = FALSE) +
  scale_colour_economist()+
  theme(panel.spacing = unit(.5, "lines"), panel.border = element_rect(color = "black", fill = NA, size = 1),
        strip.background = element_rect(color = "black", size = 1))+
  theme(text=element_text(size=12))+ theme(axis.text.x = element_text(vjust = 0, hjust = 0.5)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))+theme(axis.title.x = element_blank())

p
p+  scale_x_discrete(labels=c("01" = "Jan", "02" = "Feb", "03" = "Mar", "04" = "Apr",  "05" = "May", "06" = "Jun",
                              "07" = "Jul", "08" = "Aug", "09" ="Sep","10" = "Oct","11" = "Nov","12" ="Dec" ))


cropirrigdemanddepth<-dataset[,c(1,2,3,17,19,20)]
rm(dataset)
head(cropirrigdemanddepth)
nrow(cropirrigdemanddepth)
CropIrrdemandHUC12 <- merge(cropirrigdemanddepth, locationacres[,c(2,3,4)], by="Site", all=TRUE)
head(CropIrrdemandHUC12)
nrow(CropIrrdemandHUC12)
unique(CropIrrdemandHUC12$Crop)

#### read season files and check

datacrop4.5<-read.table("season_RCP4.5right.txt", header=TRUE)
datacrop4.5$HistFut<-"RCP 4.5"
datacrop8.5<-read.table("season_RCP8.5.txt", header=TRUE)
datacrop8.5$HistFut<-"RCP 8.5"
datacrophist<-read.table("season_hist.txt", header=TRUE)
datacrophist$scenario <- "historical"
datacrophist$HistFut<-"historical"
head(datacrophist)

medianabycrop<- datacrophist %>%
  group_by(crop) %>%
  summarize(  yearlyirr = median(irrig)*39.4)


nrow(datacrophist)
head(datacrophist)
masterdata<-rbind(datacrophist, datacrop4.5, datacrop8.5)
head(masterdata)
nrow(masterdata)
unique(masterdata$crop)

datatomerge<-masterdata[,c(1,6,9:12)]
head(datatomerge)
nrow(datatomergeHUC)

HUCandcroparea<-read.csv("HUCandcroparea.csv")
head(HUCandcroparea)

datatomergeHUC <- merge(datatomerge, HUCandcroparea[,c(2,3)], by="site", all.x=TRUE)
datatomergeHUC2<-datatomergeHUC %>% separate(YYYY.MM.DD.DOY., c("year", NA, NA), sep="-")
head(datatomergeHUC2)
medianaovergrid_inches<- datatomergeHUC2 %>%
  group_by(year,crop,scenario,HistFut,Huc) %>%
  summarize(  irrmed = median(irrig)*39.4)
head(medianaovergrid_inches)

medianaovergrid_inches_wide<-dcast(medianaovergrid_inches, year + Huc+ scenario + HistFut ~crop, value.var = "irrmed")

head(medianaovergrid_inches_wide)

##get median value to use for "other crops"
medianaovergrid_inches_wide$other<-rowMeans(medianaovergrid_inches_wide[5:10], na.rm=TRUE)
nrow(medianaovergrid_inches_wide)
head(HUCandcroparea)
### get HUC level totals
HUCandcroparea_summary<-HUCandcroparea[,c(3,5:11)] %>%
  group_by(Huc) %>%
  summarise_each(funs(sum))
head(HUCandcroparea_summary)
colnames(HUCandcroparea_summary)<-c("Huc","potato_acres","cornacres","hayacres","blueberryacres","broccoliacres","cucumberacres","otheracres")

medianaovergrid_inches_wide_irracres <- merge(medianaovergrid_inches_wide,HUCandcroparea_summary, by="Huc", all.x=TRUE)
nrow(medianaovergrid_inches_wide_irracres)
head(medianaovergrid_inches_wide_irracres)
medianaovergrid_inches_wide_irracres$blueberryAcreFt<- medianaovergrid_inches_wide_irracres$blueberry/12*medianaovergrid_inches_wide_irracres$blueberryacres
medianaovergrid_inches_wide_irracres$potatoAcreFt<- medianaovergrid_inches_wide_irracres$potato/12*medianaovergrid_inches_wide_irracres$potato_acres
medianaovergrid_inches_wide_irracres$cornAcreFt<- medianaovergrid_inches_wide_irracres$`corn-field`/12*medianaovergrid_inches_wide_irracres$cornacres
medianaovergrid_inches_wide_irracres$hayAcreFt<- medianaovergrid_inches_wide_irracres$hay/12*medianaovergrid_inches_wide_irracres$hayacres
medianaovergrid_inches_wide_irracres$broccoliAcreFt<- medianaovergrid_inches_wide_irracres$broccoli/12*medianaovergrid_inches_wide_irracres$broccoliacres
medianaovergrid_inches_wide_irracres$cucumberAcreFt<- medianaovergrid_inches_wide_irracres$cucumber/12*medianaovergrid_inches_wide_irracres$cucumberacres
medianaovergrid_inches_wide_irracres$otherAcreFt<- medianaovergrid_inches_wide_irracres$other/12*medianaovergrid_inches_wide_irracres$otheracres
View(medianaovergrid_inches_wide_irracres)
head(datatomerge)
## convert NAs to 0 before adding
medianaovergrid_inches_wide_irracres[is.na(medianaovergrid_inches_wide_irracres)] <- 0
medianaovergrid_inches_wide_irracres$TotalAcreFt<- medianaovergrid_inches_wide_irracres$blueberryAcreFt +
  medianaovergrid_inches_wide_irracres$potatoAcreFt + medianaovergrid_inches_wide_irracres$cornAcreFt +
  medianaovergrid_inches_wide_irracres$hayAcreFt + medianaovergrid_inches_wide_irracres$broccoliAcreFt +
  medianaovergrid_inches_wide_irracres$cucumberAcreFt +medianaovergrid_inches_wide_irracres$otherAcreFt
unique(medianaovergrid_inches_wide_irracres$HistFut)

head(medianaovergrid_inches_wide_irracres)
## get total across HUCs.
medianaovergrid_inches_wide_irracresTotal<- medianaovergrid_inches_wide_irracres %>%
  group_by(year,scenario,HistFut) %>%
  summarize(  TotalAcreFt = sum(TotalAcreFt) )
head(medianaovergrid_inches_wide_irracresTotal)
medianaovergrid_inches_wide_irracresTotal$Huc = "Total"
### adjust total to include 40 acres outside of the HUC-12 considered.
### average irrigation demand is used is used for other. It is hard coded  as 7.4 
medianaovergrid_inches_wide_irracresTotal$TotalAcreFt = medianaovergrid_inches_wide_irracresTotal$TotalAcreFt + 7.4/12*40
medianaovergrid_inches_wide_irracresTotal
masterplotdata<-rbind(as.data.frame(medianaovergrid_inches_wide_irracres[,c(2,3,4,26,1)]), as.data.frame(medianaovergrid_inches_wide_irracresTotal))
head(masterplotdata)            
head(medianaovergrid_inches)
library(scales)
plotforgabe<-ggplot(masterplotdata, aes(x=Huc, y=TotalAcreFt, fill=HistFut)) +
  geom_boxplot() +
  scale_fill_manual(breaks = c("historical", "RCP 4.5","RCP 8.5"), values=c("#edad08","#73af48","#94346e"))+
  labs(y = "Irrigation Requirement (Acre-ft/year)")+
  scale_y_continuous(breaks = pretty_breaks(n = 10))+
  theme(legend.position="top", legend.direction="horizontal") +
  labs(fill="")+
  theme_economist_white(gray_bg = FALSE) +
  scale_colour_economist()+
  theme(panel.spacing = unit(.5, "lines"), panel.border = element_rect(color = "black", fill = NA, size = 1),
        strip.background = element_rect(color = "black", size = 1))+
  theme(text=element_text(size=12))+ theme(axis.text.x = element_text(vjust = 0, hjust = 0, angle=90)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0)))+theme(axis.title.x = element_blank())
plotforgabe
unique(masterplotdata$Huc)

sumdata<-masterplotdata %>%
  group_by(Huc, HistFut) %>%
  summarize(median = median(TotalAcreFt), pct25 = quantile(TotalAcreFt, 0.25), pct75 = quantile(TotalAcreFt, 0.75))
write.csv(sumdata, "AllHUcsandTotalsummariesbyScenario.csv")
summary(masterplotdata[masterplotdata$Huc == "Total" & masterplotdata$HistFut == "Total",])
suma<-data.frame(unclass(summary(masterplotdata[masterplotdata$Huc == "Total",])), check.names = FALSE, stringsAsFactors = FALSE)
write.csv(suma,"summaryforTotalAcreftInBasin.csv")


summary(masterplotdata[masterplotdata$Huc == "Skagit Delta-Frontal Skagit Bay",])
suma<-data.frame(unclass(summary(masterplotdata[masterplotdata$Huc == "Skagit Delta-Frontal Skagit Bay",])), check.names = FALSE, stringsAsFactors = FALSE)
write.csv(suma,"summaryforTotalAcreftInSkagitDelta.csv")



## create median over grids
medianaovergrid<- datatomerge %>%
  group_by(YYYY.MM.DD.DOY.,crop,scenario,HistFut) %>%
  summarize(  medianirrovergrids_in = median(irrig)*39.4)
head(medianaovergrid)

datatomerge<-dcast(datatomerge, YYYY.MM.DD.DOY.+ site + scenario + HistFut ~crop, value.var = "irrig")
head(datatomerge)
unique(datatomerge$scenario)



plotirr<- ggplot(masterdata, aes(x= crop)) +
  geom_boxplot(aes(y=irrig*39.4, fill = HistFut))
plotirr2<- ggplot(datacrophist, aes(x= site)) +
  geom_boxplot(aes(y=irrig*39.4, fill = HistFut)) +
  facet_wrap(~crop)