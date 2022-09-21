library(tidyverse)
library(lubridate)
library(readxl)
library(janitor)
library(geosphere)
library(rnaturalearth)
library(ggspatial)
library(elementalist)
library(ggbump)


#Collecting each year of Data
SARH_2015_16 <- clean_names(read_excel("SARH Data 2015-2021.xlsx", sheet = "2015_16",col_types = c("date", "text", "text","text","text", "text","text", "numeric","numeric","text","numeric")))
SARH_2016_17 <- clean_names(read_excel("SARH Data 2015-2021.xlsx", sheet = "2016_17",col_types = c("date", "text", "text","text","text", "text","text", "numeric","numeric","text","numeric")))
SARH_2017_18 <- clean_names(read_excel("SARH Data 2015-2021.xlsx", sheet = "2017_18",col_types = c("date", "text", "text","text","text", "text","text", "numeric","numeric","text","numeric")))
SARH_2018_19 <- clean_names(read_excel("SARH Data 2015-2021.xlsx", sheet = "2018_19",col_types = c("date", "text", "text","text","text", "text","text", "numeric","numeric","text","numeric")))
SARH_2019_20 <- clean_names(read_excel("SARH Data 2015-2021.xlsx", sheet = "2019_20",col_types = c("date", "text", "text","text","text", "text","text", "numeric","numeric","text","numeric")))
SARH_2020_21 <- clean_names(read_excel("SARH Data 2015-2021.xlsx", sheet = "2020_21",col_types = c("date", "text", "text","text","text", "text","text", "numeric","numeric","text","numeric")))
#Combining All Togerher
SARH_2015_2021 <- rbind(SARH_2015_16,SARH_2016_17,SARH_2017_18,SARH_2018_19,SARH_2019_20,SARH_2020_21)
#Adding Base Locations
Caernarfon <- mutate(filter(SARH_2015_2021, base =="Caernarfon"), HeliType = "S92", BLat = 53.100824, BLong = -4.335326,callsign="936")
Humberside <- mutate(filter(SARH_2015_2021, base =="Humberside"), HeliType = "S92", BLat = 53.576608, BLong = -0.338628,callsign="912")
Inverness <- mutate(filter(SARH_2015_2021, base =="Inverness"), HeliType = "AW189", BLat = 57.538000, BLong = -4.047985,callsign="151")
LeeOnSolent <- mutate(filter(SARH_2015_2021, base =="Lee On Solent"), HeliType = "AW189", BLat = 50.811413, BLong = -1.208161,callsign="175")
Lydd <- mutate(filter(SARH_2015_2021, base =="Lydd"), HeliType = "AW189", BLat = 50.958359, BLong = 0.933960,callsign="163")
Newquay <- mutate(filter(SARH_2015_2021, base =="Newquay"), HeliType = "S92", BLat = 50.443431, BLong = -4.993911,callsign="924")
Prestwick <- mutate(filter(SARH_2015_2021, base =="Prestwick"), HeliType = "AW189", BLat = 55.511174, BLong = -4.581976,callsign="199")
StAthan <- mutate(filter(SARH_2015_2021, base =="St Athan"), HeliType = "AW189", BLat = 51.400839, BLong = -3.439028,callsign="187")
Stornoway <- mutate(filter(SARH_2015_2021, base =="Stornoway"), HeliType = "S92", BLat = 58.219136, BLong = -6.326690,callsign="948")
Sumburgh <- mutate(filter(SARH_2015_2021, base =="Sumburgh"), HeliType = "S92", BLat = 59.876695, BLong = -1.296892,callsign="900")
#Recombining
SARH_2015_2021 <- rbind(Caernarfon,Humberside,Inverness,LeeOnSolent,Lydd,Newquay,Prestwick,StAthan,Stornoway,Sumburgh)
#Adding Mission Distance
SARH_2015_2021 <- SARH_2015_2021 %>% rowwise()%>% mutate(distance=distm(c(BLong,BLat),c(longitude,latitude))/1000)
#Putting in date order
SARH_2015_2021 <- SARH_2015_2021%>%arrange(date)
#Renaming tasking outcome variables
SARH_2015_2021 <- SARH_2015_2021%>%mutate(tasking_outcome=ifelse({tasking_outcome=="Completed"|tasking_outcome=="Complete"|tasking_outcome=="Complete (Nothing Found)"|tasking_outcome=="Supported & Completed"},{"Completed"},{"Terminated"}))
#Renaming type of tasking 
SARH_2015_2021$type_of_tasking <- recode_factor(SARH_2015_2021$type_of_tasking, "Search (only)"="Search", "Aborted/Not Required"="Support", "Pre-arranged transfer"="Pre-arranged Transfer", 
                                                "Rescue/ Recovery"="Rescue/Recovery", "Search (Only)"="Search", "Search only"="Search", "Search Only"="Search")
#Creating Factors
SARH_2015_2021 <- SARH_2015_2021%>%mutate(tasking_outcome=as.factor(tasking_outcome),tasking_location=as.factor(tasking_location))






ggplot(filter(SARH_2015_2021,"2016" <= year(date)))+
  layer_spatial(uk,col ="#6D756F",fill = "#A8F28D")+
  geom_point(mapping=aes(x=longitude,y=latitude,colour = type_of_tasking),shape = 4)+
  facet_wrap("base",nrow = 2)+
  scale_colour_brewer(palette = "Dark2")+
  theme_bw()%+replace%theme(legend.position = "bottom",legend.title = element_text(size = rel(1.5)),axis.text = element_blank(), legend.title.align = 0.5,
                            axis.title = element_blank(),axis.ticks = element_blank(),strip.background = element_rect_round(radius = unit(8, "pt"),fill = "#5D948C"),
                            panel.background = element_rect_round(radius = unit(2, "cm"),fill="#D4EBFF"),
                            panel.border = element_blank(), panel.grid = element_blank(),
                            plot.title = element_text(hjust = 0.5, size = rel(2.5), vjust = 1.5),
                            plot.subtitle = element_text(hjust = 0.5, vjust = 1.5))+
  labs(title = "SARH Taskings by Base",subtitle = "2017-2021", colour = "Type")




ggplot(df,aes(x=year,y=rank,colour=base))+
  geom_bump(size = 2, smooth = 8)+
  geom_point(size=7)+
  geom_text(data = df%>%filter(year == min(year)), aes(x = year - .2, label = base), size = 5, hjust = 1)+
  geom_text(data = df%>%filter(year == max(year)), aes(x = year + .2, label = base), size = 5, hjust = 0)+
  scale_x_continuous(limits = c(2014.6, 2022.4),breaks = seq(2016, 2021, 1))+
  theme_bw()%+replace%theme(plot.title = element_text(hjust = 0.5,size = rel(2)))+
  theme(legend.position = "none", panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank(),
        axis.ticks = element_blank(),text = element_text(family = "sans", face = "plain"))+
  scale_y_discrete(limit = factor(rev(seq(1,10,1))))+
  scale_color_brewer(palette="Paired")+
  labs(x="Year",y="Rank",title="Busiest UKSARH Bases",caption="Data : Department for Transport")