# Patricia Tran
# Reading in Emily Stanley and Quinn Gavin's nitrogen data and get into a nice format to calculate fluxes

#### LOAD PACKAGES ####
library(tidyverse)
library(lubridate)
library(viridis)
library(ggpubr)

#### MERGE FILES ####
my_path <- "../../../../patriciatran/Dropbox/MendotaProfiles/CompiledProfileData/"
nitrogen.profiles.list <- list.files("../../../../patriciatran/Dropbox/MendotaProfiles/CompiledProfileData/")
myfiles = lapply(paste0(my_path,nitrogen.profiles.list), read.csv)

combined.profiles <- as.data.frame(myfiles[1])

for (i in 2:length(myfiles)){
  combined.profiles <- full_join(combined.profiles, as.data.frame(myfiles[i]))
  print(i)
}

saveRDS(combined.profiles, "Data/nitrogen.profiles.mendota.RDS")
write.csv(combined.profiles,"Data/nitrogen.profiles.mendota.csv", row.names = FALSE)


#### PLOT THE ENVIRONMENTAL DATA ####
my_theme <- theme_bw()+
  theme(text = element_text(size=15,colour = "black",family="sans"),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

combined.profiles <- readRDS("Data/nitrogen.profiles.mendota.RDS")

# Nitrate values below zero are below detection limit:
combined.profiles <- combined.profiles %>% mutate(NITRATE_UM_corrected = ifelse(NITRATE_UM <= 0, 0, NITRATE_UM))


combined.profiles$sampledate <- ymd(str_replace(combined.profiles$DateTime_Start, " .*",""))

colnames(combined.profiles)

ggplot(combined.profiles)+
  geom_point(aes(x=sampledate, y=NITRATE_UM_corrected, color=Depth_Target))+
  my_theme+
  scale_x_date(date_labels = "%b 20%y")+
  scale_color_viridis(direction = -1)

## Add stratification ##
stratification <- read.table("Data/start.onset.offset.TSV", sep="\t", header=TRUE)
stratification$Onset_date <- as.Date(stratification$Onset, origin=paste0(stratification$year,"-01-01"))
stratification$Offset_date <- as.Date(stratification$Offset, origin=paste0(stratification$year,"-01-01"))

stratification <- stratification %>% filter(year >= year(min(combined.profiles$sampledate, na.rm= TRUE)-30)
                                            & year <= year(max(combined.profiles$sampledate, na.rm= TRUE)+30))

stratification <- stratification %>% gather(variable_strat, date_strat, Onset_date:Offset_date)

str(stratification$date_strat)

## Ice data:
ice.data <- readRDS("Data/ice.data.RDS")

ice.data$date <- as.Date(ice.data$value, origin=paste0(ice.data$year4,"-01-01"))
ice.on <- ice.data %>% filter(variable == "ice_on_doy")
ice.off <- ice.data %>% filter(variable == "ice_off_doy")

ice.data.to.plot <- ice.data %>% filter(variable == "ice_on_doy" | variable == "ice_off_doy")

ice.data.to.plot <- ice.data.to.plot %>% mutate(color.to.plot = ifelse(variable == "ice_on_doy","red","blue"))

#ice.data.to.plot <- ice.data.to.plot %>% filter(year4 >= year(min(combined.profiles$sampledate, na.rm= TRUE)-30)
#                                                & year4 <= year(max(combined.profiles$sampledate, na.rm= TRUE)+30))

# Also from my emails I know that ice-off was on March 24, 2018 but it wasn't in the table, therefore, let's add this columnd
ice.data.to.plot[nrow(ice.data.to.plot) + 1,] = c(2018,"ice_off_doy",day(ymd("2018-03-24")), "2018-03-24","blue")
ice.data.to.plot[nrow(ice.data.to.plot) + 1,] = c(2018,"ice_on_doy",day(ymd("2018-12-15")), "2018-12-15","red")
ice.data.to.plot[nrow(ice.data.to.plot) + 1,] = c(2018,"ice_off_doy",day(ymd("2018-12-21")), "2018-12-21","blue")
ice.data.to.plot[nrow(ice.data.to.plot) + 1,] = c(2019,"ice_on_doy",day(ymd("2019-01-10")), "2019-01-10","red")
ice.data.to.plot[nrow(ice.data.to.plot) + 1,] = c(2019,"ice_off_doy",day(ymd("2019-03-31")), "2019-03-31","blue")

str(ice.data.to.plot)

saveRDS(ice.data.to.plot, "Data/ice.data.to.plot.RDS")

## ODO plot:df[nrow(df) + 1,] = c("v1","v2")
combined.profiles <- combined.profiles %>% mutate(do.color = ifelse(ODOmgL <= 0, "white", "black"))
combined.profiles.anoxia <- combined.profiles %>% filter(do.color == "white")
start.anoxia <- min(combined.profiles.anoxia$sampledate)
end.anoxia <- max(combined.profiles.anoxia$sampledate)
## Plot
ymin = min(combined.profiles$NITRATE_UM_corrected, na.rm = TRUE)
ymax = max(combined.profiles$NITRATE_UM_corrected, na.rm = TRUE)

ggplot(combined.profiles)+
  geom_point(aes(x=sampledate, y=NITRATE_UM_corrected, color=Depth_Target), size=3.5, alpha=0.7)+
  my_theme+
  scale_x_date(date_labels = "%b 20%y", date_breaks = "1 month",
               limits = ymd(c("2017-10-15","2018-10-01")))+
  scale_color_viridis(direction = -1)+
  annotate( #stratification
    "segment",
    x=stratification$date_strat,
    xend=stratification$date_strat,
    y=ymax,
    yend=ymin,
    color="dark green",
    alpha=1,
    linetype=2
  )+
  annotate( #ice-on
    "segment",
    x=ice.data.to.plot$date,
    xend=ice.data.to.plot$date,
    y=ymax,
    yend=ymin,
    color=ice.data.to.plot$color.to.plot,
    alpha=1,
    linetype=2
  )+
  geom_hline(aes(yintercept = 0), color="grey")+
  ggtitle("Nitrogen profile data",
          subtitle="Red = Ice-On, \nBlue=Ice-Off, \nGreen=Stratification onset and offset")+
  ylab(expression(paste("Nitrate ",mu,"M")))+
  xlab("Sample Date")+
  annotate("rect",
           xmin= start.anoxia,
           xmax= end.anoxia,
           ymin=190,
           ymax=200,
           color="black"
  )+
  annotate("text",
           x= start.anoxia + 20,
           y= 210,
           label= "Anoxia")

ggsave("Plots/StanleyProfiles/nitrate.um.PDF", width = 11, height = 6, units="in")
ggsave("Plots/StanleyProfiles/nitrate.um.png", dpi= 300, width = 11, height = 6, units="in")

#### Also plot the 2019 profiles from Patricia ####
my_path.patricia <- "../../../../patriciatran/Box/PhD/Research/MEHypo/Data/PatriciaTran_ProfileData/2019/SUNA/"
nitrogen.profiles.list.patricia <- list.files("../../../../patriciatran/Box/PhD/Research/MEHypo/Data/PatriciaTran_ProfileData/2019/SUNA/")
nitrogen.profiles.list.patricia

myfiles.patricia = lapply(paste0(my_path.patricia, nitrogen.profiles.list.patricia), read.csv)
myfiles.patricia

combined.profiles.patricia <- as.data.frame(myfiles.patricia[1])
combined.profiles.patricia <- full_join(as.data.frame(myfiles.patricia[1]),as.data.frame(myfiles.patricia[2]))
combined.profiles.patricia <- full_join(combined.profiles.patricia, as.data.frame(myfiles.patricia[3]))


combined.profiles.patricia$sampledate <- ymd(str_replace(combined.profiles.patricia$DateTime_Start, " .*",""))
# Merge with Quinn's data:
combined.profiles.2 <- full_join(combined.profiles, combined.profiles.patricia)

saveRDS(combined.profiles.2, "Data/combined.profiles.quinn.patricia.2017-2019.RDS")

combined.profiles.2 <- readRDS("Data/combined.profiles.quinn.patricia.2017-2019.RDS")

## Plot data:
combined.profiles <- combined.profiles %>% filter(NITRATE_UM >= 0)
start.quinn <- min(combined.profiles$sampledate, na.rm= TRUE) 
end.quinn <- max(combined.profiles.anoxia$sampledate)
start.patricia <- min(combined.profiles.anoxia$sampledate)
end.patricia <- max(combined.profiles.anoxia$sampledate)

plot1 <- ggplot(combined.profiles.2)+
  geom_point(aes(x=sampledate, y=NITRATE_UM, color=Depth_Target), size=3.5, alpha=0.7)+
  my_theme+
  scale_x_date(date_labels = "%b 20%y", date_breaks = "1 month", limits = ymd(c(start.quinn,"2019-04-01")))+
  scale_color_viridis(direction = -1)+
  annotate( #stratification
    "segment",
    x=stratification$date_strat,
    xend=stratification$date_strat,
    y=ymax,
    yend=ymin,
    color="dark green",
    alpha=1,
    linetype=2
  )+
  annotate( #ice-on
    "segment",
    x=ice.data.to.plot$date,
    xend=ice.data.to.plot$date,
    y=ymax,
    yend=ymin,
    color=ice.data.to.plot$color.to.plot,
    alpha=1,
    linetype=2
  )+
  geom_hline(aes(yintercept = 0), color="grey")+
  ggtitle("Nitrogen profile data",
          subtitle="Red = Ice-On, \nBlue=Ice-Off, \nGreen=Stratification onset and offset")+
  ylab(expression(paste("Nitrate ",mu,"M")))+
  xlab("Sample Date")+
  annotate("rect",
           xmin= start.anoxia,
           xmax= end.anoxia,
           ymin=190,
           ymax=200,
           color="black"
  )+
  annotate("text",
           x= start.anoxia + 20,
           y= 210,
           label= "Anoxia")+
  #Quinn's data:
  annotate("rect",
           xmin= ymd(min(combined.profiles$sampledate, na.rm=TRUE)),
           xmax= ymd(max(combined.profiles$sampledate, na.rm=TRUE)),
           ymin=240,
           ymax=235,
           color="light grey",
           fill="light grey"
  )+
  annotate("text",
           x= ymd(min(combined.profiles$sampledate, na.rm=TRUE)) + 40,
           y= 250,
           label= "Quinn's dataset")+
  #Patricia's data:
  annotate("rect",
           xmin= ymd(min(combined.profiles.patricia$sampledate, na.rm=TRUE)),
           xmax= ymd(max(combined.profiles.patricia$sampledate, na.rm=TRUE)),
           ymin=240,
           ymax=235,
           color="light grey",
           fill= "light grey"
  )+
  annotate("text",
           x= ymd(min(combined.profiles.patricia$sampledate, na.rm=TRUE)) + 20,
           y= 250,
           label= "Patricia's dataset")+
  ylim(0,250)

ggsave(plot = plot1, "Plots/StanleyProfiles/nitrate.um.combined.PDF", width = 11, height = 6, units="in")
ggsave(plot = plot1, "Plots/StanleyProfiles/nitrate.um.combined.png", dpi= 300, width = 11, height = 6, units="in")

### CLEAN DATA BEFORE SHARING ###
combined.profiles.2_corrected <- combined.profiles.2 %>% mutate(NITRATE_uM_corrected = ifelse(NITRATE_UM < 0, 0, NITRATE_UM) )
combined.profiles.2_corrected <- combined.profiles.2_corrected %>% mutate(Dataset = ifelse(year(sampledate) < 2019, "Quinn", "Patricia"))

view(combined.profiles.2_corrected)

combined.profiles.2_corrected <- combined.profiles.2_corrected %>% select(-do.color)
dim(combined.profiles.2_corrected)

str(combined.profiles.2_corrected)

saveRDS(combined.profiles.2_corrected, "Data/combined.profiles.2_corrected.combined.RDS")
write.csv(combined.profiles.2_corrected, "Data/combined.profiles.2_corrected.combined.csv", row.names = FALSE)

## quickly view if there is weird data:
ggplot(combined.profiles.2_corrected, 
       aes(x=TempC, y=-Depth_Target, color=Dataset))+
  geom_point()+
  my_theme+
  ylab("Depth (m)")+
  scale_color_viridis(discrete = TRUE)

ggsave("Plots/StanleyProfiles/Combined.Temp.png", dpi = 300, width = 8, height = 8, units="in")

ggplot(combined.profiles.2_corrected, 
       aes(x=ODOmgL, y=-Depth_Target, color=Dataset))+
  geom_point()+
  my_theme+
  ylab("Depth (m)")+
  scale_color_viridis(discrete = TRUE)

ggsave("Plots/StanleyProfiles/Combined.ODOmgL.png", dpi = 300, width = 8, height = 8, units="in")


ggplot(combined.profiles.2_corrected, 
       aes(x=ChlorophyllRFU, y=-Depth_Target, color=Dataset))+
  geom_point()+
  my_theme+
  ylab("Depth (m)")+
  scale_color_viridis(discrete = TRUE)

ggsave("Plots/StanleyProfiles/Combined.Chlorophyll.png", dpi = 300, width = 8, height = 8, units="in")

ggplot(combined.profiles.2_corrected, 
       aes(x=TurbidityFNU, y=-Depth_Target, color=Dataset))+
  geom_point()+
  my_theme+
  ylab("Depth (m)")+
  scale_color_viridis(discrete = TRUE)

ggsave("Plots/StanleyProfiles/Combined.Turbidity.png", dpi = 300, width = 8, height = 8, units="in")

ggplot(combined.profiles.2_corrected, 
       aes(x=fDOMRFU, y=-Depth_Target, color=Dataset))+
  geom_point()+
  my_theme+
  ylab("Depth (m)")+
  scale_color_viridis(discrete = TRUE)

ggsave("Plots/StanleyProfiles/Combined.fDOM.png", dpi = 300, width = 8, height = 8, units="in")

## Precipitation data:
precip <- read.csv("../../../../patriciatran/Dropbox/MendotaProfiles/PrecipData/SD_b2dates.csv")[-c(1:6),]
colnames(precip) <- precip[1,]
precip <- precip[-1,]

precip$Date <- mdy(precip$Date)
precip <- precip %>% filter(PRCP != "T" & PRCP != "M")
str(precip)
precip$PRCP <- as.numeric(precip$PRCP)

plot2 <- ggplot(precip, aes(x=Date, y=PRCP))+
  geom_point()+
  ylab("Precipitation")+
  my_theme+
  scale_x_date(date_labels = "%b 20%y", date_breaks = "1 month", limits = ymd(c(start.quinn,"2019-04-01")))

precip <- precip %>% gather(temperature, value, TMAX:MEAN)

precip$value <- as.numeric(precip$value)

plot3 <- ggplot(precip, aes(x=Date, y=value, color=temperature))+
  geom_point()+
  my_theme+
  ylab("Temperature C")+
  scale_x_date(date_labels = "%b 20%y", date_breaks = "1 month", limits = ymd(c(start.quinn,"2019-04-01")))

plot4<-ggarrange(plot1, plot2, plot3, nrow=3)
ggsave(plot = plot4, filename="Plots/StanleyProfiles/combined.plot.panels.png", dpi=300, height=11, width = 8.5, units="in")
