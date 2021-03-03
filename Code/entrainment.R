# Entrainment data phosphorus for Lake Mendota:

# Testing time series for Lake Mendota using Hilary's R package and LTER data:
#install.packages("devtools")
devtools::install_github("hdugan/NTLlakeloads")
library(NTLlakeloads)

# Use these objects names, hardcoded at the moment
#LTERtemp = loadLTERtemp() # Download NTL LTER data from EDI
LTERnutrients = loadLTERnutrients() # Download NTL LTER data from EDI
#LTERions = loadLTERions() # Download NTL LTER data from EDI

# Variables available for plotting:
availableVars()

# Load the Kz values:
kz_values <- read.table("k_out_2014-2019.tsv", header=TRUE)

#install.packages("tidyverse")
library(tidyverse)
library(lubridate)

kz_values.long <- kz_values %>% gather("Depth","kz",X.25:X0)
kz_values.long$Depth <- str_replace(kz_values.long$Depth, "X.","")
kz_values.long$Depth <- as.numeric(kz_values.long$Depth)
kz_values.long$Date <- ymd(kz_values.long$Date)

LTERnutrients.df <- LTERnutrients %>% filter(lakeid == "ME")

# Merge by sample date and depth:

LTERnutrient.df.kz <- full_join(LTERnutrients.df, kz_values.long, by=c("sampledate"="Date", "depth"="Depth"))

# Only get the times with kz values:
LTERnutrient.df.kz <- LTERnutrient.df.kz %>% filter(!is.na(kz),
                                                    !is.na(lakeid))

write.csv(LTERnutrient.df.kz, "Data/LTERnutrient.df.kz.csv", row.names=FALSE)
dim(LTERnutrient.df.kz)



# The entrainment calculations is:
#kz * difference(upper concentration / lower concentration)/area (thickness of layer)

# the hypsometry file would have our information...

hypso <- read.csv("Mendota2014-2019/LakeEnsemblR_bathymetry_standard.csv")


ggplot(hypso, aes(x=Area_meterSquared, y=-Depth_meter)) + geom_point()

ggsave("Plots/Entrainment/Hypso.PDF", height = 6, width = 6, units="in")
#add a column with the hypsometry file values next to the LTERnutrient.df.kz
LTERnutrient.df.kz <- left_join(LTERnutrient.df.kz, hypso, by=c("depth"="Depth_meter"))


#Plot some simple plots to explore the kz value over time in relation to area:
ggplot(LTERnutrient.df.kz,aes(x=sampledate, y=kz, color=depth))+
  geom_point()+
  geom_line()+
  theme_bw()
  #facet_grid(Area_meterSquared~., scales="free")

ggsave("Plots/Entrainment/kz.values.over.time.PDF", width = 11, height = 5, units="in")

# Before we plot anything , we must distinguish between the epilimnion and the hypolimnion:
# we have to load the temperature data and merge that to our table first:
water.temp <- read.csv("Mendota2014-2019/LakeEnsemblR_wtemp_profile_standard.csv")
# calculate the thermocline for each day:
library(rLakeAnalyzer)
# To make correct calculations, it is better to remove the NA values first:
water.temp <- water.temp %>% filter(!is.na(Water_Temperature_celsius))
average_temp_depth <- water.temp %>% group_by(datetime, Depth_meter) %>% summarise(average=mean(Water_Temperature_celsius))


average_temp_depth <- average_temp_depth %>% mutate(thermocline = thermo.depth(average, Depth_meter))
# We only care about the thermocline depth at each data, so let's collect those columns, and remove duplicates:
average_temp_depth <- average_temp_depth %>% select(datetime, thermocline) %>% distinct()
average_temp_depth$datetime <- ymd(average_temp_depth$datetime)

# Add the thermocline data to our full table:
LTERnutrient.df.kz <- left_join(LTERnutrient.df.kz, average_temp_depth, by=c("sampledate"="datetime"))

# Now I want to add a column that says whether the sample is above or below the thermocline. If depth < thermo = epi, else hypo
#LTERnutrient.df.kz<-LTERnutrient.df.kz %>% mutate(layer = ifelse(depth < thermocline, "epi","hypo"))

LTERnutrient.df.kz$layer = ""

for (i in 1:nrow(LTERnutrient.df.kz)){
  if (is.na(LTERnutrient.df.kz$thermocline[i])){
    LTERnutrient.df.kz$layer[i] = "mixed"
  }
  else if(LTERnutrient.df.kz$depth[i] < LTERnutrient.df.kz$thermocline[i]){
    LTERnutrient.df.kz$layer[i] = "epi"
  }
  else{
    LTERnutrient.df.kz$layer[i] = "hypo"
  }
  print(i)
}

table(LTERnutrient.df.kz$layer)

nrow(is.na(LTERnutrient.df.kz$layer)) # now every date has a layer classification :-)

#### SAVE DATA ####
saveRDS(LTERnutrient.df.kz, "Data/LTERnutrient.df.kz.RDS")

#### LOAD DATA ####
LTERnutrient.df.kz <- readRDS("Data/LTERnutrient.df.kz.RDS")
str(LTERnutrient.df.kz)

#### THIS IS THE PART YOU CAN EDIT DEPENDING ON YOUR QUESTION####
# DO YOU WANT TO INTEGRATE OVER THE WHOLE WATER COLUMN OR JUST A LAYER?
# WHICH VARIABLES DO YOU WANT TO PLOT?

#layer.to.plot <- c("epi","hypo","mixed") # whole water column

layer.to.plot <- c("epi","mixed") # epi layer only

# variables I've plotted: start with those that are mg/L:
#nh4_sloh, no3no2_sloh, totpuf_sloh, drp_sloh

var.to.plot <- "no3no2_sloh"



#Let's try to calculate the load difference in nh4 in the whole water column between time steps:
#LTERnutrient.df.kz.test <- LTERnutrient.df.kz %>% filter(!is.na(var.to.plot))
#LTERnutrient.df.kz %>% nrow()
#LTERnutrient.df.kz.test <- LTERnutrient.df.kz %>% filter(!is.na(no3no2_sloh))
#LTERnutrient.df.kz %>% filter(!is.na(no3no2_sloh)) %>% nrow() 
#LTERnutrient.df.kz %>% filter(!is.na(var.to.plot)) %>% nrow() 

LTERnutrient.df.kz.test <- LTERnutrient.df.kz %>% filter(!is.na(!!as.symbol(var.to.plot))) %>%
  filter(layer %in% layer.to.plot)

# Save name of the layers so I can name my plots accordingly:
layer.names <- gsub(", ","",toString(layer.to.plot))

nrow(LTERnutrient.df.kz.test)

#LTERnutrient.df.kz.test <- LTERnutrient.df.kz.test %>% 
#  group_by(sampledate) %>% 
#  mutate(massArea = trapz(depth, !!as.symbol(var.to.plot)))


LTERnutrient.df.kz.test <- LTERnutrient.df.kz.test %>% 
  group_by(sampledate) %>% 
  mutate(massArea = trapz(depth, !!as.symbol(var.to.plot)))


# Quickly plot the values over time:
ggplot(LTERnutrient.df.kz.test)+
  geom_point(aes(x=sampledate, y=massArea), color="blue")+
  geom_point(aes(x=sampledate, y=!!as.symbol(var.to.plot)), color="red")+
  theme_bw()+
  ggtitle(paste0("Red=",var.to.plot,"\nBlue = Calculated change in concentrations over depth per time"))+
  ylab("Red: [mass/time] , Blue [mg/L]")

ggsave(paste0("Plots/Entrainment/calculation.mass.over.area.",var.to.plot,"_",layer.names,".PDF"), width = 11, height = 5, units = "in")

#Now that we have our first equation, we should use that value to find the difference in that value between two time points.
LTERnutrient.df.kz.test.lag.day <- LTERnutrient.df.kz.test %>% select(sampledate, massArea)
LTERnutrient.df.kz.test.lag.day$sampledate <- ymd(LTERnutrient.df.kz.test.lag.day$sampledate)
# Convert to numeric dates:

LTERnutrient.df.kz.test.lag.day$numericdates <- as.numeric(LTERnutrient.df.kz.test.lag.day$sampledate)

# Remove duplicates:
LTERnutrient.df.kz.test.lag.day <- LTERnutrient.df.kz.test.lag.day %>% distinct()

lagDays <- NA

lagDays2 <- diff(LTERnutrient.df.kz.test.lag.day$numericdates, lag=1)
lagDays3 <- append(lagDays, lagDays2)

lagDays3


LTERnutrient.df.kz.test.lag.day$lagDay <- lagDays3

LTERnutrient.df.kz.test.unique <- LTERnutrient.df.kz.test.lag.day %>% select(sampledate, massArea, lagDay) %>% distinct()

LTERnutrient.df.kz.test.unique$changeinLoad <- lag(LTERnutrient.df.kz.test.unique$massArea)/LTERnutrient.df.kz.test.lag.day$lagDay

### NOTE ABOUT UNITS!!
# I'll change the code later to make sure the units are right but for now... whatever!

ymin = min(LTERnutrient.df.kz.test.unique$changeinLoad, na.rm=T)
ymax = max(LTERnutrient.df.kz.test.unique$changeinLoad, na.rm=T)

ggplot(LTERnutrient.df.kz.test.unique)+
  geom_point(aes(x=sampledate, y=changeinLoad))+
  ylab((paste0(var.to.plot, " in g ",expression(day^-1,m^2^-1))))+
  theme_bw()+
  ggtitle("Load difference in water column between time points")

ggsave(paste0("Plots/Entrainment/",var.to.plot,"_",layer.names,".Load.diff.over.time.one.layer.PDF"), width = 11, height = 6, units = "in")

min(LTERnutrient.df.kz.test.unique$sampledate)
max(LTERnutrient.df.kz.test.unique$sampledate)

ice.data <- readRDS("Data/ice.data.RDS")

ice.data$date <- as.Date(ice.data$value, origin=paste0(ice.data$year4,"-01-01"))
ice.on <- ice.data %>% filter(variable == "ice_on_doy")
ice.off <- ice.data %>% filter(variable == "ice_off_doy")

ice.data.to.plot <- ice.data %>% filter(variable == "ice_on_doy" | variable == "ice_off_doy")

ice.data.to.plot <- ice.data.to.plot %>% mutate(color.to.plot = ifelse(variable == "ice_on_doy","red","blue"))

## Adding stratification onset and off-set information:
stratification <- read.table("Data/start.onset.offset.TSV", sep="\t", header=TRUE)
stratification$Onset_date <- as.Date(stratification$Onset, origin=paste0(stratification$year,"-01-01"))
stratification$Offset_date <- as.Date(stratification$Offset, origin=paste0(stratification$year,"-01-01"))

stratification <- stratification %>% filter(year > 2014)

stratification <- stratification %>% gather(variable_strat, date_strat, Onset_date:Offset_date)


ggplot(LTERnutrient.df.kz.test.unique)+
  geom_point(aes(x=sampledate, y=changeinLoad))+
  ylab(expression(paste("in g ",day^-1,m^2^-1)))+
  theme_bw()+
  ggtitle("Load difference in water column between time points")+
  annotate( #ice-on
    "segment",
    x=ice.data.to.plot$date,
    xend=ice.data.to.plot$date,
    y=ymax,
    yend=0,
    color=ice.data.to.plot$color.to.plot,
    alpha=1,
    linetype=2
  )+
  annotate( #stratification
    "segment",
    x=stratification$date_strat,
    xend=stratification$date_strat,
    y=ymax,
    yend=0,
    color="dark green",
    alpha=1,
    linetype=2
  )+
  ylim(ymin-(ymin/100*20),ymax+(ymax/100*20))+
  ggtitle(paste0(as.character(var.to.plot),", layer=",layer.names),
    subtitle = "Red line = Ice on, \nBlue off = Ice off, \nGreen = Stratification onset and offset")

ggsave(paste0("Plots/Entrainment/ice.on.ice.off.strat.load.diff.",var.to.plot,"_",layer.names,".dashed.PDF"), width = 11, height = 8.5, units="in")
