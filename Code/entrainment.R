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

write.csv(LTERnutrient.df.kz, "../Data/LTERnutrient.df.kz.csv", row.names=FALSE)
dim(LTERnutrient.df.kz)



# The entrainment calculations is:
#kz * difference(upper concentration / lower concentration)/area (thickness of layer)

# the hypsometry file would have our information...

hypso <- read.csv("Mendota2014-2019/LakeEnsemblR_bathymetry_standard.csv")


ggplot(hypso, aes(x=Area_meterSquared, y=-Depth_meter)) + geom_point()


#add a column with the hypsometry file values next to the LTERnutrient.df.kz
LTERnutrient.df.kz <- left_join(LTERnutrient.df.kz, hypso, by=c("depth"="Depth_meter"))


#Plot some simple plots to explore the kz value over time in relation to area:
ggplot(LTERnutrient.df.kz,aes(x=sampledate, y=kz, color=depth))+
  geom_point()+
  geom_line()+
  theme_bw()
  #facet_grid(Area_meterSquared~., scales="free")

ggsave("Plots/Entrainment/kz.values.over.time.PDF", width = 11, height = 5, units="in")


#Let's try to calculate the load difference in nh4 in the epi between time steps:
LTERnutrient.df.kz.test <- LTERnutrient.df.kz %>% filter(!is.na(no3no2_sloh))

LTERnutrient.df.kz.test <- LTERnutrient.df.kz.test %>% group_by(sampledate) %>% mutate(massArea = sqrt(trapz(depth, no3no2_sloh)))
# Quickly plot the values over time:
ggplot(LTERnutrient.df.kz.test)+
  geom_point(aes(x=sampledate, y=massArea), color="blue")+
  geom_point(aes(x=sampledate, y=no3no2_sloh), color="red")+
  theme_bw()+
  ggtitle("Red= NO3NO2 concentrations, \nBlue = Calculated change in concentrations over dates per time")+
  ylab("Red: [mass/time] , Blue [mg/L]")

ggsave("Plots/Entrainment/calculation.mass.over.area.PDF", width = 11, height = 5, units = "in")

#Now that we have our first equation, we should use that value to find the difference in that value between two time points.
LTERnutrient.df.kz.test.lag.day <- LTERnutrient.df.kz.test %>% group_by(sampledate) %>% mutate(lagDay = lag(sampledate))

LTERnutrient.df.kz.test.unique <- LTERnutrient.df.kz.test %>% select(sampledate, massArea) %>% distinct()

LTERnutrient.df.kz.test.unique$changeinLoad <- lag(LTERnutrient.df.kz.test.unique$massArea)

ggplot(LTERnutrient.df.kz.test.unique)+
  geom_point(aes(x=sampledate, y=changeinLoad))+
  ylab(expression(paste("NO3NO2 in mg /",day^-1,m^-1)))+
  theme_bw()+
  ggtitle("Load difference in one layer (epilimnion) between time points")

ggsave("Plots/Entrainment/Load.diff.no3no2.over.time.one.layer.PDF", width = 11, height = 6, units = "in")


# To calculate the change in load between two time points

# for (i in 1:length(dates)){
#   
#   day.of.data <- LTERnutrient.df.kz.nh4 %>% filter(sampledate == dates[i]) 
#   
#   for (depth.j in 1:nrow(day.of.data)){
#     kz.value <- day.of.data$kz[depth.j]
#     upper.concentration <- day.of.data$nh4_sloh[depth.j]
#     lower.concentration <- day.of.data$nh4_sloh[depth.j]
#     depth.upper <- day.of.data$depth[depth.j]
#     depth.lower <- day.of.data$depth[depth.j+1]
#     hypso.area.upper <- hypso %>% filter(Depth_meter == depth.upper) 
#     hypso.area.upper <- hypso.area.upper$Area_meterSquared[1]
#     hypso.area.lower <- hypso %>% filter(Depth_meter == depth.lower)
#     hypso.area.lower <- hypso.area.lower$Area_meterSquared[1]
#     hypso.area.calc <- abs(hypso.area.lower-hypso.area.upper)
#     
#     equation <- (kz.value * (lower.concentration - upper.concentration) ) /hypso.area.calc
# 
#     LTER  
#     }
# }
