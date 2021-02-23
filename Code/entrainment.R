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
kz_values <- read.table("../k_out_2014-2019.tsv", header=TRUE)

#install.packages("tidyverse")
library(tidyverse)

kz_values.long <- kz_values %>% gather("Depth","kz",X.25:X0)
kz_values.long$Depth <- str_replace(kz_values.long$Depth, "X.","")
kz_values.long$Depth <- as.numeric(kz_values.long$Depth)
kz_values.long$Date <- ymd(kz_values.long$Date)

LTERnutrients.df <- LTERnutrients %>% filter(lakeid == "ME")

# Merge by sample date and depth:

LTERnutrient.df.kz <- full_join(LTERnutrients.df, kz_values.long, by=c("sampledate"="Date", "depth"="Depth"))

# Only get the times with kz values:
LTERnutrient.df.kz <- LTERnutrient.df.kz %>% filter(!is.na(kz),
                                                    !is.na(lakeid),
                                                    rep == 1)

dim(LTERnutrient.df.kz)

# The entrainment calculations is:
#kz * difference(upper concentration / lower concentration)/area (thickness of layer)

# the hypsometry file would have our information...

hypso <- read.csv("../Mendota2014-2019/LakeEnsemblR_bathymetry_standard.csv")

ggplot(hypso, aes(x=Area_meterSquared, y=-Depth_meter)) + geom_point()

LTERnutrient.df.kz$flux <- ""

LTERnutrient.df.kz.nh4 <- LTERnutrient.df.kz %>% filter(!is.na(nh4_sloh))
dates <- unique(LTERnutrient.df.kz.nh4$sampledate)

for (i in 1:length(dates)){
  
  day.of.data <- LTERnutrient.df.kz.nh4 %>% filter(sampledate == dates[i]) 
  
  for (depth.j in 1:nrow(day.of.data)){
    kz.value <- day.of.data$kz[depth.j]
    upper.concentration <- day.of.data$nh4_sloh[depth.j]
    lower.concentration <- day.of.data$nh4_sloh[depth.j]
    depth.upper <- day.of.data$depth[depth.j]
    depth.lower <- day.of.data$depth[depth.j+1]
    hypso.area.upper <- hypso %>% filter(Depth_meter == depth.upper) 
    hypso.area.upper <- hypso.area.upper$Area_meterSquared[1]
    hypso.area.lower <- hypso %>% filter(Depth_meter == depth.lower)
    hypso.area.lower <- hypso.area.lower$Area_meterSquared[1]
    hypso.area.calc <- abs(hypso.area.lower-hypso.area.upper)
    
    equation <- (kz.value * (lower.concentration - upper.concentration) ) /hypso.area.calc
  }
}
