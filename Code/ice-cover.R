### ice
# Package ID: knb-lter-ntl.33.33 Cataloging System:https://pasta.edirepository.org.
# Data set title: North Temperate Lakes LTER: Ice Duration - Madison Lakes Area 1853 - current.
# Data set creator:  NTL Lead PI - University of Wisconsin 
# Data set creator:  North Temperate Lakes LTER -  
# Data set creator:  John Magnuson - University of Wisconsin 
# Data set creator:  Stephen Carpenter - University of Wisconsin 
# Data set creator:  Emily Stanley - University of Wisconsin 
# Metadata Provider:  NTL Information Manager - University of Wisconsin 
# Contact:  NTL Information Manager -  University of Wisconsin  - ntl.infomgr@gmail.com
# Contact:  NTL Lead PI -  University of Wisconsin  - ntl.leadpi@gmail.com
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/33/33/f5bc02452cafcd461c49bd7429d8b40c" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")
dt3 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "lakeid",     
                 "season",     
                 "iceon",     
                 "ice_on",     
                 "iceoff",     
                 "ice_off",     
                 "ice_duration",     
                 "year4"    ), check.names=TRUE)
unlink(infile1)
# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
if (class(dt3$lakeid)!="factor") dt3$lakeid<- as.factor(dt3$lakeid)
if (class(dt3$season)!="factor") dt3$season<- as.factor(dt3$season)
if (class(dt3$iceon)=="factor") dt3$iceon <-as.numeric(levels(dt3$iceon))[as.integer(dt3$iceon) ]               
if (class(dt3$iceon)=="character") dt3$iceon <-as.numeric(dt3$iceon)
if (class(dt3$iceoff)=="factor") dt3$iceoff <-as.numeric(levels(dt3$iceoff))[as.integer(dt3$iceoff) ]               
if (class(dt3$iceoff)=="character") dt3$iceoff <-as.numeric(dt3$iceoff)
if (class(dt3$ice_duration)=="factor") dt3$ice_duration <-as.numeric(levels(dt3$ice_duration))[as.integer(dt3$ice_duration) ]               
if (class(dt3$ice_duration)=="character") dt3$ice_duration <-as.numeric(dt3$ice_duration)
if (class(dt3$year4)=="factor") dt3$year4 <-as.numeric(levels(dt3$year4))[as.integer(dt3$year4) ]               
if (class(dt3$year4)=="character") dt3$year4 <-as.numeric(dt3$year4)
# Convert Missing Values to NA for non-dates
# Here is the structure of the input data frame:
str(dt3)                            
attach(dt3)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 
summary(lakeid)
summary(season)
summary(iceon)
summary(ice_on)
summary(iceoff)
summary(ice_off)
summary(ice_duration)
summary(year4) 
# Get more details on character variables
summary(as.factor(dt3$lakeid)) 
summary(as.factor(dt3$season))
detach(dt3)               
df_ice <- dt3 %>%
  dplyr::filter(lakeid == 'ME') %>%
  mutate("ice_on_doy" = yday(as.Date(ice_on, format = '%m/%d/%Y')), "ice_off_doy" = yday(as.Date(ice_off, format = '%m/%d/%Y'))) %>%
  select(year4, ice_on_doy, ice_off_doy, ice_duration)
m.df.ice <- reshape2::melt(df_ice, id = 'year4')
g5 = ggplot(m.df.ice, aes(year4, value)) +
  geom_point() +
  geom_smooth(method = "loess")+
  facet_wrap(~ variable, ncol=1, scales = 'free') +
  ylab('') +
  theme_minimal();g5
ggsave(file=paste0('Plots/Entrainment/icecover.png'), g5, dpi = 300,width = 6,height = 6, units = 'in')

