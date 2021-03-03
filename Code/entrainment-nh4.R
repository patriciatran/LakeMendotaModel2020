# Entrainment data phosphorus for Lake Mendota:
LTERnutrient.df.kz <- readRDS("Data/LTERnutrient.df.kz.RDS")

#Let's try to calculate the load difference in nh4 in the whole water column between time steps:
LTERnutrient.df.kz.test <- LTERnutrient.df.kz %>% filter(!is.na(nh4_sloh)) %>% filter(layer == "epi")

LTERnutrient.df.kz.test <- LTERnutrient.df.kz.test %>% group_by(sampledate) %>% mutate(massArea = trapz(depth, nh4_sloh))
# Quickly plot the values over time:
ggplot(LTERnutrient.df.kz.test)+
  geom_point(aes(x=sampledate, y=massArea), color="blue")+
  geom_point(aes(x=sampledate, y=no3no2_sloh), color="red")+
  theme_bw()+
  ggtitle("Red= NH4 concentrations, \nBlue = Calculated change in concentrations over depth per time")+
  ylab("Red: [mass/time] , Blue [mg/L]")

ggsave("Plots/Entrainment/NH4.calculation.mass.over.area.epi.PDF", width = 11, height = 5, units = "in")

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

ggplot(LTERnutrient.df.kz.test.unique)+
  geom_point(aes(x=sampledate, y=changeinLoad))+
  ylab(expression(paste("NH4 in g /",day^-1,m^2^-1)))+
  theme_bw()+
  ggtitle("Load difference in whole water column between time points (epi)")

ggsave("Plots/Entrainment/Load.diff.nh4.over.time.one.layer.epi.PDF", width = 11, height = 6, units = "in")

min(LTERnutrient.df.kz.test.unique$sampledate)
max(LTERnutrient.df.kz.test.unique$sampledate)

ice.data <- readRDS("Data/ice.data.RDS")

ice.data$date <- as.Date(ice.data$value, origin=paste0(ice.data$year4,"-01-01"))
ice.on <- ice.data %>% filter(variable == "ice_on_doy")
ice.off <- ice.data %>% filter(variable == "ice_off_doy")

ice.data.to.plot <- ice.data %>% filter(variable == "ice_on_doy" | variable == "ice_off_doy")

ice.data.to.plot <- ice.data.to.plot %>% mutate(color.to.plot = ifelse(variable == "ice_on_doy","red","blue"))

ggplot(LTERnutrient.df.kz.test.unique)+
  geom_point(aes(x=sampledate, y=changeinLoad))+
  ylab(expression(paste("NH4 in g ",day^-1,m^2^-1)))+
  theme_bw()+
  ggtitle("Load difference in whole water column between time points (epi)")+
  annotate( #ice-on
    "segment",
    x=ice.data.to.plot$date,
    xend=ice.data.to.plot$date,
    y=2,
    yend=1.5,
    color=ice.data.to.plot$color.to.plot,
    arrow=arrow(length=unit(0.05,"npc")
    ))+
  ylim(-0.5,2)+
  ggtitle("Red arrow = Ice on, Blue arrow = Ice off")

ggsave("Plots/Entrainment/ice.on.ice.off.load.diff.nh4.epi.PDF", width = 11, height = 8.5, units="in")

ggplot(LTERnutrient.df.kz.test.unique)+
  geom_point(aes(x=sampledate, y=changeinLoad))+
  ylab(expression(paste("NH4 in g ",day^-1,m^2^-1)))+
  theme_bw()+
  ggtitle("Load difference in whole water column between time points (epi)")+
  annotate( #ice-on
    "segment",
    x=ice.data.to.plot$date,
    xend=ice.data.to.plot$date,
    y=2,
    yend=-0.5,
    color=ice.data.to.plot$color.to.plot,
    alpha=0.5,
    linetype=2
  )+
  ylim(-0.5,2)+
  ggtitle("Red line = Ice on, Blue off = Ice off")

ggsave("Plots/Entrainment/ice.on.ice.off.load.diff.nh4.dashed.epi.PDF", width = 11, height = 8.5, units="in")

## Adding stratification onset and off-set information:
stratification <- read.table("Data/start.onset.offset.TSV", sep="\t", header=TRUE)
stratification$Onset_date <- as.Date(stratification$Onset, origin=paste0(stratification$year,"-01-01"))
stratification$Offset_date <- as.Date(stratification$Offset, origin=paste0(stratification$year,"-01-01"))

stratification <- stratification %>% filter(year > 2014)

stratification <- stratification %>% gather(variable_strat, date_strat, Onset_date:Offset_date)

ggplot(LTERnutrient.df.kz.test.unique)+
  geom_point(aes(x=sampledate, y=changeinLoad))+
  ylab(expression(paste("NH4 in g ",day^-1,m^2^-1)))+
  theme_bw()+
  ggtitle("Load difference in whole water column between time points (epi)")+
  annotate( #ice-on
    "segment",
    x=ice.data.to.plot$date,
    xend=ice.data.to.plot$date,
    y=2,
    yend=-0.5,
    color=ice.data.to.plot$color.to.plot,
    alpha=1,
    linetype=2
  )+
  annotate( #stratification
    "segment",
    x=stratification$date_strat,
    xend=stratification$date_strat,
    y=2,
    yend=-0.5,
    color="dark green",
    alpha=1,
    linetype=2
  )+
  ylim(-0.5,2)+
  ggtitle("Red line = Ice on, \nBlue off = Ice off, \nGreen = Stratification onset and offset")

ggsave("Plots/Entrainment/ice.on.ice.off.strat.load.diff.nh4.dashed.epi.PDF", width = 11, height = 8.5, units="in")

