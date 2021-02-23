# Testing time series for Lake Mendota using Hilary's R package and LTER data:
install.packages("devtools")
devtools::install_github("hdugan/NTLlakeloads")
library(NTLlakeloads)

# Use these objects names, hardcoded at the moment
LTERtemp = loadLTERtemp() # Download NTL LTER data from EDI
LTERnutrients = loadLTERnutrients() # Download NTL LTER data from EDI
LTERions = loadLTERions() # Download NTL LTER data from EDI

# Variables available for plotting:
availableVars()

# printFigs = TRUE to output series of interpolated profiles (but slower)
df.ME = weeklyInterpolate(lakeAbr = 'ME', var = 'cl', maxdepth = 24, 
                          constrainMethod = 'zero', setThreshold = 0.1, printFigs = F)

plotTimeseries(df.ME$weeklyInterpolated, var = 'cl')

plotTimeseries.year(df.ME$weeklyInterpolated, df.ME$observations,  var = 'cl', chooseYear = 2008)

# calculate annual load:
df.load = calcLoad(df.ME$weeklyInterpolated,lakeAbr = 'ME', time.res = 'weekly')
df.load.annual = calcLoad(df.ME$weeklyInterpolated,lakeAbr = 'ME', time.res = 'annual')

decomposeTS(df.load, lakeAbr = 'ME', var = 'cl')
