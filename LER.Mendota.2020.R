## change working directory to the mendota folder
setwd("./Mendota2020/")

## Load required libraries for the workshop
library(gotmtools)
#library(LakeEnsemblR)
devtools::install_github('aemon-j/LakeEnsemblR')
library(LakeEnsemblR)
library(ggplot2)
library(ggpubr)
library(rLakeAnalyzer)
library(reshape)
library(RColorBrewer)
library(stringr)

# 1. Basic running of the model ensemble ----
## Have a look at the mendota folder. There will be six files
list.files()

## Look at the meteorological variables dictionary 
print(met_var_dic)

#meteo <- read.csv("LakeEnsemblR_meteo_standard_copy.csv", header=TRUE, row=1)

#export_init_cond("LakeEnsemblR.yaml",model = c("FLake", "GLM", "GOTM",
#                                               "Simstrat", "MyLake"),date = "2020-01-01 00:00:00",print = TRUE)

## Create all model subfolders, configuration and forcing files
export_config("LakeEnsemblR.yaml", model = c("FLake", "GLM", "GOTM",
                                             "Simstrat", "MyLake"))
# Here we can choose 5 models, or just a couplpe of them. It's up to us. 

## Now there are five additional folders, one for each model
list.files()

## Run the ensemble
run_ensemble("LakeEnsemblR.yaml", model = c("FLake", "GLM", "GOTM",
                                            "Simstrat", "MyLake"))

#run_ensemble("LakeEnsemblR.yaml", model = c("Simstrat"))
#run_ensemble("LakeEnsemblR.yaml", model = c("GLM"))

## Now there is an additional folder called output which contains the netcdf file
list.files("output")

# 2. Changing output options ----
## Change output to .csv files and rerun the ensemble
input_yaml_multiple("LakeEnsemblR.yaml", value = "text", key1 = "output",
                    key2 = "format")
run_ensemble("LakeEnsemblR.yaml", model = c("FLake", "GLM", "GOTM",
                                            "Simstrat", "MyLake"))

## Now there are additional .csv output files in the output folder
list.files("output")

## Change output format back to netcdf
input_yaml_multiple("LakeEnsemblR.yaml", value =  "netcdf", key1 = "output",
                    key2 = "format")

## Change the simstart parameter a_seiche to be 0.001

### Run the models:
run_ensemble("LakeEnsemblR.yaml", model = c("FLake", "GLM", "GOTM",
                                            "Simstrat", "MyLake"))


# 3. Plotting ensemble output ----
## Create heatmap plot from the netcdf file
plot_heatmap("output/ensemble_output.nc")+
  scale_colour_gradientn(limits = c(0, 30),
                         colours = rev(RColorBrewer::brewer.pal(11, "Spectral")))+
  theme_light()

ggsave("LER.with.inflow..a.seiche.0.001.pdf", width = 11, height = 8.5, units = "in")

## Create a plot with time series and time series of residuals at 10 m depth
depth <- seq(from = 0 , to= 24, by =1 )

for (i in 1:length(depth)){
  p1 <- plot_ensemble("output/ensemble_output.nc", model = c("FLake", "GLM",
                                                             "GOTM", "Simstrat",
                                                             "MyLake"),
                      var = "temp", depth = as.numeric(depth[i]),
                      residuals = TRUE)
  
  p1 <- p1 
  
  #p1
  
  # Arrange the two plots above each other
  ggarrange(p1[[1]] + theme_light(),
            p1[[2]] + theme_light(), ncol = 1, nrow = 2)
  
  ggsave(paste0("../Plots/Plots_residuals_",depth[i],".png"), height = 8.5, width = 11, units = "in")
  print(depth[i])
}


## Create a depth profile plot of the ensemble amd boxplot of the profiles for
dates <- c("2020-07-24 00:00:00","2020-08-05 00:00:00","2020-08-25 00:00:00",
           "2020-09-11 00:00:00","2020-10-08 00:00:00","2020-10-19 00:00:00")


for (i in 1:length(dates)){
  p2 <- plot_ensemble("output/ensemble_output.nc", model = c("FLake", "GLM",
                                                             "GOTM", "Simstrat",
                                                             "MyLake"),
                      var = "temp", date = dates[i],
                      boxwhisker = TRUE, residuals = FALSE)
  # Arrange the two plots above each other
  ggarrange(p2[[1]] + theme_light(),
            p2[[2]] + theme_light(), ncol = 1, nrow = 2)
  
  filenames <- str_replace(dates[i]," ","_")
  filenames <- str_replace(dates[i]," .*","")

  
  ggsave(paste0("../Plots/Plots_depthprofile_boxplot_metagenomes_",filenames,".pdf"), height = 8.5, width = 11, units = "in")
  print(i)
}



# # 4. Adding other output variables ----
# ## Add density to the output
# # input_yaml_multiple("LakeEnsemblR.yaml", value = c("temp", "ice_height",
# #                                                    "dens"),
# #                     key1 = "output", key2 = "variables")
# 
# input_yaml_multiple("LakeEnsemblR.yaml", value = c("temp", "ice_height"),
#                     key1 = "output", key2 = "variables")
# 
# ## Re-run the ensemble
# run_ensemble("LakeEnsemblR.yaml",
#              model = c("FLake", "GLM", "GOTM", "Simstrat", "MyLake"),
#              parallel = TRUE,
#              add = FALSE)

## Plot the result
# plot_heatmap("output/ensemble_output.nc", var = "dens") +
# #   theme_light() + scale_colour_gradientn(limits = c(998, 1001),
# #                                          colours = rev(brewer.pal(11, "Spectral")))
# 
# plot_ensemble("output/ensemble_output.nc", model = c("FLake", "GLM",
#                                                             "GOTM", "Simstrat",
#                                                             "MyLake"),
#                      var = "dens", date = "2020-07-24 00:00:00") +
#   theme_light()
# 
# ggarrange(p3, p4, ncol = 1, nrow = 2)


# 5. Plotting text outputs ----
# plot_model <- "MyLake" # Model names are case-sensitive
# plot_depth <- 5 # In our example, output is given every 0.5 m 
# # Read in the data
# df <- read.csv(paste0("output/Mendota_", plot_model, "_temp.csv"))
# df$datetime <- as.POSIXct(df$datetime)
# # Plot
# ggplot(df)+
#   geom_line(aes_string(x = "datetime", y = paste0("wtr_", plot_depth)))+
#   theme_light()


# 6. Calibrating the models ----
cali_result <- cali_ensemble("LakeEnsemblR.yaml",
                             model = c("FLake", "GLM", "GOTM", "Simstrat", "MyLake"),
                             num = 100,
                             cmethod = "MCMC",
                             parallel = TRUE)

# cali_result <- cali_ensemble("LakeEnsemblR.yaml",
#                              model = c("FLake", "GLM", "GOTM", "Simstrat", "MyLake"),
#                              num = 10,
#                              cmethod = "LHC",
#                              parallel = TRUE)

## Get best parameter sets
#cali_result[["MyLake"]][["bestpar"]]
cali_result[["GLM"]][["bestpar"]]
#cali_result[["Simstrat"]][["bestpar"]]
#cali_result[["FLake"]][["bestpar"]]
#cali_result[["GOTM"]][["bestpar"]]

saveRDS(cali_result, "cali_result.RDS")

## Manually change the values in the LakeEnsemblR.yaml file and re run the ensemble
export_config("LakeEnsemblR.yaml", model = c("FLake", "GLM", "GOTM",
                                             "Simstrat", "MyLake"))
run_ensemble("LakeEnsemblR.yaml", model = c("FLake", "GLM", "GOTM",
                                            "Simstrat", "MyLake"))

## Plot after the calibration:
plot_heatmap("output/ensemble_output.nc")+
  scale_colour_gradientn(limits = c(0, 30),
                         colours = rev(RColorBrewer::brewer.pal(11, "Spectral")))+
  theme_light()

ggsave("LER.after.calibration.PDF", width = 11, height = 8.5, units="in")

# Take a look at the model fits to the observed data
calc_fit(ncdf = "output/ensemble_output.nc",
         model = c("FLake", "GLM",  "GOTM", "Simstrat", "MyLake"),
         var = "temp")

# $FLake
# rmse       nse        r      bias      mae      nmae
# 1 3.719132 0.7424571 0.652774 -1.562465 2.454829 0.1673611
# 
# $GLM
# rmse       nse       r     bias      mae     nmae
# 1 3.622047 0.6254489 0.56944 2.484866 2.608849 0.202589
# 
# $GOTM
# rmse       nse         r     bias      mae      nmae
# 1 3.199232 0.7077904 0.6324468 -1.99195 2.881185 0.2429285
# 
# $Simstrat
# rmse       nse         r     bias      mae       nmae
# 1 1.404675 0.9436681 0.6519569 1.083073 1.190433 0.09160983
# 
# $MyLake
# rmse       nse         r      bias      mae      nmae
# 1 1.954097 0.8909827 0.6857507 -0.784443 1.426464 0.1153365
# 
# $ensemble_mean
# rmse       nse        r       bias      mae      nmae
# 1 1.503745 0.9354418 0.641308 0.02293459 1.084635 0.0879969

# Take a look at the model performance against residuals, time and depth
plist <- plot_resid(ncdf = "output/ensemble_output.nc",var = "temp",
                    model = c('FLake', 'GLM',  'GOTM', 'Simstrat', 'MyLake'))

ggsave(plot=plist$obs_res, "../Plots/obs_res.png", width = 11, height = 8.5)
ggsave(plot=plist$res_depth, "../Plots/res_depth.png", width = 11, height = 8.5)
ggsave(plot=plist$yday_res, "../Plots/yday_res.png", width = 11, height = 8.5)
ggsave(plot=plist$res_dist, "../Plots/res_dist.png", width = 11, height = 8.5)

# Load post-processed output data into your workspace
analyse_df <- analyse_ncdf(ncdf = "output/ensemble_output.nc", spin_up = NULL, drho = 0.1,
                           model =  c('FLake', 'GLM',  'GOTM', 'Simstrat', 'MyLake'),)

# Example plot the summer stratification period
strat_df <- analyse_df$strat

p <- ggplot(strat_df, aes(model, TotStratDur)) +
  geom_col() +
  ylab("Total stratification duration [days]") +
  xlab("") +
  theme_classic()

p 

ggsave("output/model_ensemble_stratification.png", p,  dpi = 300, width = 284, height = 284, units = "mm")

# 7. Add ensemble members ----
# Change the light atenuation coefficient
input_yaml_multiple("LakeEnsemblR.yaml", value = 2.0,
                    key1 = "input", key2 = "light", key3 = "Kw")

# Now run export_config and run_ensemble again, but add "add = TRUE" 
export_config("LakeEnsemblR.yaml", model = c("FLake", "GLM", "GOTM",
                                             "Simstrat", "MyLake"))
run_ensemble("LakeEnsemblR.yaml",
             model = c("FLake", "GLM", "GOTM", "Simstrat", "MyLake"),
             parallel = TRUE,
             add = TRUE) # default is false, will create a new NCDF file, if add=TRUE, will make a new member in the existing NCDF file

# Plot heatmap
plot_heatmap("output/ensemble_output.nc", dim = "member", dim_index = 2) # We see the two different runs

# 8. Further Post-processing
# Analyse stratification and ice dynamics
out_res <- analyse_ncdf(ncdf = "output/ensemble_output.nc",
                        model = c("FLake", "GLM", "GOTM","Simstrat", "MyLake"))
# look at returned values
names(out_res)

print(out_res[["stats"]])
print(out_res[["strat"]])

## Calculate model fits
calc_fit(ncdf = "output/ensemble_output.nc", model = c("FLake", "GLM", "GOTM",
                                                       "Simstrat", "MyLake"))
## Clot residuals
plot_resid(ncdf = "output/ensemble_output.nc", var = "temp")

