#Load packages
library(dplyr)
library(gbm)

#Source function
source("BRT_Functions.R")

#Get data
LAC <- read.csv("LACdataset180507_2015_2016.csv", header = T)

#Setup variables
variables_all <- c("Ordinal_date", #Always include date
               "elevation_m", #Landscape/watershed variables
               "max_depth_m", 
               "shrub2011", 
               "snow2011", 
               "water1992to2011",
               "snow1992to2011", 
               "eastMEAN", 
               "radSUM", 
               "lake_perc_area", 
               "lake_surface_area_m2",
               "rglac_km2", 
               "BIOTITE_GNEISS_PCT", 
               "LON_DD", 
               "ppt_week", #Climate/meteorological variables
               "tmean_month", 
               "ppt_percent_normal",
               "tmean_day", 
               "tmean_percent_normal", 
               "ppt_day", 
               "Max_SWE", 
               "Difference_SnowFree",
               "DOC", #Environmental Variables
               "FISH",
               "NO3_N",
               "NtoP",
               "TDN",
               "TDP_P",
               "TEMP")

variables_Cl_LU <- c("Ordinal_date", #Always include date
               "elevation_m", #Landscape/watershed variables
               "site_type_num",
               "max_depth_m", 
               "shrub2011", 
               "snow2011", 
               "water1992to2011",
               "snow1992to2011", 
               "eastMEAN", 
               "radSUM", 
               "lake_perc_area", 
               "lake_surface_area_m2",
               "rglac_km2", 
               "BIOTITE_GNEISS_PCT", 
               "LON_DD", 
               "ppt_week", #Climate/meteorological variables
               "tmean_month", 
               "ppt_percent_normal",
               "tmean_day", 
               "tmean_percent_normal", 
               "ppt_day", 
               "Max_SWE", 
               "Difference_SnowFree")

variables_Cl <- c("Ordinal_date", #Always include date
               "site_type_num",
               "ppt_week", #Climate/meteorological variables
               "tmean_month", 
               "ppt_percent_normal",
               "tmean_day", 
               "tmean_percent_normal", 
               "ppt_day", 
               "Max_SWE", 
               "Difference_SnowFree")

#Model with all variables
LAC2 <- LAC %>%
  select(one_of(c(variables_all, "chl_a")))

out_path <- "All_Lakes/All_no-ndep"
iterative_BRT(data = LAC2, variables = variables_all, out_path = out_path, start_cut = 1)

#Model with climate and land use variables
LAC2 <- LAC %>%
  select(one_of(c(variables_Cl_LU, "chl_a")))

out_path <- "All_Lakes/Climate+LU_no-ndep"
iterative_BRT(data = LAC2, variables = variables_Cl_LU, out_path = out_path, start_cut = 1)

#Model with climate  variables
LAC2 <- LAC %>%
  select(one_of(c(variables_Cl, "chl_a")))

out_path <- "All_Lakes/Climate_no-ndep"
iterative_BRT(data = LAC2, variables = variables_Cl, out_path = out_path, start_cut = 1)


######
#GL
#Get data
GL <- read.csv("LACdataset180507_GL.csv", header = T)

#Set output path
out_path <- "Green Lakes/No year"

#Setup variables
variables <- c("Ordinal_date", #Always include date
               "ppt_week", #Climate/meteorological variables
               "tmean_month", 
               "ppt_percent_normal",
               "tmean_day", 
               "tmean_percent_normal", 
               "ppt_day", 
               "Max_SWE", 
               "Difference_SnowFree",
               "DOC", #Environmental Variables
               "NO3_N",
               "NtoP",
               "TDN",
               "TDP_P",
               "TEMP",
               #"year",
               "site_type_num",
               "SWE_pernorm")

GL2 <- GL %>%
  select(one_of(c(variables, "chl_a")))

iterative_BRT(data = GL2, variables = variables, out_path = out_path, start_cut = 3)

##############
#LS
#Get data
LS <- read.csv("LACdataset180517_LS.csv", header = T)

#Set output path
out_path <- "Loch_Sky/No Ndep"

#Setup variables
var <- c("Ordinal_date", #Always include date
               "ppt_week", #Climate/meteorological variables
               "ppt_month",
               "tmean_week",
               "ppt_percent_normal",
               "tmean_percent_normal", 
               "ppt_day", 
               "NO3_N", #Environmental Variables
               "NtoTP",
               "NtoP",
               "TDN",
               "TEMP",
               "site_type_num")

# var <- c("Ordinal_date", #Always include date
#                "ppt_week", #Climate/meteorological variables
#                "ppt_month",
#                "tmean_week",
#                "ppt_percent_normal",
#                "tmean_percent_normal", 
#                "NO3_N", #Environmental Variables
#                "NtoTP",
#                "NtoP",
#                "TEMP")

LS2 <- LS %>%
  select(one_of(c(var, "chl_a")))

iterative_BRT(data = LS2, var = var, out_path = out_path, start_cut = 1)



