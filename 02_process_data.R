lake_directory <- getwd()
config_obs <- yaml::read_yaml(file.path(lake_directory,"configuration/observation_processing","observation_processing.yml"))

config_obs$data_location <- file.path(lake_directory,"data_raw")
config_obs$qaqc_data_directory <- file.path(lake_directory,"data_processed")
config_obs$configuration_directory <- file.path(lake_directory,"configuration")

config <- yaml::read_yaml(file.path(config_obs$configuration_directory, "FLAREr","configure_flare.yml"))
run_config <- yaml::read_yaml(file.path(config_obs$configuration_directory, "FLAREr","configure_run.yml"))

#install.packages("remotes")
#remotes::install_github("rqthomas/noaaGEFSpoint")
pacman::p_load(tidyverse, lubridate, noaaGEFSpoint, magrittr)

#source data processing files
source(file.path(lake_directory, "R/extract_CTD.R"))
source(file.path(lake_directory, "R/extract_nutrients.R"))
source(file.path(lake_directory, "R/temp_oxy_chla_qaqc.R"))
source(file.path(lake_directory, "R/extract_ch4.R"))
source(file.path(lake_directory, "R/extract_secchi.R"))
source(file.path(lake_directory, "R/in_situ_qaqc.R"))
source(file.path(lake_directory, "R/met_qaqc.R")) 
source(file.path(lake_directory, "R/inflow_qaqc.R")) 

if(is.null(config_obs$met_file)){
  met_qaqc(realtime_file = file.path(config_obs$data_location, config_obs$met_raw_obs_fname[1]),
           qaqc_file = file.path(config_obs$data_location, config_obs$met_raw_obs_fname[2]), #realtime QAQC not available so NA for 2021
           cleaned_met_file_dir = config_obs$qaqc_data_directory,
           input_file_tz = "EST",
           local_tzone = config_obs$local_tzone,
           nldas = file.path(config_obs$data_location, config_obs$nldas)) #nldas only goes up to end of 2019
}else{
  file.copy(file.path(config_obs$data_location,config_obs$met_file), cleaned_met_file, overwrite = TRUE)
}

cleaned_inflow_file <- paste0(config_obs$qaqc_data_directory, "/inflow_postQAQC.csv")

if(is.null(config_obs$inflow1_file)){ #only relevant if we have observations
  inflow_qaqc(realtime_file = file.path(config_obs$data_location, config_obs$inflow_raw_file1),
              #qaqc_file = file.path(config_obs$data_location, config$inflow_raw_file1[2]),
              nutrients_file = file.path(config_obs$data_location, config_obs$nutrients_fname),
              cleaned_inflow_file ,
              config_obs$local_tzone,
              input_file_tz = 'EST')
}else{
  file.copy(file.path(config_obs$data_location,config_obs$inflow1_file), cleaned_inflow_file, overwrite = TRUE)
}

cleaned_observations_file_long <- paste0(config_obs$qaqc_data_directory,
                                         "/observations_postQAQC_long.csv")
if(is.null(config_obs$combined_obs_file)){
  in_situ_qaqc(insitu_obs_fname = file.path(config_obs$data_location,config_obs$insitu_obs_fname),
               data_location = config_obs$data_location,
               maintenance_file = file.path(config_obs$data_location,config_obs$maintenance_file),
               ctd_fname = file.path(config_obs$data_location,config_obs$ctd_fname),
               nutrients_fname =  file.path(config_obs$data_location, config_obs$nutrients_fname),
               secchi_fname = file.path(config_obs$data_location, config_obs$secchi_fname),
               cleaned_observations_file_long = cleaned_observations_file_long,
               lake_name_code = config_obs$lake_name_code,
               config = config_obs)
}else{
  file.copy(file.path(config$file_path$data_location,config_obs$combined_obs_file), cleaned_observations_file_long, overwrite = TRUE)
}

# get NOAA met forecasts and stack first day to use as met 'obs'
source(file.path(lake_directory, "R", "stack_noaa_forecasts_cycle_loop.R"))
dates <- seq.Date(as.Date('2021-02-15'), as.Date(run_config$forecast_start_datetime), by = 'day') # cycle through historical dates 
cycle <- '00'
outfile <- config$file_path$qaqc_data_directory
stack_noaa_forecasts(dates = dates, 
                     #cycle = cycle, 
                     outfile = outfile, 
                     config = config,
                     model_name = "observed-met-noaa",
                     noaa_directory = file.path(lake_directory,"forecasted_drivers/NOAAGEFS_1hr"),
                     hist_file = "observed-met_fcre.nc")

#plot temp and DO during forecast period
# obs_2021 <- read_csv(cleaned_observations_file_long) %>% filter(date>="2021-03-15" & date<= "2021-03-31")
# obs_2021$date <- as.Date(obs_2021$date)
# 
# ggplot(subset(obs_2019, variable=="temperature"),aes(timestamp,value, color=as.factor(depth))) +geom_line() #extract_CTD.R
# ggplot(subset(obs_2021, variable=="temperature"),aes(date,value, color=as.factor(depth))) +geom_line()
       
       
       