library(magrittr)
library(lubridate)

message("Beginning generate targets")

#' Set the lake directory to the repository directory

lake_directory <- here::here()
config_set_name <- "DA_experiments"

Sys.setenv("AWS_DEFAULT_REGION" = "s3",
           "AWS_S3_ENDPOINT" = "flare-forecast.org")

#' Source the R files in the repository

source(file.path(lake_directory, "R", "met_qaqc.R"))
source(file.path(lake_directory, "R", "in_situ_qaqc.R"))
source(file.path(lake_directory, "R", "temp_oxy_chla_qaqc.R"))
source(file.path(lake_directory, "R", "extract_CTD.R"))
source(file.path(lake_directory, "R", "extract_secchi.R"))
source(file.path(lake_directory, "R", "extract_nutrients.R"))
source(file.path(lake_directory, "R", "extract_ch4.R"))
source(file.path(lake_directory, "R", "inflow_qaqc.R"))
source(file.path(lake_directory, "R", "TMWB_inflow_model.R"))

#' Generate the `config_obs` object and create directories if necessary

config_obs <- FLAREr::initialize_obs_processing(lake_directory, observation_yml = "observation_processing.yml", config_set_name = config_set_name)
dir.create(file.path(lake_directory, "targets", config_obs$site_id), showWarnings = FALSE)
use_s3 <- TRUE

#' Clone or pull from data repositories

FLAREr::get_git_repo(lake_directory,
             directory = config_obs$realtime_insitu_location,
             git_repo = "https://github.com/FLARE-forecast/BVRE-data.git")

FLAREr::get_git_repo(lake_directory,
             directory = config_obs$realtime_met_station_location,
             git_repo = "https://github.com/FLARE-forecast/FCRE-data.git")


#' Download various files from the BVR-GLM repo

#download.file("https://github.com/CareyLabVT/BVR-GLM/blob/master/field_data/field_gases.csv?raw=true",
#              "data_raw/field_gasses.csv") 

#download NLDAS data (note: will need to grab new one once appended with 2020/2021 data) - only need this for filling in missing met days in bvr inflow file
#download.file("https://github.com/CareyLabVT/BVR-GLM/blob/master/inputs/BVR_GLM_NLDAS_010113_123119_GMTadjusted.csv?raw=true",
#              "data_raw/BVR_GLM_NLDAS_010113_123119_GMTadjusted.csv")

#download.file("https://github.com/CareyLabVT/BVR-GLM/blob/master/inputs/inflow_for_EDI_2013_06Mar2020.csv?raw=true",
#              "data_raw/inflow_for_EDI_2013_06Mar2020.csv")

#download.file("https://github.com/CareyLabVT/BVR-GLM/blob/master/inputs/FCR2014_Chemistry.csv?raw=true",
#              "data_raw/FCR2014_Chemistry.csv")

#download.file("https://github.com/CareyLabVT/BVR-GLM/blob/master/inputs/BVR_GHG_Inflow_20200619.csv?raw=true",
#              "data_raw/BVR_GHG_Inflow_20200619.csv")


#' Download files from EDI

FLAREr::get_edi_file(edi_https = "https://pasta.lternet.edu/package/eml/edi/389/6/35d8d3f9390408f12d39e44e3f03abbe",
             file = config_obs$met_raw_obs_fname[2],
             lake_directory)

FLAREr::get_edi_file(edi_https = "https://pasta.lternet.edu/package/data/eml/edi/725/2/026a6e2cca8bdf18720d6a10d8860e3d",
             file = config_obs$insitu_obs_fname[2],
             lake_directory)

FLAREr::get_edi_file(edi_https = "https://pasta.lternet.edu/package/data/eml/edi/725/2/026a6e2cca8bdf18720d6a10d8860e3d",
                     file = config_obs$insitu_obs_fname[3],
                     lake_directory)

FLAREr::get_edi_file(edi_https = "https://pasta.lternet.edu/package/data/eml/edi/198/10/375f87747001e1681b0e805d00cc1341",
             file = config_obs$secchi_fname,
             lake_directory)

FLAREr::get_edi_file(edi_https = "https://pasta.lternet.edu/package/data/eml/edi/200/12/0a62d1946e8d9a511bc1404e69e59b8c",
             file = config_obs$ctd_fname,
             lake_directory)

FLAREr::get_edi_file(edi_https = "https://pasta.lternet.edu/package/data/eml/edi/199/8/da174082a3d924e989d3151924f9ef98",
             file = config_obs$nutrients_fname,
             lake_directory)

#' INFLOW - functions to create and qaqc the inflow data using the TMWB model

#create_inflow_file(realtime_file = file.path(config_obs$file_path$data_directory, config_obs$met_raw_obs_fname[1]),
#                      qaqc_file = file.path(config_obs$file_path$data_directory, config_obs$met_raw_obs_fname[2]),
#                      nldas_file = file.path(config_obs$file_path$data_directory, config_obs$nldas))
#
#inflow_qaqc(inflow_file = file.path(lake_directory,"data_processed/BVR_flow_calcs_obs_met_2015_2021.csv"),
#            qaqc_file = file.path(config_obs$file_path$data_directory, "inflow_for_EDI_2013_06Mar2020.csv"),
#            realtime_file = file.path(config_obs$file_path$data_directory, "2021_YSI_PAR_profiles.csv"), #will need to change to pressure transducer sensor when available (currently ysi temp on local computer)
#            nutrients_file = file.path(config_obs$file_path$data_directory, config_obs$nutrients_fname),
#            silica_file = file.path(config_obs$file_path$data_directory, "FCR2014_Chemistry.csv"),
#            ghg_file = file.path(config_obs$file_path$data_directory, "BVR_GHG_Inflow_20200619.csv"),
#            cleaned_inflow_file = file.path(config_obs$file_path$targets_directory, "bvre/bvre-targets-inflow.csv"))




#' OUTFLOW
download.file("https://github.com/CareyLabVT/BVR-GLM/blob/master/inputs/BVR_spillway_outflow_2014_2019_20200917_nldasInflow.csv?raw=true",
              "data_raw/BVR_spillway_outflow_2014_2019_20200917_nldasInflow.csv")



#' Clean up observed meteorology
#cleaned_met_file <- met_qaqc(realtime_file = file.path(lake_directory, "data_raw", config_obs$met_raw_obs_fname[1]),
#                             qaqc_file = file.path(lake_directory, "data_raw", config_obs$met_raw_obs_fname[2]),
#                             cleaned_met_file = file.path(lake_directory, "targets", config_obs$site_id, paste0("observed-met_",config_obs$site_id,".nc")),
#                             input_file_tz = "EST",
#                             nldas = NULL)

#' Clean up observed insitu measurements
cleaned_insitu_file <- in_situ_qaqc(insitu_obs_fname = file.path(lake_directory,"data_raw", config_obs$insitu_obs_fname),
                                    data_location = file.path(lake_directory,"data_raw"),
                                    maintenance_file = file.path(lake_directory, "data_raw", config_obs$maintenance_file),
                                    ctd_fname = file.path(lake_directory, "data_raw", config_obs$ctd_fname),
                                    nutrients_fname =  file.path(lake_directory, "data_raw", config_obs$nutrients_fname),
                                    secchi_fname = file.path(lake_directory, "data_raw", config_obs$secchi_fname),
                                    cleaned_insitu_file = file.path(lake_directory,"targets", config_obs$site_id, paste0(config_obs$site_id,"-targets-insitu.csv")),
                                    site_id = config_obs$site_id,
                                    config_obs = config_obs)

#' Move targets to s3 bucket

message("Successfully generated targets")

#FLAREr::put_targets(site_id = config_obs$site_id,
#            cleaned_insitu_file,
#            cleaned_met_file,
#            use_s3 = TRUE)

FLAREr::put_targets(site_id = config_obs$site_id,
                    cleaned_insitu_file,
                    use_s3 = FALSE)

message("Successfully moved targets to s3 bucket")

