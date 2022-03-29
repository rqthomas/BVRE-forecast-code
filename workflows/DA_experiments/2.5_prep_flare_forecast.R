library(magrittr)
library(lubridate)

if(file.exists("~/.aws")){
  warning(paste("Detected existing AWS credentials file in ~/.aws,",
                "Consider renaming these so that automated upload will work"))
}

Sys.setenv("AWS_DEFAULT_REGION" = "s3",
           "AWS_S3_ENDPOINT" = "flare-forecast.org")

#code to delete restart config file - useful when running multiple different forecast horizons instead of consecutive forecast days
#unlink(file.path(getwd(),"restart/bvre/DA_experiments/configure_run.yml"))

lake_directory <- here::here()
update_run_config <- TRUE
config_set_name <- "DA_experiments"

configure_run_file <- "configure_run.yml"

#config <- FLAREr::set_configuration(configure_run_file,lake_directory, config_set_name = config_set_name)

#config <- FLAREr::get_restart_file(config, lake_directory)


FLAREr::get_targets(lake_directory, config)

noaa_forecast_path <- FLAREr::get_driver_forecast_path(config,
                                               forecast_model = config$met$forecast_met_model)

inflow_forecast_path <- FLAREr::get_driver_forecast_path(config,
                                                 forecast_model = config$inflow$forecast_inflow_model)
#inflow_forecast_path <- NULL

pars_config <- readr::read_csv(file.path(config$file_path$configuration_directory, config$model_settings$par_config_file), col_types = readr::cols())
obs_config <- readr::read_csv(file.path(config$file_path$configuration_directory, config$model_settings$obs_config_file), col_types = readr::cols())
states_config <- readr::read_csv(file.path(config$file_path$configuration_directory, config$model_settings$states_config_file), col_types = readr::cols())

if(!is.null(noaa_forecast_path)){
  FLAREr::get_driver_forecast(lake_directory, forecast_path = noaa_forecast_path)
  forecast_dir <- file.path(config$file_path$noaa_directory, noaa_forecast_path)
}else{
  forecast_dir <- NULL
}
if(!is.null(forecast_dir)) {
list.files(forecast_dir)
}
