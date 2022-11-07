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



