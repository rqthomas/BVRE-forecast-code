#' FLARE-LER-MS Analysis

library(magrittr)
library(ggplot2)

lake_directory <- here::here()

da_freq <- c("daily", "daily_2", "daily_5", "fortnightly", "monthly", "weekly") # Add names for your DA_freq
config_set_name <- "DA_experiments"
configure_run_file <- "configure_run.yml"

source(file.path(lake_directory, "R", "read_flare_temp.R"))
source(file.path(lake_directory, "R", "combine_forecast_observation.R"))

config <- FLAREr::set_configuration(configure_run_file, lake_directory,
                                    config_set_name = config_set_name)

# Read in FLARE observations ----
target_file <- file.path(config$file_path$qaqc_data_directory,
                         "bvre-targets-insitu.csv")
obs <- read.csv(target_file)
wtemp <- obs[obs$variable == "temperature", ] # Subset to temperature

sub <- wtemp[wtemp$date > "2021-01-01" & wtemp$date < "2021-12-31", ] # Subset to target dates - need to change for your experiment
sub$Date <- as.Date(sub$date) # Line
sub$dens <- rLakeAnalyzer::water.density(sub$value) # density for stratification

# Classifies phenology
phen <- plyr::ddply(sub, "Date", function(x) {
  surf <- x$dens[x$depth == min(x$depth)]
  bott <- x$dens[x$depth == max(x$depth)]
  if(nrow(x) == 1) return(data.frame(status = NA))
  status <- ifelse(abs(surf - bott) < 0.1, "Mixed", "Stratified")
  data.frame(status = status)
})

# Line plot with depths + status
ggplot(sub) +
  geom_raster(data = phen, aes(Date, y = 3,fill = status)) +
  geom_line(aes(Date, as.numeric(value), color = factor(depth)))


for(m in seq_len(length(da_freq))) {

  fc_dir <- file.path(config$file_path$forecast_output_directory, config_set_name, da_freq[m]) # Needs to be changed
  fc_files <- list.files(fc_dir, pattern = "*.nc", full.names = TRUE)

  for(f in fc_files) {

    tmp <- read_flare_temp(f)


    # Forecasts checks - some LER forecasts are bad files...
    if(tmp[[1]] == "No forecast") {
      message("No forecast in ", basename(f))
      next
    }
    if(length(dim(tmp[[1]]$temp)) < 3) {
      message("Not enough dimensions in ", basename(f))
      next
    }

    res <- combine_forecast_observation(temp = tmp, obs = sub) # Combine forecasts & calculate turnover pct

    dir.create(file.path(config$file_path$analysis_directory, da_freq[m]), showWarnings = FALSE) # Create output dir for csv files
    write.csv(res, file.path(config$file_path$analysis_directory, "summary_files", da_freq[m], paste0("forecast_summary_", res$forecast_date[1], ".csv")),
              row.names = FALSE, quote = FALSE)
    message("[", Sys.time(), "] Finished ", basename(f))
  }
}

# end

