pacman::p_load(ggplot2, hydroGOF, scoringRules, dplyr, magrittr)

lake_directory <- here::here()

#-------------------------------------------------------------------------------#
#Read in observations and simulation results from output directory
forecast_csv <- paste0(tools::file_path_sans_ext(saved_file), ".csv")

#average across all days for each depth
forecast_depth <- read_csv(forecast_csv) %>% 
              filter(date >= forecast_start_day)  %>% 
              filter(!is.na(observed))

#average across all depths for days
forecast_daily_avg <- read_csv(forecast_csv) %>% 
  filter(date >= forecast_start_day)  %>% 
  filter(!is.na(observed)) %>%
  select(forecast_mean, date, observed) %>% group_by(date) %>% 
  summarise(avg_wc_temp = mean(forecast_mean), avg_wc_obs = mean(observed)) %>%
  mutate(DA=names(date_list[da_freq])) 

#create df with forecast evaluation metrics for each depth
forecast_depth_skill <- data.frame(Depth=unique(forecast_depth$depth),RMSE=NA, pbias=NA, bias=NA, CRPS=NA, DA=names(date_list[da_freq]), day=as.Date(config$run_config$forecast_start_datetime))
for(i in 1:length(unique(forecast_depth$depth))){
  forecast_depth_skill[i,2] <- hydroGOF::rmse(forecast_depth$forecast_mean[forecast_depth$depth[i]==unique(forecast_depth$depth)],forecast_depth$observed[forecast_depth$depth[i]==unique(forecast_depth$depth)])
  forecast_depth_skill[i,3]<- hydroGOF::pbias(forecast_depth$forecast_mean[forecast_depth$depth[i]==unique(forecast_depth$depth)],forecast_depth$observed[forecast_depth$depth[i]==unique(forecast_depth$depth)])
  forecast_depth_skill[i,4] <- mean(forecast_depth$forecast_mean[forecast_depth$depth[i]==unique(forecast_depth$depth)]) - mean(forecast_depth$observed[forecast_depth$depth[i]==unique(forecast_depth$depth)])
  forecast_depth_skill[i,5] <- mean(scoringRules::crps_sample(forecast_depth$observed[forecast_depth$depth[i]==unique(forecast_depth$depth)],cbind(forecast_depth$forecast_mean[forecast_depth$depth[i]==unique(forecast_depth$depth)],
                                                     forecast_depth$forecast_sd[forecast_depth$depth[i]==unique(forecast_depth$depth)])))
}

#create df for forecast daily average
forecast_daily_avg_skill <- data.frame(date=unique(forecast_daily_avg$date), RMSE=NA, pbias=NA, bias=NA, CRPS=NA, DA=names(date_list[da_freq]), day=seq(0,35,by=1))
for(i in 1:length(unique(forecast_daily_avg$date))){
  forecast_daily_avg_skill[i,2] <- hydroGOF::rmse(forecast_daily_avg$avg_wc_temp[forecast_daily_avg$date[i]==unique(forecast_daily_avg$date)],
                                                 forecast_daily_avg$avg_wc_obs[forecast_daily_avg$date[i]==unique(forecast_daily_avg$date)])
  forecast_daily_avg_skill[i,3] <- hydroGOF::pbias(forecast_daily_avg$avg_wc_temp[forecast_daily_avg$date[i]==unique(forecast_daily_avg$date)],
                                                   forecast_daily_avg$avg_wc_obs[forecast_daily_avg$date[i]==unique(forecast_daily_avg$date)])
  forecast_daily_avg_skill[i,4] <- mean(forecast_daily_avg$avg_wc_temp[forecast_daily_avg$date[i]==unique(forecast_daily_avg$date)]) - 
                                   mean(forecast_daily_avg$avg_wc_obs[forecast_daily_avg$date[i]==unique(forecast_daily_avg$date)])
  forecast_daily_avg_skill[i,5] <- mean(scoringRules::crps_sample(forecast_daily_avg$avg_wc_obs[forecast_daily_avg$date[i]==unique(forecast_daily_avg$date)],
                                                                  cbind(forecast_daily_avg$avg_wc_temp[forecast_daily_avg$date[i]==unique(forecast_daily_avg$date)],
                                                                        forecast_daily_avg$forecast_sd[forecast_daily_avg$date[i]==unique(forecast_daily_avg$date)])))
  }

#save forecast_eval df
readr::write_csv(forecast_depth_skill, file.path(paste0(lake_directory,"/forecasts/bvre/DA_experiments/",names(date_list)[da_freq],"/35d_depth_forecast_skill_",as.Date(config$run_config$forecast_start_datetime),".csv")))

#save daily rmse as csv
readr::write_csv(forecast_daily_avg_skill, file.path(paste0(lake_directory,"/forecasts/bvre/DA_experiments/",names(date_list)[da_freq],"/35d_daily_forecast_skill_",as.Date(config$run_config$forecast_start_datetime),".csv")))
