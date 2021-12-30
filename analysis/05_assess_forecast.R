pacman::p_load(ggplot2, hydroGOF, scoringRules, dplyr, tidyverse)

lake_directory <- here::here()
configure_run_file <- "configure_run.yml"
config <- FLAREr::set_configuration(configure_run_file,lake_directory)

#######################################################
#Read in observations and simulation results for SUMMER
output_daily <- read_csv(file.path(config$file_path$forecast_output_directory, "DA_experiments/summer/daily/bvre-2021-07-01-bvre_test.csv")) %>% 
  filter(date >= as.Date("2021-07-01")) %>% mutate(day = as.Date(date) - as.Date("2021-07-01")) %>% 
  filter(depth %in% c(0.1,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0))

output_2days <- read_csv(file.path(config$file_path$forecast_output_directory, "DA_experiments/summer/2days/bvre-2021-07-01-bvre_test.csv")) %>% 
  filter(date >= as.Date("2021-07-01")) %>% mutate(day = as.Date(date) - as.Date("2021-07-01")) %>% 
  filter(depth %in% c(0.1,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0))

output_5days <- read_csv(file.path(config$file_path$forecast_output_directory, "DA_experiments/summer/5days/bvre-2021-07-01-bvre_test.csv")) %>% 
  filter(date >= as.Date("2021-07-01")) %>% mutate(day = as.Date(date) - as.Date("2021-07-01")) %>% 
  filter(depth %in% c(0.1,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0))

#fill in the one-day ahead observations (2021-07-02); not used in DA but need to calculate RMSE, then remove NAs
output_5days$observed[output_5days$date==as.Date("2021-07-02")] <- c(output_daily$observed[output_daily$date==as.Date("2021-07-02")],NA)
output_5days <- output_5days[!is.na(output_5days$observed),]


output_weekly <- read_csv(file.path(config$file_path$forecast_output_directory, "DA_experiments/summer/weekly/bvre-2021-07-01-bvre_test.csv")) %>% 
  filter(date >= as.Date("2021-07-01")) %>% mutate(day = as.Date(date) - as.Date("2021-07-01")) %>% 
  filter(depth %in% c(0.1,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0))

#fill in the one-day ahead observations (2021-07-02); not used in DA but need to calculate RMSE, then remove NAs
output_weekly$observed[output_weekly$date==as.Date("2021-07-02")] <- c(output_daily$observed[output_daily$date==as.Date("2021-07-02")],NA)
output_weekly <- output_weekly[!is.na(output_weekly$observed),]


output_fortnightly <- read_csv(file.path(config$file_path$forecast_output_directory, "DA_experiments/summer/fortnightly/bvre-2021-07-01-bvre_test.csv")) %>% 
  filter(date >= as.Date("2021-07-01")) %>% mutate(day = as.Date(date) - as.Date("2021-07-01")) %>% 
  filter(depth %in% c(0.1,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0))

#fill in the one-day ahead observations (2021-07-02); not used in DA but need to calculate RMSE, then remove NAs
output_fortnightly$observed[output_fortnightly$date==as.Date("2021-07-02")] <- c(output_daily$observed[output_daily$date==as.Date("2021-07-02")],NA)
output_fortnightly <- output_fortnightly[!is.na(output_fortnightly$observed),]


output_monthly <- read_csv(file.path(config$file_path$forecast_output_directory, "DA_experiments/summer/monthly/bvre-2021-07-01-bvre_test.csv")) %>% 
  filter(date >= as.Date("2021-07-01")) %>% mutate(day = as.Date(date) - as.Date("2021-07-01")) %>% 
  filter(depth %in% c(0.1,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0))

#fill in the one-day ahead observations (2021-07-02); not used in DA but need to calculate RMSE, then remove NAs
output_monthly$observed[output_monthly$date==as.Date("2021-07-02")] <- c(output_daily$observed[output_daily$date==as.Date("2021-07-02")],NA)
output_monthly <- output_monthly[!is.na(output_monthly$observed),]

#######################################################
#Read in observations and simulation results for WINTER

output_daily_winter2 <- read_csv(file.path(config$file_path$forecast_output_directory, "DA_experiments/winter/daily/bvre-2021-01-01-bvre_test.csv")) %>% 
  filter(date >= as.Date("2021-01-01")) %>% mutate(day = as.Date(date) - as.Date("2021-01-01")) %>% 
  filter(depth %in% c(0.1,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0))

output_2days_winter2 <- read_csv(file.path(config$file_path$forecast_output_directory, "DA_experiments/winter/2days/bvre-2021-01-01-bvre_test.csv")) %>% 
  filter(date >= as.Date("2021-01-01")) %>% mutate(day = as.Date(date) - as.Date("2021-01-01")) %>% 
  filter(depth %in% c(0.1,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0))

#fill in the one-day ahead observations (2021-01-02); not used in DA but need to calculate RMSE, then remove NAs
output_2days_winter2$observed[output_2days_winter2$date==as.Date("2021-01-02")] <- c(output_daily_winter2$observed[output_daily_winter2$date==as.Date("2021-01-02")],NA)
output_2days_winter2 <- output_2days_winter2[!is.na(output_2days_winter2$observed),]

output_5days_winter2 <- read_csv(file.path(config$file_path$forecast_output_directory, "DA_experiments/winter/5days/bvre-2021-01-01-bvre_test.csv")) %>% 
  filter(date >= as.Date("2021-01-01")) %>% mutate(day = as.Date(date) - as.Date("2021-01-01")) %>% 
  filter(depth %in% c(0.1,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0))

#fill in the one-day ahead observations (2021-01-02); not used in DA but need to calculate RMSE, then remove NAs
output_5days_winter2$observed[output_5days_winter2$date==as.Date("2021-01-02")] <- c(output_daily_winter2$observed[output_daily_winter2$date==as.Date("2021-01-02")],NA)
output_5days_winter2 <- output_5days_winter2[!is.na(output_5days_winter2$observed),]


output_weekly_winter2 <- read_csv(file.path(config$file_path$forecast_output_directory, "DA_experiments/winter/weekly/bvre-2021-01-01-bvre_test.csv")) %>% 
  filter(date >= as.Date("2021-01-01")) %>% mutate(day = as.Date(date) - as.Date("2021-01-01")) %>% 
  filter(depth %in% c(0.1,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0))

#fill in the one-day ahead observations (2021-01-02); not used in DA but need to calculate RMSE, then remove NAs
output_weekly_winter2$observed[output_weekly_winter2$date==as.Date("2021-01-02")] <- c(output_daily_winter2$observed[output_daily_winter2$date==as.Date("2021-01-02")],NA)
output_weekly_winter2 <- output_weekly_winter2[!is.na(output_weekly_winter2$observed),]


output_fortnightly_winter2 <- read_csv(file.path(config$file_path$forecast_output_directory, "DA_experiments/winter/fortnightly/bvre-2021-01-01-bvre_test.csv")) %>% 
  filter(date >= as.Date("2021-01-01")) %>% mutate(day = as.Date(date) - as.Date("2021-01-01")) %>% 
  filter(depth %in% c(0.1,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0))

#fill in the one-day ahead observations (2021-01-02); not used in DA but need to calculate RMSE, then remove NAs
output_fortnightly_winter2$observed[output_fortnightly_winter2$date==as.Date("2021-01-02")] <- c(output_daily_winter2$observed[output_daily_winter2$date==as.Date("2021-01-02")],NA)
output_fortnightly_winter2 <- output_fortnightly_winter2[!is.na(output_fortnightly_winter2$observed),]


output_monthly_winter2 <- read_csv(file.path(config$file_path$forecast_output_directory, "DA_experiments/winter/monthly/bvre-2021-01-01-bvre_test.csv")) %>% 
  filter(date >= as.Date("2021-01-01")) %>% mutate(day = as.Date(date) - as.Date("2021-01-01")) %>% 
  filter(depth %in% c(0.1,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0))

#fill in the one-day ahead observations (2021-01-02); not used in DA but need to calculate RMSE, then remove NAs
output_monthly_winter2$observed[output_monthly_winter2$date==as.Date("2021-01-02")] <- c(output_daily_winter2$observed[output_daily_winter2$date==as.Date("2021-01-02")],NA)
output_monthly_winter2 <- output_monthly_winter2[!is.na(output_monthly_winter2$observed),]

#######################################################
#Read in observations and simulation results for SPRING

output_daily_spring2 <- read_csv(file.path(config$file_path$forecast_output_directory, "DA_experiments/spring/daily/bvre-2021-04-01-bvre_test.csv")) %>% 
  filter(date >= as.Date("2021-04-01")) %>% mutate(day = as.Date(date) - as.Date("2021-04-01")) %>% 
  filter(depth %in% c(0.1,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0)) %>% filter(!is.na(observed))

output_2days_spring2 <- read_csv(file.path(config$file_path$forecast_output_directory, "DA_experiments/spring/2days/bvre-2021-04-01-bvre_test.csv")) %>% 
  filter(date >= as.Date("2021-04-01")) %>% mutate(day = as.Date(date) - as.Date("2021-04-01")) %>% 
  filter(depth %in% c(0.1,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0))

#fill in the one-day ahead observations (2021-04-02); not used in DA but need to calculate RMSE, then remove NAs
output_2days_spring2$observed[output_2days_spring2$date==as.Date("2021-04-02")] <- c(output_daily_spring2$observed[output_daily_spring2$date==as.Date("2021-04-02")],NA)
output_2days_spring2 <- output_2days_spring2[!is.na(output_2days_spring2$observed),]


output_5days_spring2 <- read_csv(file.path(config$file_path$forecast_output_directory, "DA_experiments/spring/5days/bvre-2021-04-01-bvre_test.csv")) %>% 
  filter(date >= as.Date("2021-04-01")) %>% mutate(day = as.Date(date) - as.Date("2021-04-01")) %>% 
  filter(depth %in% c(0.1,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0))

#fill in the one-day ahead observations (2021-04-02); not used in DA but need to calculate RMSE, then remove NAs
output_5days_spring2$observed[output_5days_spring2$date==as.Date("2021-04-02")] <- c(output_daily_spring2$observed[output_daily_spring2$date==as.Date("2021-04-02")],NA)
output_5days_spring2 <- output_5days_spring2[!is.na(output_5days_spring2$observed),]


output_weekly_spring2 <- read_csv(file.path(config$file_path$forecast_output_directory, "DA_experiments/spring/weekly/bvre-2021-04-01-bvre_test.csv")) %>% 
  filter(date >= as.Date("2021-04-01")) %>% mutate(day = as.Date(date) - as.Date("2021-04-01")) %>% 
  filter(depth %in% c(0.1,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0))

#fill in the one-day ahead observations (2021-04-02); not used in DA but need to calculate RMSE, then remove NAs
output_weekly_spring2$observed[output_weekly_spring2$date==as.Date("2021-04-02")] <- c(output_daily_spring2$observed[output_daily_spring2$date==as.Date("2021-04-02")],NA)
output_weekly_spring2 <- output_weekly_spring2[!is.na(output_weekly_spring2$observed),]


output_fortnightly_spring2 <- read_csv(file.path(config$file_path$forecast_output_directory, "DA_experiments/spring/fortnightly/bvre-2021-04-01-bvre_test.csv")) %>% 
  filter(date >= as.Date("2021-04-01")) %>% mutate(day = as.Date(date) - as.Date("2021-04-01")) %>% 
  filter(depth %in% c(0.1,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0))

#fill in the one-day ahead observations (2021-04-02); not used in DA but need to calculate RMSE, then remove NAs
output_fortnightly_spring2$observed[output_fortnightly_spring2$date==as.Date("2021-04-02")] <- c(output_daily_spring2$observed[output_daily_spring2$date==as.Date("2021-04-02")],NA)
output_fortnightly_spring2 <- output_fortnightly_spring2[!is.na(output_fortnightly_spring2$observed),]


output_monthly_spring2 <- read_csv(file.path(config$file_path$forecast_output_directory, "DA_experiments/spring/monthly/bvre-2021-04-01-bvre_test.csv")) %>% 
  filter(date >= as.Date("2021-04-01")) %>% mutate(day = as.Date(date) - as.Date("2021-04-01")) %>% 
  filter(depth %in% c(0.1,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0))

#fill in the one-day ahead observations (2021-04-02); not used in DA but need to calculate RMSE, then remove NAs
output_monthly_spring2$observed[output_monthly_spring2$date==as.Date("2021-04-02")] <- c(output_daily_spring2$observed[output_daily_spring2$date==as.Date("2021-04-02")],NA)
output_monthly_spring2 <- output_monthly_spring2[!is.na(output_monthly_spring2$observed),]

#######################################################
#Read in observations and simulation results for FALL

output_daily_fall2 <- read_csv(file.path(config$file_path$forecast_output_directory, "DA_experiments/fall/daily/bvre-2021-10-01-bvre_test.csv")) %>% 
  filter(date >= as.Date("2021-10-01")) %>% mutate(day = as.Date(date) - as.Date("2021-10-01")) %>% 
  filter(depth %in% c(0.1,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0)) %>% filter(!is.na(observed))

output_2days_fall2 <- read_csv(file.path(config$file_path$forecast_output_directory, "DA_experiments/fall/2days/bvre-2021-10-01-bvre_test.csv")) %>% 
  filter(date >= as.Date("2021-10-01")) %>% mutate(day = as.Date(date) - as.Date("2021-10-01")) %>% 
  filter(depth %in% c(0.1,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0))  %>% filter(!is.na(observed))

output_5days_fall2 <- read_csv(file.path(config$file_path$forecast_output_directory, "DA_experiments/fall/5days/bvre-2021-10-01-bvre_test.csv")) %>% 
  filter(date >= as.Date("2021-10-01")) %>% mutate(day = as.Date(date) - as.Date("2021-10-01")) %>% 
  filter(depth %in% c(0.1,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0))

#fill in the one-day ahead observations (2021-10-02); not used in DA but need to calculate RMSE, then remove NAs
output_5days_fall2$observed[output_5days_fall2$date==as.Date("2021-10-02")] <- c(output_daily_fall2$observed[output_daily_fall2$date==as.Date("2021-10-02")],NA)
output_5days_fall2 <- output_5days_fall2[!is.na(output_5days_fall2$observed),]


output_weekly_fall2 <- read_csv(file.path(config$file_path$forecast_output_directory, "DA_experiments/fall/weekly/bvre-2021-10-01-bvre_test.csv")) %>% 
  filter(date >= as.Date("2021-10-01")) %>% mutate(day = as.Date(date) - as.Date("2021-10-01")) %>% 
  filter(depth %in% c(0.1,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0))

#fill in the one-day ahead observations (2021-10-02); not used in DA but need to calculate RMSE, then remove NAs
output_weekly_fall2$observed[output_weekly_fall2$date==as.Date("2021-10-02")] <- c(output_daily_fall2$observed[output_daily_fall2$date==as.Date("2021-10-02")],NA)
output_weekly_fall2 <- output_weekly_fall2[!is.na(output_weekly_fall2$observed),]


output_fortnightly_fall2 <- read_csv(file.path(config$file_path$forecast_output_directory, "DA_experiments/fall/fortnightly/bvre-2021-10-01-bvre_test.csv")) %>% 
  filter(date >= as.Date("2021-10-01")) %>% mutate(day = as.Date(date) - as.Date("2021-10-01")) %>% 
  filter(depth %in% c(0.1,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0))

#fill in the one-day ahead observations (2021-10-02); not used in DA but need to calculate RMSE, then remove NAs
output_fortnightly_fall2$observed[output_fortnightly_fall2$date==as.Date("2021-10-02")] <- c(output_daily_fall2$observed[output_daily_fall2$date==as.Date("2021-10-02")],NA)
output_fortnightly_fall2 <- output_fortnightly_fall2[!is.na(output_fortnightly_fall2$observed),]


output_monthly_fall2 <- read_csv(file.path(config$file_path$forecast_output_directory, "DA_experiments/fall/monthly/bvre-2021-10-01-bvre_test.csv")) %>% 
  filter(date >= as.Date("2021-10-01")) %>% mutate(day = as.Date(date) - as.Date("2021-10-01")) %>% 
  filter(depth %in% c(0.1,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0))

#fill in the one-day ahead observations (2021-10-02); not used in DA but need to calculate RMSE, then remove NAs
output_monthly_fall2$observed[output_monthly_fall2$date==as.Date("2021-10-02")] <- c(output_daily_fall2$observed[output_daily_fall2$date==as.Date("2021-10-02")],NA)
output_monthly_fall2 <- output_monthly_fall2[!is.na(output_monthly_fall2$observed),]

#-------------------------------------------------------------------------------#
#create dfs with forecast evaluation metrics for SUMMER
forecast_eval_daily <- data.frame(Depth=unique(output_daily$depth),RMSE=NA, pbias=NA, bias=NA, CRPS=NA, DA="daily", season="summer2")
for(i in 1:length(unique(output_daily$depth))){
forecast_eval_daily[i,2] <- hydroGOF::rmse(output_daily$forecast_mean[output_daily$depth[i]==unique(output_daily$depth)],output_daily$observed[output_daily$depth[i]==unique(output_daily$depth)])
forecast_eval_daily[i,3]<- hydroGOF::pbias(output_daily$forecast_mean[output_daily$depth[i]==unique(output_daily$depth)],output_daily$observed[output_daily$depth[i]==unique(output_daily$depth)])
forecast_eval_daily[i,4] <- mean(output_daily$forecast_mean[output_daily$depth[i]==unique(output_daily$depth)]) - mean(output_daily$observed[output_daily$depth[i]==unique(output_daily$depth)])
forecast_eval_daily[i,5] <- mean(scoringRules::crps_sample(output_daily$observed[output_daily$depth[i]==unique(output_daily$depth)],cbind(output_daily$forecast_mean[output_daily$depth[i]==unique(output_daily$depth)],
                                                           output_daily$forecast_sd[output_daily$depth[i]==unique(output_daily$depth)])))
}

forecast_eval_2days <- data.frame(Depth=unique(output_2days$depth),RMSE=NA, pbias=NA, bias=NA, CRPS=NA, DA="2day", season="summer2")
for(i in 1:length(unique(output_2days$depth))){
  forecast_eval_2days[i,2] <- hydroGOF::rmse(output_2days$forecast_mean[output_2days$depth[i]==unique(output_2days$depth)],output_2days$observed[output_2days$depth[i]==unique(output_2days$depth)])
  forecast_eval_2days[i,3]<- hydroGOF::pbias(output_2days$forecast_mean[output_2days$depth[i]==unique(output_2days$depth)],output_2days$observed[output_2days$depth[i]==unique(output_2days$depth)])
  forecast_eval_2days[i,4] <- mean(output_2days$forecast_mean[output_2days$depth[i]==unique(output_2days$depth)]) - mean(output_2days$observed[output_2days$depth[i]==unique(output_2days$depth)])
  forecast_eval_2days[i,5] <- mean(scoringRules::crps_sample(output_2days$observed[output_2days$depth[i]==unique(output_2days$depth)],cbind(output_2days$forecast_mean[output_2days$depth[i]==unique(output_2days$depth)],
                                                             output_2days$forecast_sd[output_2days$depth[i]==unique(output_2days$depth)])))
}

forecast_eval_5days <- data.frame(Depth=unique(output_5days$depth),RMSE=NA, pbias=NA, bias=NA, CRPS=NA, DA="5day", season="summer2")
for(i in 1:length(unique(output_5days$depth))){
  forecast_eval_5days[i,2] <- hydroGOF::rmse(output_5days$forecast_mean[output_5days$depth[i]==unique(output_5days$depth)],output_5days$observed[output_5days$depth[i]==unique(output_5days$depth)])
  forecast_eval_5days[i,3]<- hydroGOF::pbias(output_5days$forecast_mean[output_5days$depth[i]==unique(output_5days$depth)],output_5days$observed[output_5days$depth[i]==unique(output_5days$depth)])
  forecast_eval_5days[i,4] <- mean(output_5days$forecast_mean[output_5days$depth[i]==unique(output_5days$depth)]) - mean(output_5days$observed[output_5days$depth[i]==unique(output_5days$depth)])
  forecast_eval_5days[i,5] <- mean(scoringRules::crps_sample(output_5days$observed[output_5days$depth[i]==unique(output_5days$depth)],cbind(output_5days$forecast_mean[output_5days$depth[i]==unique(output_5days$depth)],
                                                             output_5days$forecast_sd[output_5days$depth[i]==unique(output_5days$depth)])))
}

forecast_eval_weekly <- data.frame(Depth=unique(output_weekly$depth),RMSE=NA, pbias=NA, bias=NA, CRPS=NA, DA="weekly", season="summer2")
for(i in 1:length(unique(output_weekly$depth))){
  forecast_eval_weekly[i,2] <- hydroGOF::rmse(output_weekly$forecast_mean[output_weekly$depth[i]==unique(output_weekly$depth)],output_weekly$observed[output_weekly$depth[i]==unique(output_weekly$depth)])
  forecast_eval_weekly[i,3]<- hydroGOF::pbias(output_weekly$forecast_mean[output_weekly$depth[i]==unique(output_weekly$depth)],output_weekly$observed[output_weekly$depth[i]==unique(output_weekly$depth)])
  forecast_eval_weekly[i,4] <- mean(output_weekly$forecast_mean[output_weekly$depth[i]==unique(output_weekly$depth)]) - mean(output_weekly$observed[output_weekly$depth[i]==unique(output_weekly$depth)])
  forecast_eval_weekly[i,5] <- mean(scoringRules::crps_sample(output_weekly$observed[output_weekly$depth[i]==unique(output_weekly$depth)],cbind(output_weekly$forecast_mean[output_weekly$depth[i]==unique(output_weekly$depth)],
                                                              output_weekly$forecast_sd[output_weekly$depth[i]==unique(output_weekly$depth)])))
}

forecast_eval_fortnightly <- data.frame(Depth=unique(output_fortnightly$depth),RMSE=NA, pbias=NA, bias=NA, CRPS=NA, DA="fortnightly", season="summer2")
for(i in 1:length(unique(output_fortnightly$depth))){
  forecast_eval_fortnightly[i,2] <- hydroGOF::rmse(output_fortnightly$forecast_mean[output_fortnightly$depth[i]==unique(output_fortnightly$depth)],output_fortnightly$observed[output_fortnightly$depth[i]==unique(output_fortnightly$depth)])
  forecast_eval_fortnightly[i,3]<- hydroGOF::pbias(output_fortnightly$forecast_mean[output_fortnightly$depth[i]==unique(output_fortnightly$depth)],output_fortnightly$observed[output_fortnightly$depth[i]==unique(output_fortnightly$depth)])
  forecast_eval_fortnightly[i,4] <- mean(output_fortnightly$forecast_mean[output_fortnightly$depth[i]==unique(output_fortnightly$depth)]) - mean(output_fortnightly$observed[output_fortnightly$depth[i]==unique(output_fortnightly$depth)])
  forecast_eval_fortnightly[i,5] <- mean(scoringRules::crps_sample(output_fortnightly$observed[output_fortnightly$depth[i]==unique(output_fortnightly$depth)],cbind(output_fortnightly$forecast_mean[output_fortnightly$depth[i]==unique(output_fortnightly$depth)],
                                                                   output_fortnightly$forecast_sd[output_fortnightly$depth[i]==unique(output_fortnightly$depth)])))
}

forecast_eval_monthly <- data.frame(Depth=unique(output_monthly$depth),RMSE=NA, pbias=NA, bias=NA, CRPS=NA, DA="monthly", season="summer2")
for(i in 1:length(unique(output_monthly$depth))){
  forecast_eval_monthly[i,2] <- hydroGOF::rmse(output_monthly$forecast_mean[output_monthly$depth[i]==unique(output_monthly$depth)],output_monthly$observed[output_monthly$depth[i]==unique(output_monthly$depth)])
  forecast_eval_monthly[i,3]<- hydroGOF::pbias(output_monthly$forecast_mean[output_monthly$depth[i]==unique(output_monthly$depth)],output_monthly$observed[output_monthly$depth[i]==unique(output_monthly$depth)])
  forecast_eval_monthly[i,4] <- mean(output_monthly$forecast_mean[output_monthly$depth[i]==unique(output_monthly$depth)]) - mean(output_monthly$observed[output_monthly$depth[i]==unique(output_monthly$depth)])
  forecast_eval_monthly[i,5] <- mean(scoringRules::crps_sample(output_monthly$observed[output_monthly$depth[i]==unique(output_monthly$depth)],cbind(output_monthly$forecast_mean[output_monthly$depth[i]==unique(output_monthly$depth)],
                                                                                                                                                    output_monthly$forecast_sd[output_monthly$depth[i]==unique(output_monthly$depth)])))
}

#######################################################
#create dfs with forecast evaluation metrics for WINTER
forecast_eval_daily_winter2 <- data.frame(Depth=unique(output_daily_winter2$depth),RMSE=NA, pbias=NA, bias=NA, CRPS=NA, DA="daily", season="winter2")
for(i in 1:length(unique(output_daily_winter2$depth))){
  forecast_eval_daily_winter2[i,2] <- hydroGOF::rmse(output_daily_winter2$forecast_mean[output_daily_winter2$depth[i]==unique(output_daily_winter2$depth)],output_daily_winter2$observed[output_daily_winter2$depth[i]==unique(output_daily_winter2$depth)])
  forecast_eval_daily_winter2[i,3]<- hydroGOF::pbias(output_daily_winter2$forecast_mean[output_daily_winter2$depth[i]==unique(output_daily_winter2$depth)],output_daily_winter2$observed[output_daily_winter2$depth[i]==unique(output_daily_winter2$depth)])
  forecast_eval_daily_winter2[i,4] <- mean(output_daily_winter2$forecast_mean[output_daily_winter2$depth[i]==unique(output_daily_winter2$depth)]) - mean(output_daily_winter2$observed[output_daily_winter2$depth[i]==unique(output_daily_winter2$depth)])
  forecast_eval_daily_winter2[i,5] <- mean(scoringRules::crps_sample(output_daily_winter2$observed[output_daily_winter2$depth[i]==unique(output_daily_winter2$depth)],cbind(output_daily_winter2$forecast_mean[output_daily_winter2$depth[i]==unique(output_daily_winter2$depth)],
                                                                     output_daily_winter2$forecast_sd[output_daily_winter2$depth[i]==unique(output_daily_winter2$depth)])))
}

forecast_eval_2days_winter2 <- data.frame(Depth=unique(output_2days_winter2$depth),RMSE=NA, pbias=NA, bias=NA, CRPS=NA, DA="2day", season="winter2")
for(i in 1:length(unique(output_2days_winter2$depth))){
  forecast_eval_2days_winter2[i,2] <- hydroGOF::rmse(output_2days_winter2$forecast_mean[output_2days_winter2$depth[i]==unique(output_2days_winter2$depth)],output_2days_winter2$observed[output_2days_winter2$depth[i]==unique(output_2days_winter2$depth)])
  forecast_eval_2days_winter2[i,3]<- hydroGOF::pbias(output_2days_winter2$forecast_mean[output_2days_winter2$depth[i]==unique(output_2days_winter2$depth)],output_2days_winter2$observed[output_2days_winter2$depth[i]==unique(output_2days_winter2$depth)])
  forecast_eval_2days_winter2[i,4] <- mean(output_2days_winter2$forecast_mean[output_2days_winter2$depth[i]==unique(output_2days_winter2$depth)]) - mean(output_2days_winter2$observed[output_2days_winter2$depth[i]==unique(output_2days_winter2$depth)])
  forecast_eval_2days_winter2[i,5] <- mean(scoringRules::crps_sample(output_2days_winter2$observed[output_2days_winter2$depth[i]==unique(output_2days_winter2$depth)],cbind(output_2days_winter2$forecast_mean[output_2days_winter2$depth[i]==unique(output_2days_winter2$depth)],
                                                                     output_2days_winter2$forecast_sd[output_2days_winter2$depth[i]==unique(output_2days_winter2$depth)])))
}

forecast_eval_5days_winter2 <- data.frame(Depth=unique(output_5days_winter2$depth),RMSE=NA, pbias=NA, bias=NA, CRPS=NA, DA="5day", season="winter2")
for(i in 1:length(unique(output_5days_winter2$depth))){
  forecast_eval_5days_winter2[i,2] <- hydroGOF::rmse(output_5days_winter2$forecast_mean[output_5days_winter2$depth[i]==unique(output_5days_winter2$depth)],output_5days_winter2$observed[output_5days_winter2$depth[i]==unique(output_5days_winter2$depth)])
  forecast_eval_5days_winter2[i,3]<- hydroGOF::pbias(output_5days_winter2$forecast_mean[output_5days_winter2$depth[i]==unique(output_5days_winter2$depth)],output_5days_winter2$observed[output_5days_winter2$depth[i]==unique(output_5days_winter2$depth)])
  forecast_eval_5days_winter2[i,4] <- mean(output_5days_winter2$forecast_mean[output_5days_winter2$depth[i]==unique(output_5days_winter2$depth)]) - mean(output_5days_winter2$observed[output_5days_winter2$depth[i]==unique(output_5days_winter2$depth)])
  forecast_eval_5days_winter2[i,5] <- mean(scoringRules::crps_sample(output_5days_winter2$observed[output_5days_winter2$depth[i]==unique(output_5days_winter2$depth)],cbind(output_5days_winter2$forecast_mean[output_5days_winter2$depth[i]==unique(output_5days_winter2$depth)],
                                                                     output_5days_winter2$forecast_sd[output_5days_winter2$depth[i]==unique(output_5days_winter2$depth)])))
}

forecast_eval_weekly_winter2 <- data.frame(Depth=unique(output_weekly_winter2$depth),RMSE=NA, pbias=NA, bias=NA, CRPS=NA, DA="weekly", season="winter2")
for(i in 1:length(unique(output_weekly_winter2$depth))){
  forecast_eval_weekly_winter2[i,2] <- hydroGOF::rmse(output_weekly_winter2$forecast_mean[output_weekly_winter2$depth[i]==unique(output_weekly_winter2$depth)],output_weekly_winter2$observed[output_weekly_winter2$depth[i]==unique(output_weekly_winter2$depth)])
  forecast_eval_weekly_winter2[i,3]<- hydroGOF::pbias(output_weekly_winter2$forecast_mean[output_weekly_winter2$depth[i]==unique(output_weekly_winter2$depth)],output_weekly_winter2$observed[output_weekly_winter2$depth[i]==unique(output_weekly_winter2$depth)])
  forecast_eval_weekly_winter2[i,4] <- mean(output_weekly_winter2$forecast_mean[output_weekly_winter2$depth[i]==unique(output_weekly_winter2$depth)]) - mean(output_weekly_winter2$observed[output_weekly_winter2$depth[i]==unique(output_weekly_winter2$depth)])
  forecast_eval_weekly_winter2[i,5] <- mean(scoringRules::crps_sample(output_weekly_winter2$observed[output_weekly_winter2$depth[i]==unique(output_weekly_winter2$depth)],cbind(output_weekly_winter2$forecast_mean[output_weekly_winter2$depth[i]==unique(output_weekly_winter2$depth)],
                                                                      output_weekly_winter2$forecast_sd[output_weekly_winter2$depth[i]==unique(output_weekly_winter2$depth)])))
}

forecast_eval_fortnightly_winter2 <- data.frame(Depth=unique(output_fortnightly_winter2$depth),RMSE=NA, pbias=NA, bias=NA, CRPS=NA, DA="fortnightly", season="winter2")
for(i in 1:length(unique(output_fortnightly_winter2$depth))){
  forecast_eval_fortnightly_winter2[i,2] <- hydroGOF::rmse(output_fortnightly_winter2$forecast_mean[output_fortnightly_winter2$depth[i]==unique(output_fortnightly_winter2$depth)],output_fortnightly_winter2$observed[output_fortnightly_winter2$depth[i]==unique(output_fortnightly_winter2$depth)])
  forecast_eval_fortnightly_winter2[i,3]<- hydroGOF::pbias(output_fortnightly_winter2$forecast_mean[output_fortnightly_winter2$depth[i]==unique(output_fortnightly_winter2$depth)],output_fortnightly_winter2$observed[output_fortnightly_winter2$depth[i]==unique(output_fortnightly_winter2$depth)])
  forecast_eval_fortnightly_winter2[i,4] <- mean(output_fortnightly_winter2$forecast_mean[output_fortnightly_winter2$depth[i]==unique(output_fortnightly_winter2$depth)]) - mean(output_fortnightly_winter2$observed[output_fortnightly_winter2$depth[i]==unique(output_fortnightly_winter2$depth)])
  forecast_eval_fortnightly_winter2[i,5] <- mean(scoringRules::crps_sample(output_fortnightly_winter2$observed[output_fortnightly_winter2$depth[i]==unique(output_fortnightly_winter2$depth)],cbind(output_fortnightly_winter2$forecast_mean[output_fortnightly_winter2$depth[i]==unique(output_fortnightly_winter2$depth)],
                                                                                                                                                                                                    output_fortnightly_winter2$forecast_sd[output_fortnightly_winter2$depth[i]==unique(output_fortnightly_winter2$depth)])))
}

forecast_eval_monthly_winter2 <- data.frame(Depth=unique(output_monthly_winter2$depth),RMSE=NA, pbias=NA, bias=NA, CRPS=NA, DA="monthly", season="winter2")
for(i in 1:length(unique(output_monthly_winter2$depth))){
  forecast_eval_monthly_winter2[i,2] <- hydroGOF::rmse(output_monthly_winter2$forecast_mean[output_monthly_winter2$depth[i]==unique(output_monthly_winter2$depth)],output_monthly_winter2$observed[output_monthly_winter2$depth[i]==unique(output_monthly_winter2$depth)])
  forecast_eval_monthly_winter2[i,3]<- hydroGOF::pbias(output_monthly_winter2$forecast_mean[output_monthly_winter2$depth[i]==unique(output_monthly_winter2$depth)],output_monthly_winter2$observed[output_monthly_winter2$depth[i]==unique(output_monthly_winter2$depth)])
  forecast_eval_monthly_winter2[i,4] <- mean(output_monthly_winter2$forecast_mean[output_monthly_winter2$depth[i]==unique(output_monthly_winter2$depth)]) - mean(output_monthly_winter2$observed[output_monthly_winter2$depth[i]==unique(output_monthly_winter2$depth)])
  forecast_eval_monthly_winter2[i,5] <- mean(scoringRules::crps_sample(output_monthly_winter2$observed[output_monthly_winter2$depth[i]==unique(output_monthly_winter2$depth)],cbind(output_monthly_winter2$forecast_mean[output_monthly_winter2$depth[i]==unique(output_monthly_winter2$depth)],
                                                                       output_monthly_winter2$forecast_sd[output_monthly_winter2$depth[i]==unique(output_monthly_winter2$depth)])))
}

#######################################################
#create dfs with forecast evaluation metrics for SPRING
forecast_eval_daily_spring2 <- data.frame(Depth=unique(output_daily_spring2$depth),RMSE=NA, pbias=NA, bias=NA, CRPS=NA, DA="daily", season="spring2")
for(i in 1:length(unique(output_daily_spring2$depth))){
  forecast_eval_daily_spring2[i,2] <- hydroGOF::rmse(output_daily_spring2$forecast_mean[output_daily_spring2$depth[i]==unique(output_daily_spring2$depth)],output_daily_spring2$observed[output_daily_spring2$depth[i]==unique(output_daily_spring2$depth)])
  forecast_eval_daily_spring2[i,3]<- hydroGOF::pbias(output_daily_spring2$forecast_mean[output_daily_spring2$depth[i]==unique(output_daily_spring2$depth)],output_daily_spring2$observed[output_daily_spring2$depth[i]==unique(output_daily_spring2$depth)])
  forecast_eval_daily_spring2[i,4] <- mean(output_daily_spring2$forecast_mean[output_daily_spring2$depth[i]==unique(output_daily_spring2$depth)]) - mean(output_daily_spring2$observed[output_daily_spring2$depth[i]==unique(output_daily_spring2$depth)])
  forecast_eval_daily_spring2[i,5] <- mean(scoringRules::crps_sample(output_daily_spring2$observed[output_daily_spring2$depth[i]==unique(output_daily_spring2$depth)],cbind(output_daily_spring2$forecast_mean[output_daily_spring2$depth[i]==unique(output_daily_spring2$depth)],
                                                                     output_daily_spring2$forecast_sd[output_daily_spring2$depth[i]==unique(output_daily_spring2$depth)])))
}

forecast_eval_2days_spring2 <- data.frame(Depth=unique(output_2days_spring2$depth),RMSE=NA, pbias=NA, bias=NA, CRPS=NA, DA="2day", season="spring2")
for(i in 1:length(unique(output_2days_spring2$depth))){
  forecast_eval_2days_spring2[i,2] <- hydroGOF::rmse(output_2days_spring2$forecast_mean[output_2days_spring2$depth[i]==unique(output_2days_spring2$depth)],output_2days_spring2$observed[output_2days_spring2$depth[i]==unique(output_2days_spring2$depth)])
  forecast_eval_2days_spring2[i,3]<- hydroGOF::pbias(output_2days_spring2$forecast_mean[output_2days_spring2$depth[i]==unique(output_2days_spring2$depth)],output_2days_spring2$observed[output_2days_spring2$depth[i]==unique(output_2days_spring2$depth)])
  forecast_eval_2days_spring2[i,4] <- mean(output_2days_spring2$forecast_mean[output_2days_spring2$depth[i]==unique(output_2days_spring2$depth)]) - mean(output_2days_spring2$observed[output_2days_spring2$depth[i]==unique(output_2days_spring2$depth)])
  forecast_eval_2days_spring2[i,5] <- mean(scoringRules::crps_sample(output_2days_spring2$observed[output_2days_spring2$depth[i]==unique(output_2days_spring2$depth)],cbind(output_2days_spring2$forecast_mean[output_2days_spring2$depth[i]==unique(output_2days_spring2$depth)],
                                                                     output_2days_spring2$forecast_sd[output_2days_spring2$depth[i]==unique(output_2days_spring2$depth)])))
}

forecast_eval_5days_spring2 <- data.frame(Depth=unique(output_5days_spring2$depth),RMSE=NA, pbias=NA, bias=NA, CRPS=NA, DA="5day", season="spring2")
for(i in 1:length(unique(output_5days_spring2$depth))){
  forecast_eval_5days_spring2[i,2] <- hydroGOF::rmse(output_5days_spring2$forecast_mean[output_5days_spring2$depth[i]==unique(output_5days_spring2$depth)],output_5days_spring2$observed[output_5days_spring2$depth[i]==unique(output_5days_spring2$depth)])
  forecast_eval_5days_spring2[i,3]<- hydroGOF::pbias(output_5days_spring2$forecast_mean[output_5days_spring2$depth[i]==unique(output_5days_spring2$depth)],output_5days_spring2$observed[output_5days_spring2$depth[i]==unique(output_5days_spring2$depth)])
  forecast_eval_5days_spring2[i,4] <- mean(output_5days_spring2$forecast_mean[output_5days_spring2$depth[i]==unique(output_5days_spring2$depth)]) - mean(output_5days_spring2$observed[output_5days_spring2$depth[i]==unique(output_5days_spring2$depth)])
  forecast_eval_5days_spring2[i,5] <- mean(scoringRules::crps_sample(output_5days_spring2$observed[output_5days_spring2$depth[i]==unique(output_5days_spring2$depth)],cbind(output_5days_spring2$forecast_mean[output_5days_spring2$depth[i]==unique(output_5days_spring2$depth)],
                                                                     output_5days_spring2$forecast_sd[output_5days_spring2$depth[i]==unique(output_5days_spring2$depth)])))
}

forecast_eval_weekly_spring2 <- data.frame(Depth=unique(output_weekly_spring2$depth),RMSE=NA, pbias=NA, bias=NA, CRPS=NA, DA="weekly", season="spring2")
for(i in 1:length(unique(output_weekly_spring2$depth))){
  forecast_eval_weekly_spring2[i,2] <- hydroGOF::rmse(output_weekly_spring2$forecast_mean[output_weekly_spring2$depth[i]==unique(output_weekly_spring2$depth)],output_weekly_spring2$observed[output_weekly_spring2$depth[i]==unique(output_weekly_spring2$depth)])
  forecast_eval_weekly_spring2[i,3]<- hydroGOF::pbias(output_weekly_spring2$forecast_mean[output_weekly_spring2$depth[i]==unique(output_weekly_spring2$depth)],output_weekly_spring2$observed[output_weekly_spring2$depth[i]==unique(output_weekly_spring2$depth)])
  forecast_eval_weekly_spring2[i,4] <- mean(output_weekly_spring2$forecast_mean[output_weekly_spring2$depth[i]==unique(output_weekly_spring2$depth)]) - mean(output_weekly_spring2$observed[output_weekly_spring2$depth[i]==unique(output_weekly_spring2$depth)])
  forecast_eval_weekly_spring2[i,5] <- mean(scoringRules::crps_sample(output_weekly_spring2$observed[output_weekly_spring2$depth[i]==unique(output_weekly_spring2$depth)],cbind(output_weekly_spring2$forecast_mean[output_weekly_spring2$depth[i]==unique(output_weekly_spring2$depth)],
                                                                      output_weekly_spring2$forecast_sd[output_weekly_spring2$depth[i]==unique(output_weekly_spring2$depth)])))
}

forecast_eval_fortnightly_spring2 <- data.frame(Depth=unique(output_fortnightly_spring2$depth),RMSE=NA, pbias=NA, bias=NA, CRPS=NA, DA="fortnightly", season="spring2")
for(i in 1:length(unique(output_fortnightly_spring2$depth))){
  forecast_eval_fortnightly_spring2[i,2] <- hydroGOF::rmse(output_fortnightly_spring2$forecast_mean[output_fortnightly_spring2$depth[i]==unique(output_fortnightly_spring2$depth)],output_fortnightly_spring2$observed[output_fortnightly_spring2$depth[i]==unique(output_fortnightly_spring2$depth)])
  forecast_eval_fortnightly_spring2[i,3]<- hydroGOF::pbias(output_fortnightly_spring2$forecast_mean[output_fortnightly_spring2$depth[i]==unique(output_fortnightly_spring2$depth)],output_fortnightly_spring2$observed[output_fortnightly_spring2$depth[i]==unique(output_fortnightly_spring2$depth)])
  forecast_eval_fortnightly_spring2[i,4] <- mean(output_fortnightly_spring2$forecast_mean[output_fortnightly_spring2$depth[i]==unique(output_fortnightly_spring2$depth)]) - mean(output_fortnightly_spring2$observed[output_fortnightly_spring2$depth[i]==unique(output_fortnightly_spring2$depth)])
  forecast_eval_fortnightly_spring2[i,5] <- mean(scoringRules::crps_sample(output_fortnightly_spring2$observed[output_fortnightly_spring2$depth[i]==unique(output_fortnightly_spring2$depth)],cbind(output_fortnightly_spring2$forecast_mean[output_fortnightly_spring2$depth[i]==unique(output_fortnightly_spring2$depth)],
                                                                           output_fortnightly_spring2$forecast_sd[output_fortnightly_spring2$depth[i]==unique(output_fortnightly_spring2$depth)])))
}

forecast_eval_monthly_spring2 <- data.frame(Depth=unique(output_monthly_spring2$depth),RMSE=NA, pbias=NA, bias=NA, CRPS=NA, DA="monthly", season="spring2")
for(i in 1:length(unique(output_monthly_spring2$depth))){
  forecast_eval_monthly_spring2[i,2] <- hydroGOF::rmse(output_monthly_spring2$forecast_mean[output_monthly_spring2$depth[i]==unique(output_monthly_spring2$depth)],output_monthly_spring2$observed[output_monthly_spring2$depth[i]==unique(output_monthly_spring2$depth)])
  forecast_eval_monthly_spring2[i,3]<- hydroGOF::pbias(output_monthly_spring2$forecast_mean[output_monthly_spring2$depth[i]==unique(output_monthly_spring2$depth)],output_monthly_spring2$observed[output_monthly_spring2$depth[i]==unique(output_monthly_spring2$depth)])
  forecast_eval_monthly_spring2[i,4] <- mean(output_monthly_spring2$forecast_mean[output_monthly_spring2$depth[i]==unique(output_monthly_spring2$depth)]) - mean(output_monthly_spring2$observed[output_monthly_spring2$depth[i]==unique(output_monthly_spring2$depth)])
  forecast_eval_monthly_spring2[i,5] <- mean(scoringRules::crps_sample(output_monthly_spring2$observed[output_monthly_spring2$depth[i]==unique(output_monthly_spring2$depth)],cbind(output_monthly_spring2$forecast_mean[output_monthly_spring2$depth[i]==unique(output_monthly_spring2$depth)],
                                                                       output_monthly_spring2$forecast_sd[output_monthly_spring2$depth[i]==unique(output_monthly_spring2$depth)])))
}

#######################################################
#create dfs with forecast evaluation metrics for FALL
forecast_eval_daily_fall2 <- data.frame(Depth=unique(output_daily_fall2$depth),RMSE=NA, pbias=NA, bias=NA, CRPS=NA, DA="daily", season="fall2")
for(i in 1:length(unique(output_daily_fall2$depth))){
  forecast_eval_daily_fall2[i,2] <- hydroGOF::rmse(output_daily_fall2$forecast_mean[output_daily_fall2$depth[i]==unique(output_daily_fall2$depth)],output_daily_fall2$observed[output_daily_fall2$depth[i]==unique(output_daily_fall2$depth)])
  forecast_eval_daily_fall2[i,3]<- hydroGOF::pbias(output_daily_fall2$forecast_mean[output_daily_fall2$depth[i]==unique(output_daily_fall2$depth)],output_daily_fall2$observed[output_daily_fall2$depth[i]==unique(output_daily_fall2$depth)])
  forecast_eval_daily_fall2[i,4] <- mean(output_daily_fall2$forecast_mean[output_daily_fall2$depth[i]==unique(output_daily_fall2$depth)]) - mean(output_daily_fall2$observed[output_daily_fall2$depth[i]==unique(output_daily_fall2$depth)])
  forecast_eval_daily_fall2[i,5] <- mean(scoringRules::crps_sample(output_daily_fall2$observed[output_daily_fall2$depth[i]==unique(output_daily_fall2$depth)],cbind(output_daily_fall2$forecast_mean[output_daily_fall2$depth[i]==unique(output_daily_fall2$depth)],
                                                                   output_daily_fall2$forecast_sd[output_daily_fall2$depth[i]==unique(output_daily_fall2$depth)])))
}

forecast_eval_2days_fall2 <- data.frame(Depth=unique(output_2days_fall2$depth),RMSE=NA, pbias=NA, bias=NA, CRPS=NA, DA="2day", season="fall2")
for(i in 1:length(unique(output_2days_fall2$depth))){
  forecast_eval_2days_fall2[i,2] <- hydroGOF::rmse(output_2days_fall2$forecast_mean[output_2days_fall2$depth[i]==unique(output_2days_fall2$depth)],output_2days_fall2$observed[output_2days_fall2$depth[i]==unique(output_2days_fall2$depth)])
  forecast_eval_2days_fall2[i,3]<- hydroGOF::pbias(output_2days_fall2$forecast_mean[output_2days_fall2$depth[i]==unique(output_2days_fall2$depth)],output_2days_fall2$observed[output_2days_fall2$depth[i]==unique(output_2days_fall2$depth)])
  forecast_eval_2days_fall2[i,4] <- mean(output_2days_fall2$forecast_mean[output_2days_fall2$depth[i]==unique(output_2days_fall2$depth)]) - mean(output_2days_fall2$observed[output_2days_fall2$depth[i]==unique(output_2days_fall2$depth)])
  forecast_eval_2days_fall2[i,5] <- mean(scoringRules::crps_sample(output_2days_fall2$observed[output_2days_fall2$depth[i]==unique(output_2days_fall2$depth)],cbind(output_2days_fall2$forecast_mean[output_2days_fall2$depth[i]==unique(output_2days_fall2$depth)],
                                                                   output_2days_fall2$forecast_sd[output_2days_fall2$depth[i]==unique(output_2days_fall2$depth)])))
}

forecast_eval_5days_fall2 <- data.frame(Depth=unique(output_5days_fall2$depth),RMSE=NA, pbias=NA, bias=NA, CRPS=NA, DA="5day", season="fall2")
for(i in 1:length(unique(output_5days_fall2$depth))){
  forecast_eval_5days_fall2[i,2] <- hydroGOF::rmse(output_5days_fall2$forecast_mean[output_5days_fall2$depth[i]==unique(output_5days_fall2$depth)],output_5days_fall2$observed[output_5days_fall2$depth[i]==unique(output_5days_fall2$depth)])
  forecast_eval_5days_fall2[i,3]<- hydroGOF::pbias(output_5days_fall2$forecast_mean[output_5days_fall2$depth[i]==unique(output_5days_fall2$depth)],output_5days_fall2$observed[output_5days_fall2$depth[i]==unique(output_5days_fall2$depth)])
  forecast_eval_5days_fall2[i,4] <- mean(output_5days_fall2$forecast_mean[output_5days_fall2$depth[i]==unique(output_5days_fall2$depth)]) - mean(output_5days_fall2$observed[output_5days_fall2$depth[i]==unique(output_5days_fall2$depth)])
  forecast_eval_5days_fall2[i,5] <- mean(scoringRules::crps_sample(output_5days_fall2$observed[output_5days_fall2$depth[i]==unique(output_5days_fall2$depth)],cbind(output_5days_fall2$forecast_mean[output_5days_fall2$depth[i]==unique(output_5days_fall2$depth)],
                                                                   output_5days_fall2$forecast_sd[output_5days_fall2$depth[i]==unique(output_5days_fall2$depth)])))
}

forecast_eval_weekly_fall2 <- data.frame(Depth=unique(output_weekly_fall2$depth),RMSE=NA, pbias=NA, bias=NA, CRPS=NA, DA="weekly", season="fall2")
for(i in 1:length(unique(output_weekly_fall2$depth))){
  forecast_eval_weekly_fall2[i,2] <- hydroGOF::rmse(output_weekly_fall2$forecast_mean[output_weekly_fall2$depth[i]==unique(output_weekly_fall2$depth)],output_weekly_fall2$observed[output_weekly_fall2$depth[i]==unique(output_weekly_fall2$depth)])
  forecast_eval_weekly_fall2[i,3]<- hydroGOF::pbias(output_weekly_fall2$forecast_mean[output_weekly_fall2$depth[i]==unique(output_weekly_fall2$depth)],output_weekly_fall2$observed[output_weekly_fall2$depth[i]==unique(output_weekly_fall2$depth)])
  forecast_eval_weekly_fall2[i,4] <- mean(output_weekly_fall2$forecast_mean[output_weekly_fall2$depth[i]==unique(output_weekly_fall2$depth)]) - mean(output_weekly_fall2$observed[output_weekly_fall2$depth[i]==unique(output_weekly_fall2$depth)])
  forecast_eval_weekly_fall2[i,5] <- mean(scoringRules::crps_sample(output_weekly_fall2$observed[output_weekly_fall2$depth[i]==unique(output_weekly_fall2$depth)],cbind(output_weekly_fall2$forecast_mean[output_weekly_fall2$depth[i]==unique(output_weekly_fall2$depth)],
                                                                    output_weekly_fall2$forecast_sd[output_weekly_fall2$depth[i]==unique(output_weekly_fall2$depth)])))
}

forecast_eval_fortnightly_fall2 <- data.frame(Depth=unique(output_fortnightly_fall2$depth),RMSE=NA, pbias=NA, bias=NA, CRPS=NA, DA="fortnightly", season="fall2")
for(i in 1:length(unique(output_fortnightly_fall2$depth))){
  forecast_eval_fortnightly_fall2[i,2] <- hydroGOF::rmse(output_fortnightly_fall2$forecast_mean[output_fortnightly_fall2$depth[i]==unique(output_fortnightly_fall2$depth)],output_fortnightly_fall2$observed[output_fortnightly_fall2$depth[i]==unique(output_fortnightly_fall2$depth)])
  forecast_eval_fortnightly_fall2[i,3]<- hydroGOF::pbias(output_fortnightly_fall2$forecast_mean[output_fortnightly_fall2$depth[i]==unique(output_fortnightly_fall2$depth)],output_fortnightly_fall2$observed[output_fortnightly_fall2$depth[i]==unique(output_fortnightly_fall2$depth)])
  forecast_eval_fortnightly_fall2[i,4] <- mean(output_fortnightly_fall2$forecast_mean[output_fortnightly_fall2$depth[i]==unique(output_fortnightly_fall2$depth)]) - mean(output_fortnightly_fall2$observed[output_fortnightly_fall2$depth[i]==unique(output_fortnightly_fall2$depth)])
  forecast_eval_fortnightly_fall2[i,5] <- mean(scoringRules::crps_sample(output_fortnightly_fall2$observed[output_fortnightly_fall2$depth[i]==unique(output_fortnightly_fall2$depth)],cbind(output_fortnightly_fall2$forecast_mean[output_fortnightly_fall2$depth[i]==unique(output_fortnightly_fall2$depth)],
                                                                         output_fortnightly_fall2$forecast_sd[output_fortnightly_fall2$depth[i]==unique(output_fortnightly_fall2$depth)])))
}

forecast_eval_monthly_fall2 <- data.frame(Depth=unique(output_monthly_fall2$depth),RMSE=NA, pbias=NA, bias=NA, CRPS=NA, DA="monthly", season="fall2")
for(i in 1:length(unique(output_monthly_fall2$depth))){
  forecast_eval_monthly_fall2[i,2] <- hydroGOF::rmse(output_monthly_fall2$forecast_mean[output_monthly_fall2$depth[i]==unique(output_monthly_fall2$depth)],output_monthly_fall2$observed[output_monthly_fall2$depth[i]==unique(output_monthly_fall2$depth)])
  forecast_eval_monthly_fall2[i,3]<- hydroGOF::pbias(output_monthly_fall2$forecast_mean[output_monthly_fall2$depth[i]==unique(output_monthly_fall2$depth)],output_monthly_fall2$observed[output_monthly_fall2$depth[i]==unique(output_monthly_fall2$depth)])
  forecast_eval_monthly_fall2[i,4] <- mean(output_monthly_fall2$forecast_mean[output_monthly_fall2$depth[i]==unique(output_monthly_fall2$depth)]) - mean(output_monthly_fall2$observed[output_monthly_fall2$depth[i]==unique(output_monthly_fall2$depth)])
  forecast_eval_monthly_fall2[i,5] <- mean(scoringRules::crps_sample(output_monthly_fall2$observed[output_monthly_fall2$depth[i]==unique(output_monthly_fall2$depth)],cbind(output_monthly_fall2$forecast_mean[output_monthly_fall2$depth[i]==unique(output_monthly_fall2$depth)],
                                                                     output_monthly_fall2$forecast_sd[output_monthly_fall2$depth[i]==unique(output_monthly_fall2$depth)])))
}

#------------------------------------------------------------------------------#

#combine dfs
forecast_eval_final <- rbind(forecast_eval_daily,forecast_eval_2days,forecast_eval_5days,forecast_eval_weekly,forecast_eval_fortnightly,forecast_eval_monthly,
                             forecast_eval_daily_winter2,forecast_eval_2days_winter2,forecast_eval_5days_winter2,forecast_eval_weekly_winter2,forecast_eval_fortnightly_winter2,forecast_eval_monthly_winter2,
                             forecast_eval_daily_spring2,forecast_eval_2days_spring2,forecast_eval_5days_spring2,forecast_eval_weekly_spring2,forecast_eval_fortnightly_spring2,forecast_eval_monthly_spring2,
                             forecast_eval_daily_fall2,forecast_eval_2days_fall2,forecast_eval_5days_fall2,forecast_eval_weekly_fall2,forecast_eval_fortnightly_fall2,forecast_eval_monthly_fall2)

#visualize rmse over depth
ggplot(forecast_eval_daily, aes(RMSE,-Depth)) + geom_point() + geom_path()

ggplot(output_daily, aes(observed, forecast_mean, col=as.factor(depth), group=depth)) + geom_point() +
  geom_abline(slope=1)

#plot forecast skill comparison - all depths
ggplot(forecast_eval_final, aes(as.factor(DA), RMSE, fill=as.factor(DA))) + geom_boxplot() +xlab("") + facet_wrap(~season) +
  theme_light() + theme(text = element_text(size=24), legend.position = "none", axis.text = element_text(size=22, color="black")) +
  scale_x_discrete(limits = c("daily","2day","5day","weekly","fortnightly","monthly"))
ggplot(forecast_eval_final, aes(as.factor(DA), bias)) + geom_boxplot() +xlab("DA") + 
  scale_x_discrete(limits = c("daily","2day","5day","weekly","fortnightly","monthly"))
ggplot(forecast_eval_final, aes(as.factor(DA), CRPS)) + geom_boxplot() +xlab("DA") +
  scale_x_discrete(limits = c("daily","2day","5day","weekly","fortnightly","monthly"))

#separate by DA frequency and metric
ggplot(subset(forecast_eval_final, DA="daily"), aes(RMSE, as.factor(-Depth))) + geom_point() + facet_wrap(~season)

#by depth
ggplot(forecast_eval_final, aes(RMSE, as.factor(-Depth), color=as.factor(DA))) + geom_point(size=4) + ylab("Depth (m)") + facet_wrap(~season) + 
  theme_light()  + theme(legend.title = element_blank(), text = element_text(size=24), legend.position=c(0.88, 0.13),  axis.text = element_text(size=22, color="black"), legend.text = element_text(size=24)) 
ggplot(forecast_eval_final, aes(bias, as.factor(-Depth), color=as.factor(DA))) + geom_point() + ylab("Depth (m)") + theme(legend.title = element_blank()) 
ggplot(forecast_eval_final, aes(CRPS, as.factor(-Depth), color=as.factor(DA))) + geom_point() + ylab("Depth (m)") + theme(legend.title = element_blank()) 

## daily forecast horizon RMSE figs
forecast_daily <- read_csv(file.path(config$file_path$forecast_output_directory, "DA_experiments/summer/daily/bvre-2021-07-01-bvre_test.csv")) %>% 
  filter(date >= as.Date(config$run_config$forecast_start_datetime)) %>% mutate(day = as.Date(date) - as.Date(config$run_config$forecast_start_datetime))

forecast_daily_alldepths <- forecast_daily  %>%  select(forecast_mean, day, observed) %>% group_by(day) %>% summarise(avg_wc_temp = mean(forecast_mean), avg_wc_obs = mean(observed))

forecast_daily_RMSE <- data.frame(day=unique(forecast_daily_alldepths$day), RMSE=NA)
for(i in 1:length(unique(forecast_daily_alldepths$day))){
  forecast_daily_RMSE[i,2] <- hydroGOF::rmse(forecast_daily_alldepths$avg_wc_temp[forecast_daily_alldepths$day[i]==unique(forecast_daily_alldepths$day)],
                                            forecast_daily_alldepths$avg_wc_obs[forecast_daily_alldepths$day[i]==unique(forecast_daily_alldepths$day)])
}

ggplot(forecast_daily_RMSE, aes(as.factor(day),RMSE,group=1)) + geom_point(size=4) + xlab("forecast horizon (days)") + theme_light()  + 
  theme(legend.title = element_blank(), text = element_text(size=20)) + geom_line(color="black")


#calculate average rmse by DA type
mean(forecast_eval_final$RMSE[forecast_eval_final$DA=="daily" & forecast_eval_final$season=="summer2"])
mean(forecast_eval_final$RMSE[forecast_eval_final$DA=="2day"& forecast_eval_final$season=="summer2"])
mean(forecast_eval_final$RMSE[forecast_eval_final$DA=="5day"& forecast_eval_final$season=="summer2"])
mean(forecast_eval_final$RMSE[forecast_eval_final$DA=="weekly"& forecast_eval_final$season=="summer2"])
mean(forecast_eval_final$RMSE[forecast_eval_final$DA=="fortnightly"& forecast_eval_final$season=="summer2"])
mean(forecast_eval_final$RMSE[forecast_eval_final$DA=="monthly"& forecast_eval_final$season=="summer2"])

mean(forecast_eval_final$RMSE[forecast_eval_final$DA=="daily"& forecast_eval_final$season=="winter2"])
mean(forecast_eval_final$RMSE[forecast_eval_final$DA=="2day"& forecast_eval_final$season=="winter2"])
mean(forecast_eval_final$RMSE[forecast_eval_final$DA=="5day"& forecast_eval_final$season=="winter2"])
mean(forecast_eval_final$RMSE[forecast_eval_final$DA=="weekly"& forecast_eval_final$season=="winter2"])
mean(forecast_eval_final$RMSE[forecast_eval_final$DA=="fortnightly"& forecast_eval_final$season=="winter2"])
mean(forecast_eval_final$RMSE[forecast_eval_final$DA=="monthly"& forecast_eval_final$season=="winter2"])

mean(forecast_eval_final$RMSE[forecast_eval_final$DA=="daily"& forecast_eval_final$season=="spring2"])
mean(forecast_eval_final$RMSE[forecast_eval_final$DA=="2day"& forecast_eval_final$season=="spring2"])
mean(forecast_eval_final$RMSE[forecast_eval_final$DA=="5day"& forecast_eval_final$season=="spring2"])
mean(forecast_eval_final$RMSE[forecast_eval_final$DA=="weekly"& forecast_eval_final$season=="spring2"])
mean(forecast_eval_final$RMSE[forecast_eval_final$DA=="fortnightly"& forecast_eval_final$season=="spring2"])
mean(forecast_eval_final$RMSE[forecast_eval_final$DA=="monthly"& forecast_eval_final$season=="spring2"])

mean(forecast_eval_final$RMSE[forecast_eval_final$DA=="daily"& forecast_eval_final$season=="fall2"])
mean(forecast_eval_final$RMSE[forecast_eval_final$DA=="2day"& forecast_eval_final$season=="fall2"])
mean(forecast_eval_final$RMSE[forecast_eval_final$DA=="5day"& forecast_eval_final$season=="fall2"])
mean(forecast_eval_final$RMSE[forecast_eval_final$DA=="weekly"& forecast_eval_final$season=="fall2"])
mean(forecast_eval_final$RMSE[forecast_eval_final$DA=="fortnightly"& forecast_eval_final$season=="fall2"])
mean(forecast_eval_final$RMSE[forecast_eval_final$DA=="monthly"& forecast_eval_final$season=="fall2"])

