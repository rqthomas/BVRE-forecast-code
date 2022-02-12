pacman::p_load(ggplot2, hydroGOF, scoringRules, dplyr, tidyverse)

lake_directory <- here::here()
configure_run_file <- "configure_run.yml"
config <- FLAREr::set_configuration(configure_run_file,lake_directory)

#######################################################
#Read in observations and simulation results for SUMMER
output_daily <- read_csv(file.path(config$file_path$forecast_output_directory, "DA_experiments/summer/daily/bvre-2021-07-01-bvre_test.csv")) %>% 
  filter(date >= as.Date("2021-07-01")) %>% mutate(day = as.Date(date) - as.Date("2021-07-01")) %>% 
  filter(depth %in% c(0.1,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0)) %>% filter(!is.na(observed))

output_2days <- read_csv(file.path(config$file_path$forecast_output_directory, "DA_experiments/summer/2days/bvre-2021-07-01-bvre_test.csv")) %>% 
  filter(date >= as.Date("2021-07-01")) %>% mutate(day = as.Date(date) - as.Date("2021-07-01")) %>% 
  filter(depth %in% c(0.1,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0)) %>% filter(!is.na(observed))

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
  filter(depth %in% c(0.1,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0)) %>% filter(!is.na(observed))

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

#plot forecast skill comparison - all depths
#jpeg("./analysis/Figures/RMSE_vs_DAfrequency.jpg", width = 6, height = 4, units = "in",res = 300)
ggplot(forecast_eval_final, aes(as.factor(DA), RMSE, fill=as.factor(DA))) + geom_boxplot() +xlab("") + facet_wrap(~season) +
  theme_light() + theme(text = element_text(size=14), legend.position = "none", axis.text = element_text(size=14, color="black"),
                        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=12)) +
  scale_x_discrete(limits = c("daily","2day","5day","weekly","fortnightly","monthly"))
#dev.off()

#by depth
ggplot(forecast_eval_final, aes(RMSE, Depth, color=DA)) + geom_point(size=4) + ylab("Depth (m)") + facet_wrap(~season) + ylim(c(10,0))+
  theme_light()  + theme(legend.title = element_blank(), text = element_text(size=24), legend.position=c(0.88, 0.13),  axis.text = element_text(size=22, color="black"), legend.text = element_text(size=24)) 

#-------------------------------------------------------------------------------#
## SUMMER2 forecast horizon RMSE figs
f_summer_obs <- read_csv(file.path(config$file_path$forecast_output_directory, "DA_experiments/summer/daily/bvre-2021-07-01-bvre_test.csv")) %>% 
  filter(date >= as.Date("2021-07-01")) %>% mutate(day = as.Date(date) - as.Date("2021-07-01"))

forecast_daily_avg_summer2 <- read_csv(file.path(config$file_path$forecast_output_directory, "DA_experiments/summer/daily/bvre-2021-07-01-bvre_test.csv")) %>% 
  filter(date >= as.Date("2021-07-01")) %>% mutate(day = as.Date(date) - as.Date("2021-07-01"))  %>% filter(!is.na(observed)) %>%
  select(forecast_mean, day, observed) %>% group_by(day) %>% summarise(avg_wc_temp = mean(forecast_mean), avg_wc_obs = mean(observed)) %>%
  mutate(season="summer2") %>% mutate(DA="daily") 

forecast_2day_avg_summer2 <-read_csv(file.path(config$file_path$forecast_output_directory, "DA_experiments/summer/2days/bvre-2021-07-01-bvre_test.csv")) %>% 
  filter(date >= as.Date("2021-07-01")) %>% mutate(day = as.Date(date) - as.Date("2021-07-01")) %>% mutate(observed = f_summer_obs$observed) %>% filter(!is.na(observed)) %>%
  select(forecast_mean, day, observed) %>% group_by(day) %>% summarise(avg_wc_temp = mean(forecast_mean), avg_wc_obs = mean(observed)) %>%
  mutate(season="summer2") %>% mutate(DA="2day") 

forecast_5day_avg_summer2 <- read_csv(file.path(config$file_path$forecast_output_directory, "DA_experiments/summer/5days/bvre-2021-07-01-bvre_test.csv")) %>% 
  filter(date >= as.Date("2021-07-01")) %>% mutate(day = as.Date(date) - as.Date("2021-07-01"))%>% mutate(observed = f_summer_obs$observed) %>%  filter(!is.na(observed)) %>%
  select(forecast_mean, day, observed) %>% group_by(day) %>% summarise(avg_wc_temp = mean(forecast_mean), avg_wc_obs = mean(observed)) %>%
  mutate(season="summer2") %>% mutate(DA="5day")

forecast_weekly_avg_summer2 <- read_csv(file.path(config$file_path$forecast_output_directory, "DA_experiments/summer/weekly/bvre-2021-07-01-bvre_test.csv")) %>% 
  filter(date >= as.Date("2021-07-01")) %>% mutate(day = as.Date(date) - as.Date("2021-07-01"))%>% mutate(observed = f_summer_obs$observed) %>%  filter(!is.na(observed)) %>%
  select(forecast_mean, day, observed) %>% group_by(day) %>% summarise(avg_wc_temp = mean(forecast_mean), avg_wc_obs = mean(observed)) %>%
  mutate(season="summer2") %>% mutate(DA="weekly")

forecast_fortnightly_avg_summer2 <- read_csv(file.path(config$file_path$forecast_output_directory, "DA_experiments/summer/fortnightly/bvre-2021-07-01-bvre_test.csv")) %>% 
  filter(date >= as.Date("2021-07-01")) %>% mutate(day = as.Date(date) - as.Date("2021-07-01")) %>% mutate(observed = f_summer_obs$observed)%>%  filter(!is.na(observed)) %>%
  select(forecast_mean, day, observed) %>% group_by(day) %>% summarise(avg_wc_temp = mean(forecast_mean), avg_wc_obs = mean(observed)) %>%
  mutate(season="summer2") %>% mutate(DA="fortnightly")

forecast_monthly_avg_summer2 <- read_csv(file.path(config$file_path$forecast_output_directory, "DA_experiments/summer/monthly/bvre-2021-07-01-bvre_test.csv")) %>% 
  filter(date >= as.Date("2021-07-01")) %>% mutate(day = as.Date(date) - as.Date("2021-07-01")) %>% mutate(observed = f_summer_obs$observed)%>%  filter(!is.na(observed)) %>%
  select(forecast_mean, day, observed) %>% group_by(day) %>% summarise(avg_wc_temp = mean(forecast_mean), avg_wc_obs = mean(observed)) %>%
  mutate(season="summer2") %>% mutate(DA="monthly")

#dfs to calculate avg rmse for each forecast horizon
forecast_daily_avg_summer2_RMSE <- data.frame(day=unique(forecast_daily_avg_summer2$day), RMSE=NA)
for(i in 1:length(unique(forecast_daily_avg_summer2$day))){
  forecast_daily_avg_summer2_RMSE[i,2] <- hydroGOF::rmse(forecast_daily_avg_summer2$avg_wc_temp[forecast_daily_avg_summer2$day[i]==unique(forecast_daily_avg_summer2$day)],
                                             forecast_daily_avg_summer2$avg_wc_obs[forecast_daily_avg_summer2$day[i]==unique(forecast_daily_avg_summer2$day)])
}

forecast_2day_avg_summer2_RMSE <- data.frame(day=unique(forecast_2day_avg_summer2$day), RMSE=NA)
for(i in 1:length(unique(forecast_2day_avg_summer2$day))){
  forecast_2day_avg_summer2_RMSE[i,2] <- hydroGOF::rmse(forecast_2day_avg_summer2$avg_wc_temp[forecast_2day_avg_summer2$day[i]==unique(forecast_2day_avg_summer2$day)],
                                                         forecast_2day_avg_summer2$avg_wc_obs[forecast_2day_avg_summer2$day[i]==unique(forecast_2day_avg_summer2$day)])
}

forecast_5day_avg_summer2_RMSE <- data.frame(day=unique(forecast_5day_avg_summer2$day), RMSE=NA)
for(i in 1:length(unique(forecast_5day_avg_summer2$day))){
  forecast_5day_avg_summer2_RMSE[i,2] <- hydroGOF::rmse(forecast_5day_avg_summer2$avg_wc_temp[forecast_5day_avg_summer2$day[i]==unique(forecast_5day_avg_summer2$day)],
                                                        forecast_5day_avg_summer2$avg_wc_obs[forecast_5day_avg_summer2$day[i]==unique(forecast_5day_avg_summer2$day)])
}

forecast_weekly_avg_summer2_RMSE <- data.frame(day=unique(forecast_weekly_avg_summer2$day), RMSE=NA)
for(i in 1:length(unique(forecast_weekly_avg_summer2$day))){
  forecast_weekly_avg_summer2_RMSE[i,2] <- hydroGOF::rmse(forecast_weekly_avg_summer2$avg_wc_temp[forecast_weekly_avg_summer2$day[i]==unique(forecast_weekly_avg_summer2$day)],
                                                          forecast_weekly_avg_summer2$avg_wc_obs[forecast_weekly_avg_summer2$day[i]==unique(forecast_weekly_avg_summer2$day)])
}

forecast_fortnightly_avg_summer2_RMSE <- data.frame(day=unique(forecast_fortnightly_avg_summer2$day), RMSE=NA)
for(i in 1:length(unique(forecast_fortnightly_avg_summer2$day))){
  forecast_fortnightly_avg_summer2_RMSE[i,2] <- hydroGOF::rmse(forecast_fortnightly_avg_summer2$avg_wc_temp[forecast_fortnightly_avg_summer2$day[i]==unique(forecast_fortnightly_avg_summer2$day)],
                                                               forecast_fortnightly_avg_summer2$avg_wc_obs[forecast_fortnightly_avg_summer2$day[i]==unique(forecast_fortnightly_avg_summer2$day)])
}

forecast_monthly_avg_summer2_RMSE <- data.frame(day=unique(forecast_monthly_avg_summer2$day), RMSE=NA)
for(i in 1:length(unique(forecast_monthly_avg_summer2$day))){
  forecast_monthly_avg_summer2_RMSE[i,2] <- hydroGOF::rmse(forecast_monthly_avg_summer2$avg_wc_temp[forecast_monthly_avg_summer2$day[i]==unique(forecast_monthly_avg_summer2$day)],
                                                           forecast_monthly_avg_summer2$avg_wc_obs[forecast_monthly_avg_summer2$day[i]==unique(forecast_monthly_avg_summer2$day)])
}


#add forecast DA frequency 
forecast_daily_avg_summer2_RMSE$DA <- "daily"
forecast_2day_avg_summer2_RMSE$DA <- "2day"
forecast_5day_avg_summer2_RMSE$DA <- "5day"
forecast_weekly_avg_summer2_RMSE$DA <- "weekly"
forecast_fortnightly_avg_summer2_RMSE$DA <- "fortnightly"
forecast_monthly_avg_summer2_RMSE$DA <- "monthly"

#combine all DA dfs
summer2_avg_rmse <- rbind(forecast_daily_avg_summer2_RMSE,forecast_2day_avg_summer2_RMSE,forecast_5day_avg_summer2_RMSE,
                          forecast_weekly_avg_summer2_RMSE,forecast_fortnightly_avg_summer2_RMSE,forecast_monthly_avg_summer2_RMSE)

#order 
summer2_avg_rmse$DA <- ordered(summer2_avg_rmse$DA,levels=c("daily","2day","5day","weekly","fortnightly","monthly"))

#plot avg rmse vs forecast horizon
#jpeg("./analysis/Figures/RMSE_vs_horizon_summer2.jpg", width = 6, height = 4, units = "in",res = 300)
ggplot(summer2_avg_rmse, aes(as.factor(day),RMSE,color=as.factor(DA))) + geom_point(size=4) + xlab("forecast horizon (days)") + theme_bw()  + 
  theme(legend.title = element_blank(),axis.text.x=element_text(size=7) ,text = element_text(size=11),legend.position = c(0.91,0.18), 
        legend.background = element_rect(fill='transparent'), legend.box.background = element_blank(), legend.key = element_rect(fill='transparent'),
        legend.direction = "horizontal", panel.grid.major = element_blank(),panel.grid.minor = element_blank(), legend.key.size = unit(0.5, "cm")) + 
  geom_line(aes(group=DA, color=DA)) + guides(color=guide_legend(nrow=6))
#dev.off()

#####################################
## WINTER2 forecast horizon RMSE figs
f_winter_obs <- read_csv(file.path(config$file_path$forecast_output_directory, "DA_experiments/winter/daily/bvre-2021-01-01-bvre_test.csv")) %>% 
  filter(date >= as.Date("2021-01-01")) %>% mutate(day = as.Date(date) - as.Date("2021-01-01"))

forecast_daily_avg_winter2 <- read_csv(file.path(config$file_path$forecast_output_directory, "DA_experiments/winter/daily/bvre-2021-01-01-bvre_test.csv")) %>% 
  filter(date >= as.Date("2021-01-01")) %>% mutate(day = as.Date(date) - as.Date("2021-01-01"))  %>% filter(!is.na(observed)) %>%
  select(forecast_mean, day, observed) %>% group_by(day) %>% summarise(avg_wc_temp = mean(forecast_mean), avg_wc_obs = mean(observed)) %>%
  mutate(season="winter2") %>% mutate(DA="daily") 

forecast_2day_avg_winter2 <-read_csv(file.path(config$file_path$forecast_output_directory, "DA_experiments/winter/2days/bvre-2021-01-01-bvre_test.csv")) %>% 
  filter(date >= as.Date("2021-01-01")) %>% mutate(day = as.Date(date) - as.Date("2021-01-01")) %>% mutate(observed = f_winter_obs$observed) %>% filter(!is.na(observed)) %>%
  select(forecast_mean, day, observed) %>% group_by(day) %>% summarise(avg_wc_temp = mean(forecast_mean), avg_wc_obs = mean(observed)) %>%
  mutate(season="winter2") %>% mutate(DA="2day") 

forecast_5day_avg_winter2 <- read_csv(file.path(config$file_path$forecast_output_directory, "DA_experiments/winter/5days/bvre-2021-01-01-bvre_test.csv")) %>% 
  filter(date >= as.Date("2021-01-01")) %>% mutate(day = as.Date(date) - as.Date("2021-01-01"))%>% mutate(observed = f_winter_obs$observed) %>%  filter(!is.na(observed)) %>%
  select(forecast_mean, day, observed) %>% group_by(day) %>% summarise(avg_wc_temp = mean(forecast_mean), avg_wc_obs = mean(observed)) %>%
  mutate(season="winter2") %>% mutate(DA="5day")

forecast_weekly_avg_winter2 <- read_csv(file.path(config$file_path$forecast_output_directory, "DA_experiments/winter/weekly/bvre-2021-01-01-bvre_test.csv")) %>% 
  filter(date >= as.Date("2021-01-01")) %>% mutate(day = as.Date(date) - as.Date("2021-01-01"))%>% mutate(observed = f_winter_obs$observed) %>%  filter(!is.na(observed)) %>%
  select(forecast_mean, day, observed) %>% group_by(day) %>% summarise(avg_wc_temp = mean(forecast_mean), avg_wc_obs = mean(observed)) %>%
  mutate(season="winter2") %>% mutate(DA="weekly")

forecast_fortnightly_avg_winter2 <- read_csv(file.path(config$file_path$forecast_output_directory, "DA_experiments/winter/fortnightly/bvre-2021-01-01-bvre_test.csv")) %>% 
  filter(date >= as.Date("2021-01-01")) %>% mutate(day = as.Date(date) - as.Date("2021-01-01")) %>% mutate(observed = f_winter_obs$observed)%>%  filter(!is.na(observed)) %>%
  select(forecast_mean, day, observed) %>% group_by(day) %>% summarise(avg_wc_temp = mean(forecast_mean), avg_wc_obs = mean(observed)) %>%
  mutate(season="winter2") %>% mutate(DA="fortnightly")

forecast_monthly_avg_winter2 <- read_csv(file.path(config$file_path$forecast_output_directory, "DA_experiments/winter/monthly/bvre-2021-01-01-bvre_test.csv")) %>% 
  filter(date >= as.Date("2021-01-01")) %>% mutate(day = as.Date(date) - as.Date("2021-01-01")) %>% mutate(observed = f_winter_obs$observed)%>%  filter(!is.na(observed)) %>%
  select(forecast_mean, day, observed) %>% group_by(day) %>% summarise(avg_wc_temp = mean(forecast_mean), avg_wc_obs = mean(observed)) %>%
  mutate(season="winter2") %>% mutate(DA="monthly")

#dfs to calculate avg rmse for each forecast horizon
forecast_daily_avg_winter2_RMSE <- data.frame(day=unique(forecast_daily_avg_winter2$day), RMSE=NA)
for(i in 1:length(unique(forecast_daily_avg_winter2$day))){
  forecast_daily_avg_winter2_RMSE[i,2] <- hydroGOF::rmse(forecast_daily_avg_winter2$avg_wc_temp[forecast_daily_avg_winter2$day[i]==unique(forecast_daily_avg_winter2$day)],
                                                         forecast_daily_avg_winter2$avg_wc_obs[forecast_daily_avg_winter2$day[i]==unique(forecast_daily_avg_winter2$day)])
}

forecast_2day_avg_winter2_RMSE <- data.frame(day=unique(forecast_2day_avg_winter2$day), RMSE=NA)
for(i in 1:length(unique(forecast_2day_avg_winter2$day))){
  forecast_2day_avg_winter2_RMSE[i,2] <- hydroGOF::rmse(forecast_2day_avg_winter2$avg_wc_temp[forecast_2day_avg_winter2$day[i]==unique(forecast_2day_avg_winter2$day)],
                                                        forecast_2day_avg_winter2$avg_wc_obs[forecast_2day_avg_winter2$day[i]==unique(forecast_2day_avg_winter2$day)])
}

forecast_5day_avg_winter2_RMSE <- data.frame(day=unique(forecast_5day_avg_winter2$day), RMSE=NA)
for(i in 1:length(unique(forecast_5day_avg_winter2$day))){
  forecast_5day_avg_winter2_RMSE[i,2] <- hydroGOF::rmse(forecast_5day_avg_winter2$avg_wc_temp[forecast_5day_avg_winter2$day[i]==unique(forecast_5day_avg_winter2$day)],
                                                        forecast_5day_avg_winter2$avg_wc_obs[forecast_5day_avg_winter2$day[i]==unique(forecast_5day_avg_winter2$day)])
}

forecast_weekly_avg_winter2_RMSE <- data.frame(day=unique(forecast_weekly_avg_winter2$day), RMSE=NA)
for(i in 1:length(unique(forecast_weekly_avg_winter2$day))){
  forecast_weekly_avg_winter2_RMSE[i,2] <- hydroGOF::rmse(forecast_weekly_avg_winter2$avg_wc_temp[forecast_weekly_avg_winter2$day[i]==unique(forecast_weekly_avg_winter2$day)],
                                                          forecast_weekly_avg_winter2$avg_wc_obs[forecast_weekly_avg_winter2$day[i]==unique(forecast_weekly_avg_winter2$day)])
}

forecast_fortnightly_avg_winter2_RMSE <- data.frame(day=unique(forecast_fortnightly_avg_winter2$day), RMSE=NA)
for(i in 1:length(unique(forecast_fortnightly_avg_winter2$day))){
  forecast_fortnightly_avg_winter2_RMSE[i,2] <- hydroGOF::rmse(forecast_fortnightly_avg_winter2$avg_wc_temp[forecast_fortnightly_avg_winter2$day[i]==unique(forecast_fortnightly_avg_winter2$day)],
                                                               forecast_fortnightly_avg_winter2$avg_wc_obs[forecast_fortnightly_avg_winter2$day[i]==unique(forecast_fortnightly_avg_winter2$day)])
}

forecast_monthly_avg_winter2_RMSE <- data.frame(day=unique(forecast_monthly_avg_winter2$day), RMSE=NA)
for(i in 1:length(unique(forecast_monthly_avg_winter2$day))){
  forecast_monthly_avg_winter2_RMSE[i,2] <- hydroGOF::rmse(forecast_monthly_avg_winter2$avg_wc_temp[forecast_monthly_avg_winter2$day[i]==unique(forecast_monthly_avg_winter2$day)],
                                                           forecast_monthly_avg_winter2$avg_wc_obs[forecast_monthly_avg_winter2$day[i]==unique(forecast_monthly_avg_winter2$day)])
}


#add forecast DA frequency 
forecast_daily_avg_winter2_RMSE$DA <- "daily"
forecast_2day_avg_winter2_RMSE$DA <- "2day"
forecast_5day_avg_winter2_RMSE$DA <- "5day"
forecast_weekly_avg_winter2_RMSE$DA <- "weekly"
forecast_fortnightly_avg_winter2_RMSE$DA <- "fortnightly"
forecast_monthly_avg_winter2_RMSE$DA <- "monthly"

#combine all DA dfs
winter2_avg_rmse <- rbind(forecast_daily_avg_winter2_RMSE,forecast_2day_avg_winter2_RMSE,forecast_5day_avg_winter2_RMSE,
                          forecast_weekly_avg_winter2_RMSE,forecast_fortnightly_avg_winter2_RMSE,forecast_monthly_avg_winter2_RMSE)

#order 
winter2_avg_rmse$DA <- ordered(winter2_avg_rmse$DA,levels=c("daily","2day","5day","weekly","fortnightly","monthly"))

#plot avg rmse vs forecast horizon
#jpeg("./analysis/Figures/RMSE_vs_horizon_winter2.jpg", width = 6, height = 4, units = "in",res = 300)
ggplot(winter2_avg_rmse, aes(as.factor(day),RMSE,color=as.factor(DA))) + geom_point(size=4) + xlab("forecast horizon (days)") + theme_bw()  + 
  theme(legend.title = element_blank(),axis.text.x=element_text(size=7) ,text = element_text(size=11),legend.position = c(0.09,0.82), 
        legend.background = element_rect(fill='transparent'), legend.box.background = element_blank(), legend.key = element_rect(fill='transparent'),
        legend.direction = "horizontal", panel.grid.major = element_blank(),panel.grid.minor = element_blank(), legend.key.size = unit(0.5, "cm")) + 
  geom_line(aes(group=DA, color=DA)) + guides(color=guide_legend(nrow=6))
#dev.off()

#####################################
## SPRING2 forecast horizon RMSE figs
f_spring_obs <- read_csv(file.path(config$file_path$forecast_output_directory, "DA_experiments/spring/daily/bvre-2021-04-01-bvre_test.csv")) %>% 
  filter(date >= as.Date("2021-04-01")) %>% mutate(day = as.Date(date) - as.Date("2021-04-01"))

forecast_daily_avg_spring2 <- read_csv(file.path(config$file_path$forecast_output_directory, "DA_experiments/spring/daily/bvre-2021-04-01-bvre_test.csv")) %>% 
  filter(date >= as.Date("2021-04-01")) %>% mutate(day = as.Date(date) - as.Date("2021-04-01"))  %>% filter(!is.na(observed)) %>%
  select(forecast_mean, day, observed) %>% group_by(day) %>% summarise(avg_wc_temp = mean(forecast_mean), avg_wc_obs = mean(observed)) %>%
  mutate(season="spring2") %>% mutate(DA="daily") 

forecast_2day_avg_spring2 <-read_csv(file.path(config$file_path$forecast_output_directory, "DA_experiments/spring/2days/bvre-2021-04-01-bvre_test.csv")) %>% 
  filter(date >= as.Date("2021-04-01")) %>% mutate(day = as.Date(date) - as.Date("2021-04-01")) %>% mutate(observed = f_spring_obs$observed) %>% filter(!is.na(observed)) %>%
  select(forecast_mean, day, observed) %>% group_by(day) %>% summarise(avg_wc_temp = mean(forecast_mean), avg_wc_obs = mean(observed)) %>%
  mutate(season="spring2") %>% mutate(DA="2day") 

forecast_5day_avg_spring2 <- read_csv(file.path(config$file_path$forecast_output_directory, "DA_experiments/spring/5days/bvre-2021-04-01-bvre_test.csv")) %>% 
  filter(date >= as.Date("2021-04-01")) %>% mutate(day = as.Date(date) - as.Date("2021-04-01"))%>% mutate(observed = f_spring_obs$observed) %>%  filter(!is.na(observed)) %>%
  select(forecast_mean, day, observed) %>% group_by(day) %>% summarise(avg_wc_temp = mean(forecast_mean), avg_wc_obs = mean(observed)) %>%
  mutate(season="spring2") %>% mutate(DA="5day")

forecast_weekly_avg_spring2 <- read_csv(file.path(config$file_path$forecast_output_directory, "DA_experiments/spring/weekly/bvre-2021-04-01-bvre_test.csv")) %>% 
  filter(date >= as.Date("2021-04-01")) %>% mutate(day = as.Date(date) - as.Date("2021-04-01"))%>% mutate(observed = f_spring_obs$observed) %>%  filter(!is.na(observed)) %>%
  select(forecast_mean, day, observed) %>% group_by(day) %>% summarise(avg_wc_temp = mean(forecast_mean), avg_wc_obs = mean(observed)) %>%
  mutate(season="spring2") %>% mutate(DA="weekly")

forecast_fortnightly_avg_spring2 <- read_csv(file.path(config$file_path$forecast_output_directory, "DA_experiments/spring/fortnightly/bvre-2021-04-01-bvre_test.csv")) %>% 
  filter(date >= as.Date("2021-04-01")) %>% mutate(day = as.Date(date) - as.Date("2021-04-01")) %>% mutate(observed = f_spring_obs$observed)%>%  filter(!is.na(observed)) %>%
  select(forecast_mean, day, observed) %>% group_by(day) %>% summarise(avg_wc_temp = mean(forecast_mean), avg_wc_obs = mean(observed)) %>%
  mutate(season="spring2") %>% mutate(DA="fortnightly")

forecast_monthly_avg_spring2 <- read_csv(file.path(config$file_path$forecast_output_directory, "DA_experiments/spring/monthly/bvre-2021-04-01-bvre_test.csv")) %>% 
  filter(date >= as.Date("2021-04-01")) %>% mutate(day = as.Date(date) - as.Date("2021-04-01")) %>% mutate(observed = f_spring_obs$observed)%>%  filter(!is.na(observed)) %>%
  select(forecast_mean, day, observed) %>% group_by(day) %>% summarise(avg_wc_temp = mean(forecast_mean), avg_wc_obs = mean(observed)) %>%
  mutate(season="spring2") %>% mutate(DA="monthly")

#dfs to calculate avg rmse for each forecast horizon
forecast_daily_avg_spring2_RMSE <- data.frame(day=unique(forecast_daily_avg_spring2$day), RMSE=NA)
for(i in 1:length(unique(forecast_daily_avg_spring2$day))){
  forecast_daily_avg_spring2_RMSE[i,2] <- hydroGOF::rmse(forecast_daily_avg_spring2$avg_wc_temp[forecast_daily_avg_spring2$day[i]==unique(forecast_daily_avg_spring2$day)],
                                                         forecast_daily_avg_spring2$avg_wc_obs[forecast_daily_avg_spring2$day[i]==unique(forecast_daily_avg_spring2$day)])
}

forecast_2day_avg_spring2_RMSE <- data.frame(day=unique(forecast_2day_avg_spring2$day), RMSE=NA)
for(i in 1:length(unique(forecast_2day_avg_spring2$day))){
  forecast_2day_avg_spring2_RMSE[i,2] <- hydroGOF::rmse(forecast_2day_avg_spring2$avg_wc_temp[forecast_2day_avg_spring2$day[i]==unique(forecast_2day_avg_spring2$day)],
                                                        forecast_2day_avg_spring2$avg_wc_obs[forecast_2day_avg_spring2$day[i]==unique(forecast_2day_avg_spring2$day)])
}

forecast_5day_avg_spring2_RMSE <- data.frame(day=unique(forecast_5day_avg_spring2$day), RMSE=NA)
for(i in 1:length(unique(forecast_5day_avg_spring2$day))){
  forecast_5day_avg_spring2_RMSE[i,2] <- hydroGOF::rmse(forecast_5day_avg_spring2$avg_wc_temp[forecast_5day_avg_spring2$day[i]==unique(forecast_5day_avg_spring2$day)],
                                                        forecast_5day_avg_spring2$avg_wc_obs[forecast_5day_avg_spring2$day[i]==unique(forecast_5day_avg_spring2$day)])
}

forecast_weekly_avg_spring2_RMSE <- data.frame(day=unique(forecast_weekly_avg_spring2$day), RMSE=NA)
for(i in 1:length(unique(forecast_weekly_avg_spring2$day))){
  forecast_weekly_avg_spring2_RMSE[i,2] <- hydroGOF::rmse(forecast_weekly_avg_spring2$avg_wc_temp[forecast_weekly_avg_spring2$day[i]==unique(forecast_weekly_avg_spring2$day)],
                                                          forecast_weekly_avg_spring2$avg_wc_obs[forecast_weekly_avg_spring2$day[i]==unique(forecast_weekly_avg_spring2$day)])
}

forecast_fortnightly_avg_spring2_RMSE <- data.frame(day=unique(forecast_fortnightly_avg_spring2$day), RMSE=NA)
for(i in 1:length(unique(forecast_fortnightly_avg_spring2$day))){
  forecast_fortnightly_avg_spring2_RMSE[i,2] <- hydroGOF::rmse(forecast_fortnightly_avg_spring2$avg_wc_temp[forecast_fortnightly_avg_spring2$day[i]==unique(forecast_fortnightly_avg_spring2$day)],
                                                               forecast_fortnightly_avg_spring2$avg_wc_obs[forecast_fortnightly_avg_spring2$day[i]==unique(forecast_fortnightly_avg_spring2$day)])
}

forecast_monthly_avg_spring2_RMSE <- data.frame(day=unique(forecast_monthly_avg_spring2$day), RMSE=NA)
for(i in 1:length(unique(forecast_monthly_avg_spring2$day))){
  forecast_monthly_avg_spring2_RMSE[i,2] <- hydroGOF::rmse(forecast_monthly_avg_spring2$avg_wc_temp[forecast_monthly_avg_spring2$day[i]==unique(forecast_monthly_avg_spring2$day)],
                                                           forecast_monthly_avg_spring2$avg_wc_obs[forecast_monthly_avg_spring2$day[i]==unique(forecast_monthly_avg_spring2$day)])
}


#add forecast DA frequency 
forecast_daily_avg_spring2_RMSE$DA <- "daily"
forecast_2day_avg_spring2_RMSE$DA <- "2day"
forecast_5day_avg_spring2_RMSE$DA <- "5day"
forecast_weekly_avg_spring2_RMSE$DA <- "weekly"
forecast_fortnightly_avg_spring2_RMSE$DA <- "fortnightly"
forecast_monthly_avg_spring2_RMSE$DA <- "monthly"

#combine all DA dfs
spring2_avg_rmse <- rbind(forecast_daily_avg_spring2_RMSE,forecast_2day_avg_spring2_RMSE,forecast_5day_avg_spring2_RMSE,
                          forecast_weekly_avg_spring2_RMSE,forecast_fortnightly_avg_spring2_RMSE,forecast_monthly_avg_spring2_RMSE)

#order 
spring2_avg_rmse$DA <- ordered(spring2_avg_rmse$DA,levels=c("daily","2day","5day","weekly","fortnightly","monthly"))

#plot avg rmse vs forecast horizon
#jpeg("./analysis/Figures/RMSE_vs_horizon_spring2.jpg", width = 6, height = 4, units = "in",res = 300)
ggplot(spring2_avg_rmse, aes(as.factor(day),RMSE,color=as.factor(DA))) + geom_point(size=4) + xlab("forecast horizon (days)") + theme_bw()  + 
  theme(legend.title = element_blank(),axis.text.x=element_text(size=7) ,text = element_text(size=11),legend.position = c(0.09,0.82), 
        legend.background = element_rect(fill='transparent'), legend.box.background = element_blank(), legend.key = element_rect(fill='transparent'),
        legend.direction = "horizontal", panel.grid.major = element_blank(),panel.grid.minor = element_blank(), legend.key.size = unit(0.5, "cm")) + 
  geom_line(aes(group=DA, color=DA)) + guides(color=guide_legend(nrow=6))
#dev.off()

#####################################
## FALL2 forecast horizon RMSE figs
f_fall2_obs <- read_csv(file.path(config$file_path$forecast_output_directory, "DA_experiments/fall/daily/bvre-2021-10-01-bvre_test.csv")) %>% 
  filter(date >= as.Date("2021-10-01")) %>% mutate(day = as.Date(date) - as.Date("2021-10-01"))

forecast_daily_avg_fall2 <- read_csv(file.path(config$file_path$forecast_output_directory, "DA_experiments/fall/daily/bvre-2021-10-01-bvre_test.csv")) %>% 
  filter(date >= as.Date("2021-10-01")) %>% mutate(day = as.Date(date) - as.Date("2021-10-01"))  %>% filter(!is.na(observed)) %>%
  select(forecast_mean, day, observed) %>% group_by(day) %>% summarise(avg_wc_temp = mean(forecast_mean), avg_wc_obs = mean(observed)) %>%
  mutate(season="fall2") %>% mutate(DA="daily") 

forecast_2day_avg_fall2 <-read_csv(file.path(config$file_path$forecast_output_directory, "DA_experiments/fall/2days/bvre-2021-10-01-bvre_test.csv")) %>% 
  filter(date >= as.Date("2021-10-01")) %>% mutate(day = as.Date(date) - as.Date("2021-10-01")) %>% mutate(observed = f_fall2_obs$observed) %>% filter(!is.na(observed)) %>%
  select(forecast_mean, day, observed) %>% group_by(day) %>% summarise(avg_wc_temp = mean(forecast_mean), avg_wc_obs = mean(observed)) %>%
  mutate(season="fall2") %>% mutate(DA="2day") 

forecast_5day_avg_fall2 <- read_csv(file.path(config$file_path$forecast_output_directory, "DA_experiments/fall/5days/bvre-2021-10-01-bvre_test.csv")) %>% 
  filter(date >= as.Date("2021-10-01")) %>% mutate(day = as.Date(date) - as.Date("2021-10-01"))%>% mutate(observed = f_fall2_obs$observed) %>%  filter(!is.na(observed)) %>%
  select(forecast_mean, day, observed) %>% group_by(day) %>% summarise(avg_wc_temp = mean(forecast_mean), avg_wc_obs = mean(observed)) %>%
  mutate(season="fall2") %>% mutate(DA="5day")

forecast_weekly_avg_fall2 <- read_csv(file.path(config$file_path$forecast_output_directory, "DA_experiments/fall/weekly/bvre-2021-10-01-bvre_test.csv")) %>% 
  filter(date >= as.Date("2021-10-01")) %>% mutate(day = as.Date(date) - as.Date("2021-10-01"))%>% mutate(observed = f_fall2_obs$observed) %>%  filter(!is.na(observed)) %>%
  select(forecast_mean, day, observed) %>% group_by(day) %>% summarise(avg_wc_temp = mean(forecast_mean), avg_wc_obs = mean(observed)) %>%
  mutate(season="fall2") %>% mutate(DA="weekly")

forecast_fortnightly_avg_fall2 <- read_csv(file.path(config$file_path$forecast_output_directory, "DA_experiments/fall/fortnightly/bvre-2021-10-01-bvre_test.csv")) %>% 
  filter(date >= as.Date("2021-10-01")) %>% mutate(day = as.Date(date) - as.Date("2021-10-01")) %>% mutate(observed = f_fall2_obs$observed)%>%  filter(!is.na(observed)) %>%
  select(forecast_mean, day, observed) %>% group_by(day) %>% summarise(avg_wc_temp = mean(forecast_mean), avg_wc_obs = mean(observed)) %>%
  mutate(season="fall2") %>% mutate(DA="fortnightly")

forecast_monthly_avg_fall2 <- read_csv(file.path(config$file_path$forecast_output_directory, "DA_experiments/fall/monthly/bvre-2021-10-01-bvre_test.csv")) %>% 
  filter(date >= as.Date("2021-10-01")) %>% mutate(day = as.Date(date) - as.Date("2021-10-01")) %>% mutate(observed = f_fall2_obs$observed)%>%  filter(!is.na(observed)) %>%
  select(forecast_mean, day, observed) %>% group_by(day) %>% summarise(avg_wc_temp = mean(forecast_mean), avg_wc_obs = mean(observed)) %>%
  mutate(season="fall2") %>% mutate(DA="monthly")

#dfs to calculate avg rmse for each forecast horizon
forecast_daily_avg_fall2_RMSE <- data.frame(day=unique(forecast_daily_avg_fall2$day), RMSE=NA)
for(i in 1:length(unique(forecast_daily_avg_fall2$day))){
  forecast_daily_avg_fall2_RMSE[i,2] <- hydroGOF::rmse(forecast_daily_avg_fall2$avg_wc_temp[forecast_daily_avg_fall2$day[i]==unique(forecast_daily_avg_fall2$day)],
                                                       forecast_daily_avg_fall2$avg_wc_obs[forecast_daily_avg_fall2$day[i]==unique(forecast_daily_avg_fall2$day)])
}

forecast_2day_avg_fall2_RMSE <- data.frame(day=unique(forecast_2day_avg_fall2$day), RMSE=NA)
for(i in 1:length(unique(forecast_2day_avg_fall2$day))){
  forecast_2day_avg_fall2_RMSE[i,2] <- hydroGOF::rmse(forecast_2day_avg_fall2$avg_wc_temp[forecast_2day_avg_fall2$day[i]==unique(forecast_2day_avg_fall2$day)],
                                                      forecast_2day_avg_fall2$avg_wc_obs[forecast_2day_avg_fall2$day[i]==unique(forecast_2day_avg_fall2$day)])
}

forecast_5day_avg_fall2_RMSE <- data.frame(day=unique(forecast_5day_avg_fall2$day), RMSE=NA)
for(i in 1:length(unique(forecast_5day_avg_fall2$day))){
  forecast_5day_avg_fall2_RMSE[i,2] <- hydroGOF::rmse(forecast_5day_avg_fall2$avg_wc_temp[forecast_5day_avg_fall2$day[i]==unique(forecast_5day_avg_fall2$day)],
                                                      forecast_5day_avg_fall2$avg_wc_obs[forecast_5day_avg_fall2$day[i]==unique(forecast_5day_avg_fall2$day)])
}

forecast_weekly_avg_fall2_RMSE <- data.frame(day=unique(forecast_weekly_avg_fall2$day), RMSE=NA)
for(i in 1:length(unique(forecast_weekly_avg_fall2$day))){
  forecast_weekly_avg_fall2_RMSE[i,2] <- hydroGOF::rmse(forecast_weekly_avg_fall2$avg_wc_temp[forecast_weekly_avg_fall2$day[i]==unique(forecast_weekly_avg_fall2$day)],
                                                        forecast_weekly_avg_fall2$avg_wc_obs[forecast_weekly_avg_fall2$day[i]==unique(forecast_weekly_avg_fall2$day)])
}

forecast_fortnightly_avg_fall2_RMSE <- data.frame(day=unique(forecast_fortnightly_avg_fall2$day), RMSE=NA)
for(i in 1:length(unique(forecast_fortnightly_avg_fall2$day))){
  forecast_fortnightly_avg_fall2_RMSE[i,2] <- hydroGOF::rmse(forecast_fortnightly_avg_fall2$avg_wc_temp[forecast_fortnightly_avg_fall2$day[i]==unique(forecast_fortnightly_avg_fall2$day)],
                                                             forecast_fortnightly_avg_fall2$avg_wc_obs[forecast_fortnightly_avg_fall2$day[i]==unique(forecast_fortnightly_avg_fall2$day)])
}

forecast_monthly_avg_fall2_RMSE <- data.frame(day=unique(forecast_monthly_avg_fall2$day), RMSE=NA)
for(i in 1:length(unique(forecast_monthly_avg_fall2$day))){
  forecast_monthly_avg_fall2_RMSE[i,2] <- hydroGOF::rmse(forecast_monthly_avg_fall2$avg_wc_temp[forecast_monthly_avg_fall2$day[i]==unique(forecast_monthly_avg_fall2$day)],
                                                         forecast_monthly_avg_fall2$avg_wc_obs[forecast_monthly_avg_fall2$day[i]==unique(forecast_monthly_avg_fall2$day)])
}


#add forecast DA frequency 
forecast_daily_avg_fall2_RMSE$DA <- "daily"
forecast_2day_avg_fall2_RMSE$DA <- "2day"
forecast_5day_avg_fall2_RMSE$DA <- "5day"
forecast_weekly_avg_fall2_RMSE$DA <- "weekly"
forecast_fortnightly_avg_fall2_RMSE$DA <- "fortnightly"
forecast_monthly_avg_fall2_RMSE$DA <- "monthly"

#combine all DA dfs
fall2_avg_rmse <- rbind(forecast_daily_avg_fall2_RMSE,forecast_2day_avg_fall2_RMSE,forecast_5day_avg_fall2_RMSE,
                          forecast_weekly_avg_fall2_RMSE,forecast_fortnightly_avg_fall2_RMSE,forecast_monthly_avg_fall2_RMSE)

#order 
fall2_avg_rmse$DA <- ordered(fall2_avg_rmse$DA,levels=c("daily","2day","5day","weekly","fortnightly","monthly"))

#plot avg rmse vs forecast horizon
#jpeg("./analysis/Figures/RMSE_vs_horizon_fall2.jpg", width = 6, height = 4, units = "in",res = 300)
ggplot(fall2_avg_rmse, aes(as.factor(day),RMSE,color=as.factor(DA))) + geom_point(size=4) + xlab("forecast horizon (days)") + theme_bw()  + 
  theme(legend.title = element_blank(),axis.text.x=element_text(size=7) ,text = element_text(size=11),legend.position = c(0.09,0.82), 
        legend.background = element_rect(fill='transparent'), legend.box.background = element_blank(), legend.key = element_rect(fill='transparent'),
        legend.direction = "horizontal", panel.grid.major = element_blank(),panel.grid.minor = element_blank(), legend.key.size = unit(0.5, "cm")) + 
  geom_line(aes(group=DA, color=DA)) + guides(color=guide_legend(nrow=6))
#dev.off()

#-------------------------------------------------------------------------------#
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

