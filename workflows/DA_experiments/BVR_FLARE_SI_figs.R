#BVR FLARE SI figs

#inflation parameter figs (fixed, 1.02, 1.04)
#and different start days (27nov, 24nov, 22nov)

#read in all forecast csvs
daily_27nov <- list.files(file.path(lake_directory,"analysis/summary_files/27nov_start/daily"), pattern="csv", full.names=TRUE)
daily_27nov <- lapply(daily_27nov, read_csv) %>% bind_rows() %>% mutate(DA = "Daily")

daily_24nov <- list.files(file.path(lake_directory,"analysis/summary_files/24nov_start/daily"), pattern="csv", full.names=TRUE)
daily_24nov <- lapply(daily_24nov, read_csv) %>% bind_rows() %>% mutate(DA = "Daily")

daily_22nov <- list.files(file.path(lake_directory,"analysis/summary_files/22nov_start/daily"), pattern="csv", full.names=TRUE)
daily_22nov <- lapply(daily_22nov, read_csv) %>% bind_rows() %>% mutate(DA = "Daily")

daily_fixed <- list.files(file.path(lake_directory,"analysis/summary_files/fixed_params/daily"), pattern="csv", full.names=TRUE)
daily_fixed <- lapply(daily_fixed, read_csv) %>% bind_rows() %>% mutate(DA = "Daily")

daily_1.04 <- list.files(file.path(lake_directory,"analysis/summary_files/inflat_1.04/daily"), pattern="csv", full.names=TRUE)
daily_1.04 <- lapply(daily_1.04, read_csv) %>% bind_rows() %>% mutate(DA = "Daily")

weekly_27nov <- list.files(file.path(lake_directory,"analysis/summary_files/27nov_start/weekly"), pattern="csv", full.names=TRUE)
weekly_27nov <- lapply(weekly_27nov, read_csv) %>% bind_rows() %>% mutate(DA = "Weekly")

weekly_24nov <- list.files(file.path(lake_directory,"analysis/summary_files/24nov_start/weekly"), pattern="csv", full.names=TRUE)
weekly_24nov <- lapply(weekly_24nov, read_csv) %>% bind_rows() %>% mutate(DA = "Weekly")

weekly_22nov <- list.files(file.path(lake_directory,"analysis/summary_files/22nov_start/weekly"), pattern="csv", full.names=TRUE)
weekly_22nov <- lapply(weekly_22nov, read_csv) %>% bind_rows() %>% mutate(DA = "Weekly")

weekly_fixed <- list.files(file.path(lake_directory,"analysis/summary_files/fixed_params/weekly"), pattern="csv", full.names=TRUE)
weekly_fixed <- lapply(weekly_fixed, read_csv) %>% bind_rows() %>% mutate(DA = "Weekly")

weekly_1.04 <- list.files(file.path(lake_directory,"analysis/summary_files/inflat_1.04/weekly"), pattern="csv", full.names=TRUE)
weekly_1.04 <- lapply(weekly_1.04, read_csv) %>% bind_rows() %>% mutate(DA = "Weekly")

fortnightly_27nov <- list.files(file.path(lake_directory,"analysis/summary_files/27nov_start/fortnightly"), pattern="csv", full.names=TRUE)
fortnightly_27nov <- lapply(fortnightly_27nov, read_csv) %>% bind_rows() %>% mutate(DA = "Fortnightly")

fortnightly_24nov <- list.files(file.path(lake_directory,"analysis/summary_files/24nov_start/fortnightly"), pattern="csv", full.names=TRUE)
fortnightly_24nov <- lapply(fortnightly_24nov, read_csv) %>% bind_rows() %>% mutate(DA = "Fortnightly")

fortnightly_22nov <- list.files(file.path(lake_directory,"analysis/summary_files/22nov_start/fortnightly"), pattern="csv", full.names=TRUE)
fortnightly_22nov <- lapply(fortnightly_22nov, read_csv) %>% bind_rows() %>% mutate(DA = "Fortnightly")

fortnightly_fixed <- list.files(file.path(lake_directory,"analysis/summary_files/fixed_params/fortnightly"), pattern="csv", full.names=TRUE)
fortnightly_fixed <- lapply(fortnightly_fixed, read_csv) %>% bind_rows() %>% mutate(DA = "Fortnightly")

fortnightly_1.04 <- list.files(file.path(lake_directory,"analysis/summary_files/inflat_1.04/fortnightly"), pattern="csv", full.names=TRUE)
fortnightly_1.04 <- lapply(fortnightly_1.04, read_csv) %>% bind_rows() %>% mutate(DA = "Fortnightly")

monthly_27nov <- list.files(file.path(lake_directory,"analysis/summary_files/27nov_start/monthly"), pattern="csv", full.names=TRUE)
monthly_27nov <- lapply(monthly_27nov, read_csv) %>% bind_rows() %>% mutate(DA = "Monthly")

monthly_24nov <- list.files(file.path(lake_directory,"analysis/summary_files/24nov_start/monthly"), pattern="csv", full.names=TRUE)
monthly_24nov <- lapply(monthly_24nov, read_csv) %>% bind_rows() %>% mutate(DA = "Monthly")

monthly_22nov <- list.files(file.path(lake_directory,"analysis/summary_files/22nov_start/monthly"), pattern="csv", full.names=TRUE)
monthly_22nov <- lapply(monthly_22nov, read_csv) %>% bind_rows() %>% mutate(DA = "Monthly")

monthly_fixed <- list.files(file.path(lake_directory,"analysis/summary_files/fixed_params/monthly"), pattern="csv", full.names=TRUE)
monthly_fixed <- lapply(monthly_fixed, read_csv) %>% bind_rows() %>% mutate(DA = "Monthly")

monthly_1.04 <- list.files(file.path(lake_directory,"analysis/summary_files/inflat_1.04/monthly"), pattern="csv", full.names=TRUE)
monthly_1.04 <- lapply(monthly_1.04, read_csv) %>% bind_rows() %>% mutate(DA = "Monthly")


detach(dplyr)
library(plyr)

#merge all DA frequencies
all_27nov <- rbind(daily_27nov, weekly_27nov, fortnightly_27nov, monthly_27nov)
all_24nov <- rbind(daily_24nov, weekly_24nov, fortnightly_24nov, monthly_24nov)
all_22nov <- rbind(daily_22nov, weekly_22nov, fortnightly_22nov, monthly_22nov)
all_fixed <- rbind(daily_fixed, weekly_fixed, fortnightly_fixed, monthly_fixed)
all_1.04 <- rbind(daily_1.04, weekly_1.04, fortnightly_1.04, monthly_1.04)


#round depths to nearest m
all_27nov$depth <- ceiling(all_27nov$depth)
all_24nov$depth <- ceiling(all_24nov$depth)
all_22nov$depth <- ceiling(all_22nov$depth)
all_fixed$depth <- ceiling(all_fixed$depth)
all_1.04$depth <- ceiling(all_1.04$depth)


#add date column --> forecast_date + horizon
all_27nov$date <- all_27nov$forecast_date + all_27nov$horizon
all_24nov$date <- all_24nov$forecast_date + all_24nov$horizon
all_22nov$date <- all_22nov$forecast_date + all_22nov$horizon
all_fixed$date <- all_fixed$forecast_date + all_fixed$horizon
all_1.04$date <-  all_1.04$forecast_date + all_1.04$horizon

strat_date<- "2021-11-07"

#add stratified vs mixed col
all_27nov$phen <- ifelse(all_27nov$date <= as.Date(strat_date) & 
                                  all_27nov$date >="2021-03-13","Stratified", "Mixed")

all_24nov$phen <- ifelse(all_24nov$date <= as.Date(strat_date) & 
                               all_24nov$date >="2021-03-13","Stratified", "Mixed")

all_22nov$phen <- ifelse(all_22nov$date <= as.Date(strat_date) & 
                                all_22nov$date >="2021-03-13","Stratified", "Mixed")

all_fixed$phen <- ifelse(all_fixed$date <= as.Date(strat_date) & 
                           all_fixed$date >="2021-03-13","Stratified", "Mixed")

all_1.04$phen <- ifelse(all_1.04$date <= as.Date(strat_date) & 
                           all_1.04$date >="2021-03-13","Stratified", "Mixed")


#forecast skill for each depth and horizon
forecast_skill_depth_horizon_27nov <-  plyr::ddply(all_27nov, c("depth", "forecast_date","horizon", "DA"), function(x) {
  data.frame(
    RMSE = sqrt(mean((x$mean - x$obs)^2, na.rm = TRUE)),
    MAE = mean(abs(x$mean - x$obs), na.rm = TRUE),
    pbias = 100 * (sum(x$mean - x$obs, na.rm = TRUE) / sum(x$obs, na.rm = TRUE)),
    CRPS = verification::crps(x$obs, as.matrix(x[, 4:5]))$CRPS
  )
}, .progress = plyr::progress_text(), .parallel = FALSE) 

forecast_skill_depth_horizon_24nov <-  plyr::ddply(all_24nov, c("depth", "forecast_date","horizon", "DA"), function(x) {
  data.frame(
    RMSE = sqrt(mean((x$mean - x$obs)^2, na.rm = TRUE)),
    MAE = mean(abs(x$mean - x$obs), na.rm = TRUE),
    pbias = 100 * (sum(x$mean - x$obs, na.rm = TRUE) / sum(x$obs, na.rm = TRUE)),
    CRPS = verification::crps(x$obs, as.matrix(x[, 4:5]))$CRPS
  )
}, .progress = plyr::progress_text(), .parallel = FALSE) 

forecast_skill_depth_horizon_22nov <-  plyr::ddply(all_22nov, c("depth", "forecast_date","horizon", "DA"), function(x) {
  data.frame(
    RMSE = sqrt(mean((x$mean - x$obs)^2, na.rm = TRUE)),
    MAE = mean(abs(x$mean - x$obs), na.rm = TRUE),
    pbias = 100 * (sum(x$mean - x$obs, na.rm = TRUE) / sum(x$obs, na.rm = TRUE)),
    CRPS = verification::crps(x$obs, as.matrix(x[, 4:5]))$CRPS
  )
}, .progress = plyr::progress_text(), .parallel = FALSE) 

forecast_skill_depth_horizon_fixed <-  plyr::ddply(all_fixed, c("depth", "forecast_date","horizon", "DA"), function(x) {
  data.frame(
    RMSE = sqrt(mean((x$mean - x$obs)^2, na.rm = TRUE)),
    MAE = mean(abs(x$mean - x$obs), na.rm = TRUE),
    pbias = 100 * (sum(x$mean - x$obs, na.rm = TRUE) / sum(x$obs, na.rm = TRUE)),
    CRPS = verification::crps(x$obs, as.matrix(x[, 4:5]))$CRPS
  )
}, .progress = plyr::progress_text(), .parallel = FALSE) 

forecast_skill_depth_horizon_1.04 <-  plyr::ddply(all_1.04, c("depth", "forecast_date","horizon", "DA"), function(x) {
  data.frame(
    RMSE = sqrt(mean((x$mean - x$obs)^2, na.rm = TRUE)),
    MAE = mean(abs(x$mean - x$obs), na.rm = TRUE),
    pbias = 100 * (sum(x$mean - x$obs, na.rm = TRUE) / sum(x$obs, na.rm = TRUE)),
    CRPS = verification::crps(x$obs, as.matrix(x[, 4:5]))$CRPS
  )
}, .progress = plyr::progress_text(), .parallel = FALSE) 

##add in mixed/stratified period
forecast_skill_depth_horizon_27nov$phen <- ifelse(forecast_skill_depth_horizon_27nov$forecast_date <= as.Date(strat_date) & 
                                              forecast_skill_depth_horizon_27nov$forecast_date >="2021-03-13","Stratified", "Mixed")
forecast_skill_depth_horizon_24nov$phen <- ifelse(forecast_skill_depth_horizon_24nov$forecast_date <= as.Date(strat_date) & 
                                                    forecast_skill_depth_horizon_24nov$forecast_date >="2021-03-13","Stratified", "Mixed")
forecast_skill_depth_horizon_22nov$phen <- ifelse(forecast_skill_depth_horizon_22nov$forecast_date <= as.Date(strat_date) & 
                                                    forecast_skill_depth_horizon_22nov$forecast_date >="2021-03-13","Stratified", "Mixed")
forecast_skill_depth_horizon_fixed$phen <- ifelse(forecast_skill_depth_horizon_fixed$forecast_date <= as.Date(strat_date) & 
                                                    forecast_skill_depth_horizon_fixed$forecast_date >="2021-03-13","Stratified", "Mixed")
forecast_skill_depth_horizon_1.04$phen <- ifelse(forecast_skill_depth_horizon_1.04$forecast_date <= as.Date(strat_date) & 
                                                    forecast_skill_depth_horizon_1.04$forecast_date >="2021-03-13","Stratified", "Mixed")

#order DA frequencies
forecast_skill_depth_horizon_27nov$DA <- factor(forecast_skill_depth_horizon_27nov$DA, levels=c("Daily", "Weekly", "Fortnightly", "Monthly"))
forecast_skill_depth_horizon_24nov$DA <- factor(forecast_skill_depth_horizon_24nov$DA, levels=c("Daily", "Weekly", "Fortnightly", "Monthly"))
forecast_skill_depth_horizon_22nov$DA <- factor(forecast_skill_depth_horizon_22nov$DA, levels=c("Daily", "Weekly", "Fortnightly", "Monthly"))
forecast_skill_depth_horizon_fixed$DA <- factor(forecast_skill_depth_horizon_fixed$DA, levels=c("Daily", "Weekly", "Fortnightly", "Monthly"))
forecast_skill_depth_horizon_1.04$DA <- factor(forecast_skill_depth_horizon_1.04$DA, levels=c("Daily", "Weekly", "Fortnightly", "Monthly"))


#------------------------------------------------------------------------------------------------#
# Start date forecast skill comparison

#add new column for start date / parameter
forecast_skill_depth_horizon_27nov$da_start_date <- "27nov"
forecast_skill_depth_horizon_24nov$da_start_date <- "24nov"
forecast_skill_depth_horizon_22nov$da_start_date <- "22nov"
forecast_skill_depth_horizon_fixed$da_start_date <- "27nov"
forecast_skill_depth_horizon_1.04$da_start_date <- "27nov"


forecast_skill_depth_horizon_27nov$param <- "1.02"
forecast_skill_depth_horizon_24nov$param <- "1.02"
forecast_skill_depth_horizon_22nov$param <- "1.02"
forecast_skill_depth_horizon_fixed$param <- "fixed"
forecast_skill_depth_horizon_1.04$param <- "1.04"

#combine to make a massive df
start_dates <- rbind(forecast_skill_depth_horizon_27nov,forecast_skill_depth_horizon_24nov,forecast_skill_depth_horizon_22nov)
params <- rbind(forecast_skill_depth_horizon_27nov,forecast_skill_depth_horizon_fixed,forecast_skill_depth_horizon_1.04)

#round depth to nearest integer
start_dates$depth <- ceiling(start_dates$depth)
params$depth <- ceiling(params$depth)

#FIGS
start_dates %>% filter(depth %in% c(1,5,9)) %>%
  group_by(DA,depth,horizon) %>%  # do the same calcs for each box
  mutate(value2 = filter_lims(RMSE)) %>%
  ggplot(aes(DA, value2, fill=as.factor(da_start_date))) +  ylab("RMSE") + xlab("")+
  geom_boxplot(outlier.shape = NA) + theme_bw() + guides(fill=guide_legend(title="DA Start Date")) +
  theme(text = element_text(size=8), axis.text = element_text(size=6, color="black"), legend.position = c(0.75,0.25), legend.background = element_blank(),
        legend.title = element_text(size = 2.5),legend.text  = element_text(size = 2.5),legend.key.size = unit(0.5, "lines"), legend.direction = "horizontal",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        plot.margin = unit(c(0,0.05,-0.2,0), "cm"),panel.spacing=unit(0.1, "cm")) +
  facet_grid(depth~phen, scales="free",labeller = labeller(depth = depths)) + scale_fill_manual(values=c("#81A665","#E0CB48","#D08151")) 
ggsave(file.path(lake_directory,"analysis/figures/RMSEvsDAfreq_depth_facets_start_dates.jpg"))

params %>% filter(depth %in% c(1,5,9)) %>%
  group_by(DA,depth,horizon) %>%  # do the same calcs for each box
  mutate(value2 = filter_lims(RMSE)) %>%
  ggplot(aes(DA, value2, fill=as.factor(param))) +  ylab("RMSE") + xlab("")+
  geom_boxplot(outlier.shape = NA) + theme_bw() + guides(fill=guide_legend(title="Params")) +
  theme(text = element_text(size=8), axis.text = element_text(size=6, color="black"), legend.position = c(0.75,0.25), legend.background = element_blank(),
        legend.title = element_text(size = 2.5),legend.text  = element_text(size = 2.5),legend.key.size = unit(0.5, "lines"), legend.direction = "horizontal",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        plot.margin = unit(c(0,0.05,-0.2,0), "cm"), panel.spacing=unit(0.1, "cm")) +
  facet_grid(depth~phen, scales="free",labeller = labeller(depth = depths)) + scale_fill_manual(values=c("#81A665","#E0CB48","#D08151")) 
ggsave(file.path(lake_directory,"analysis/figures/RMSEvsDAfreq_depth_facets_parameters.jpg"))



