#scipt to generate figures for DA experiment forecasts

#read in packages
pacman::p_load(dplyr,readr,ggplot2)

#set wd
lake_directory <- here::here()
setwd(lake_directory)

#2021 phenology: 2021-03-08 is first time when >3 consecutive days had difference between surface and bottom >1C
# Stratified period = 8Mar21 - 12Nov2l; mixed period = 01Jan21-07Mar21 and 13Nov21-31Dec21

#read in forecast eval tables for each da frequency - note that date selection is weird and min actually starts one day ahead of what is called for
forecasts_daily_horizon <- list.files(file.path(lake_directory,"forecasts/bvre/DA_experiments/daily"), pattern="daily.+.csv", full.names=TRUE)[-c(1)] #ignoring first file because this is the DA period
forecasts_daily_horizon_s <- lapply(forecasts_daily_horizon, read_csv) %>% bind_rows() %>%
  mutate(season = ifelse(date >= "2021-02-28" & date <= "2021-05-31", "Spring (Mar-May)", ifelse(
    date >= "2021-05-31" & date <= "2021-08-31", "Summer (Jun-Aug)", ifelse(date >= "2021-08-31" & date <= "2021-11-30", "Fall (Sep-Nov)","Winter (Dec-Feb)")))) %>%
  group_by(day,season) %>% summarise(RMSE = mean(RMSE)) %>% mutate(DA = "Daily")
  
forecasts_daily_horizon_p <- lapply(forecasts_daily_horizon, read_csv) %>% bind_rows() %>%
  mutate(phen = ifelse(date>="2021-03-08" & date<= "2021-11-12","Stratified","Mixed")) %>%
  group_by(day,phen) %>% summarise(RMSE = mean(RMSE)) %>% mutate(DA = "Daily")


forecasts_2day_horizon <- list.files(file.path(lake_directory,"forecasts/bvre/DA_experiments/daily_2"), pattern="daily.+.csv", full.names=TRUE)[-c(1)]
forecasts_2day_horizon_s <- lapply(forecasts_2day_horizon, read_csv) %>% bind_rows() %>%
  mutate(season = ifelse(date >= "2021-02-28" & date <= "2021-05-31", "Spring (Mar-May)", ifelse(
    date >= "2021-05-31" & date <= "2021-08-31", "Summer (Jun-Aug)", ifelse(date >= "2021-08-31" & date <= "2021-11-30", "Fall (Sep-Nov)","Winter (Dec-Feb)")))) %>%
  group_by(day,season) %>% summarise(RMSE = mean(RMSE)) %>% mutate(DA = "2Day")

forecasts_2day_horizon_p <- lapply(forecasts_2day_horizon, read_csv) %>% bind_rows() %>%
  mutate(phen = ifelse(date>="2021-03-08" & date<= "2021-11-12","Stratified","Mixed")) %>%
  group_by(day,phen) %>% summarise(RMSE = mean(RMSE)) %>% mutate(DA = "2Day")

forecasts_5day_horizon <- list.files(file.path(lake_directory,"forecasts/bvre/DA_experiments/daily_5"), pattern="daily.+.csv", full.names=TRUE)[-c(1)]
forecasts_5day_horizon_s <- lapply(forecasts_5day_horizon, read_csv) %>% bind_rows() %>%
  mutate(season = ifelse(date >= "2021-02-28" & date <= "2021-05-31", "Spring (Mar-May)", ifelse(
    date >= "2021-05-31" & date <= "2021-08-31", "Summer (Jun-Aug)", ifelse(date >= "2021-08-31" & date <= "2021-11-30", "Fall (Sep-Nov)","Winter (Dec-Feb)")))) %>%
  group_by(day,season) %>% summarise(RMSE = mean(RMSE)) %>% mutate(DA = "5Day")

forecasts_5day_horizon_p <- lapply(forecasts_5day_horizon, read_csv) %>% bind_rows() %>%
  mutate(phen = ifelse(date>="2021-03-08" & date<= "2021-11-12","Stratified","Mixed")) %>%
  group_by(day,phen) %>% summarise(RMSE = mean(RMSE)) %>% mutate(DA = "5Day")

forecasts_weekly_horizon <- list.files(file.path(lake_directory,"forecasts/bvre/DA_experiments/weekly"), pattern="daily.+.csv", full.names=TRUE)[-c(1)]
forecasts_weekly_horizon_s <- lapply(forecasts_weekly_horizon, read_csv) %>% bind_rows() %>%
  mutate(season = ifelse(date >= "2021-02-28" & date <= "2021-05-31", "Spring (Mar-May)", ifelse(
    date >= "2021-05-31" & date <= "2021-08-31", "Summer (Jun-Aug)", ifelse(date >= "2021-08-31" & date <= "2021-11-30", "Fall (Sep-Nov)","Winter (Dec-Feb)")))) %>%
  group_by(day,season) %>% summarise(RMSE = mean(RMSE)) %>% mutate(DA = "Weekly")

forecasts_weekly_horizon_p <- lapply(forecasts_weekly_horizon, read_csv) %>% bind_rows() %>%
  mutate(phen = ifelse(date>="2021-03-08" & date<= "2021-11-12","Stratified","Mixed")) %>%
  group_by(day,phen) %>% summarise(RMSE = mean(RMSE)) %>% mutate(DA = "Weekly")

forecasts_fortnightly_horizon <- list.files(file.path(lake_directory,"forecasts/bvre/DA_experiments/fortnightly"), pattern="daily.+.csv", full.names=TRUE)[-c(1)]
forecasts_fortnightly_horizon_s <- lapply(forecasts_fortnightly_horizon, read_csv) %>% bind_rows() %>%
  mutate(season = ifelse(date >= "2021-02-28" & date <= "2021-05-31", "Spring (Mar-May)", ifelse(
    date >= "2021-05-31" & date <= "2021-08-31", "Summer (Jun-Aug)", ifelse(date >= "2021-08-31" & date <= "2021-11-30", "Fall (Sep-Nov)","Winter (Dec-Feb)")))) %>%
  group_by(day,season) %>% summarise(RMSE = mean(RMSE)) %>% mutate(DA = "Fortnightly")

forecasts_fortnightly_horizon_p <- lapply(forecasts_fortnightly_horizon, read_csv) %>% bind_rows() %>%
  mutate(phen = ifelse(date>="2021-03-08" & date<= "2021-11-12","Stratified","Mixed")) %>%
  group_by(day,phen) %>% summarise(RMSE = mean(RMSE)) %>% mutate(DA = "Fortnightly")

forecasts_monthly_horizon <- list.files(file.path(lake_directory,"forecasts/bvre/DA_experiments/monthly"), pattern="daily.+.csv", full.names=TRUE)[-c(1)]
forecasts_monthly_horizon_s <- lapply(forecasts_monthly_horizon, read_csv) %>% bind_rows() %>%
  mutate(season = ifelse(date >= "2021-02-28" & date <= "2021-05-31", "Spring (Mar-May)", ifelse(
    date >= "2021-05-31" & date <= "2021-08-31", "Summer (Jun-Aug)", ifelse(date >= "2021-08-31" & date <= "2021-11-30", "Fall (Sep-Nov)","Winter (Dec-Feb)")))) %>%
  group_by(day,season) %>% summarise(RMSE = mean(RMSE)) %>% mutate(DA = "Monthly")

forecasts_monthly_horizon_p <- lapply(forecasts_monthly_horizon, read_csv) %>% bind_rows() %>%
  mutate(phen = ifelse(date>="2021-03-08" & date<= "2021-11-12","Stratified","Mixed")) %>%
  group_by(day,phen) %>% summarise(RMSE = mean(RMSE)) %>% mutate(DA = "Monthly")

#merge DA horizon dfs
daily_forecasts_season <- rbind(forecasts_daily_horizon_s,forecasts_2day_horizon_s,forecasts_5day_horizon_s,
                         forecasts_weekly_horizon_s,forecasts_fortnightly_horizon_s,forecasts_monthly_horizon_s)

daily_forecasts_phen <- rbind(forecasts_daily_horizon_p,forecasts_2day_horizon_p,forecasts_5day_horizon_p,
                                forecasts_weekly_horizon_p,forecasts_fortnightly_horizon_p,forecasts_monthly_horizon_p)

#read in forecast eval tables for each da frequency - RMSE by depth
forecasts_daily_depth <- list.files(file.path(lake_directory,"forecasts/bvre/DA_experiments/daily"), pattern="depth.+.csv", full.names=TRUE)[-c(1)]
forecasts_daily_depth_s <- lapply(forecasts_daily_depth, read_csv) %>% bind_rows() %>%
  mutate(season = ifelse(day >= "2021-03-01" & day <= "2021-05-31", "Spring (Mar-May)", ifelse(
    day >= "2021-06-01" & day <= "2021-08-31", "Summer (Jun-Aug)", ifelse(day >= "2021-09-01" & day <= "2021-11-30", "Fall (Sep-Nov)","Winter (Dec-Feb)")))) %>%
  group_by(Depth,season) %>% summarise(RMSE = mean(RMSE), CRPS = mean(CRPS)) %>% mutate(DA = "Daily")

forecasts_daily_depth_p <- lapply(forecasts_daily_depth, read_csv) %>% bind_rows() %>%
  mutate(phen = ifelse(day>="2021-03-08" & day<= "2021-11-12","Stratified","Mixed")) %>%
  group_by(Depth,phen) %>% summarise(RMSE = mean(RMSE), CRPS = mean(CRPS)) %>% mutate(DA = "Daily")

forecasts_2day_depth <- list.files(file.path(lake_directory,"forecasts/bvre/DA_experiments/daily_2"), pattern="depth.+.csv", full.names=TRUE)[-c(1)]
forecasts_2day_depth_s <- lapply(forecasts_2day_depth, read_csv) %>% bind_rows() %>%
  mutate(season = ifelse(day >= "2021-03-01" & day <= "2021-05-31", "Spring (Mar-May)", ifelse(
    day >= "2021-06-01" & day <= "2021-08-31", "Summer (Jun-Aug)", ifelse(day >= "2021-09-01" & day <= "2021-11-30", "Fall (Sep-Nov)","Winter (Dec-Feb)")))) %>%
  group_by(Depth,season) %>% summarise(RMSE = mean(RMSE), CRPS = mean(CRPS)) %>% mutate(DA = "2Day")

forecasts_2day_depth_p <- lapply(forecasts_2day_depth, read_csv) %>% bind_rows() %>%
  mutate(phen = ifelse(day>="2021-03-08" & day<= "2021-11-12","Stratified","Mixed")) %>%
  group_by(Depth,phen) %>% summarise(RMSE = mean(RMSE), CRPS = mean(CRPS)) %>% mutate(DA = "2Day")

forecasts_5day_depth <- list.files(file.path(lake_directory,"forecasts/bvre/DA_experiments/daily_5"), pattern="depth.+.csv", full.names=TRUE)[-c(1)]
forecasts_5day_depth_s <- lapply(forecasts_5day_depth, read_csv) %>% bind_rows() %>%
  mutate(season = ifelse(day >= "2021-03-01" & day <= "2021-05-31", "Spring (Mar-May)", ifelse(
    day >= "2021-06-01" & day <= "2021-08-31", "Summer (Jun-Aug)", ifelse(day >= "2021-09-01" & day <= "2021-11-30", "Fall (Sep-Nov)","Winter (Dec-Feb)")))) %>%
  group_by(Depth,season) %>% summarise(RMSE = mean(RMSE), CRPS = mean(CRPS)) %>% mutate(DA = "5Day")

forecasts_5day_depth_p <- lapply(forecasts_5day_depth, read_csv) %>% bind_rows() %>%
  mutate(phen = ifelse(day>="2021-03-08" & day<= "2021-11-12","Stratified","Mixed")) %>%
  group_by(Depth,phen) %>% summarise(RMSE = mean(RMSE), CRPS = mean(CRPS)) %>% mutate(DA = "5Day")

forecasts_weekly_depth <- list.files(file.path(lake_directory,"forecasts/bvre/DA_experiments/weekly"), pattern="depth.+.csv", full.names=TRUE)[-c(1)]
forecasts_weekly_depth_s <- lapply(forecasts_weekly_depth, read_csv) %>% bind_rows() %>%
  mutate(season = ifelse(day >= "2021-03-01" & day <= "2021-05-31", "Spring (Mar-May)", ifelse(
    day >= "2021-06-01" & day <= "2021-08-31", "Summer (Jun-Aug)", ifelse(day >= "2021-09-01" & day <= "2021-11-30", "Fall (Sep-Nov)","Winter (Dec-Feb)")))) %>%
  group_by(Depth,season) %>% summarise(RMSE = mean(RMSE), CRPS = mean(CRPS)) %>% mutate(DA = "Weekly")

forecasts_weekly_depth_p <- lapply(forecasts_weekly_depth, read_csv) %>% bind_rows() %>%
  mutate(phen = ifelse(day>="2021-03-08" & day<= "2021-11-12","Stratified","Mixed")) %>%
  group_by(Depth,phen) %>% summarise(RMSE = mean(RMSE), CRPS = mean(CRPS)) %>% mutate(DA = "Weekly")

forecasts_fortnightly_depth <- list.files(file.path(lake_directory,"forecasts/bvre/DA_experiments/fortnightly"), pattern="depth.+.csv", full.names=TRUE)[-c(1)]
forecasts_fortnightly_depth_s <- lapply(forecasts_fortnightly_depth, read_csv) %>% bind_rows() %>%
  mutate(season = ifelse(day >= "2021-03-01" & day <= "2021-05-31", "Spring (Mar-May)", ifelse(
    day >= "2021-06-01" & day <= "2021-08-31", "Summer (Jun-Aug)", ifelse(day >= "2021-09-01" & day <= "2021-11-30", "Fall (Sep-Nov)","Winter (Dec-Feb)")))) %>%
  group_by(Depth,season) %>% summarise(RMSE = mean(RMSE), CRPS = mean(CRPS)) %>% mutate(DA = "Fortnightly")

forecasts_fortnightly_depth_p <- lapply(forecasts_fortnightly_depth, read_csv) %>% bind_rows() %>%
  mutate(phen = ifelse(day>="2021-03-08" & day<= "2021-11-12","Stratified","Mixed")) %>%
  group_by(Depth,phen) %>% summarise(RMSE = mean(RMSE), CRPS = mean(CRPS)) %>% mutate(DA = "Fortnightly")

forecasts_monthly_depth <- list.files(file.path(lake_directory,"forecasts/bvre/DA_experiments/monthly"), pattern="depth.+.csv", full.names=TRUE)[-c(1)]
forecasts_monthly_depth_s <- lapply(forecasts_monthly_depth, read_csv) %>% bind_rows() %>%
  mutate(season = ifelse(day >= "2021-03-01" & day <= "2021-05-31", "Spring (Mar-May)", ifelse(
    day >= "2021-06-01" & day <= "2021-08-31", "Summer (Jun-Aug)", ifelse(day >= "2021-09-01" & day <= "2021-11-30", "Fall (Sep-Nov)","Winter (Dec-Feb)")))) %>%
  group_by(Depth,season) %>% summarise(RMSE = mean(RMSE), CRPS = mean(CRPS)) %>% mutate(DA = "Monthly")

forecasts_monthly_depth_p <- lapply(forecasts_monthly_depth, read_csv) %>% bind_rows() %>%
  mutate(phen = ifelse(day>="2021-03-08" & day<= "2021-11-12","Stratified","Mixed")) %>%
  group_by(Depth,phen) %>% summarise(RMSE = mean(RMSE), CRPS = mean(CRPS)) %>% mutate(DA = "Monthly")

#merge DA depth dfs
depth_forecasts_season <- rbind(forecasts_daily_depth_s,forecasts_2day_depth_s,forecasts_5day_depth_s,
                         forecasts_weekly_depth_s,forecasts_fortnightly_depth_s,forecasts_monthly_depth_s)

depth_forecasts_phen <- rbind(forecasts_daily_depth_p,forecasts_2day_depth_p,forecasts_5day_depth_p,
                         forecasts_weekly_depth_p,forecasts_fortnightly_depth_p,forecasts_monthly_depth_p)

#----------------------------------------------------------------------------------#
#visualize forecasts by season

#change DA factor order and month order
daily_forecasts_season$DA <- factor(daily_forecasts_season$DA, levels = c("Daily", "2Day", "5Day", "Weekly","Fortnightly","Monthly"))
daily_forecasts_phen$DA <- factor(daily_forecasts_phen$DA, levels = c("Daily", "2Day", "5Day", "Weekly","Fortnightly","Monthly"))
depth_forecasts_season$DA <- factor(depth_forecasts_season$DA, levels = c("Daily", "2Day", "5Day", "Weekly","Fortnightly","Monthly"))
depth_forecasts_phen$DA <- factor(depth_forecasts_phen$DA, levels = c("Daily", "2Day", "5Day", "Weekly","Fortnightly","Monthly"))
daily_forecasts_season$season <- factor(daily_forecasts_season$season, levels = c("Winter (Dec-Feb)", "Spring (Mar-May)", "Summer (Jun-Aug)", "Fall (Sep-Nov)"))


ggplot(daily_forecasts_season, aes(as.factor(DA), RMSE, fill=DA)) + geom_boxplot() +xlab("") + facet_wrap(~season) + 
  theme_bw() + theme(axis.text.x = element_text(angle = 90),panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  scale_fill_viridis_d() +   guides(fill=guide_legend(title="DA frequency"))
ggsave(file.path(lake_directory,"analysis/figures/RMSEvsDAfreq.jpg"))

ggplot(daily_forecasts_phen, aes(as.factor(DA), RMSE, fill=DA)) + geom_boxplot() +xlab("") + facet_wrap(~phen) + 
  theme_bw() + theme(axis.text.x = element_text(angle = 90),panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  scale_fill_viridis_d() +   guides(fill=guide_legend(title="DA frequency"))
ggsave(file.path(lake_directory,"analysis/figures/RMSEvsDAfreq_phen.jpg"))

#visualize depth differences by season
ggplot(depth_forecasts_season, aes(RMSE, Depth, color=DA)) + facet_wrap(~season) + geom_path() +
  theme_bw() + theme(axis.text.x = element_text(angle = 90),panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  scale_color_viridis_d() +   guides(color=guide_legend(title="DA frequency")) + scale_y_reverse()
ggsave(file.path(lake_directory,"analysis/figures/DepthvsRMSE_allfreqs.jpg"))

ggplot(depth_forecasts_phen, aes(RMSE, Depth, color=DA)) + facet_wrap(~phen) + geom_path() +
  theme_bw() + theme(axis.text.x = element_text(angle = 90),panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  scale_color_viridis_d() +   guides(color=guide_legend(title="DA frequency")) + scale_y_reverse()
ggsave(file.path(lake_directory,"analysis/figures/DepthvsRMSE_allfreqs_phen.jpg"))

#visualize rmse differences over forecast horizon for each freq
ggplot(daily_forecasts_season, aes(day, RMSE, color=DA)) + geom_line(aes(group=DA,color=DA),size=1.5) +xlab("forecast horizon (days)") + facet_wrap(~season)+
  theme_bw() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  scale_color_viridis_d() +   guides(color=guide_legend(title="DA frequency"))
ggsave(file.path(lake_directory,"analysis/figures/RMSEvshorizon_allfreqs.jpg"))

ggplot(daily_forecasts_phen, aes(day, RMSE, color=DA)) + geom_line(aes(group=DA,color=DA),size=1.5) +xlab("forecast horizon (days)") + facet_wrap(~phen)+
  theme_bw() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  scale_color_viridis_d() +   guides(color=guide_legend(title="DA frequency"))
ggsave(file.path(lake_directory,"analysis/figures/RMSEvshorizon_allfreqs_phen.jpg"))

#forecast skill comparison - all depths
depth_forecasts_allavg_s <- depth_forecasts_season %>% group_by(Depth, DA) %>% summarise(RMSE=mean(RMSE),CRPS=mean(CRPS))
depth_forecasts_allavg_s$DA <- factor(depth_forecasts_allavg_s$DA, levels= c("Daily", "2Day", "5Day", "Weekly","Fortnightly","Monthly"))

ggplot(depth_forecasts_allavg_s, aes(as.factor(DA), RMSE, fill=as.factor(DA))) + geom_boxplot() +xlab("")+
  theme_bw() + theme(text = element_text(size=14), legend.position = "none", axis.text = element_text(size=14, color="black"),
                        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=12), panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  scale_fill_viridis_d()
ggsave(file.path(lake_directory,"analysis/figures/RMSEvsDAfrequency.jpg"))

#figure - 35-day average RMSE by depth
ggplot(depth_forecasts_allavg_s, aes(RMSE, Depth, color=DA)) + 
  geom_path() + ylab("Depth (m)") + ylim(c(10,0))+ labs(color="DA_frequency") +  theme_bw()  + 
  theme(axis.text.x=element_text(size=7) ,text = element_text(size=10),
        legend.background = element_rect(fill='transparent'), legend.box.background = element_blank(), legend.key = element_rect(fill='transparent'),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(), legend.key.size = unit(0.5, "cm")) + 
  guides(color = guide_legend(title="DA frequency")) + scale_color_viridis_d() + scale_y_reverse()
ggsave(file.path(lake_directory,"analysis/figures/DepthvsRMSE.jpg"))

#plot avg rmse vs forecast horizon
daily_forecasts_allavg_s <- daily_forecasts_season %>% group_by(day, DA) %>% summarise(RMSE=mean(RMSE))

ggplot(daily_forecasts_allavg_s, aes(day,RMSE,color=as.factor(DA))) + xlab("forecast horizon (days)") + theme_bw()  + 
  theme(legend.title = element_blank(),axis.text.x=element_text(size=7) ,text = element_text(size=11),legend.position = c(0.89,0.18), 
        legend.background = element_rect(fill='transparent'), legend.box.background = element_blank(), legend.key = element_rect(fill='transparent'),
        legend.direction = "horizontal", panel.grid.major = element_blank(),panel.grid.minor = element_blank(), legend.key.size = unit(0.5, "cm")) + 
  geom_line(aes(group=DA, color=DA),size=1.5) + guides(color=guide_legend(nrow=6)) + scale_color_viridis_d()
ggsave(file.path(lake_directory,"analysis/figures/RMSEvsHorizon.jpg"))

##Final plots looking at 1-day ahead forecast skill 
ggplot(subset(daily_forecasts_phen, day==1), aes(DA, RMSE, color=DA, shape=phen)) + xlab("") + 
  theme_bw() + theme(text = element_text(size=14), axis.text = element_text(size=14, color="black"), legend.key.size = unit(0.5, "cm"),
                     axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=10), panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  geom_point(size=4) + guides(color = guide_legend(title="DA frequency"), shape=guide_legend(title="Season")) + scale_color_viridis_d()
ggsave(file.path(lake_directory,"analysis/figures/RMSEvsfreq_phen.jpg")) 

