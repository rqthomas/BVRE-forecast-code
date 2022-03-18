#scipt to generate figures for DA experiment forecasts

#read in forecast eval tables for each da frequency - DAILY RMSE
forecasts_daily_horizon <- list.files(file.path(lake_directory,"forecasts/bvre/DA_experiments/daily"), pattern="daily.+.csv", full.names=TRUE)
forecasts_daily_horizon <- lapply(forecasts_daily_horizon, read_csv) %>% bind_rows() %>%
  group_by(day) %>% summarise(RMSE = mean(RMSE)) %>% mutate(DA = "daily")

forecasts_2day_horizon <- list.files(file.path(lake_directory,"forecasts/bvre/DA_experiments/daily_2"), pattern="daily.+.csv", full.names=TRUE)
forecasts_2day_horizon <- lapply(forecasts_2day_horizon, read_csv) %>% bind_rows() %>%
  group_by(day) %>% summarise(RMSE = mean(RMSE)) %>% mutate(DA = "2day")

forecasts_5day_horizon <- list.files(file.path(lake_directory,"forecasts/bvre/DA_experiments/daily_5"), pattern="daily.+.csv", full.names=TRUE)
forecasts_5day_horizon <- lapply(forecasts_5day_horizon, read_csv) %>% bind_rows() %>%
  group_by(day) %>% summarise(RMSE = mean(RMSE)) %>% mutate(DA = "5day")

forecasts_weekly_horizon <- list.files(file.path(lake_directory,"forecasts/bvre/DA_experiments/weekly"), pattern="daily.+.csv", full.names=TRUE)
forecasts_weekly_horizon <- lapply(forecasts_weekly_horizon, read_csv) %>% bind_rows() %>%
  group_by(day) %>% summarise(RMSE = mean(RMSE)) %>% mutate(DA = "weekly")

forecasts_fortnightly_horizon <- list.files(file.path(lake_directory,"forecasts/bvre/DA_experiments/fortnightly"), pattern="daily.+.csv", full.names=TRUE)
forecasts_fortnightly_horizon <- lapply(forecasts_fortnightly_horizon, read_csv) %>% bind_rows() %>%
  group_by(day) %>% summarise(RMSE = mean(RMSE)) %>% mutate(DA = "fortnightly")

forecasts_monthly_horizon <- list.files(file.path(lake_directory,"forecasts/bvre/DA_experiments/monthly"), pattern="daily.+.csv", full.names=TRUE)
forecasts_monthly_horizon <- lapply(forecasts_monthly_horizon, read_csv) %>% bind_rows() %>%
  group_by(day) %>% summarise(RMSE = mean(RMSE)) %>% mutate(DA = "monthly")

#merge DA horizon dfs
daily_forecasts <- rbind(forecasts_daily_horizon,forecasts_2day_horizon,forecasts_5day_horizon,
                         forecasts_weekly_horizon,forecasts_fortnightly_horizon,forecasts_monthly_horizon)

#read in forecast eval tables for each da frequency - RMSE by depth
forecasts_daily_depth <- list.files(file.path(lake_directory,"forecasts/bvre/DA_experiments/daily"), pattern="depth.+.csv", full.names=TRUE)
forecasts_daily_depth <- lapply(forecasts_daily_depth, read_csv) %>% bind_rows() %>%
  group_by(Depth) %>% summarise(RMSE = mean(RMSE)) %>% mutate(DA = "daily")

forecasts_2day_depth <- list.files(file.path(lake_directory,"forecasts/bvre/DA_experiments/daily_2"), pattern="depth.+.csv", full.names=TRUE)
forecasts_2day_depth <- lapply(forecasts_2day_depth, read_csv) %>% bind_rows() %>%
  group_by(Depth) %>% summarise(RMSE = mean(RMSE)) %>% mutate(DA = "2day")

forecasts_5day_depth <- list.files(file.path(lake_directory,"forecasts/bvre/DA_experiments/daily_5"), pattern="depth.+.csv", full.names=TRUE)
forecasts_5day_depth <- lapply(forecasts_5day_depth, read_csv) %>% bind_rows() %>%
  group_by(Depth) %>% summarise(RMSE = mean(RMSE)) %>% mutate(DA = "5day")

forecasts_weekly_depth <- list.files(file.path(lake_directory,"forecasts/bvre/DA_experiments/weekly"), pattern="depth.+.csv", full.names=TRUE)
forecasts_weekly_depth <- lapply(forecasts_weekly_depth, read_csv) %>% bind_rows() %>%
  group_by(Depth) %>% summarise(RMSE = mean(RMSE)) %>% mutate(DA = "weekly")

forecasts_fortnightly_depth <- list.files(file.path(lake_directory,"forecasts/bvre/DA_experiments/fortnightly"), pattern="depth.+.csv", full.names=TRUE)
forecasts_fortnightly_depth <- lapply(forecasts_fortnightly_depth, read_csv) %>% bind_rows() %>%
  group_by(Depth) %>% summarise(RMSE = mean(RMSE)) %>% mutate(DA = "fortnightly")

forecasts_monthly_depth <- list.files(file.path(lake_directory,"forecasts/bvre/DA_experiments/monthly"), pattern="depth.+.csv", full.names=TRUE)
forecasts_monthly_depth <- lapply(forecasts_monthly_depth, read_csv) %>% bind_rows() %>%
  group_by(Depth) %>% summarise(RMSE = mean(RMSE)) %>% mutate(DA = "monthly")

#merge DA depth dfs
depth_forecasts <- rbind(forecasts_daily_depth,forecasts_2day_depth,forecasts_5day_depth,
                         forecasts_weekly_depth,forecasts_fortnightly_depth,forecasts_monthly_depth)

#----------------------------------------------------------------------------------#
#forecast skill comparison - all depths
ggplot(depth_forecasts, aes(as.factor(DA), RMSE, fill=as.factor(DA))) + geom_boxplot() +xlab("")+
    theme_light() + theme(text = element_text(size=14), legend.position = "none", axis.text = element_text(size=14, color="black"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=12)) +
    scale_x_discrete(limits = c("daily","2day","5day","weekly","fortnightly","monthly"))
ggsave(file.path(lake_directory,"analysis/figures/RMSEvsDAfrequency.jpg"))

#figure - 35-day average RMSE by depth
ggplot(depth_forecasts, aes(RMSE, Depth, color=DA)) + geom_point(size=4) + 
    geom_path() + ylab("Depth (m)") + ylim(c(10,0))+ labs(color="DA_frequency")+
    theme_light()  + theme(legend.title = element_text(), text = element_text(size=10), 
    axis.text = element_text(size=9, color="black"), legend.text = element_text(size=10))
ggsave(file.path(lake_directory,"analysis/figures/DepthvsRMSE.jpg"))

#plot avg rmse vs forecast horizon
ggplot(daily_forecasts, aes(as.factor(day),RMSE,color=as.factor(DA))) + geom_point(size=4) + xlab("forecast horizon (days)") + theme_bw()  + 
  theme(legend.title = element_blank(),axis.text.x=element_text(size=7) ,text = element_text(size=11),legend.position = c(0.91,0.18), 
        legend.background = element_rect(fill='transparent'), legend.box.background = element_blank(), legend.key = element_rect(fill='transparent'),
        legend.direction = "horizontal", panel.grid.major = element_blank(),panel.grid.minor = element_blank(), legend.key.size = unit(0.5, "cm")) + 
  geom_line(aes(group=DA, color=DA)) + guides(color=guide_legend(nrow=6))
ggsave(file.path(lake_directory,"analysis/figures/RMSEvsHorizon.jpg"))
