#separating this from 2.5 so I can iteratively loop through this scipt for each day I want to generate forecasts
states_config <- FLAREr::generate_states_to_obs_mapping(states_config, obs_config)

model_sd <- FLAREr::initiate_model_error(config, states_config)

init <- FLAREr::generate_initial_conditions(states_config,
                                            obs_config,
                                            pars_config,
                                            obs,
                                            config,
                                            historical_met_error = met_out$historical_met_error)
#Run EnKF
da_forecast_output <- FLAREr::run_da_forecast(states_init = init$states,
                                              pars_init = init$pars,
                                              aux_states_init = init$aux_states_init,
                                              obs = obs,
                                              obs_sd = obs_config$obs_sd,
                                              model_sd = model_sd,
                                              working_directory = config$file_path$execute_directory,
                                              met_file_names = met_out$filenames,
                                              inflow_file_names = NULL,#list.files(inflow_file_dir, pattern='INFLOW-'),
                                              outflow_file_names = NULL,#list.files(inflow_file_dir, pattern='OUTFLOW-'),
                                              config = config,
                                              pars_config = pars_config,
                                              states_config = states_config,
                                              obs_config = obs_config,
                                              management = NULL,
                                              da_method = config$da_setup$da_method,
                                              par_fit_method = config$da_setup$par_fit_method)


# Save forecast

saved_file <- FLAREr::write_forecast_netcdf(da_forecast_output = da_forecast_output,
                                            forecast_output_directory = config$file_path$forecast_output_directory,
                                            use_short_filename = TRUE)

forecast_df <- FLAREr::write_forecast_arrow(da_forecast_output = da_forecast_output,
                                            use_s3 = use_s3,
                                            bucket = config$s3$forecasts_parquet$bucket,
                                            endpoint = config$s3$forecasts_parquet$endpoint,
                                            local_directory = file.path(lake_directory, config$s3$forecasts_parquet$bucket))


FLAREr::generate_forecast_score_arrow(targets_file = file.path(config$file_path$qaqc_data_directory,paste0(config$location$site_id, "-targets-insitu.csv")),
                                      forecast_df = forecast_df,
                                      use_s3 = use_s3,
                                      bucket = config$s3$scores$bucket,
                                      endpoint = config$s3$scores$endpoint,
                                      local_directory = file.path(lake_directory, config$s3$scores$bucket))

#rm(da_forecast_output)
#gc()
message("Generating plot")
FLAREr::plotting_general_2(file_name = saved_file,
                           target_file = file.path(config$file_path$qaqc_data_directory, paste0(config$location$site_id, "-targets-insitu.csv")),
                           ncore = 2,
                           obs_csv = FALSE)

FLAREr::put_forecast(saved_file, eml_file_name, config)

unlink(saved_file)

unlink(config$run_config$restart_file)

rm(da_forecast_output)
gc()
