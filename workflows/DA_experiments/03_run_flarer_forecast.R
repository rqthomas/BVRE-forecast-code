#separating this from 2.5 so I can iteratively loop through this scipt for each day I want to generate forecasts
states_config <- FLAREr::generate_states_to_obs_mapping(states_config, obs_config)

model_sd <- FLAREr::initiate_model_error(config, states_config)

#change restart file directory for DA experiments
if(!is.na(config$run_config$restart_file)){
config$run_config$restart_file <- file.path(config$file_path$forecast_output_directory,"DA_experiments",basename(config$run_config$restart_file))
}

init <- FLAREr::generate_initial_conditions(states_config,
                                            obs_config,
                                            pars_config,
                                            obs,
                                            config,
                                            restart_file = config$run_config$restart_file,
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

#Create EML Metadata
eml_file_name <- FLAREr::create_flare_metadata(file_name = saved_file,
                                               da_forecast_output = da_forecast_output)

#Clean up temp files and large objects in memory
#unlink(config$file_path$execute_directory, recursive = TRUE)

FLAREr::put_forecast(saved_file, eml_file_name, config)

rm(da_forecast_output)
gc()

FLAREr::update_run_config(config, lake_directory, configure_run_file, saved_file, new_horizon = NA, day_advance = 1, new_start_datetime = TRUE)

setwd(lake_directory)
#unlink(config$run_config$restart_file)
#unlink(forecast_dir, recursive = TRUE)
#unlink(file.path(lake_directory, "flare_tempdir", config$location$site_id, config$run_config$sim_name), recursive = TRUE)

message(paste0("successfully generated flare forecats for: ", basename(saved_file)))
