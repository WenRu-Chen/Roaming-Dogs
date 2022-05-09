# ------------------------------------------------------------
# Pick reasonable values:
if (interactive()) {
  library(manipulate)
  manipulate({
    set.seed(seed)
    simulation_data <- sim_glmmfields(df = df, n_data_points = 50, seed = NULL,
                                      n_draws = 1, n_knots = 7, gp_theta = gp_theta, gp_sigma = gp_sigma,
                                      obs_error = "gamma", sd_obs = CV)
    print(simulation_data$plot)
  }, gp_theta = slider(0.05, 10, 1, step = 0.25),
  gp_sigma = slider(0.05, 10, 0.5, step = 0.25),
  df = slider(2, 50, 4, step = 1),
  CV = slider(0.01, 1, 0.05, step = 0.05),
  seed = slider(1, 300, 10, step = 1))
}
