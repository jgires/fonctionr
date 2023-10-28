# run checkhelper::print_globals() to get the globalVariables

globalVariables(unique(c(
  # central_group:
  "indice", "indice_low", "indice_upp", "n_tot_sample",
  # quali_distrib_group:
  "prop",
  # prop_group:
  "prop", "prop_low", "prop_upp", "n_tot_sample", "n_tot_weighted_se"
)))
