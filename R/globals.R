# run checkhelper::print_globals() to get the globalVariables

globalVariables(unique(c(
  # central_group:
  "indice", "indice_low", "indice_upp", "n_tot_sample",
  # distrib_group_discrete:
  "prop", "n_weighted_se",
  # prop_group:
  "prop", "prop_low", "prop_upp", "n_tot_sample", "n_tot_weighted_se",
  # distrib_discrete:
  "n_weighted_low", "n_weighted_upp"
)))
