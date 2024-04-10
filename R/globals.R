# run checkhelper::print_globals() to get the globalVariables

globalVariables(unique(c(
  # central_group:
  "indice", "indice_low", "indice_upp", "median", "n_sample",
  # distrib_discrete:
  "median", "n_sample", "n_weighted_low", "n_weighted_upp", "prop", "prop_low", "prop_upp", "group",
  # distrib_group_discrete:
  "prop",
  # many_prop:
  "list_col", "median", "n_sample", "prop", "prop_low", "prop_upp",
  # many_prop_group:
  "list_col", "n_sample", "prop", "prop_low", "prop_upp",
  # pivot_longer_survey:
  "type",
  # prop_group:
  "median", "n_sample", "prop", "prop_low", "prop_upp", "fonctionr_express_bin",
  # distrib_continuous:
  "coord_x", "quantFct", "segment", "x", "y",
  # distrib_group_continuous:
  "level", "moustache_prob", "position", "probs", "quantFct", "quantile", "segment", "x", "xbegin", "xend", "y", "y_ridges"
)))
