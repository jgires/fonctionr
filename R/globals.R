# run checkhelper::print_globals() to get the globalVariables

globalVariables(unique(c(
  # central_group:
  "indice", "indice_low", "indice_upp", "n_sample",
  # distrib_continuous:
  "coord_max", "coord_x", "indice", "indice_low", "indice_upp", "quantFct", "segment", "y",
  # distrib_discrete:
  "n_sample", "prop", "prop_low", "prop_upp",
  # distrib_group_continuous:
  ".", "indice", "indice_low", "indice_upp", "level", "moustache_prob", "position", "probs", "quantFct", "quantile", "segment", "xbegin", "xend", "y", "y_ridges",
  # distrib_group_discrete:
  "n_sample", "prop",
  # fonctionr_filter:
  "fonctionr_rows_to_keep", "quanti_exp_flattened",
  # make_surface:
  "incr_unit", "indice_sqrt", "row_coef", "row_num", "the_medians", "xmax", "xmean", "xmin",
  # many_val:
  "indice", "indice_low", "indice_upp", "list_col", "n_sample",
  # many_val_group:
  "indice", "indice_low", "indice_upp", "list_col", "longest", "n_sample", "show_value_stack",
  # pivot_longer_survey:
  "type",
  # prop_group:
  "fonctionr_express_bin", "n_sample", "prop", "prop_low", "prop_upp"
)))
