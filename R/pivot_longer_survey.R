#' pivot_longer_survey
#'
#' Function to pivot, from wide to long, a dataframe produced  produced by srvyr::summarise with group(s)
#'
#' @param data A dataframe produced  produced by srvyr::summarise with group(s)
#' @param n_groups Number of groups by which data has been agregated
#'
#' @return A pivoted dataframe
#' @import dplyr
#' @import tidyr
#' @export
#'
#' @examples
#' # Loading data
#'data(eusilc, package = "laeken")
#'
#'# Loading srvyr
#'library(srvyr)
#'
#'# Making srvyr object
#'eusilc_srvyr <- as_survey_design(eusilc, ids = db030, strata = db040, weights = rb050)
#'
#'# computing srvyr result using summarise()
#'result_srvyr<-eusilc_srvyr %>%
#'  group_by(rb090,pb220a) %>% # by sex and nationality
#'    summarise(mean_eqIncome = survey_mean(eqIncome),mean_age =survey_mean(age))
#'
#'# Showing the srvyr summirise output
#'result_srvyr
#'
#'# Pivoting the out with pivot_longer_survey()
#'    pivoted_result <- pivot_longer_survey(result_srvyr, n_groups = 2)
#'
#'# Output is pivoted
#'pivoted_result
#'

pivot_longer_survey <- function(data,
                                n_groups) {
  # J'isole les effectifs (+ le nom du groupe, pour joindre apres)
  # ATTENTION : le(s) groupe(s) doivent etre dans la/les premiere(s) colonne(s) !
  n_numbers <- data |>
    select(all_of(1:n_groups), starts_with("n_"), matches("^n$"))

  data_renamed <- data |>
    ungroup() |>
    # J'enleve les effectifs (toute colonne nommee "n" ou "n_quelquechose") et la premiere colonne (le nom du groupe)
    select(-all_of(1:n_groups), -starts_with("n_"), -matches("^n$")) |>
    # Par securite, je remplace tous les "_" par "lWPtZR9Wf2g9RSp" dans toutes les colonne, sauf le "_" de _low/_upp/_se/_var/_cv
    rename_with(~stringr::str_replace_all(., "_(?!low$|upp$|se$|var$|cv$)", "lWPtZR9Wf2g9RSp")) |>
    # J'ajoute _value a tous les noms des colonnes qui ne se terminent pas par _low/_upp/_se/_var/_cv
    rename_with(~paste0(., "_value"), !matches(c("_low$", "_upp$", "_se$", "_var$", "_cv$"))) |>
    bind_cols(data |> ungroup() |> select(all_of(1:n_groups))) |> # Je remets le nom du groupe
    relocate((last_col()-(n_groups-1)):last_col()) |> # Je la positionne en premiere place
    # Je pivote avec names_sep = "_"
    pivot_longer(-all_of(1:n_groups), names_to = c("type",".value"), names_sep = "_") |>
    mutate(type = stringr::str_replace_all(type, "lWPtZR9Wf2g9RSp", "_")) |> # Je remets le "_" dans ce qui etait au depart les noms des colonnes
    left_join(n_numbers) # J'ajoute les effectifs

  return(data_renamed)
}
