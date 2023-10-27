#' pivot_longer_survey : fonction pour transformer les sorties de srvyr
#'
#' @param data
#' @param n_groups
#'
#' @return
#' @import dplyr
#' @import tidyr
#' @export
#'
#' @examples
pivot_longer_survey <- function(data,
                                n_groups) {
  # J'isole les effectifs (+ le nom du groupe, pour joindre après)
  n_numbers <- data %>%
    select(all_of(1:n_groups), starts_with("n_"), matches("^n$"))

  data_renamed <- data %>%
    ungroup() %>%
    # J'enlève les effectifs (toute colonne nommée "n" ou "n_quelquechose") et la première colonne (le nom du groupe)
    select(-all_of(1:n_groups), -starts_with("n_"), -matches("^n$")) %>%
    # Par sécurité, je remplace tous les "_" par "" dans toutes les colonne qui ne se terminent pas par _low/_upp/_se/_var/_cv
    rename_with(~str_replace_all(., "_(?!low$|upp$|se$|var$|cv$)", "")) %>%
    # J'ajoute _value à tous les noms des colonnes qui ne se terminent pas par _low/_upp/_se/_var/_cv
    rename_with(~paste0(., "_value"), !matches(c("_low$", "_upp$", "_se$", "_var$", "_cv$"))) %>%
    bind_cols(data %>% ungroup() %>% select(all_of(1:n_groups))) %>% # Je remets le nom du groupe
    relocate((last_col()-(n_groups-1)):last_col()) %>% # Je la positionne en première place
    # Je pivote
    pivot_longer(-all_of(1:n_groups), names_to = c("type",".value"), names_sep = "_") %>%
    left_join(n_numbers) # J'ajoute les effectifs

  return(data_renamed)
}
