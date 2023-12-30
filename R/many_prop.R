#' many_prop
#'
#' @param data A dataframe or an object from the survey package or an object from the srvyr package.
#' @param group A variable defining groups be compared.
#' @param bin_vars A vector containing names of the binarized variables on which to compute the proportions
#' @param ... All options possible in as_survey_design in srvyr package.
#'
#' @return
#' @import rlang
#' @import survey
#' @import srvyr
#' @import dplyr
#' @import ggplot2
#' @import forcats
#' @import stringr
#' @import openxlsx
#' @export
#'
#' @examples
#'
many_prop = function(data,
                     bin_vars,
                     bin_vars_label = NULL,
                     facet_var = NULL,
                     filter_exp = NULL,
                     prop_method = "beta", # Possibilité de choisir la methode d'ajustement des IC, car empiriquement, j'ai eu des problèmes avec logit
                     ...,
                     unit = "%",
                     caption = NULL,
                     title = NULL, # Le titre du graphique
                     subtitle = NULL,
                     xlab = NULL, # Le nom de l'axe de la variable catégorielle
                     ylab = NULL,
                     scale = 100,
                     digits = 0,
                     reorder = FALSE,
                     show_labs = TRUE,
                     show_n = FALSE,
                     show_value = TRUE, # Possibilité de ne pas vouloir avoir les valeurs sur le graphique
                     dodge = 0.9,
                     fill = "mediumseagreen",
                     error_bar = T,
                     font ="Roboto",
                     wrap_width = 25){

  # Check des arguments nécessaires
  if((missing(data) | missing(bin_vars)) == TRUE){
    stop("Les arguments data et bin_vars doivent être remplis")
  }

  # Check des autres arguments
  check_character(arg = list(prop_method, unit, caption, title, subtitle, xlab, ylab, fill, font))
  check_character_long(arg = list(bin_vars_label))
  check_logical(arg = list(reorder, show_labs, show_n, show_value, error_bar))
  check_numeric(arg = list(scale, digits, dodge, wrap_width))

  # Petite fonction utile
  `%ni%` <- Negate(`%in%`)

  # On crée une quosure de facet_var & filter_exp => pour if statements dans la fonction (voir ci-dessous)
  # Solution trouvée ici : https://rpubs.com/tjmahr/quo_is_missing
  quo_facet <- enquo(facet_var)
  quo_filter <- enquo(filter_exp)

  # On transforme les colonnes binarisée en un vecteur caractère (plus facile pour le code !)
  vec_bin_vars <- all.vars(substitute(bin_vars))
  message("Variable(s) binaires entrées : ", paste(vec_bin_vars, collapse = ", "))

  # On procède d'abord à un test : il faut que toutes les variables entrées soient présentes dans data => sinon stop et erreur
  # On crée un vecteur string qui contient toutes les variables entrées
  vars_input_char <- vec_bin_vars
  # On ajoute facet si non-NULL
  if(!quo_is_null(quo_facet)){
    vars_input_char <- c(vars_input_char, as.character(substitute(facet_var)))
  }
  # On ajoute filter si non-NULL
  if(!quo_is_null(quo_filter)){
    vars_filter <- all.vars(substitute(filter_exp))
    vars_input_char <- c(vars_input_char, as.character(vars_filter))
  }
  # Ici la condition et le stop à proprement parler
  # Si data.frame
  if(any(class(data) %ni% c("survey.design2","survey.design")) & any(class(data) %ni% c("tbl_svy")) & any(class(data) %in% c("data.frame"))){
    if(all(vars_input_char %in% names(data)) == FALSE){
      stop("Au moins une des variables introduites dans bin_vars, filter_exp ou facet n'est pas présente dans data")
    }
  }
  # Si objet sondage
  if(any(class(data) %in% c("survey.design2","survey.design","tbl_svy","svyrep.design"))){
    if(all(vars_input_char %in% names(data[["variables"]])) == FALSE){
      stop("Au moins une des variables introduites dans bin_vars, filter_exp ou facet n'est pas présente dans data")
    }
  }

  # On convertit d'abord en objet srvyr
  data_W <- convert_to_srvyr(data, ...)

  # On ne garde que les colonnes entrées en input
  data_W <- data_W %>%
    select(all_of(vars_input_char))

  # On filtre si filter est non NULL
  if(!quo_is_null(quo_filter)){
    data_W <- data_W %>%
      filter({{ filter_exp }})
  }

  # On supprime les NA sur la variable de facet si non-NULL
  if(!quo_is_null(quo_facet)){
    data_W <- data_W %>%
      filter(!is.na({{ facet_var }}))
  }

  # On supprime les NA sur la/les variable(s) binarisées dans tous les cas, sinon ambigu => de cette façon les n par groupe sont toujours les effectifs pour lesquels la/les variable(s) binarisées sont non missing (et pas tout le groupe : ça on s'en fout)
  # On calcule les effectifs avant filtre
  before <- data_W %>%
    summarise(n=unweighted(n()))
  # On filtre via boucle => solution trouvée ici : https://dplyr.tidyverse.org/articles/programming.html#loop-over-multiple-variables
  for (var in vec_bin_vars) {
    data_W <- data_W %>%
      filter(!is.na(.data[[var]]))
  }
  # On calcule les effectifs après filtre
  after <- data_W %>%
    summarise(n=unweighted(n()))
  # On affiche le nombre de lignes supprimées (pour vérification)
  message(paste0(before[[1]] - after[[1]]), " lignes supprimées avec valeur(s) manquante(s) pour le(s) variable(s) binarisées")

  # On convertit la variable de facet en facteur si facet non-NULL
  if (!quo_is_null(quo_facet)) {
    data_W <- data_W %>%
      mutate(
        "{{ facet_var }}" := droplevels(as.factor({{ facet_var }})) # droplevels pour éviter qu'un level soit encodé alors qu'il n'a pas d'effectifs (pb pour le test khi2)
        )
  }

  # On calcule les proportions
  # Si facet
  if (!quo_is_null(quo_facet)) {
    tab <- tibble()
    for (i in vec_bin_vars) {
      tab_i <- data_W %>%
        group_by({{ facet_var }}) %>%
        summarise(
          bin_col = i,
          prop = survey_mean(.data[[i]], na.rm = T, proportion = T, prop_method = prop_method, vartype = "ci"),
          n_sample = unweighted(n()),
          n_true_weighted = survey_total(.data[[i]], na.rm = T, vartype = "ci"),
          n_tot_weighted = survey_total(vartype = "ci")
        )

      tab <- rbind(tab, tab_i)
    }
  }

  # Si pas de facet (= NULL)
  if (quo_is_null(quo_facet)) {
    tab <- tibble()
    for (i in vec_bin_vars) {
      tab_i <- data_W %>%
        summarise(
          bin_col = i,
          prop = survey_mean(.data[[i]], na.rm = T, proportion = T, prop_method = prop_method, vartype = "ci"),
          n_sample = unweighted(n()),
          n_true_weighted = survey_total(.data[[i]], na.rm = T, vartype = "ci"),
          n_tot_weighted = survey_total(vartype = "ci")
        )

      tab <- rbind(tab, tab_i)
    }
  }

  # On remplace bin_vars par les labels bin_vars_label
  if (!is.null(bin_vars_label)) {

    # vérifier que bin_vars a une même longueur que bin_vars_label
    # si non, message avec erreur...
    if (length(vec_bin_vars) != length(bin_vars_label)) {
      message("Le nombre de labels n'est pas égal au nombre de variables")

    # si oui, on remplace dans tab$bin_col le nom des variables par les labels définis par l'utilisateur dans bin_vars_label
    } else {

      for (i in seq_along(vec_bin_vars)) {
        tab[["bin_col"]][tab[["bin_col"]] == vec_bin_vars[i]] <- bin_vars_label[i]
      }
    }
  }

  # Si reorder = T, on crée un vecteur pour ordonner les levels
  if (reorder == T) {
    levels <- levels(reorder(
      tab[["bin_col"]],
      tab[["prop"]],
      FUN = median,
      decreasing = T
    ))
  }

  # Si reorder = F, l'ordre = celui rentré en input
  if (reorder == F) {
    if (length(vec_bin_vars) == length(bin_vars_label)) {
      levels <- rev(bin_vars_label)
    } else {
      levels <- rev(vec_bin_vars)
    }
  }

  # On calcule la valeur max de la proportion, pour l'écart des geom_text dans le ggplot
  max_ggplot <- max(tab$prop, na.rm = TRUE)

  # On charge et active les polices
  load_and_active_fonts()

  # On crée le graphique

  graph <- tab %>%
    ggplot(aes(
      x = bin_col,
      y = prop,
    )) +
    geom_bar(
      width = dodge,
      stat = "identity",
      position = "dodge",
      fill = fill
    ) +
    theme_fonctionr(font = font) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = wrap_width),
                     limits = levels
    )+
    labs(title = title,
         subtitle = subtitle,
         caption = caption
    ) +
    coord_flip()

  # Ajouter les axes
  if(show_labs == TRUE){
    graph <- graph +
      labs(y = ifelse(is.null(xlab),
                      paste0("Proportion : ", paste(vec_bin_vars, collapse = ", ")),
                      xlab))

      graph <- graph +
        labs(x = ylab)
  }

  # Masquer les axes si show_labs == FALSE
  if(show_labs == FALSE){
    graph <- graph +
      labs(x = NULL,
           y = NULL)
  }

  # Ajouter les facets au besoin + scale_y si facet
  if (!quo_is_null(quo_facet)) {
    graph <- graph +
      facet_wrap(vars({{ facet_var }})) +
      theme(panel.spacing.x = unit(1, "lines")) +
      scale_y_continuous(
        labels = function(x) { paste0(x * scale, unit) },
        limits = function(x) { c(min(x), max(x)) },
        expand = expansion(mult = c(.01, .2))
      )
  }

  # scale_y si pas de facet
  if (quo_is_null(quo_facet)) {
    graph <- graph +
      scale_y_continuous(
        labels = function(x) { paste0(x * scale, unit) },
        limits = function(x) { c(min(x), max(x)) },
        expand = expansion(mult = c(.01, .05))
      )
  }

  # Ajouter les IC si error_bar == T
  if (error_bar == T) {
    graph <- graph +
      geom_errorbar(aes(ymin = prop_low, ymax = prop_upp),
                    width = dodge * 0.05,
                    colour = "black",
                    alpha = 0.5,
                    linewidth = 0.5,
                    position = position_dodge(width = dodge)
      )
  }

  # Ajouter les valeurs calculées
  if (show_value == TRUE) {
    graph <- graph +
      geom_text(
        aes(
          y = (prop) + (0.01 * max_ggplot),
          label = paste0(round(prop * scale,
                               digits = digits),
                         unit),
          family = font),
        size = 3.5,
        vjust = ifelse(error_bar == T,
                       -0.5,
                       0.5),
        hjust = 0,
        color = "black",
        alpha = 0.9,
        # position = position_stack(vjust = .5))
        position = position_dodge(width = dodge)
      )
  }

  # Ajouter le nombre d'individus au besoin
  if (show_n == TRUE) {
    graph <- graph +
      geom_text(
        aes(
          y = 0 + (0.01 * max_ggplot), # Pour ajouter des labels avec les effectifs en dessous des barres
          label = paste0("n=", n_sample),
          family = font),
        size = 3,
        alpha = 0.7,
        hjust = 0, # Justifié à droite
        vjust = 0.4,
        position = position_dodge(width = dodge)
      )
  }

  # On crée l'objet final
  res <- list()
  res$tab <- tab
  res$graph <- graph

  return(res)
}
