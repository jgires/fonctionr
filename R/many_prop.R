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

  # On transforme les colonnes binarisée en un vecteur caractère (plus facile pour le code !)
  vec_bin_vars <- all.vars(substitute(bin_vars))
  message(vec_bin_vars)

  # Petite fonction utile
  `%ni%` <- Negate(`%in%`)

  # On crée une quosure de facet_var & filter_exp => pour if statements dans la fonction (voir ci-dessous)
  # Solution trouvée ici : https://rpubs.com/tjmahr/quo_is_missing
  quo_facet <- enquo(facet_var)
  quo_filter <- enquo(filter_exp)

  # On convertit d'abord en objet srvyr
  data_W <- convert_to_srvyr(data, ...)

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

  # On convertit la variable de facet en facteur si facet non-NULL
  if (!quo_is_null(quo_facet)) {
    data_W <- data_W %>%
      mutate(
        "{{ facet_var }}" := droplevels(as.factor({{ facet_var }})) # droplevels pour éviter qu'un level soit encodé alors qu'il n'a pas d'effectifs (pb pour le test khi2)
      )

    tab <- tibble()
    for (i in vec_bin_vars) {
      tab_i <- data_W %>%
        group_by({{ facet_var }}) %>%
        summarise(
          bin_col = i,
          prop = survey_mean(.data[[i]], na.rm = T, proportion = T, prop_method = prop_method, vartype = "ci"),
          n_tot_sample = unweighted(n()),
          n_tot_weighted = survey_total()
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
          n_tot_sample = unweighted(n()),
          n_tot_weighted = survey_total()
        )

      tab <- rbind(tab, tab_i)
    }
  }

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

  # On transforme la variable bin_col en facteur (pour réordonner éventuellement)
#  tab[["bin_col"]] <- as.factor(tab[["bin_col"]])



  if (reorder == T ) {
    # On crée un vecteur pour ordonner les levels de quali_var selon prop, en mettant NA en premier (= en dernier sur le graphique ggplot)
    levels <- levels(reorder(
        tab[[deparse(substitute(bin_col))]],
        tab[["prop"]],
        FUN = median,
        decreasing = T
      ))

  }

  if (reorder == F) {
    # On crée un vecteur pour ordonner les levels de quali_var pour mettre NA en premier (= en dernier sur le graphique ggplot)
    if(length(vec_bin_vars) == length(bin_vars_label)){
      levels <-   rev(bin_vars_label)
    }
    else{
      levels <-   rev(vec_bin_vars)
    }

  }

  # On calcule la valeur max de la proportion, pour l'écart des geom_text dans le ggplot
  max_ggplot <- max(tab$prop, na.rm = TRUE)

  # On crée le graphique

  # On charge et active les polices
  load_and_active_fonts()

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
    theme_minimal() +
    theme(
      panel.grid.minor.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = "#dddddd"),
      text = element_text(family = font),
      axis.line = element_line(color = "black"),
      axis.ticks = element_blank(),
      #axis.ticks = element_line(color = "black"),
      axis.text = element_text(color = "black"),
      legend.position = "bottom",
      plot.margin = margin(10, 15, 10, 10)
    )  +
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


  if (show_n == TRUE) {
    graph <- graph +
      geom_text(
        aes(
          y = 0 + (0.01 * max_ggplot), # Pour ajouter des labels avec les effectifs en dessous des barres
          label = paste0("n=", n_tot_sample),
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
