#' esth_graph
#'
#' Function to construct a graphic following the aestetics of the other function function of this package from a table
#'
#' @param tab Table with the variables to be ploted.
#' @param ind_var The variable in tab with the indicators to be ploted.
#' @param cat_var The variable in tab with the labels of the indicators to be ploted.
#' @param facet_var A variable in tab defining the faceting group, if applicable. Default is NULL.
#' @param unit The unit showd on the plot. Default is percent.
#' @param caption Caption of the graphic.
#' @param title Title of the graphic.
#' @param subtitle Subtitle of the graphic.
#' @param xlab X label on the graphic. As coord_flip() is used in the graphic, xlab refers to the x label on the graphic, after the coord_flip(), and not to cat_var in tab.
#' @param ylab Y label on the graphic. As coord_flip() is used in the graphic, ylab refers to the y label on the graphic, after the coord_flip(), and not to ind_var in tab.
#' @param scale Denominator of the proportion. Default is 100 to interprets numbers as percentages.
#' @param digits Numbers of digits showed on the values labels on the graphic. Default is 0.
#' @param n_var The variable in tab containing the number of observation per for each indicator. Default is NULL, not showing the number of observation on the plot.
#' @param show_value TRUE if you want to show the values of ind_var on the graphic. FALSE if you do not want to show the proportion. Default is TRUE.
#' @param dodge Width of the bar, between 0 and 1.
#' @param reorder TRUE if you want to reorder cat_var according to ind_var. FALSE if you do not want to reorder. Default is FALSE.
#' @param error_low The variable in tab that is the lower bound of the confidence interval. If either error_low or error_upp is NULL error bars are not shown on the graphic.
#' @param error_upp The variable in tab that is the upper bound of the confidence interval. If either error_low or error_upp is NULL error bars are not shown on the graphic.
#' @param total_name Name of the total bar on the graphic.
#' @param fill Colour of the bars.
#' @param font Font used in the graphic. Available fonts, included in the package itself, are "Roboto", "Montserrat" and "Gotham Narrow". Default is "Roboto".
#' @param wrap_width Number of characters before before going to the line. Applies to the labels cat_var. Default is 25.
#'
#' @return
#' @export
#'
#' @examples
#'
esth_graph <- function(tab,
                       ind_var,
                       cat_var,
                       facet_var = NULL,
                       unit = "",
                       caption = NULL,
                       title = NULL, # Le titre du graphique
                       subtitle = NULL,
                       xlab = NULL, # Le nom de l'axe de la variable catégorielle
                       ylab = NULL,
                       scale = 1,
                       digits = 2,
                       n_var = NULL,
                       show_value = TRUE, # Possibilité de ne pas vouloir avoir les valeurs sur le graphique
                       dodge = 0.9,
                       reorder = F,
                       error_low = NULL,
                       error_upp = NULL,
                       total_name = NULL,
                       fill = "indianred4",
                       font ="Roboto",
                       wrap_width = 25) {

  # Check des arguments nécessaires
  if((missing(tab) | missing(ind_var) | missing(cat_var)) == TRUE){
    stop("Les arguments tab, ind_var et cat_var doivent être remplis")
  }

  # Check s'il n'y a pas 2 lignes avec des NA
  if(sum(is.na(tab[[deparse(substitute(cat_var))]])) > 1){
    stop("Il y a 2 lignes avec des NA dans la variable de groupe")
  }

  # Check si le total existe dans cat_var
  if (!is.null(total_name)) {
    if(!total_name %in% tab[[deparse(substitute(cat_var))]]){
      stop("Le nom indiqué pour le total n'existe pas dans cat_var")
    }
  }

  # Check des autres arguments
  check_character(arg = list(unit, caption, title, subtitle, xlab, ylab, total_name, fill, font))
  check_logical(arg = list(show_value, reorder))
  check_numeric(arg = list(scale, digits, dodge, wrap_width))

  # On crée des quosures => pour if statements dans la fonction (voir ci-dessous)
  # Solution trouvée ici : https://rpubs.com/tjmahr/quo_is_missing
  quo_facet <- enquo(facet_var)
  quo_low <- enquo(error_low)
  quo_up <- enquo(error_upp)
  quo_n <- enquo(n_var)

  # On convertit la variable catégorielle en facteur si pas facteur
  tab <- tab %>%
    mutate(
      "{{ cat_var }}" := as.factor({{ cat_var }})
    )

  # On crée la palette
  if (!is.null(total_name)) {
    # Avec le total au début (en gris foncé) puis x fois le bleu selon le nombre de levels - 1 (le total étant déjà un niveau)
    palette <- c(rep(fill, nlevels(tab[[deparse(substitute(cat_var))]]) - 1), "grey40")
  }
  if (is.null(total_name)) {
    # Sans (différencier le) total
    palette <- c(rep(fill, nlevels(tab[[deparse(substitute(cat_var))]])))
  }

  # Créer max_ggplot
  max_ggplot <- max(tab[[deparse(substitute(ind_var))]])

  # Si reorder == T
  if (reorder == T) {
    if (!is.null(total_name))  {
      # On crée un vecteur pour ordonner les levels de cat_var selon ind_var, en mettant Total et NA en premier (= en dernier sur le graphique ggplot)
      levels <- c(
        total_name,
        NA,
        levels(reorder(
          tab[[deparse(substitute(cat_var))]],
          tab[[deparse(substitute(ind_var))]],
          FUN = median,
          decreasing = T
        ))[levels(reorder(
          tab[[deparse(substitute(cat_var))]],
          tab[[deparse(substitute(ind_var))]],
          FUN = median,
          decreasing = T
        )) != total_name]
      )
    }
    if (is.null(total_name))  {
      # On crée un vecteur pour ordonner les levels de cat_var selon ind_var, en mettant NA en premier (= en dernier sur le graphique ggplot)
      levels <- c(
        NA,
        levels(reorder(
          tab[[deparse(substitute(cat_var))]],
          tab[[deparse(substitute(ind_var))]],
          FUN = median,
          decreasing = T
          )
          )
      )
    }
  }

  # Si reorder == F
  if (reorder == F) {
    if (!is.null(total_name))  {
      # On crée un vecteur pour ordonner les levels de cat_var pour mettre Total et NA en premier (= en dernier sur le graphique ggplot)
      levels <- c(
        total_name,
        NA,
        rev(
          levels(
            tab[[deparse(substitute(cat_var))]]
          )
        )[rev(
          levels(
            tab[[deparse(substitute(cat_var))]]
          ) != total_name
        )]
      )
    }
    if (is.null(total_name))  {
      # On crée un vecteur pour ordonner les levels de cat_var pour mettre NA en premier (= en dernier sur le graphique ggplot)
      levels <- c(
        NA,
        rev(
          levels(
            tab[[deparse(substitute(cat_var))]]
          )
        )
      )
    }
  }

  # Dans le vecteur qui ordonne les levels, on a mis un NA => Or parfois pas de missing
  # On les supprime donc ssi pas de missing sur la variable de cat_var
  if (sum(is.na(tab[[deparse(substitute(cat_var))]])) == 0)  {
    levels <- levels[!is.na(levels)]
  }

  # On charge et active les polices
  load_and_active_fonts()

  # On crée le graphique

  graph <- tab %>%
    ggplot(aes(
      x = {{ cat_var }},
      y = {{ ind_var }},
      fill = {{ cat_var }}
    )) +
    geom_bar(
      width = dodge,
      stat = "identity",
      position = "dodge"
    ) +
    theme_fonctionr(font = font) +
    theme(
      legend.position = "none"
    ) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = wrap_width),
                     limits = levels) +
    scale_fill_manual(
      values = palette,
      na.value = "grey"
    ) +
    labs(title = title,
         subtitle = subtitle,
         caption = caption,
         y = xlab,
         x = ylab) +
    coord_flip()

  # Ajouter les facets au besoin + scale_y si facet
  if (!quo_is_null(quo_facet)) {
    graph <- graph +
      facet_wrap(vars({{ facet_var }})) +
      theme(panel.spacing.x = unit(1, "lines")) +
      scale_y_continuous(
        labels = function(x) { paste0(x * scale, unit) },
        limits = function(x) { c(min(x), max(x)) },
        expand = expansion(mult = c(.01, .20))
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

  # Ajouter les IC s'ils sont présents
  if (!quo_is_null(quo_low) & !quo_is_null(quo_up)) {
      graph <- graph +
      geom_errorbar(aes(ymin = {{error_low}}, ymax = {{error_upp}}),
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
          y = ({{ ind_var }}) + (0.01 * max_ggplot),
          label = paste0(round({{ ind_var }} * scale,
                               digits = digits),
                         unit),
          family = font),
        size = 3.5,
        vjust = ifelse(!quo_is_null(quo_low) & !quo_is_null(quo_up),
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
  if (!quo_is_null(quo_n)) {
    graph <- graph +
      geom_text(
        aes(
          y = 0 + (0.01 * max_ggplot), # Pour ajouter des labels avec les effectifs en dessous des barres
          label = paste0("n=", {{n_var}}),
          family = font),
        size = 3,
        alpha = 0.7,
        hjust = 0, # Justifié à droite
        vjust = 0.4
      )
  }

  return(graph)

}
