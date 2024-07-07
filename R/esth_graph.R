#' esth_graph
#'
#' Function to construct a graphic following the aestetics of the other function function of this package from a table
#'
#' @param tab dataframe with the variables to be ploted.
#' @param var The variable in tab with the labels of the indicators to be ploted.
#' @param value The variable in tab with the values of the indicator to be ploted.
#' @param error_low The variable in tab that is the lower bound of the confidence interval. If either error_low or error_upp is NULL error bars are not shown on the graphic.
#' @param error_upp The variable in tab that is the upper bound of the confidence interval. If either error_low or error_upp is NULL error bars are not shown on the graphic.
#' @param facet A variable in tab defining the faceting group, if applicable. Default is NULL.
#' @param n_var The variable in tab containing the number of observation per for each indicator. Default is NULL, not showing the number of observation on the plot.
#' @param pvalue The p-value to show in the caption. It can a numeric value or the pvalue object from a statsistical test.
#' @param reorder TRUE if you want to reorder var according to value. FALSE if you do not want to reorder. Default is FALSE.
#' @param show_value TRUE if you want to show the values of value on the graphic. FALSE if you do not want to show the proportion. Default is TRUE.
#' @param name_total Name of the var label that may contain the total. When indicated, it is displayed separately on the graph.
#' @param scale Denominator of the proportion. Default is 100 to interprets numbers as percentages.
#' @param digits Numbers of digits showed on the values labels on the graphic. Default is 0.
#' @param unit The unit showd on the plot. Default is percent.
#' @param dec Decimal mark shown on the graphic. Default is ","
#' @param fill Colour of the bars.
#' @param dodge Width of the bar, between 0 and 1.
#' @param font Font used in the graphic. See load_and_active_fonts() for available fonts.
#' @param wrap_width_y Number of characters before going to the line. Applies to the labels var. Default is 25.
#' @param title Title of the graphic.
#' @param subtitle Subtitle of the graphic.
#' @param xlab X label on the graphic. As coord_flip() is used in the graphic, xlab refers to the x label on the graphic, after the coord_flip(), and not to var in tab.
#' @param ylab Y label on the graphic. As coord_flip() is used in the graphic, ylab refers to the y label on the graphic, after the coord_flip(), and not to value in tab.
#' @param caption Caption of the graphic.
#'
#' @return A ggplot graphic.
#' @import rlang
#' @import dplyr
#' @import ggplot2
#' @export
#'
#' @examples
#'
esth_graph <- function(tab,
                       var,
                       value,
                       error_low = NULL,
                       error_upp = NULL,
                       facet = NULL,
                       n_var = NULL,
                       pvalue = NULL,
                       reorder = F,
                       show_value = TRUE,
                       name_total = NULL,
                       scale = 1,
                       digits = 2,
                       unit = "",
                       dec = ",",
                       fill = "indianred4",
                       dodge = 0.9,
                       font ="Roboto",
                       wrap_width_y = 25,
                       title = NULL,
                       subtitle = NULL,
                       xlab = NULL,
                       ylab = NULL,
                       caption = NULL) {


  # 1. CHECKS DES ARGUMENTS --------------------

  # Check des arguments necessaires
  if((missing(tab) | missing(value) | missing(var)) == TRUE){
    stop("Les arguments tab, value et var doivent etre remplis")
  }

  # Check s'il n'y a pas 2 lignes avec des NA
  if(sum(is.na(tab[[deparse(substitute(var))]])) > 1){
    stop("Il y a 2 lignes avec des NA dans la variable de groupe")
  }

  # Check si le total existe dans var
  if (!is.null(name_total)) {
    if(!name_total %in% tab[[deparse(substitute(var))]]){
      stop("Le nom indique pour le total n'existe pas dans var")
    }
  }

  # Check des autres arguments
  check_arg(
    arg = list(
      name_total = name_total,
      unit = unit,
      dec = dec,
      fill = fill,
      font = font,
      title = title,
      subtitle = subtitle,
      xlab = xlab,
      ylab = ylab,
      caption = caption
    ),
    type = "character"
  )
  check_arg(
    arg = list(
      reorder = reorder,
      show_value = show_value
    ),
    type = "logical"
  )
  check_arg(
    arg = list(
      scale = scale,
      digits = digits,
      dodge = dodge,
      wrap_width_y = wrap_width_y
    ),
    type = "numeric"
  )

  # On cree des quosures => pour if statements dans la fonction (voir ci-dessous)
  # Solution trouvee ici : https://rpubs.com/tjmahr/quo_is_missing
  quo_low <- enquo(error_low)
  quo_up <- enquo(error_upp)
  quo_facet <- enquo(facet)
  quo_n <- enquo(n_var)


  # 2. PROCESSING DES DONNEES --------------------

  # On convertit la variable categorielle en facteur si pas facteur
  tab <- tab %>%
    mutate(
      "{{ var }}" := droplevels(as.factor({{ var }}))
    )


  # 3. CREATION DU GRAPHIQUE --------------------

  # On cree la palette
  # Petit truc dans le cas ou il y a un total
  if (!is.null(name_total)) { total_add <- 1 } else { total_add <- 0 }

  if(all(isColor(fill)) == TRUE){
    # Avec le total au debut (en gris fonce) puis x fois le fill selon le nombre de levels - total_add (1 s'il y a un total, le total etant deja un niveau, sinon 0)
    palette <- c(rep(fill, nlevels(tab[[deparse(substitute(var))]]) - total_add), "grey40")
  } else {
    palette <- c(rep("indianred4", nlevels(tab[[deparse(substitute(var))]]) - total_add), "grey40")
    warning("La couleur indiquee dans pal n'existe pas : la palette par defaut est utilisee")
  }

  # Creer max_ggplot
  max_ggplot <- max(tab[[deparse(substitute(value))]])

  # Si reorder == T
  if (reorder == T) {
    if (!is.null(name_total))  {
      # On cree un vecteur pour ordonner les levels de var selon value, en mettant Total et NA en premier (= en dernier sur le graphique ggplot)
      levels <- c(
        name_total,
        NA,
        levels(reorder(
          tab[[deparse(substitute(var))]],
          tab[[deparse(substitute(value))]],
          FUN = "median",
          decreasing = T
        ))[levels(reorder(
          tab[[deparse(substitute(var))]],
          tab[[deparse(substitute(value))]],
          FUN = "median",
          decreasing = T
        )) != name_total]
      )
    }
    if (is.null(name_total))  {
      # On cree un vecteur pour ordonner les levels de var selon value, en mettant NA en premier (= en dernier sur le graphique ggplot)
      levels <- c(
        NA,
        levels(reorder(
          tab[[deparse(substitute(var))]],
          tab[[deparse(substitute(value))]],
          FUN = "median",
          decreasing = T
          )
          )
      )
    }
  }

  # Si reorder == F
  if (reorder == F) {
    if (!is.null(name_total))  {
      # On cree un vecteur pour ordonner les levels de var pour mettre Total et NA en premier (= en dernier sur le graphique ggplot)
      levels <- c(
        name_total,
        NA,
        rev(
          levels(
            tab[[deparse(substitute(var))]]
          )
        )[rev(
          levels(
            tab[[deparse(substitute(var))]]
          ) != name_total
        )]
      )
    }
    if (is.null(name_total))  {
      # On cree un vecteur pour ordonner les levels de var pour mettre NA en premier (= en dernier sur le graphique ggplot)
      levels <- c(
        NA,
        rev(
          levels(
            tab[[deparse(substitute(var))]]
          )
        )
      )
    }
  }

  # Dans le vecteur qui ordonne les levels, on a mis un NA => Or parfois pas de missing
  # On les supprime donc ssi pas de missing sur la variable de var
  if (sum(is.na(tab[[deparse(substitute(var))]])) == 0)  {
    levels <- levels[!is.na(levels)]
  }

  # On definit l'ordre du facteur dans tab, pour que les couleurs soient associees aux bonnes modalites
  tab <- tab %>%
    mutate(
      "{{ var }}" := factor({{ var }}, levels = rev(levels)) # rev car ggplot ordonne dans le sens inverse (a cause du coord_flip() sans doute)
    )

  # Par coherence avec autres fonctions => si xlab/ylab == "", alors NULL (pour le faire disparaitre sur le ggplot)
  if(all(!is.null(xlab), xlab == "")){
    xlab <- NULL
  }
  if(all(!is.null(ylab), ylab == "")){
    ylab <- NULL
  }

  # On cree le graphique

  graph <- tab %>%
    ggplot(aes(
      x = {{ var }},
      y = {{ value }},
      fill = {{ var }}
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
    scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = wrap_width_y),
                     limits = levels) +
    scale_fill_manual(
      values = palette,
      na.value = "grey"
    ) +
    labs(title = title,
         subtitle = subtitle,
         y = xlab,
         x = ylab) +
    coord_flip()

  # Pour caption

  if (!is.null(caption) & !is.null(pvalue)) { # Permet de passer a la ligne par rapport au test stat
    caption <- paste0("\n", stringr::str_wrap(caption, width = 100))
  }
  if (!is.null(pvalue)) {
    graph <- graph +
      labs(
        caption = paste0(
          "H0 : ", scales::pvalue(pvalue, add_p = T),
          caption
        )
      )
  }
  if (is.null(pvalue)) {
    graph <- graph +
      labs(
        caption = stringr::str_wrap(caption, width = 100)
      )
  }

  # Ajouter les facets au besoin + scale_y si facet
  if (!quo_is_null(quo_facet)) {
    graph <- graph +
      facet_wrap(vars({{ facet }})) +
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

  # Ajouter les IC s'ils sont presents
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

  # Ajouter les valeurs calculees
  if (show_value == TRUE) {
    graph <- graph +
      geom_text(
        aes(
          y = ({{ value }}) + (0.01 * max_ggplot),
          label = paste0(stringr::str_replace(round({{ value }} * scale,
                                                    digits = digits),
                                              "[.]",
                                              dec),
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
        hjust = 0, # Justifie a droite
        vjust = 0.4
      )
  }


  # 4. RESULTAT --------------------

  return(graph)

}
