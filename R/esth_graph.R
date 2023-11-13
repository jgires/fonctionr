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
#' @param fill Colour of the bars.
#' @param font Font used in the graphic. Available fonts, included in the package itself, are "Roboto", "Montserrat" and "Gotham Narrow". Default is "Roboto".
#' @param wrap_width Number of characters before before going to the line. Applies to the labels cat_var. Default is 25.
#'
#' @return
#' @export
#'
#' @examples
#'
#'
esth_graph <- function(tab,
                       ind_var,
                       cat_var,
                       facet_var = NULL,
                       unit = "%",
                       caption = NULL,
                       title = NULL, # Le titre du graphique
                       subtitle = NULL,
                       xlab = NULL, # Le nom de l'axe de la variable catégorielle
                       ylab = NULL,
                       scale = 100,
                       digits = 0,
                       n_var = NULL,
                       show_value = TRUE, # Possibilité de ne pas vouloir avoir les valeurs sur le graphique
                       dodge = 0.9,
                       reorder = F,
                       error_low = NULL,
                       error_upp = NULL,
                       fill = "deepskyblue3",
                       font ="Roboto",
                       wrap_width = 25) {


  # On crée une quosure de facet_var & filter_exp => pour if statements dans la fonction (voir ci-dessous)
  # Solution trouvée ici : https://rpubs.com/tjmahr/quo_is_missing
  quo_facet <- enquo(facet_var)
  quo_low <- enquo(error_low)
  quo_up <- enquo(error_upp)
  quo_n <- enquo(n_var)


  #chager les fonts
  load_and_active_fonts()



  #créer max_ggplot
  max_ggplot <- max(tab[[deparse(substitute(ind_var))]])

  #On crée l'ordre pour la variable si reorder = TRUE
  if (reorder == F ) {
    levels <- levels(tab[[deparse(substitute(cat_var))]]
    )
    tab2<-tab
  }


  if (reorder == T ) {
    levels <- levels(reorder(
      tab[[deparse(substitute(cat_var))]],tab[[deparse(substitute(ind_var))]]
    ))
    tab2<-tab %>% arrange({{ind_var}})
  }

  #On crée le graphique
graph <- tab2 %>%
  ggplot(aes(
    x = {{ cat_var }},
    y = {{ind_var}},
    fill = {{ cat_var }}
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
    axis.ticks = element_line(color = "black"),
    axis.text = element_text(color = "black"),
    legend.position = "none"
  ) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = wrap_width),
                   limits = levels)+
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

if (show_value == TRUE) {
  graph <- graph +
    geom_text(
      aes(
        y = ({{ind_var}}) + (0.01 * max_ggplot),
        label = paste0(round({{ind_var}} * scale,
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
