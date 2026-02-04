#' esth_graph
#'
#' Function to construct a graphic following the aestetics of the other functions of functionr from a table
#'
#' @param tab dataframe with the indicators to be ploted.
#' @param var The variable in tab with the labels of the indicator to be ploted.
#' @param value The variable in tab with the values of the indicator to be ploted.
#' @param error_low The variable in tab with the lower bound of the confidence interval. If either error_low or error_upp is NULL error bars are not shown on the graphic.
#' @param error_upp The variable in tab with the upper bound of the confidence interval. If either error_low or error_upp is NULL error bars are not shown on the graphic.
#' @param facet A variable in tab defining the faceting group, if applicable. Default is NULL.
#' @param n_var The variable in tab containing the number of observations for each indicator ploted. Default (NULL) does not show the numbers of observations on the plot.
#' @param pvalue The p-value to show in the caption. It can a numeric value or the pvalue object from a statsistical test.
#' @param reorder TRUE if you want to reorder var according to value. FALSE if you do not want to reorder. Default is FALSE.
#' @param show_value TRUE if you want to show the values on the graphic. FALSE if you do not want to show them. Default is TRUE.
#' @param name_total Name of the var label that may contain the total. When indicated, it is displayed separately (bold name and value color is 'grey40') on the graph.
#' @param scale Denominator of the indicator. Default is 1 to not modify indicators.
#' @param digits Number of decimal places displayed on the values labels on the graphic. Default is 0.
#' @param unit The unit displayed on the grphaic. Default is no unit.
#' @param dec Decimal mark shown on the graphic. Default is ","
#' @param pal For compatibility with old versions.
#' @param col Color of the bars. col must be a R color or an hexadecimal color code. Default is "indianred4". The color of total is always "grey40".
#' @param dodge Width of the bars. Default is 0.9 to let a small space between bars. A value of 1 leads to no space betweens bars. Values higher than 1 are not advised because they cause an overlaping of the bars.
#' @param font Font used in the graphic. See load_and_active_fonts() for available fonts. Default is "Roboto".
#' @param wrap_width_y Number of characters before going to the line for the labels of var Default is 25.
#' @param title Title of the graphic.
#' @param subtitle Subtitle of the graphic.
#' @param xlab X label on the graphic. As coord_flip() is used in the graphic, xlab refers to the x label on the graphic, after the coord_flip(), and not to var in tab.
#' @param ylab Y label on the graphic. As coord_flip() is used in the graphic, ylab refers to the y label on the graphic, after the coord_flip(), and not to value in tab.
#' @param caption Caption of the graphic.
#' @param theme Theme of the graphic. Default is "fonctionr". "IWEPS" adds y axis lines and ticks. NULL uses the default grey ggplot2 theme.
#' @param coef_font A multiplier factor for font size of all fonts on the graphic. Default is 1. Usefull when exporting the graphic for a publication (e.g. in a Quarto document).
#'
#' @return A ggplot graphic.
#' @import rlang
#' @import dplyr
#' @import ggplot2
#' @export
#'
#' @examples
#' # Making fictional dataframe
#'
#' data_test<-data.frame(Indicators = c("Variable 1",
#'                                      "Variable 2",
#'                                      "Variable 3",
#'                                      "Variable 4",
#'                                      "Variable 5",
#'                                      "Tot"),
#'                       Estimates = c(1.52,1.63,2.34,4.15,1.32,2.13),
#'                       IC_low = c(1.32,1.4,1.98,4,14.2,26),
#'                       IC_upp = c(1.73,1.81,22.4,47.44,1.45,2.34),
#'                       sample_size = c(215,300,129,212,189,1045))
#'
#' # Using dataframe to make a plot
#' plot_test<-esth_graph(data_test,
#'            var = Indicators,
#'            value = Estimates,
#'            error_low = IC_low,
#'            error_upp = IC_upp,
#'            n_var = sample_size,
#'            pvalue = .001,
#'            reorder = TRUE,
#'            show_value = TRUE,
#'            name_total = "Tot",
#'            scale = 1,
#'            digits = 1,
#'            unit = "%",
#'            dec = ".",
#'            col = "green4",
#'            dodge = 0.8,
#'            font = "Montserrat",
#'            wrap_width_y = 25,
#'            title = "Plot",
#'            subtitle = "Using fake data",
#'            xlab = "Proportion (in %)",
#'            ylab = "Indicators",
#'            caption = "Source: fictional own calculation",
#'            theme = "IWEPS")
#'
#'# Result is a ggplot
#'plot_test
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
                       pal = NULL,
                       col = "indianred4",
                       dodge = 0.9,
                       font ="Roboto",
                       wrap_width_y = 25,
                       title = NULL,
                       subtitle = NULL,
                       xlab = NULL,
                       ylab = NULL,
                       caption = NULL,
                       theme = "fonctionr",
                       coef_font = 1) {

  # Les arguments par defaut
  formals.args <- formals()
  # On enregistre le call
  call <- match.call()
  # On cree un vecteur avec le nom des arguments definis explicitement par l'utilisateur (sans le nom de la fonction)
  user.args <- names(call[-1])

  # On cree une liste avec les noms et valeurs des arguments definis dans fonctionr_options()
  list_opt_fonctionr <- options()[names(options()) == "fonctionr.options"]
  # On ne retient que les options definies dans fonctionr_options() MAIS qui ne sont pas definies par l'utilisateur et qui sont bien utilisees dans cette fonction (pour ne pas creer d'objets pour rien, source potentielle d'erreur)
  list_opt_fonctionr$fonctionr.options <- list_opt_fonctionr$fonctionr.options[!(names(list_opt_fonctionr$fonctionr.options) %in% user.args) & names(list_opt_fonctionr$fonctionr.options) %in% names(formals.args)]

  # NOTE : on fait la suite SSI il y a des options qui remplissent cette condition
  if(length(list_opt_fonctionr$fonctionr.options > 0)){

    warning(
      "Active parameters in function r_options(): ",
      paste(
        names(list_opt_fonctionr$fonctionr.options),
        collapse = ", "
      )
    )

    # On cree des objets avec les valeurs definies dans la liste pour toutes ces options (= on remplace les arguments par defaut de la fonction)
    for(x in names(list_opt_fonctionr$fonctionr.options)) assign(x, list_opt_fonctionr$fonctionr.options[[x]])
  }


  # 1. CHECKS DES ARGUMENTS --------------------

  # Check des arguments necessaires
  if((missing(tab) | missing(value) | missing(var)) == TRUE){
    stop("Arguments tab, value and var should be filled in")
  }

  # Check si le total existe dans var
  if (!is.null(name_total)) {
    if(!name_total %in% tab[[deparse(substitute(var))]]){
      stop("The name specified for total is not in var")
    }
  }

  # Check des autres arguments
  check_arg(
    arg = list(
      name_total = name_total,
      unit = unit,
      dec = dec,
      col = col,
      font = font,
      title = title,
      subtitle = subtitle,
      xlab = xlab,
      ylab = ylab,
      caption = caption,
      theme = theme
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
      wrap_width_y = wrap_width_y,
      coef_font = coef_font
    ),
    type = "numeric"
  )

  # On cree des quosures => pour if statements dans la fonction (voir ci-dessous)
  # Solution trouvee ici : https://rpubs.com/tjmahr/quo_is_missing
  quo_low <- enquo(error_low)
  quo_up <- enquo(error_upp)
  quo_facet <- enquo(facet)
  quo_n <- enquo(n_var)

  # Check s'il n'y a pas 2 lignes avec des NA
  if (!quo_is_null(quo_facet)) {
    # Avec facet, il ne peut pas y avoir plus de 1 NA par facet
    check_NA <- tab |>
      group_by({{ facet}}) |>
      summarise(n_NA = sum(is.na({{ var }})))
    if(any(check_NA$n_NA > 1)){
      stop("There are several lines with NAs in the variable var.")
    }
  }
  if (quo_is_null(quo_facet)) {
    if(sum(is.na(tab[[deparse(substitute(var))]])) > 1){
      stop("There are several lines with NAs in the variable var.")
    }
  }

  # 2. PROCESSING DES DONNEES --------------------

  # On convertit la variable categorielle en facteur si pas facteur
  tab <- tab |>
    mutate(
      "{{ var }}" := droplevels(as.factor({{ var }}))
    )


  # 3. CREATION DU GRAPHIQUE --------------------

  # On cree la palette : avec le total au debut (en gris fonce) puis x fois la col selon le nombre de levels - 1 (le total etant deja un niveau)
  if(!is.null(col) & all(isColor(col)) == TRUE){
    palette <- c(rep(col, nlevels(tab[[deparse(substitute(var))]]) - 1), "grey40")
  # Si col est NULL ou n'est pas valide => on met la couleur par defaut
  } else {
    if(!is.null(col) & (all(isColor(col)) == FALSE)){ # Warning uniquement si une couleur fausse a ete entree
      warning("col is not valid: default color is used")
    }
    col <- "indianred4" # Alors col == "indianred4"
    palette <- c(rep(col, nlevels(tab[[deparse(substitute(var))]]) - 1), "grey40")
  }
  # Si pas de total, alors pas de gris mais tout en col (indiquee par l'utilisateur ou par defaut si n'existe pas)
  if(is.null(name_total)) {
    palette[palette == "grey40"] <- col
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
  tab <- tab |>
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

  graph <- tab |>
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
    theme_fonctionr(
      font = font,
      theme = theme,
      display = "ggtext",
      coef_font = coef_font
    ) +
    theme(
      legend.position = "none"
    ) +
    scale_x_discrete(
      # fonction interne relabel_ggtext() pour compatibilite avec ggtext
      labels = ~relabel_ggtext(x = ., wrap_width = wrap_width_y, total_name = name_total),
      limits = levels) +
    scale_fill_manual(
      # Les couleurs de la palette sont associees aux levels avec un named vector (pour eviter les erreurs) => le named vector ne comprend pas l'eventuel groupe NA, dont la couleur est geree par na.value (argument ci-dessous)
      values = stats::setNames(rev(palette), levels[!is.na(levels)]),
      na.value = "grey"
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      y = xlab,
      x = ylab
    ) +
    coord_flip()

  # Ajouter les valeurs calculees
  if (show_value == TRUE) {
    graph <- graph +
      geom_text(
        aes(
          y = if(!is.null(name_total)) ifelse({{ var }} != name_total|is.na({{ var }}), {{ value }} + (0.01 * max_ggplot), NA) else {{ value }} + (0.01 * max_ggplot),
          label = paste0(stringr::str_replace(round({{ value }} * scale,
                                                    digits = digits),
                                              "[.]",
                                              dec),
                         unit),
          family = font),
        size = coef_font * fonctionr_font_size(type = "normal"),
        vjust = ifelse(!quo_is_null(quo_low) & !quo_is_null(quo_up),
                       -0.5,
                       0.5),
        hjust = 0,
        color = "black",
        alpha = 0.9,
        # position = position_stack(vjust = .5))
        position = position_dodge(width = dodge)
      )
    if(!is.null(name_total)) {
      graph <- graph +
        geom_text(
          aes(
            y = ifelse({{ var }} == name_total, ({{ value }}) + (0.01 * max_ggplot), NA),
            label = paste0(stringr::str_replace(round({{ value }} * scale,
                                                      digits = digits),
                                                "[.]",
                                                dec),
                           unit),
            family = font),
          size = coef_font * fonctionr_font_size(type = "normal"),
          vjust = ifelse(!quo_is_null(quo_low) & !quo_is_null(quo_up),
                         -0.5,
                         0.5),
          hjust = 0,
          color = "black",
          fontface = "bold",
          alpha = 0.9,
          # position = position_stack(vjust = .5))
          position = position_dodge(width = dodge)
        )
    }

  }

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

  # Ajouter le nombre d'individus au besoin
  if (!quo_is_null(quo_n)) {
    graph <- graph +
      geom_text(
        aes(
          y = 0 + (0.01 * max_ggplot), # Pour ajouter des labels avec les effectifs en dessous des barres
          label = paste0("n=", {{n_var}}),
          family = font),
        size = coef_font * fonctionr_font_size(type = "little"),
        alpha = 0.7,
        hjust = 0, # Justifie a droite
        vjust = 0.4
      )
  }


  # 4. RESULTAT --------------------

  return(graph)

}
