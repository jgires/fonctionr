#' distrib_discrete
#'
#' Function describe the distribution of a discrete variable from complex survey data. It produces a table and a graphic.
#'
#' @param data A dataframe or an object from the survey package or an object from the srvyr package.
#' @param quali_var The discrete variable that is studied.
#' @param facet_var A variable defining the faceting group.
#' @param filter_exp An expression that filters the data, preserving the design.
#' @param prop_method Type of proportion method to use. See svyciprop in survey package for details. Default is the beta method.
#' @param ... All options possible in as_survey_design in srvyr package.
#' @param na.rm TRUE if you want to remove the NAs in quali_var. FALSE if you want to create a NA category in the graphic and the table. Default is TRUE.
#' @param fill Colour of the bars. NA bar, in case if na.rm.group = FALSE, and total bar are always in grey.
#' @param show_n TRUE if you want to show on the graphic the number of individuals in the sample in each group. FALSE if you do not want to show this number. Default is FALSE.
#' @param show_value TRUE if you want to show the proportion of each category on the graphic. FALSE if you do not want to show the proportion.
#' @param reorder TRUE if you want to reorder the categories according to their proportion. NA value, in case if na.rm = FALSE, is not included in the reorder.
#' @param scale Denominator of the proportion. Default is 100 to interprets numbers as percentages.
#' @param dodge Width of the bar, between 0 and 1.
#' @param unit Unit showed in the graphic. Default is percent.
#' @param error_bar TRUE if you want to show the error bars on the graphic. FALSE if you do not want to show the error bars.
#' @param caption Caption in the graphic.
#' @param title Title of the graphic.
#' @param subtitle Subtitle of the graphic.
#' @param xlab X label on the graphic. As coord_flip() is used in the graphic, xlab refers to the x label on the graphic, after the coord_flip(), and not to the x variable in the data.
#' @param ylab Y label on the graphic. As coord_flip() is used in the graphic, xlab refers to the x label on the graphic, after the coord_flip(), and not to the x variable in the data.
#' @param show_labs TRUE if you want to show axes, titles and caption labels. FALSE if you do not want to show any label on axes and titles. Default is TRUE.
#' @param font Font used in the graphic. Available fonts, included in the package itself, are "Roboto", "Montserrat" and "Gotham Narrow". Default is "Roboto".
#' @param digits Numbers of digits showed on the values labels on the graphic. Default is 0.
#' @param wrap_width Number of characters before going to the line. Applies to the labels of the categories. Default is 25.
#' @param export_path Path to export the results in an xlsx file. The file includes two sheets : the table and the graphic.
#'
#' @return A list that contains a table, a graphic and a statistical test
#' @import rlang
#' @import ggplot2
#' @import stringr
#' @import survey
#' @import scales
#' @import srvyr
#' @import dplyr
#' @import showtext
#' @import sysfonts
#' @export
#'
#' @examples
#'
distrib_discrete <- function(data, # Données en format srvyr
                             quali_var, # Variable catégorielle
                             facet_var = NULL,
                             filter_exp = NULL,
                             ...,
                             na.rm = T,
                             fill = "sienna2",
                             show_n = FALSE,
                             show_value = TRUE,
                             reorder = FALSE,
                             scale = 100,
                             dodge = 0.9,
                             prop_method = "beta", # Possibilité de choisir la methode d'ajustement des IC, car empiriquement, j'ai eu des problèmes avec logit
                             unit = "%",
                             dec = ",",
                             error_bar = T,
                             caption = NULL,
                             title = NULL, # Le titre du graphique
                             subtitle = NULL,
                             xlab = NULL, # Le nom de l'axe de la variable catégorielle
                             ylab = NULL,
                             show_labs = TRUE,
                             font ="Roboto", # Quelle font par défaut?
                             digits = 0,
                             wrap_width = 25,
                             export_path = NULL) {

  # Un check impératif
  if((missing(data) | missing(quali_var)) == TRUE){
    stop("Les arguments data et quali_var doivent être remplis")
  }

  # Check des autres arguments
  check_character(arg = list(prop_method, unit, caption, title, subtitle, xlab, font, fill, export_path))
  check_logical(arg = list(show_n, show_labs, show_value, reorder, error_bar, na.rm))
  check_numeric(arg = list(scale, digits, dodge, wrap_width))

  # Petite fonction utile
  `%ni%` <- Negate(`%in%`)

  # On crée une quosure de facet_var & filter_exp => pour if statements dans la fonction (voir ci-dessous)
  # Solution trouvée ici : https://rpubs.com/tjmahr/quo_is_missing
  quo_facet <- enquo(facet_var)
  quo_filter <- enquo(filter_exp)

  # On procède d'abord à un test : il faut que toutes les variables entrées soient présentes dans data => sinon stop et erreur
  # On crée un vecteur string qui contient toutes les variables entrées
  vars_input_char <- c(as.character(substitute(quali_var)))
  # On ajoute facet si non-NULL
  if(!quo_is_null(quo_facet)){
    vars_input_char <- c(vars_input_char, as.character(substitute(facet_var)))
  }
  # On ajoute filter si non-NULL
  if(!quo_is_null(quo_filter)){
    vars_filter <- all.vars(substitute(filter_exp))
    vars_input_char <- c(vars_input_char, as.character(vars_filter))
  }
  # Ici la contition et le stop à proprement parler
  # Si data.frame
  if(any(class(data) %ni% c("survey.design2","survey.design")) & any(class(data) %ni% c("tbl_svy")) & any(class(data) %in% c("data.frame"))){
    if(all(vars_input_char %in% names(data)) == FALSE){
      stop("Au moins une des variables introduites dans quali_var, filter_exp ou facet n'est pas présente dans data")
    }
  }
  # Si objet sondage
  if(any(class(data) %in% c("survey.design2","survey.design","tbl_svy","svyrep.design"))){
    if(all(vars_input_char %in% names(data[["variables"]])) == FALSE){
      stop("Au moins une des variables introduites dans quali_var, filter_exp ou facet n'est pas présente dans data")
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

 # On supprime les NA de quali_var si na.rm == T
  if(na.rm == T){
    data_W <- data_W %>%
      filter(!is.na({{ quali_var }}))
    # idem sur la variable de facet si non-NULL
    if(!quo_is_null(quo_facet)){
      data_W <- data_W %>%
        filter(!is.na({{ facet_var }}))
    }
  }

  # On convertit en facteurs si pas facteurs
  data_W <- data_W %>%
    mutate(
      "{{ quali_var }}" := droplevels(as.factor({{ quali_var }})) # droplevels pour éviter qu'un level soit encodé alors qu'il n'a pas d'effectifs (pb pour le test khi2)
    )
  # On convertit également la variable de facet en facteur si facet non-NULL
  if(!quo_is_null(quo_facet)){
    data_W <- data_W %>%
      mutate(
        "{{ facet_var }}" := droplevels(as.factor({{ facet_var }}))) # droplevels pour éviter qu'un level soit encodé alors qu'il n'a pas d'effectifs (pb pour le test khi2)
  }

  # Faire la table ------------------

  # On calcule les fréquences relatives
  if (quo_is_null(quo_facet)) {
    tab <- data_W %>%
      group_by({{ quali_var }}) %>%
      srvyr::summarize(prop = survey_prop(vartype = "ci", proportion = T, prop_method = prop_method),
                       n_sample = unweighted(n()),
                       n_weighted = survey_total(vartype = "ci"), # si l'on met les poids pondéré, je trouve nécessaire et pertinent de mettre leurs IC
                       )
  }
  if (!quo_is_null(quo_facet)) {
    tab <- data_W %>%
      group_by({{ facet_var }}, {{ quali_var }}) %>%
      srvyr::summarize(prop = survey_prop(vartype = "ci", proportion = T, prop_method = prop_method),
                       n_sample = unweighted(n()),
                       n_weighted = survey_total(vartype = "ci"), # si l'on met les poids pondéré, je trouve nécessaire et pertinent de mettre leurs IC
                       )
  }

  # Faire le graphique ---------------

  # On crée la palette : x fois la couleur selon le nombre de levels
  palette <- c(rep(fill, nlevels(tab[[deparse(substitute(quali_var))]])))

  # On calcule la valeur max de la proportion, pour l'écart des geom_text dans le ggplot
  max_ggplot <- max(tab$prop, na.rm = TRUE)

  if (reorder == T ) {
    # On crée un vecteur pour ordonner les levels de quali_var selon prop, en mettant NA en premier (= en dernier sur le graphique ggplot)
    levels <- c(
      NA,
      levels(reorder(
        tab[[deparse(substitute(quali_var))]],
        tab[["prop"]],
        FUN = median,
        decreasing = T
      ))
    )
  }

  if (reorder == F) {
    # On crée un vecteur pour ordonner les levels de quali_var pour mettre NA en premier (= en dernier sur le graphique ggplot)
    levels <- c(
      NA,
      rev(
        levels(
          tab[[deparse(substitute(quali_var))]]
        )
      )
    )
  }

  # Dans le vecteur qui ordonne les levels, on a mis un NA => Or parfois pas de missing pour quali_var, même si na.rm = F !
  # On les supprime donc ssi na.rm = F et pas de missing sur la variable quali_var **OU** na.rm = T
  if ((na.rm == F & sum(is.na(tab[[deparse(substitute(quali_var))]])) == 0) | na.rm == T)  {
    levels <- levels[!is.na(levels)]
  }

  # On charge et active les polices
  load_and_active_fonts()

  # Le graphique proprement dit

  graph <- tab %>%
    ggplot(aes(
      x = {{ quali_var }},
      y = prop,
      fill = {{ quali_var }})
      ) +
    geom_bar(
      width = dodge,
      stat="identity"
      ) +
    scale_fill_manual(
      values = palette,
      na.value = "grey"
      ) +
    scale_x_discrete(
      labels = function(x) str_wrap(x, width = wrap_width),
      limits = levels
      ) +
    theme_fonctionr(font = font) +
    theme(
      legend.position = "none"
    ) +
    coord_flip() +
    labs(title = title,
         subtitle = subtitle,
         caption = caption)

  # Ajouter les axes au besoin
  if(show_labs == TRUE){
    # X ---
    if(any(is.null(xlab), xlab != "")){
      graph <- graph +
        labs(#x = NULL, # Pour cette fonction, x est vide dans tous les cas (à voir si c'est adapté dans tous les cas)
             y = ifelse(is.null(xlab),
                        paste0("Distribution : ", deparse(substitute(quali_var))),
                        xlab))
    }
    if(all(!is.null(xlab), xlab == "")){
      graph <- graph +
        labs(#x = NULL, # Pour cette fonction, x est vide dans tous les cas (à voir si c'est adapté dans tous les cas)
             y = NULL)
    }

    # Y ---
    if(any(is.null(ylab), ylab != "")){
      if(!is.null(ylab)){
        graph <- graph +
          labs(x = ylab)
      }
    }
    if(all(!is.null(ylab), ylab == "")){
      graph <- graph +
        labs(x = NULL)
    }
  }

  # Masquer les axes si show_labs == FALSE
  if(show_labs == FALSE){
    graph <- graph +
      labs(x = NULL,
           y = NULL)
    }

  # Ajouter les IC si error_bar == T
  if (error_bar == T) {
    graph <- graph +
      geom_errorbar(aes(ymin = prop_low,
                        ymax = prop_upp),
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
          label = paste0(str_replace(round(prop * scale,
                                           digits = digits),
                                     "[.]",
                                     dec),
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
        vjust = 0.4
      )
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

  # Retourner les résultat
  res <- list()
  res$tab <- tab
  res$graph <- graph

  if (!is.null(export_path)) {
    # L'export en excel

    # Pour être intégré au fichier excel, le graphique doit être affiché => https://ycphs.github.io/openxlsx/reference/insertPlot.html
    print(graph)

    # On crée ici manuellement le dataframe pour le test, celui-ci n'étant pas encore implémenté
    test_stat_excel <- data.frame(Parameter = c("test.error"),
                                  Value = "Test pas encore implémenté pour quali_distrib()",
                                  row.names = NULL)

    # J'exporte les résultats en Excel
    export_excel(tab_excel = tab,
                 graph = graph,
                 test_stat_excel = test_stat_excel,
                 facet_null = quo_is_null(quo_facet),
                 export_path = export_path,
                 percent_fm = TRUE,
                 fgFill = "sienna3",
                 bivariate = FALSE)
  }

  return(res)
}


#' @rdname distrib_discrete
#' @export
distrib_d <- function(...) {
  distrib_discrete(...)
}
