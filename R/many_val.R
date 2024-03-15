#' many_val
#'
#' Function to compute de proportions of a set of several binary variables. It can use complex survey data. It produces a table and a graphic.
#'
#' @param data A dataframe or an object from the survey package or an object from the srvyr package.
#' @param ... All options possible in as_survey_design in srvyr package.
#' @param list_vars A vector containing names of the dummy variables on which to compute the proportions
#' @param type "mean" to compute means ; "median" to compute medians ; "prop" to compute proportions.
#' @param list_vars_lab names of the variables
#' @param facet A variable defining the faceting group.
#' @param filter_exp An expression that filters the data, preserving the design.
#' @param prop_method Type of proportion method to use. See svyciprop in survey package for details. Default is the beta method.
#' @param reorder TRUE if you want to reorder the variables according to the proportion.
#' @param show_ci TRUE if you want to show the error bars on the graphic. FALSE if you do not want to show the error bars.
#' @param show_n TRUE if you want to show on the graphic the number of individuals in the sample in each group. FALSE if you do not want to show this number. Default is FALSE.
#' @param show_value TRUE if you want to show the proportion in each group on the graphic. FALSE if you do not want to show the proportion.
#' @param show_lab TRUE if you want to show axes, titles and caption labels. FALSE if you do not want to show any label on axes and titles. Default is TRUE.
#' @param scale Denominator of the proportion. Default is 100 to interprets numbers as percentages.
#' @param digits Numbers of digits showed on the values labels on the graphic. Default is 0.
#' @param unit Unit showed in the graphic. Default is percent.
#' @param dec Decimal mark shown on the graphic. Default is ","
#' @param pretty_pal Color palette used on the graphic. The palettes from the packages MetBrewer, MoMAColors and PrettyCols are available.
#' @param direction Direction of the palette color. Default is 1. The opposite direction is -1.
#' @param dodge Width of the bar, between 0 and 1.
#' @param font Font used in the graphic. Available fonts, included in the package itself, are "Roboto", "Montserrat" and "Gotham Narrow". Default is "Roboto".
#' @param wrap_width_y Number of characters before before going to the line. Applies to the labels of the groups. Default is 25.
#' @param title Title of the graphic.
#' @param subtitle Subtitle of the graphic.
#' @param xlab X label on the graphic. As coord_flip() is used in the graphic, xlab refers to the x label on the graphic, after the coord_flip(), and not to the x variable in the data.
#' @param ylab Y label on the graphic. As coord_flip() is used in the graphic, xlab refers to the x label on the graphic, after the coord_flip(), and not to the x variable in the data.
#' @param caption Caption of the graphic.
#' @param export_path Path to export the results in an xlsx file. The file includes two sheets : the table and the graphic.
#'
#' @return A list that contains a table and a graphic
#' @import rlang
#' @import survey
#' @import srvyr
#' @import dplyr
#' @import ggplot2
#' @export
#'
#' @examples
#' # Loading of data
#' data(eusilc, package = "laeken")
#'
#' # Recoding variables
#' eusilc$worker <- 0
#' eusilc$worker[eusilc$pl030 == "1"]<-1
#' eusilc$worker[eusilc$pl030 == "2"]<-1
#' eusilc$austrian<-0
#' eusilc$austrian[eusilc$pb220a == "AT"]<-1
#'
#' # Computation, taking sample design into account
#' eusilc_many_prop <- many_prop(
#' eusilc,
#' list_vars = c(worker,austrian),
#' list_vars_lab = c("% of workers","% of Austrian"),
#' facet = rb090,
#' strata = db040,
#' ids = db030,
#' weight = rb050,
#' title = "Proportion of workers and Autrian according to gender",
#' subtitle = "Example with austrian SILC data from 'laeken' package"
#' )
#' eusilc_many_prop$graph
#' eusilc_many_prop$tab
#'
many_val = function(data,
                    ...,
                    list_vars,
                     type,
                     list_vars_lab = NULL,
                     facet = NULL,
                     filter_exp = NULL,
                     # na.rm.facet, #à compléter
                     # na.var,#à compléter
                     prop_method = "beta", # Possibilité de choisir la methode d'ajustement des IC, car empiriquement, j'ai eu des problèmes avec logit
                     reorder = FALSE,
                     show_ci = T,
                     show_n = FALSE,
                     show_value = TRUE, # Possibilité de ne pas vouloir avoir les valeurs sur le graphique
                     show_lab = TRUE,
                     scale = NULL,
                     digits = 0,
                     unit = NULL,
                     dec = ",",
                     pretty_pal = "Egypt",
                     direction = 1,
                     dodge = 0.9,
                     font ="Roboto",
                     wrap_width_y = 25,
                     title = NULL, # Le titre du graphique
                     subtitle = NULL,
                     xlab = NULL, # Le nom de l'axe de la variable catégorielle
                     ylab = NULL,
                     caption = NULL,
                     export_path = NULL){

  # Check des arguments nécessaires
  if(missing(type) == TRUE){
    stop("L'argument type doit être rempli")
  }
  if((missing(data) | missing(list_vars)) == TRUE){
    stop("Les arguments data et list_vars doivent être remplis")
  }

  # Check des autres arguments
  check_arg(
    arg = list(
      type = type,
      prop_method = prop_method,
      unit = unit,
      dec = dec,
      pretty_pal = pretty_pal,
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
    arg = list(list_vars_lab = list_vars_lab),
    short = F,
    type = "character"
  )
  check_arg(
    arg = list(
      reorder = reorder,
      show_ci = show_ci,
      show_n = show_n,
      show_value = show_value,
      show_lab = show_lab
    ),
    type = "logical"
  )
  check_arg(
    arg = list(
      scale = scale,
      digits = digits,
      direction = direction,
      dodge = dodge,
      wrap_width_y = wrap_width_y
    ),
    type = "numeric"
  )

  # Check que les arguments avec choix précis sont les bons
  match.arg(type, choices = c("mean", "median", "prop"))

  # Petite fonction utile
  `%ni%` <- Negate(`%in%`)

  # On crée une quosure de facet & filter_exp => pour if statements dans la fonction (voir ci-dessous)
  # Solution trouvée ici : https://rpubs.com/tjmahr/quo_is_missing
  quo_facet <- enquo(facet)
  quo_filter <- enquo(filter_exp)

  # On transforme les colonnes entrées en un vecteur caractère (plus facile pour le code !)
  vec_list_vars <- all.vars(substitute(list_vars))
  names(vec_list_vars) <- rep("list_vars", length(vec_list_vars))

  # Check que list_vars ne comprend que des variables binaires
  if(type == "prop"){
    check_bin(data = data,
              vec_list_vars = vec_list_vars)
  }

  message("Variable(s) entrées : ", paste(vec_list_vars, collapse = ", "))

  # On procède d'abord à un test : il faut que toutes les variables entrées soient présentes dans data => sinon stop et erreur
  # On crée un vecteur string qui contient toutes les variables entrées
  vars_input_char <- vec_list_vars
  # On ajoute facet si non-NULL
  if(!quo_is_null(quo_facet)){
    vec_facet <- c(facet = as.character(substitute(facet)))
    vars_input_char <- c(vars_input_char, vec_facet)
  }
  # On ajoute filter si non-NULL
  if(!quo_is_null(quo_filter)){
    vec_filter_exp <- all.vars(substitute(filter_exp))
    names(vec_filter_exp) <- rep("filter_exp", length(vec_filter_exp))
    vars_input_char <- c(vars_input_char, vec_filter_exp)
  }

  # Ici la condition et le stop à proprement parler
  check_input(data,
              vars_input_char)

  # On convertit d'abord en objet srvyr
  data_W <- convert_to_srvyr(data, ...)

  # On ne garde que les colonnes entrées en input
  data_W <- data_W %>%
    select(all_of(unname(vars_input_char)))

  # On filtre si filter est non NULL
  if(!quo_is_null(quo_filter)){
    data_W <- data_W %>%
      filter({{ filter_exp }})
  }

  # On supprime les NA sur la variable de facet si non-NULL
  if(!quo_is_null(quo_facet)){
    data_W <- data_W %>%
      filter(!is.na({{ facet }}))
  }

  # On supprime les NA sur la/les variable(s) entrées dans tous les cas, sinon ambigu => de cette façon les n par groupe sont toujours les effectifs pour lesquels la/les variable(s) entrées sont non missing (et pas tout le groupe : ça on s'en fout)
  # On calcule les effectifs avant filtre
  before <- data_W %>%
    summarise(n=unweighted(n()))
  # On filtre via boucle => solution trouvée ici : https://dplyr.tidyverse.org/articles/programming.html#loop-over-multiple-variables
  for (var in vec_list_vars) {
    data_W <- data_W %>%
      filter(!is.na(.data[[var]]))
  }
  # On calcule les effectifs après filtre
  after <- data_W %>%
    summarise(n=unweighted(n()))
  # On affiche le nombre de lignes supprimées (pour vérification)
  message(paste0(before[[1]] - after[[1]]), " lignes supprimées avec valeur(s) manquante(s) pour le(s) variable(s) entrées")

  # On convertit la variable de facet en facteur si facet non-NULL
  if (!quo_is_null(quo_facet)) {
    data_W <- data_W %>%
      mutate(
        "{{ facet }}" := droplevels(as.factor({{ facet }})) # droplevels pour éviter qu'un level soit encodé alors qu'il n'a pas d'effectifs (pb pour le test khi2)
        )
  }

  # Si facet
  if (!quo_is_null(quo_facet)) {
    data_W <- data_W %>%
      group_by({{ facet }})
  }
  tab <- tibble()
  # On calcule les proportions
  if(type == "prop"){
    for (i in vec_list_vars) {
      tab_i <- data_W %>%
        summarise(
          list_col = i,
          indice = survey_mean(.data[[i]], na.rm = T, proportion = T, prop_method = prop_method, vartype = "ci"),
          n_sample = unweighted(n()),
          n_true_weighted = survey_total(.data[[i]], na.rm = T, vartype = "ci"),
          n_tot_weighted = survey_total(vartype = "ci")
        )

      tab <- rbind(tab, tab_i)
    }
  }
  # On calcule les moyennes/médianes
  if(type == "median" | type == "mean"){
    for (i in vec_list_vars) {
      tab_i <- data_W %>%
        summarise(
          list_col = i,
          indice = if (type == "median") {
            survey_median(.data[[i]], na.rm = T, vartype = "ci")
          } else if (type == "mean") survey_mean(.data[[i]], na.rm = T, vartype = "ci"),
          n_sample = unweighted(n()),
          n_weighted = survey_total(vartype = "ci")
        )

      tab <- rbind(tab, tab_i)
    }
  }

  # On remplace list_vars par les labels list_vars_lab
  if (!is.null(list_vars_lab)) {

    # vérifier que list_vars a une même longueur que list_vars_lab
    # si non, message avec erreur...
    if (length(vec_list_vars) != length(list_vars_lab)) {
      message("Le nombre de labels n'est pas égal au nombre de variables")

    # si oui, on remplace dans tab$list_col le nom des variables par les labels définis par l'utilisateur dans list_vars_lab
    } else {

      for (i in seq_along(vec_list_vars)) {
        tab[["list_col"]][tab[["list_col"]] == vec_list_vars[i]] <- list_vars_lab[i]
      }
      # On définit l'ordre tel qu'il est entré par l'utilisateur (pour ggplot)
      tab$list_col <- factor(tab$list_col, levels = list_vars_lab)
    }
  }
  # On définit l'ordre tel qu'il est entré par l'utilisateur (pour ggplot)
  if (is.null(list_vars_lab)) {
    tab$list_col <- factor(tab$list_col, levels = vec_list_vars)
  }

  # Si reorder = T, on crée un vecteur pour ordonner les levels
  if (reorder == T) {
    levels <- levels(reorder(
      tab[["list_col"]],
      tab[["indice"]],
      FUN = median,
      decreasing = T
    ))
  }

  # Si reorder = F, l'ordre = celui rentré en input
  if (reorder == F) {
    if (length(vec_list_vars) == length(list_vars_lab)) {
      levels <- rev(list_vars_lab)
    } else {
      levels <- rev(vec_list_vars)
    }
  }

  # On crée la palette avec le package met.brewer
  if(pretty_pal %in% names(MetBrewer::MetPalettes)){
    palette <- as.character(MetBrewer::met.brewer(name = pretty_pal, n = nlevels(tab[["list_col"]]), type = "continuous", direction = direction))
  }
  #ou la crée avec le package MoMAColors
  if(pretty_pal %in% names(MoMAColors::MoMAPalettes)){
    palette <- as.character(MoMAColors::moma.colors(palette_name = pretty_pal, n = nlevels(tab[["list_col"]]), type = "continuous", direction = direction))
  }
  # On crée la palette avec le package PrettyCols
  if(pretty_pal %in% names(PrettyCols::PrettyColsPalettes)){
    palette <- as.character(PrettyCols::prettycols(name = pretty_pal, n = nlevels(tab[["list_col"]]), type = "continuous", direction = direction))
  }

  # On calcule la valeur max de la proportion, pour l'écart des geom_text dans le ggplot
  max_ggplot <- max(tab$indice, na.rm = TRUE)

  # On charge et active les polices
  load_and_active_fonts()

  # On définit le nom de l'indicateur (proportion, médiane ou moyenne) et l'échelle qui seront affichées dans le graphique ggplot
  if(type == "prop"){
    # Si l'échelle n'est pas définie par l'utilisateur => échelle = 100
    if(is.null(scale)){
      scale <- 100
    }
    # Si l'unité n'est pas définie par l'utilisateur => unité = "%"
    if(is.null(unit)){
      unit <- "%"
    }
    type_ggplot <- "Proportion"
  }
  # Par contre, pour la médiane et la moyenne => échelle = 1 (équivalence avec la variable entrée)
  if(type == "median"){
    if(is.null(scale)){
      scale <- 1
    }
    type_ggplot <- "Médiane"
  }
  if(type == "mean"){
    if(is.null(scale)){
      scale <- 1
    }
    type_ggplot <- "Moyenne"
  }

  # On crée le graphique

  graph <- tab %>%
    ggplot(aes(
      x = list_col,
      y = indice,
      fill = list_col
    )) +
    geom_bar(
      width = dodge,
      stat = "identity",
      position = "dodge"
    ) +
    theme_fonctionr(font = font) +
    scale_fill_manual(
      values = palette
    ) +
    scale_x_discrete(
      labels = function(x) stringr::str_wrap(x, width = wrap_width_y),
      limits = levels
    )+
    labs(title = title,
         subtitle = subtitle,
         caption = caption
    ) +
    guides(fill="none") +
    coord_flip()

  # Ajouter les axes
  if(show_lab == TRUE){
      # X ---
      if(any(is.null(xlab), xlab != "")){
        graph <- graph +
          labs(y = ifelse(is.null(xlab),
                          paste0(type_ggplot, " : ", paste(vec_list_vars, collapse = ", ")),
                          xlab))
      }
      if(all(!is.null(xlab), xlab == "")){
        graph <- graph +
          labs(y = NULL)
      }

      # Y ---
      if(any(is.null(ylab), ylab != "")){
        if(!is.null(ylab)){
          graph <- graph +
            labs(x = ylab)
        }
        if(is.null(ylab)){
          graph <- graph +
            labs(x = "Indicateurs")
        }
      }
      if(all(!is.null(ylab), ylab == "")){
        graph <- graph +
          labs(x = NULL)
      }
  }

  # Masquer les axes si show_lab == FALSE
  if(show_lab == FALSE){
    graph <- graph +
      labs(x = NULL,
           y = NULL)
  }

  # Ajouter les facets au besoin + scale_y si facet
  if (!quo_is_null(quo_facet)) {
    graph <- graph +
      facet_wrap(vars({{ facet }})) +
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

  # Ajouter les IC si show_ci == T
  if (show_ci == T) {
    graph <- graph +
      geom_errorbar(aes(ymin = indice_low, ymax = indice_upp),
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
          y = (indice) + (0.01 * max_ggplot),
          label = paste0(stringr::str_replace(round(indice * scale,
                                                    digits = digits),
                                              "[.]",
                                              ","),
                         unit),
          family = font),
        size = 3.5,
        vjust = ifelse(show_ci == T,
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

  # Dans un but de lisibilité, on renomme les indices "mean" ou "median" selon la fonction appelée
  if (type == "prop") {
    tab <- tab %>%
      rename(prop = indice,
             prop_low = indice_low,
             prop_upp = indice_upp)
  }
  if (type == "median") {
    tab <- tab %>%
      rename(median = indice,
             median_low = indice_low,
             median_upp = indice_upp)
  }
  if (type == "mean") {
    tab <- tab %>%
      rename(mean = indice,
             mean_low = indice_low,
             mean_upp = indice_upp)
  }

  # On crée l'objet final
  res <- list()
  res$tab <- tab
  res$graph <- graph

  if (!is.null(export_path)) {
    # L'export en excel

    # Pour être intégré au fichier excel, le graphique doit être affiché => https://ycphs.github.io/openxlsx/reference/insertPlot.html
    print(graph)

    # Pour many_prop, test pas encore implémenté => on crée un data.frame à la main
    test_stat_excel <- data.frame(Parameter = c("test.error"),
                                  Value = "Test pas encore implémenté dans many_prop",
                                  row.names = NULL)

    # J'exporte les résultats en Excel
    export_excel(tab_excel = tab,
                 graph = graph,
                 test_stat_excel = test_stat_excel,
                 facet_null = quo_is_null(quo_facet),
                 export_path = export_path,
                 percent_fm = ifelse(type == "prop", TRUE, FALSE),
                 fgFill = "mediumseagreen",
                 bivariate = FALSE)
  }

  return(res)
}


#' @rdname many_val
#' @export
many_prop <- function(..., type = "prop") {
  many_val(..., type = type)
}


#' @rdname many_val
#' @export
many_median <- function(..., type = "median") {
  many_val(..., type = type)
}


#' @rdname many_val
#' @export
many_mean <- function(..., type = "mean") {
  many_val(..., type = type)
}
