#' many_val_group
#'
#' @param data A dataframe or an object from the survey package or an object from the srvyr package.
#' @param group A variable defining groups be compared.
#' @param list_vars A vector containing names of the dummy variables on which to compute the proportions
#' @param type "mean" to compute means by group ; "median" to compute medians by group ; "prop" to compute medians by group.
#' @param list_vars_lab names of the variables
#' @param facet_var A variable defining the faceting group.
#' @param filter_exp An expression that filters the data, preserving the design.
#' @param ... All options possible in as_survey_design in srvyr package.
#' @param na.rm.group TRUE if you want to remove observations with NA on the group variable or NA on the facet variable. FALSE if you want to create a group with the NA value for the group variable and a facet with the NA value for the facet variable. NA in the variables included in prop_exp are not affected in this argument. All the observation with a NA in the variables included in prop_exp are excluded.
#' @param prop_method Type of proportion method to use. See svyciprop in survey package for details. Default is the beta method.
#' @param position Position adjustment for geom_bar
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
#' @param wrap_width_leg Number of characters before before going to the line. Applies to the labels of the legend. Default is 25.
#' @param legend_ncol Number maximum of colomn in the legend. Default is 4.
#' @param title Title of the graphic.
#' @param subtitle Subtitle of the graphic.
#' @param xlab X label on the graphic. As coord_flip() is used in the graphic, xlab refers to the x label on the graphic, after the coord_flip(), and not to the x variable in the data.
#' @param ylab Y label on the graphic. As coord_flip() is used in the graphic, xlab refers to the x label on the graphic, after the coord_flip(), and not to the x variable in the data.
#' @param legend_lab Legend (fill) label on the graphic.
#' @param caption Caption of the graphic.
#' @param export_path Path to export the results in an xlsx file. The file includes two sheets : the table and the graphic.
#'
#' @return
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
#' eusilc_many_mean_group <- many_mean_group(
#' eusilc,
#' group = rb090,
#' list_vars = c(py010n,py050n,py090n,py100n),
#' list_vars_lab = c("Wage","Self-employement income","unemployement benefit","pension"),
#' strata = db040,
#' ids = db030,
#' weight = rb050,
#' title = "Average incomes according to gender",
#' subtitle = "Example with austrian SILC data from 'laeken' package"
#' )
#' eusilc_many_mean_group$graph
#' eusilc_many_mean_group$tab
#'
many_val_group = function(data,
                          group,
                          list_vars,
                          type,
                          list_vars_lab = NULL,
                          facet_var = NULL,
                          filter_exp = NULL,
                          ...,
                          na.rm.group = T,
                          # na.rm.facet = T,# à compléter
                          # na.var = rm, #à compléter rm = remove, rm.all = remove tous ceux qui ont au moins 1 NA et include, c'est uniquement pour les prop
                          prop_method = "beta", # Possibilité de choisir la methode d'ajustement des IC, car empiriquement, j'ai eu des problèmes avec logit
                          position = "dodge",
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
                          wrap_width_leg = 25,
                          legend_ncol = 4,
                          title = NULL, # Le titre du graphique
                          subtitle = NULL,
                          xlab = NULL, # Le nom de l'axe de la variable catégorielle
                          ylab = NULL,
                          legend_lab = NULL,
                          caption = NULL,
                          export_path = NULL){

  # Check des arguments nécessaires
  if(missing(type) == TRUE){
    stop("L'argument type doit être rempli")
  }
  if((missing(data) | missing(group) | missing(list_vars)) == TRUE){
    stop("Les arguments data, group, list_vars doivent être remplis")
  }

  # Check des autres arguments
  check_arg(
    arg = list(
      type = type,
      prop_method = prop_method,
      position = position,
      unit = unit,
      dec = dec,
      pretty_pal = pretty_pal,
      font = font,
      title = title,
      subtitle = subtitle,
      xlab = xlab,
      ylab = ylab,
      legend_lab = legend_lab,
      caption = caption
    ),
    type = "character"
  )
  check_arg(
    arg = list(
      list_vars_lab = list_vars_lab),
    short = F,
    type = "character"
  )
  check_arg(
    arg = list(
      na.rm.group = na.rm.group,
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
      dodge = dodge,
      wrap_width_y = wrap_width_y,
      wrap_width_leg = wrap_width_leg,
      legend_ncol = legend_ncol
    ),
    type = "numeric"
  )

  # Check que les arguments avec choix précis sont les bons
  match.arg(type, choices = c("mean", "median", "prop"))
  match.arg(position, choices = c("dodge", "stack"))

  # Petite fonction utile
  `%ni%` <- Negate(`%in%`)

  # On crée une quosure de facet_var & filter_exp => pour if statements dans la fonction (voir ci-dessous)
  # Solution trouvée ici : https://rpubs.com/tjmahr/quo_is_missing
  quo_facet <- enquo(facet_var)
  quo_filter <- enquo(filter_exp)

  # On transforme les colonnes entrées en un vecteur caractère (plus facile pour le code !)
  vec_list_vars <- all.vars(substitute(list_vars))

  # Check que list_vars ne comprend que des variables binaires
  if(type == "prop"){
    check_bin(data = data,
              vec_list_vars = vec_list_vars)
  }

  message("Variable(s) entrées : ", paste(vec_list_vars, collapse = ", "))

  # On procède d'abord à un test : il faut que toutes les variables entrées soient présentes dans data => sinon stop et erreur
  # On crée un vecteur string qui contient toutes les variables entrées
  vars_input_char <- c(vec_list_vars, as.character(substitute(group)))
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
      stop("Au moins une des variables introduites dans list_vars, group, filter_exp ou facet_var n'est pas présente dans data")
    }
    # # DESACTIVé : NE FONCTIONNE PAS !
    # # Check du design. Solution trouvée ici : https://stackoverflow.com/questions/70652685/how-to-set-aliases-for-function-arguments-in-an-r-package
    # vars_survey <- as.character(substitute(...()))[names(as.list(substitute(...()))) %in% c("strata", "ids", "weight", "weights", "probs", "variables", "fpc")]
    # if(all(vars_survey %in% names(data)) == FALSE){
    #   stop("Au moins une des variables du design n'est pas présente dans data")
    # }
  }
  # Si objet sondage
  if(any(class(data) %in% c("survey.design2","survey.design","tbl_svy","svyrep.design"))){
    if(all(vars_input_char %in% names(data[["variables"]])) == FALSE){
      stop("Au moins une des variables introduites dans list_vars, group, filter_exp ou facet_var n'est pas présente dans data")
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

  # On supprime les NA sur le groupe si na.rm.group = T
  if (na.rm.group == T) {
    data_W <- data_W %>%
      filter(!is.na({{ group }}))
    # idem sur la variable de facet si non-NULL
    if(!quo_is_null(quo_facet)){
      data_W <- data_W %>%
        filter(!is.na({{ facet_var }}))
    }
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

  # On convertit la variable de groupe en facteur si pas facteur
  data_W <- data_W %>%
    mutate(
      "{{ group }}" := droplevels(as.factor({{ group }})) # droplevels pour éviter qu'un level soit encodé alors qu'il n'a pas d'effectifs (pb pour le test khi2)
    )
  # On convertit également la variable de facet en facteur si facet non-NULL
  if(!quo_is_null(quo_facet)){
    data_W <- data_W %>%
      mutate(
        "{{ facet_var }}" := droplevels(as.factor({{ facet_var }}))) # droplevels pour éviter qu'un level soit encodé alors qu'il n'a pas d'effectifs (pb pour le test khi2)
  }

  if(type == "prop"){
    # On calcule les proportions par groupe
    tab <- tibble()
    if(quo_is_null(quo_facet)){
      for(i in vec_list_vars) {
        tab_i <- data_W %>%
          group_by({{ group }}) %>%
          summarise(
            list_col = i,
            indice = survey_mean(.data[[i]], na.rm = T, proportion = T, prop_method = prop_method, vartype = "ci"),
            n_sample = unweighted(n()),
            n_true_weighted = survey_total(.data[[i]] == 1, na.rm = T, vartype = "ci"),
            n_tot_weighted = survey_total(vartype = "ci")
          )

        tab <- rbind(tab, tab_i)
      }
    }
    # Version avec facet
    if(!quo_is_null(quo_facet)){
      for(i in vec_list_vars) {
        tab_i <- data_W %>%
          group_by({{ facet_var }}, {{ group }}) %>%
          summarise(
            list_col = i,
            indice = survey_mean(.data[[i]], na.rm = T, proportion = T, prop_method = prop_method, vartype = "ci"),
            n_sample = unweighted(n()),
            n_true_weighted = survey_total(.data[[i]] == 1, na.rm = T, vartype = "ci"),
            n_tot_weighted = survey_total(vartype = "ci")
          )

        tab <- rbind(tab, tab_i)
      }
    }
  }
  if(type == "median"){
    # On calcule les proportions par groupe
    tab <- tibble()
    if(quo_is_null(quo_facet)){
      for(i in vec_list_vars) {
        tab_i <- data_W %>%
          group_by({{ group }}) %>%
          summarise(
            list_col = i,
            indice = survey_median(.data[[i]], na.rm = T, vartype = "ci"),
            n_sample = unweighted(n()),
            n_weighted = survey_total(vartype = "ci")
          )

        tab <- rbind(tab, tab_i)
      }
    }
    # Version avec facet
    if(!quo_is_null(quo_facet)){
      for(i in vec_list_vars) {
        tab_i <- data_W %>%
          group_by({{ facet_var }}, {{ group }}) %>%
          summarise(
            list_col = i,
            indice = survey_median(.data[[i]], na.rm = T, vartype = "ci"),
            n_sample = unweighted(n()),
            n_tot_weighted = survey_total(vartype = "ci")
          )

        tab <- rbind(tab, tab_i)
      }
    }
  }
  if(type == "mean"){
    # On calcule les proportions par groupe
    tab <- tibble()
    if(quo_is_null(quo_facet)){
      for(i in vec_list_vars) {
        tab_i <- data_W %>%
          group_by({{ group }}) %>%
          summarise(
            list_col = i,
            indice = survey_mean(.data[[i]], na.rm = T, vartype = "ci"),
            n_sample = unweighted(n()),
            n_weighted = survey_total(vartype = "ci")
          )

        tab <- rbind(tab, tab_i)
      }
    }
    # Version avec facet
    if(!quo_is_null(quo_facet)){
      for(i in vec_list_vars) {
        tab_i <- data_W %>%
          group_by({{ facet_var }}, {{ group }}) %>%
          summarise(
            list_col = i,
            indice = survey_mean(.data[[i]], na.rm = T, vartype = "ci"),
            n_sample = unweighted(n()),
            n_tot_weighted = survey_total(vartype = "ci")
          )

        tab <- rbind(tab, tab_i)
      }
    }
  }

  # On remplace list_vars par les labels list_vars_lab
  if (!is.null(list_vars_lab)) {

    # vérifier que list_vars a une même longueur que list_vars_lab
    # si non, message avec erreur...
    if (length(vec_list_vars) != length(list_vars_lab)) {
      warning("Le nombre de labels n'est pas égal au nombre de variables : les labels ne sont pas pris en compte")

      # On crée un facteur avec l'ordre tel qu'il est entré par l'utilisateur (pour ggplot)
      tab$list_col <- factor(tab$list_col, levels = rev(vec_list_vars))

      # si oui, on remplace dans tab$list_col le nom des variables par les labels définis par l'utilisateur dans list_vars_lab
    } else {

      for (i in seq_along(vec_list_vars)) {
        tab[["list_col"]][tab[["list_col"]] == vec_list_vars[i]] <- list_vars_lab[i]
      }
      # On définit l'ordre tel qu'il est entré par l'utilisateur (pour ggplot)
      tab$list_col <- factor(tab$list_col, levels = rev(list_vars_lab))
    }
  }
  # On crée un facteur avec l'ordre tel qu'il est entré par l'utilisateur (pour ggplot)
  if (is.null(list_vars_lab)) {
    tab$list_col <- factor(tab$list_col, levels = rev(vec_list_vars))
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
    mutate("{{ group }}" := forcats::fct_rev({{ group }})) %>%
    ggplot(aes(
      x = {{ group }},
      y = indice,
      fill = list_col
    )) +
    geom_bar(
      width = dodge,
      stat = "identity",
      position = position
    ) +
    theme_fonctionr(font = font) +
    theme(
      legend.position = "bottom"
    ) +
    scale_fill_manual(
      values = palette,
      labels = function(x) stringr::str_wrap(x, width = wrap_width_leg),
      na.value = "grey"

    ) +
    scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = wrap_width_y))+
    labs(title = title,
         subtitle = subtitle,
         caption = caption
    ) +
    guides(fill = guide_legend(ncol = legend_ncol,
                               reverse = TRUE)) +
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
    }
    if(all(!is.null(ylab), ylab == "")){
      graph <- graph +
        labs(x = NULL)
    }

    # LEGEND ---
    if(all(!is.null(legend_lab), legend_lab != "")){
      graph <- graph +
        labs(fill = stringr::str_wrap(legend_lab, wrap_width_leg))
    }
    if(all(!is.null(legend_lab), legend_lab == "")){
      graph <- graph +
        labs(fill = NULL)
    }
    if(is.null(legend_lab)){
      graph <- graph +
        labs(fill = NULL)
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

  # Ajouter les IC si show_ci == T
  if (show_ci == T & position == "dodge") {
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
          y = if (position == "dodge") (indice) + (0.01 * max_ggplot) else indice,
          label = paste0(stringr::str_replace(round(indice * scale,
                                                    digits = digits),
                                              "[.]",
                                              dec),
                         unit),
          family = font),
        size = 3,
        vjust = if (position == "dodge") ifelse(show_ci == T, -0.25, 0.5) else 0.4,
        hjust = if (position == "dodge") "left" else "center",
        color = if (position == "dodge") "black" else "white",
        alpha = 0.9,
        # position = position_stack(vjust = .5))
        position = if (position == "dodge") position_dodge(width = dodge) else position_stack(vjust = .5)
      )
  }

  # Ajouter le nombre d'individus au besoin
  if (show_n == TRUE) {
    graph <- graph +
      geom_text(
        aes(
          y = if (position == "dodge") 0 + (0.01 * max_ggplot) else indice, # Pour ajouter des labels avec les effectifs en dessous des barres
          label = paste0("n=", n_sample),
          family = font),
        size = 3,
        alpha = 0.7,
        hjust = 0, # Justifié à droite
        vjust = 0.4,
        position = if (position == "dodge") position_dodge(width = dodge) else position_stack(vjust = 0)
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

    # Pour many_val_group, test pas encore implémenté => on crée un data.frame à la main
    test_stat_excel <- data.frame(Parameter = c("test.error"),
                                    Value = "Test pas encore implémenté dans many_val_group",
                                    row.names = NULL)

    # J'exporte les résultats en Excel
    export_excel(tab_excel = tab,
                 graph = graph,
                 test_stat_excel = test_stat_excel,
                 facet_null = quo_is_null(quo_facet),
                 export_path = export_path,
                 percent_fm = ifelse(type == "prop", TRUE, FALSE),
                 fgFill = "mediumvioletred",
                 bivariate = TRUE)
  }

  return(res)
}


#' @rdname many_val_group
#' @export
many_prop_group <- function(..., type = "prop") {
  many_val_group(..., type = type)
}


#' @rdname many_val_group
#' @export
many_median_group <- function(..., type = "median") {
  many_val_group(..., type = type)
}


#' @rdname many_val_group
#' @export
many_mean_group <- function(..., type = "mean") {
  many_val_group(..., type = type)
}
