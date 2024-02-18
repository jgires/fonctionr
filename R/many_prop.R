#' many_prop
#'
#' @param data A dataframe or an object from the survey package or an object from the srvyr package.
#' @param list_vars A vector containing names of the dummy variables on which to compute the proportions
#' @param list_vars_lab names of the variables
#' @param facet_var A variable defining the faceting group.
#' @param filter_exp An expression that filters the data, preserving the design.
#' @param ... All options possible in as_survey_design in srvyr package.
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
#' @param fill Colour of the bars.
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
#' facet_var = rb090,
#' strata = db040,
#' ids = db030,
#' weight = rb050,
#' title = "Proportion of workers and Autrian according to gender",
#' subtitle = "Example with austrian SILC data from 'laeken' package"
#' )
#' eusilc_many_prop$graph
#' eusilc_many_prop$tab
#'
many_prop = function(data,
                     list_vars,
                     list_vars_lab = NULL,
                     facet_var = NULL,
                     filter_exp = NULL,
                     ...,
                     # na.rm.facet, #à compléter
                     # na.var,#à compléter
                     prop_method = "beta", # Possibilité de choisir la methode d'ajustement des IC, car empiriquement, j'ai eu des problèmes avec logit
                     reorder = FALSE,
                     show_ci = T,
                     show_n = FALSE,
                     show_value = TRUE, # Possibilité de ne pas vouloir avoir les valeurs sur le graphique
                     show_lab = TRUE,
                     scale = 100,
                     digits = 0,
                     unit = "%",
                     dec = ",",
                     fill = "mediumseagreen",
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
  if((missing(data) | missing(list_vars)) == TRUE){
    stop("Les arguments data et list_vars doivent être remplis")
  }

  # Check des autres arguments
  check_arg(
    arg = list(
      prop_method = prop_method,
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
      dodge = dodge,
      wrap_width_y = wrap_width_y
    ),
    type = "numeric"
  )

  # Petite fonction utile
  `%ni%` <- Negate(`%in%`)

  # On crée une quosure de facet_var & filter_exp => pour if statements dans la fonction (voir ci-dessous)
  # Solution trouvée ici : https://rpubs.com/tjmahr/quo_is_missing
  quo_facet <- enquo(facet_var)
  quo_filter <- enquo(filter_exp)

  # On transforme les colonnes entrées en un vecteur caractère (plus facile pour le code !)
  vec_list_vars <- all.vars(substitute(list_vars))
  message("Variable(s) entrées : ", paste(vec_list_vars, collapse = ", "))

  # On procède d'abord à un test : il faut que toutes les variables entrées soient présentes dans data => sinon stop et erreur
  # On crée un vecteur string qui contient toutes les variables entrées
  vars_input_char <- vec_list_vars
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
      stop("Au moins une des variables introduites dans list_vars, filter_exp ou facet_var n'est pas présente dans data")
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
      stop("Au moins une des variables introduites dans list_vars, filter_exp ou facet_var n'est pas présente dans data")
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
        "{{ facet_var }}" := droplevels(as.factor({{ facet_var }})) # droplevels pour éviter qu'un level soit encodé alors qu'il n'a pas d'effectifs (pb pour le test khi2)
        )
  }

  # On calcule les proportions
  # Si facet
  if (!quo_is_null(quo_facet)) {
    tab <- tibble()
    for (i in vec_list_vars) {
      tab_i <- data_W %>%
        group_by({{ facet_var }}) %>%
        summarise(
          list_col = i,
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
    for (i in vec_list_vars) {
      tab_i <- data_W %>%
        summarise(
          list_col = i,
          prop = survey_mean(.data[[i]], na.rm = T, proportion = T, prop_method = prop_method, vartype = "ci"),
          n_sample = unweighted(n()),
          n_true_weighted = survey_total(.data[[i]], na.rm = T, vartype = "ci"),
          n_tot_weighted = survey_total(vartype = "ci")
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
      tab[["prop"]],
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

  # On calcule la valeur max de la proportion, pour l'écart des geom_text dans le ggplot
  max_ggplot <- max(tab$prop, na.rm = TRUE)

  # On charge et active les polices
  load_and_active_fonts()

  # On crée le graphique

  graph <- tab %>%
    ggplot(aes(
      x = list_col,
      y = prop,
    )) +
    geom_bar(
      width = dodge,
      stat = "identity",
      position = "dodge",
      fill = fill
    ) +
    theme_fonctionr(font = font) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = wrap_width_y),
                     limits = levels
    )+
    labs(title = title,
         subtitle = subtitle,
         caption = caption
    ) +
    coord_flip()

  # Ajouter les axes
  if(show_lab == TRUE){
      # X ---
      if(any(is.null(xlab), xlab != "")){
        graph <- graph +
          labs(y = ifelse(is.null(xlab),
                          paste0("Proportion : ", paste(vec_list_vars, collapse = ", ")),
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
  if (show_ci == T) {
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
          label = paste0(str_replace(round(prop * scale,
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
                 percent_fm = TRUE,
                 fgFill = "mediumseagreen",
                 bivariate = FALSE)
  }

  return(res)
}
