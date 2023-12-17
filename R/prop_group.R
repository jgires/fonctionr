#' prop_group
#'
#' Function to compare proportions in different groups from complex survey data. It produces a table, a graphic and a statistical test.
#'
#' @param data A dataframe or an object from the survey package or an object from the srvyr package.
#' @param group A variable defining groups be compared.
#' @param prop_exp An expression that define the proportion to be computed.
#' @param facet_var A variable defining the faceting group.
#' @param filter_exp An expression that filters the data, preserving the design.
#' @param prop_method Type of proportion method to use. See svyciprop in survey package for details. Default is the beta method.
#' @param ... All options possible in as_survey_design in srvyr package.
#' @param unit Unit showed in the graphic. Default is percent.
#' @param caption Caption of the graphic.
#' @param title Title of the graphic.
#' @param subtitle Subtitle of the graphic.
#' @param xlab X label on the graphic. As coord_flip() is used in the graphic, xlab refers to the x label on the graphic, after the coord_flip(), and not to the x variable in the data.
#' @param ylab Y label on the graphic. As coord_flip() is used in the graphic, xlab refers to the x label on the graphic, after the coord_flip(), and not to the x variable in the data.
#' @param scale Denominator of the proportion. Default is 100 to interprets numbers as percentages.
#' @param digits Numbers of digits showed on the values labels on the graphic. Default is 0.
#' @param show_labs TRUE if you want to show axes, titles and caption labels. FALSE if you do not want to show any label on axes and titles. Default is TRUE.
#' @param show_n TRUE if you want to show on the graphic the number of individuals in the sample in each group. FALSE if you do not want to show this number. Default is FALSE.
#' @param show_value TRUE if you want to show the proportion in each group on the graphic. FALSE if you do not want to show the proportion.
#' @param dodge Width of the bar, between 0 and 1.
#' @param reorder TRUE if you want to reorder the groups according to the proportion. NA value, in case if na.rm.group = FALSE, is not included in the reorder.
#' @param error_bar TRUE if you want to show the error bars on the graphic. FALSE if you do not want to show the error bars.
#' @param fill Colour of the bars. NA bar, in case if na.rm.group = FALSE, and total bar are always in grey.
#' @param na.rm.group TRUE if you want to remove observations with NA on the group variable or NA on the facet variable. FALSE if you want to create a group with the NA value for the group variable and a facet with the NA value for the facet variable. NA in the variables included in prop_exp are not affected in this argument. All the observation with a NA in the variables included in prop_exp are excluded.
#' @param total_name Name of the total bar on the graphic. Default is Total.
#' @param font Font used in the graphic. Available fonts, included in the package itself, are "Roboto", "Montserrat" and "Gotham Narrow". Default is "Roboto".
#' @param wrap_width Number of characters before before going to the line. Applies to the labels of the groups. Default is 25.
#' @param export_path Path to export the results in an xlsx file. The file includes two sheets : the table and the graphic.
#'
#' @return A list that contains a table, a graphic and a statistical test
#' @import rlang
#' @import survey
#' @import srvyr
#' @import dplyr
#' @import ggplot2
#' @import scales
#' @importFrom stats as.formula
#' @import forcats
#' @import stringr
#' @import openxlsx
#' @import broom
#' @export
#'
#' @examples
prop_group <- function(data,
                       group,
                       prop_exp,
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
                       show_labs = TRUE,
                       show_n = FALSE,
                       show_value = TRUE, # Possibilité de ne pas vouloir avoir les valeurs sur le graphique
                       dodge = 0.9,
                       reorder = F,
                       error_bar = T,
                       fill = "deepskyblue3",
                       na.rm.group = T,
                       total_name = "Total",
                       font ="Roboto",
                       wrap_width = 25,
                       export_path = NULL) {

  # Check des arguments nécessaires
  if((missing(data) | missing(group) | missing(prop_exp)) == TRUE){
    stop("Les arguments data, group et prop_exp doivent être remplis")
  }

  # Check des autres arguments
  check_character(arg = list(prop_method, unit, caption, title, subtitle, xlab, ylab, fill, total_name, font, export_path))
  check_logical(arg = list(show_labs, show_n, show_value, reorder, error_bar, na.rm.group))
  check_numeric(arg = list(scale, digits, dodge, wrap_width))

  # Petite fonction utile
  `%ni%` <- Negate(`%in%`)

  # On crée une quosure de facet_var & filter_exp => pour if statements dans la fonction (voir ci-dessous)
  # Solution trouvée ici : https://rpubs.com/tjmahr/quo_is_missing
  quo_facet <- enquo(facet_var)
  quo_filter <- enquo(filter_exp)

  # On procède d'abord à un test : il faut que toutes les variables entrées soient présentes dans data => sinon stop et erreur
  # On détecte d'abord les variables entrées dans l'expression pour calculer la proportion
  # Solution trouvée ici : https://stackoverflow.com/questions/63727729/r-how-to-extract-object-names-from-expression
  vars_expression <- all.vars(substitute(prop_exp))
  # On crée ensuite un vecteur string qui contient toutes les variables entrées
  vars_input_char <- c(as.character(vars_expression), as.character(substitute(group)))
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
      stop("Au moins une des variables introduites dans quali_var, filter_exp ou facet n'est pas présente dans data")
    }
  }
  # Si objet sondage
  if(any(class(data) %in% c("survey.design2","survey.design","tbl_svy","svyrep.design"))){
    if(all(vars_input_char %in% names(data[["variables"]])) == FALSE){
      stop("Au moins une des variables introduites dans quali_var, filter_exp ou facet n'est pas présente dans data")
    }
  }

  # L'expression ne peut pas contenir la fonction is.na() => il est utile de calculer la proportion de NA, mais vu qu'on supprime les NA dans la suite (voir plus loin), ça ne marche pas !
  # On regarde donc si la fonction is.na() est utilisée dans l'expression, et on bloque si c'est le cas
  names_expression <- all.names(substitute(prop_exp))
  if("is.na" %in% names_expression){
    stop("is.na() est détecté dans l'expression : prop_group() ne permet pas de calculer la proportion de valeurs manquantes")
  }

  # # On extrait les & ou | dans l'expression => interdit car ça pose problème pour le filtrage des NA sur les variables utilisées dans l'expression si plusieurs variables (voir la partie avec "filter(!is.na(express_bin))")
  # express_check <- str_extract_all((deparse(substitute(prop_exp))), "[\\&\\|]+")[[1]]
  # if(length(express_check) > 0){
  #   if(express_check %in% c("&", "&&", "|", "||")){
  #    stop("L'expression ne peut pas comprendre de conditions multiples")
  #   }
  # }

  # On convertit d'abord en objet srvyr
  data_W <- convert_to_srvyr(data, ...)

  # On filtre si filter est non NULL
  if(!quo_is_null(quo_filter)){
    data_W <- data_W %>%
      filter({{ filter_exp }})
  }

  # /!\ NOTE : méthode de filtrage pas sure ! Remplacée par version avec filtre sur "express_bin" /!\
  # # On choppe la colonne sur laquelle on calcule la proportion dans l'expression => devient un vecteur string
  # var_prop <- str_extract((deparse(substitute(prop_exp))), "\\w+\\b")
  # # Et on filtre le data.frame pour enlever les valeurs manquantes sur cette variable => sinon ambigu : de cette façon les n par groupe sont toujours les effectifs pour lesquels la variable var_prop est non missing (et pas tout le groupe : ça on s'en fout)
  # data <- data %>%
  #   filter(!is.na(var_prop))

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

  # On supprime les NA sur la/les variable(s) de l'expression dans tous les cas, sinon ambigu => de cette façon les n par groupe sont toujours les effectifs pour lesquels la/les variable(s) de l'expression sont non missing (et pas tout le groupe : ça on s'en fout)
  # On affiche les variables entrées dans l'expression via message (pour vérification) => presentes dans vars_expression créé au début
  message("Variable(s) détectée(s) dans l'expression : ", paste(vars_expression, collapse = ", "))
  # On calcule les effectifs avant filtre
  before <- data_W %>%
    summarise(n=unweighted(n()))
  # On filtre via boucle => solution trouvée ici : https://dplyr.tidyverse.org/articles/programming.html#loop-over-multiple-variables
  for (var in vars_expression) {
    data_W <- data_W %>%
      filter(!is.na(.data[[var]]))
  }
  # On calcule les effectifs après filtre
  after <- data_W %>%
    summarise(n=unweighted(n()))
  # On affiche le nombre de lignes supprimées (pour vérification)
  message(paste0(before[[1]] - after[[1]]), " lignes supprimées avec valeur(s) manquante(s) pour le(s) variable(s) de l'expression")

  # On convertit la variable de groupe en facteur si pas facteur
  # On crée également une variable binaire liée à la proportion pour le khi2
  data_W <- data_W %>%
    mutate(
      "{{ group }}" := droplevels(as.factor({{ group }})), # droplevels pour éviter qu'un level soit encodé alors qu'il n'a pas d'effectifs (pb pour le test khi2)
      express_bin = {{ prop_exp }}
    )
  # On convertit également la variable de facet en facteur si facet non-NULL
  if(!quo_is_null(quo_facet)){
    data_W <- data_W %>%
      mutate(
        "{{ facet_var }}" := droplevels(as.factor({{ facet_var }}))) # droplevels pour éviter qu'un level soit encodé alors qu'il n'a pas d'effectifs (pb pour le test khi2)
  }

  # /!\ NOTE : méthode de filtrage remplacée par version avec filtre sur vars_expression avec boucle for (voir précédent) /!\
  # # Méthode plus sure de filtrage des NA sur la variable sur laquelle est calculée la proportion
  # express_na <- data_W %>%
  #   summarise(na_express = sum(is.na(express_bin)))
  # message(paste0(express_na[[1]]), " valeur(s) manquante(s) supprimée(s) dans l'expression")
  # data_W <- data_W %>%
  #   filter(!is.na(express_bin))

  # Ici je crée une copie des données dans data_W_NA
  # L'idée est de recoder les NA des 2 variables group et facet_var en level "NA", pour que le test stat s'applique aussi aux NA
  # Voir si simplification possible pour ne pas créer 2 objets : data_W & data_W_NA => cela implique de changer la suite : à voir car le fait d'avoir les NA en missing réel est pratique
  if(na.rm.group == F){
    data_W_NA <- data_W %>%
      # Idée : fct_na_value_to_level() pour ajouter un level NA encapsulé dans un droplevels() pour le retirer s'il n'existe pas de NA
      mutate("{{ group }}" := droplevels(fct_na_value_to_level({{ group }}, "NA"))
      )
    if(!quo_is_null(quo_facet)){
      data_W_NA <- data_W_NA %>% # On repart de data_W_NA => on enlève séquentiellement les NA de group puis facet_var
        mutate("{{ facet_var }}" := droplevels(fct_na_value_to_level({{ facet_var }}, "NA"))
        )
    }
  }

  # On réalise les tests statistiques
  # Ici un test khi2 sur une variable binaire "express_bin" oui/non pour l'expression
  if(quo_is_null(quo_facet)){
    group_fmla <- as.character(substitute(group))
    fmla <- as.formula(paste("~", group_fmla, "+", "express_bin"))
  }
  # Avec facet : prévoir une boucle pour chacune des modalité de facet_var => A FAIRE PLUS TARD
  if(!quo_is_null(quo_facet)){
    facet_fmla <- as.character(substitute(facet_var))
    fmla <- as.formula(paste("~", facet_fmla, "+", "express_bin"))
  }
  if(na.rm.group == F){
    test.stat <- svychisq(fmla, data_W_NA)
  }
  if(na.rm.group == T){
    test.stat <- svychisq(fmla, data_W)
  }

  # On calcule les proportions par groupe
  if(quo_is_null(quo_facet)){
    tab <- data_W %>%
      group_by({{ group }}) %>%
      cascade(
        prop = survey_mean({{ prop_exp }}, na.rm = T, proportion = T, prop_method = prop_method, vartype = "ci"),
        n_sample = unweighted(n()),
        n_true_weighted = survey_total({{ prop_exp }}, na.rm = T, vartype = "ci"),
        n_tot_weighted = survey_total(vartype = "ci"),
        .fill = total_name, # Le total = colonne "Total"
      )
  }
  # Version avec facet
  if(!quo_is_null(quo_facet)){
    tab <- data_W %>%
      group_by({{ facet_var }}, {{ group }}) %>%
      cascade(
        prop = survey_mean({{ prop_exp }}, na.rm = T, proportion = T, prop_method = prop_method, vartype = "ci"),
        n_sample = unweighted(n()),
        n_true_weighted = survey_total({{ prop_exp }}, na.rm = T, vartype = "ci"),
        n_tot_weighted = survey_total(vartype = "ci"),
        .fill = total_name, # Le total = colonne "Total"
      ) %>%
      filter({{ facet_var }} != total_name | is.na({{ facet_var }}))
  }

  # On crée la palette : avec le total au début (en gris foncé) puis x fois le bleu selon le nombre de levels - 1 (le total étant déjà un niveau)
  palette <- c(rep(fill, nlevels(tab[[deparse(substitute(group))]]) - 1), "grey40")

  # On calcule la valeur max de la proportion, pour l'écart des geom_text dans le ggplot
  max_ggplot <- max(tab$prop, na.rm = TRUE)

  if (reorder == T ) {
    # On crée un vecteur pour ordonner les levels de group selon prop, en mettant Total et NA en premier (= en dernier sur le graphique ggplot)
    levels <- c(
      total_name,
      NA,
      levels(reorder(
        tab[[deparse(substitute(group))]],
        tab[["prop"]],
        FUN = median,
        decreasing = T
      ))[levels(reorder(
        tab[[deparse(substitute(group))]],
        tab[["prop"]],
        FUN = median,
        decreasing = T
        )) != total_name]
    )
  }

  if (reorder == F ) {
    # On crée un vecteur pour ordonner les levels de group pour mettre Total et NA en premier (= en dernier sur le graphique ggplot)
    levels <- c(
      total_name,
      NA,
      rev(
        levels(
          tab[[deparse(substitute(group))]]
        )
      )[rev(
        levels(
          tab[[deparse(substitute(group))]]
        ) != total_name
      )]
    )
  }

  # Dans le vecteur qui ordonne les levels, on a mis un NA => Or parfois pas de missing pour le groupe, même si na.rm.group = F !
  # On les supprime donc ssi na.rm.group = F et pas de missing sur la variable de groupe **OU** na.rm.group = T
  if ((na.rm.group == F & sum(is.na(tab[[deparse(substitute(group))]])) == 0) | na.rm.group == T)  {
    levels <- levels[!is.na(levels)]
  }

  # On crée le graphique

  # On charge et active les polices
  load_and_active_fonts()

  graph <- tab %>%
    ggplot(aes(
      x = {{ group }},
      y = prop,
      fill = {{ group }}
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
    scale_fill_manual(
      values = palette,
      na.value = "grey"
    ) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = wrap_width),
                     limits = levels)+
    labs(title = title,
         subtitle = subtitle,
         caption = paste0(
           "Khi2 d'indépendance : ", pvalue(test.stat$p.value, add_p = T),
           "\n",
           caption)
         ) +
    coord_flip()

  # Ajouter les axes
  if(show_labs == TRUE){
    graph <- graph +
      labs(y = ifelse(is.null(xlab),
                      paste0("Proportion : ", deparse(substitute(prop_exp))),
                      xlab))
    if(!is.null(ylab)){
      graph <- graph +
        labs(x = ylab)
    }
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
          label = paste0("n=", n_sample),
          family = font),
        size = 3,
        alpha = 0.7,
        hjust = 0, # Justifié à droite
        vjust = 0.4
      )
  }

  # On crée l'objet final
  res <- list()
  res$tab <- tab
  res$graph <- graph
  res$test.stat <- test.stat

  if (!is.null(export_path)) {
    # L'export en excel

    # # Pour être intégré au fichier excel, le graphique doit être affiché => https://ycphs.github.io/openxlsx/reference/insertPlot.html
    # print(graph)

    # On transforme le test stat en dataframe
    test_stat_excel <- test.stat %>%
      broom::tidy() %>%
      t() %>%
      as.data.frame()
    test_stat_excel$names <- rownames(test_stat_excel)
    test_stat_excel <- test_stat_excel[, c(2,1)]
    names(test_stat_excel)[1] <- "Parameter"
    names(test_stat_excel)[2] <- "Value"

    # J'exporte les résultats en Excel
    export_excel(tab_excel = tab,
                 graph = graph,
                 test_stat_excel = test_stat_excel,
                 facet_null = quo_is_null(quo_facet),
                 export_path = export_path,
                 percent_fm = TRUE,
                 fgFill = "skyblue3",
                 bivariate = FALSE)
  }

  return(res)

}
