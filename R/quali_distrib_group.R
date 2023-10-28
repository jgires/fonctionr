#' quali_distrib_group : fonction pour calculer facilement des profils lignes par groupe
#'
#' @param data
#' @param group
#' @param quali_var
#' @param facet_var
#' @param filter_exp
#' @param prop_method
#' @param ...
#' @param caption
#' @param show_value
#' @param unit
#' @param digits
#' @param dodge
#' @param pretty_pal
#' @param direction
#' @param wrap_width
#' @param legend_ncol
#' @param na.rm
#'
#' @return
#' @import rlang
#' @import survey
#' @import srvyr
#' @import dplyr
#' @import scales
#' @import MetBrewer
#' @import ggplot2
#' @importFrom stats as.formula
#' @import forcats
#' @import stringr
#' @import openxlsx
#' @export
#'
#' @examples
quali_distrib_group <- function(data,
                                group,
                                quali_var,
                                facet_var = NULL,
                                filter_exp = NULL,
                                prop_method = "beta",
                                ...,
                                caption = NULL,
                                show_value = TRUE,
                                unit = "",
                                digits = 0,
                                dodge = 0.9,
                                pretty_pal = "Hokusai1",
                                direction = 1,
                                scale = 100,
                                wrap_width = 25,
                                legend_ncol = 4,
                                font ="Montserrat",
                                na.rm = T,
                                export_path = NULL) {

  # Petite fonction utile
  `%ni%` = Negate(`%in%`)

  # On ajoute les polices contenues dans le package et on les active
  font_add(family = "Montserrat", regular = paste0(system.file("font", package = "fonctionr"), "/Montserrat-Regular.otf"))
  showtext_auto()

  # On crée une quosure de facet_var & filter_exp => pour if statements dans la fonction (voir ci-dessous)
  # Solution trouvée ici : https://rpubs.com/tjmahr/quo_is_missing
  quo_facet <- enquo(facet_var)
  quo_filter <- enquo(filter_exp)

  # On procède d'abord à un test : il faut que toutes les variables entrées soient présentes dans data => sinon stop et erreur
  # On crée un vecteur string qui contient toutes les variables entrées
  vars_input_char <- c(as.character(substitute(quali_var)), as.character(substitute(group)))
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
  if(all(vars_input_char %in% names(data)) == FALSE){
    stop("Au moins une des variables introduites dans group, quali_var, filter_exp ou facet n'est pas présente dans data")
  }

  # L'expression ne peut pas contenir la fonction is.na() => il est utile de calculer la proportion de NA, mais vu qu'on supprime les NA dans la suite (voir plus loin), ça ne marche pas !
  # On regarde donc si la fonction is.na() est utilisée dans l'expression, et on bloque si c'est le cas
  names_expression <- all.names(substitute(prop_exp))
  if("is.na" %in% names_expression){
    stop("is.na() est détecté dans l'expression : prop_group() ne permet pas de calculer la proportion de valeurs manquantes")
  }

  # On convertit d'abord en objet srvyr
  # Si objet survey (avec replicates ou non)
  if(any(class(data) %in% c("survey.design2","survey.design")) & all(class(data) %ni% c("tbl_svy"))){
    message("Input : objet survey")
    data_W <- data %>%
      as_survey_design()
  }
  if(any(class(data) %in% c("svyrep.design")) & all(class(data) %ni% c("tbl_svy"))){
    message("Input : objet survey")
    data_W <- data %>%
      as_survey_rep()
  }
  # Si objet srvyr (avec replicates ou non)
  if(any(class(data) %in% c("tbl_svy"))){
    message("Input : objet srvyr")
    data_W <- data
  }
  # Si data.frame (pas de replicate prévu => A FAIRE A TERME)
  if(any(class(data) %ni% c("survey.design2","survey.design")) & any(class(data) %ni% c("tbl_svy")) & any(class(data) %in% c("data.frame"))){
    message("Input : data.frame")
    data_W <- data %>%
      as_survey_design(...)
  }

  message("Variables du design :", " cluster : ", paste(names(data_W$cluster), collapse = " "), " | strata : ",  paste(names(data_W$strata), collapse = " "), " | weights : ",  paste(names(data_W$allprob), collapse = " "))

  # On filtre si filter est non NULL
  if(!quo_is_null(quo_filter)){
    data_W <- data_W %>%
      filter({{ filter_exp }})
  }

  # On supprime les NA des 2 variables si na.rm == T
  if(na.rm == T){
    data_W <- data_W %>%
      filter(!is.na({{ group }}) & !is.na({{ quali_var }}))
    # idem sur la variable de facet si non-NULL
    if(!quo_is_null(quo_facet)){
      data_W <- data_W %>%
        filter(!is.na({{ facet_var }}))
    }
  }

  # On convertit en facteurs si pas facteurs
  data_W <- data_W %>%
    mutate(
      "{{ quali_var }}" := droplevels(as.factor({{ quali_var }})), # droplevels pour éviter qu'un level soit encodé alors qu'il n'a pas d'effectifs (pb pour le test khi2)
      "{{ group }}" := droplevels(as.factor({{ group }}))
    )
  # On convertit également la variable de facet en facteur si facet non-NULL
  if(!quo_is_null(quo_facet)){
    data_W <- data_W %>%
      mutate(
        "{{ facet_var }}" := droplevels(as.factor({{ facet_var }}))) # droplevels pour éviter qu'un level soit encodé alors qu'il n'a pas d'effectifs (pb pour le test khi2)
  }

  # Ici je crée une copie des données dans data_W_NA
  # L'idée est de recoder les NA des 2 variables croisées en level "NA", pour que le khi2 s'applique aussi aux NA
  # Voir si simplification possible pour ne pas créer 2 objets : data_W & data_W_NA => cela implique de changer la suite : à voir car le fait d'avoir les NA en missing réel est pratique
  if(na.rm == F){
    data_W_NA <- data_W %>%
      # Idée : fct_na_value_to_level() pour ajouter un level NA encapsulé dans un droplevels() pour le retirer s'il n'existe pas de NA
      mutate("{{ group }}" := droplevels(fct_na_value_to_level({{ group }}, "NA")),
             "{{ quali_var }}" := droplevels(fct_na_value_to_level({{ quali_var }}, "NA"))
      )
    if(!quo_is_null(quo_facet)){
      data_W_NA <- data_W_NA %>% # On repart de data_W_NA => on enlève séquentiellement les NA de group puis facet_var
        mutate("{{ facet_var }}" := droplevels(fct_na_value_to_level({{ facet_var }}, "NA"))
        )
    }
  }

  # On réalise les tests statistiques
  # NOTE : pour l'instant uniquement lorsque pas de facet => pour facet mon idée c'est une analyse loglinéaire => pas bien pigé avec survey
  if(quo_is_null(quo_facet)){
    quali_var_fmla <- as.character(substitute(quali_var))
    group_fmla <- as.character(substitute(group))
    fmla <- as.formula(paste("~", group_fmla, "+", quali_var_fmla))
    if(na.rm == F){
      # On utilise un tryCatch pour bypasser le test s'il produit une erreur => possible lorsque les conditions ne sont pas remplies
      test.stat <- tryCatch(
        expr = {
          svychisq(fmla, data_W_NA)
        },
      # test.stat devient un vecteur string avec 1 chaîne de caractères si erreur du test
        error = function(e){
          "Conditions non remplies"
        }
      )
    }
    if(na.rm == T){
      test.stat <- tryCatch(
        expr = {
          svychisq(fmla, data_W)
        },
        error = function(e){
          "Conditions non remplies"
        }
      )
    }
  }

  # On calcule les fréquences relatives par groupe
  if(quo_is_null(quo_facet)){
    tab <- data_W %>%
      group_by({{ group }}, {{ quali_var }}) %>%
      summarise(
        prop = survey_prop(proportion = T, prop_method = prop_method, vartype = c("ci")),
        n = unweighted(n()),
        n_weighted = survey_total()
      )
  }
  if(!quo_is_null(quo_facet)){
    tab <- data_W %>%
      group_by({{ facet_var }}, {{ group }}, {{ quali_var }}) %>%
      summarise(
        prop = survey_prop(proportion = T, prop_method = prop_method, vartype = c("ci")),
        n = unweighted(n()),
        n_weighted = survey_total()
      )
  }

  # On crée la palette avecle package met.brewer
  palette <- as.character(met.brewer(name = pretty_pal, n = nlevels(as.factor(tab[[deparse(substitute(quali_var))]])), type = "continuous", direction = direction))

  # On crée un vecteur pour ordonner les levels de group pour mettre NA en premier (= en dernier sur le graphique ggplot)
  levels <- c(
    NA,
    rev(
      levels(
        tab[[deparse(substitute(group))]]
        )
    )
  )

  # Dans le vecteur qui ordonne les levels, on a mis un NA => Or parfois pas de missing pour le groupe, même si na.rm.group = F !
  # On les supprime donc ssi na.rm = F et pas de missing sur la variable de groupe **OU** na.rm = T
  if ((na.rm == F & sum(is.na(tab[[deparse(substitute(group))]])) == 0) | na.rm == T)  {
    levels <- levels[!is.na(levels)]
  }

  # On crée le graphique
  graph <- tab %>%
    ggplot(aes(
      x = {{ group }},
      y = prop,
      fill = {{ quali_var }}
    )) +
    geom_bar(
      width = dodge,
      stat = "identity",
      position = position_stack(reverse = TRUE)
    ) +
    theme_minimal() +
    theme(
      panel.grid.minor.y = element_blank(),
      panel.grid.minor.x = element_line(color = "#dddddd"),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = "#dddddd"),
      text = element_text(family = font),
      legend.position = "bottom"
    ) +
    ylab(paste0("distribution: ", deparse(substitute(quali_var)))) +
    scale_fill_manual(values = palette,
                      labels = function(x) str_wrap(x, width = 25),
                      na.value = "grey") +
    scale_y_continuous(
      labels = function(x) { paste0(x * scale, unit) },
      expand = expansion(mult = c(.01, .05))
    ) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = wrap_width),
                     limits = levels) +
    guides(fill = guide_legend(ncol = legend_ncol)) +
    coord_flip()

  if (!quo_is_null(quo_facet)) {
    graph <- graph +
      facet_wrap(vars({{ facet_var }}))
  }

  if (quo_is_null(quo_facet)) {
    if (inherits(test.stat, "htest")) { # Condition sur inherits car si le test a réussi => test.stat est de class "htest", sinon "character"
      graph <- graph +
        labs(
          caption = paste0(
            "Khi2 d'indépendance : ", pvalue(test.stat$p.value, add_p = T),
            "\n",
            caption
          )
        )
    }
  }
  if (quo_is_null(quo_facet)) {
    if (inherits(test.stat, "character")) { # Condition sur inherits car si le test a réussi => test.stat est de class "htest", sinon "character"
      graph <- graph +
        labs(
          caption = paste0(
            "Khi2 d'indépendance : conditions non remplies",
            "\n",
            caption
          )
        )
    }
  }
  # Ce n'est pas un khi2 s'il y a des facet
  if (!quo_is_null(quo_facet)) {
    graph <- graph +
      labs(
        caption = caption
      )
  }

  if (show_value == TRUE) {
    graph <- graph +
      geom_text(
        aes(
          label = ifelse(prop > 0.02,
                         paste0(round(prop * scale,
                                      digits = digits),
                                unit),
                         NA),
          family = font),
        color = "white",
        position = position_stack(vjust = .5,
                                  reverse = TRUE)
      )
  }

  # On crée l'objet final
  res <- list()
  res$tab <- tab
  res$graph <- graph
  # Pour l'instant, test uniquement si pas de facet
  if (quo_is_null(quo_facet)) {
    res$test.stat <- test.stat
  }

  if (!is.null(export_path)) {
    # L'export en excel

    # Pour être intégré au fichier excel, le graphique doit être affiché => https://ycphs.github.io/openxlsx/reference/insertPlot.html
    print(graph)

    # On simplifie le tableau à exporter
    tab_excel <- tab %>% select(-n_weighted_se)

    # On transforme le test stat en dataframe
    if (quo_is_null(quo_facet)) { # Pour l'instant, test uniquement si pas de facet
      if(all(test.stat != "Conditions non remplies")){
      test_stat_excel <- test.stat %>%
        broom::tidy() %>%
        t() %>%
        as.data.frame()
      test_stat_excel$names <- rownames(test_stat_excel)
      test_stat_excel <- test_stat_excel[, c(2,1)]
      names(test_stat_excel)[1] <- "Parameter"
      names(test_stat_excel)[2] <- "Value"
      }
      if(all(test.stat == "Conditions non remplies")){
          test_stat_excel <- data.frame(Parameter = c("test.error"),
                                        Value = test.stat,
                                        row.names = NULL)
      }
    }
    if (!quo_is_null(quo_facet)) {
      test_stat_excel <- data.frame(Parameter = c("test.error"),
                                    Value = "Test pas encore implémenté avec le faceting",
                                    row.names = NULL)
    }

    # Je formate un fichier Excel dans lequel j'exporte les résultats

    wb <- createWorkbook() # On crée l'objet dans lequel on va formater toutes les infos en vue d'un export en fichier Excel
    addWorksheet(wb, "Résultats") # On ajoute une feuille pour les résultats
    addWorksheet(wb, "Graphique") # On ajoute une feuille pour le graphique
    addWorksheet(wb, "Test statistique") # On ajoute une feuille pour le résultat du test stat

    writeData(wb, "Résultats", tab_excel, keepNA = TRUE, na.string = "NA") # On écrit les résultats en gardant les NA
    insertPlot(wb,"Graphique", dpi = 80, width = 12, height = 8)
    writeData(wb, "Test statistique", test_stat_excel) # On écrit le résultat du test stat

    setColWidths(wb, "Résultats", widths = 20, cols = 1:ncol(tab_excel)) # Largeur des colonnes
    hs <- createStyle(fontColour = "#ffffff", fgFill = "mediumseagreen",  # Style de la première ligne
                      halign = "center", textDecoration = "Bold",
                      fontName = "Arial Narrow")
    firstC <- createStyle (halign = "left", textDecoration = "Bold", # Style de la première colonne
                           fontName = "Arial Narrow")
    body <- createStyle (halign = "center", # Style des cellules du tableau
                         fontName = "Arial Narrow")
    percent <- createStyle(numFmt = "percentage")

    addStyle(wb, "Résultats", hs, cols = 1:ncol(tab_excel), rows = 1) # On applique le style à la première ligne
    addStyle(wb, "Résultats", body, cols = 2:ncol(tab_excel), rows = 2:(nrow(tab_excel)+1), gridExpand = TRUE, stack = TRUE) # On applique le style aux reste des cellules
    # Des if statements dans le cas où le résultat est démultiplié par modalité de facet_var => Pas les mêmes règles vu qu'il y a une colonne en plus à mettre en gras
    if (!quo_is_null(quo_facet)) {
      addStyle(wb, "Résultats", firstC, cols = 1:3, rows = 2:(nrow(tab_excel)+1), gridExpand = TRUE, stack = TRUE) # On applique le style à la première colonne (sans la première ligne)
      addStyle(wb, "Résultats", percent, cols = 4:6, rows = 2:(nrow(tab_excel)+1), gridExpand = TRUE, stack = TRUE) # On applique le style de pourcentage aux proportions
    }
    if (quo_is_null(quo_facet)) {
      addStyle(wb, "Résultats", firstC, cols = 1:2, rows = 2:(nrow(tab_excel)+1), gridExpand = TRUE, stack = TRUE) # On applique le style à la première colonne (sans la première ligne)
      addStyle(wb, "Résultats", percent, cols = 3:5, rows = 2:(nrow(tab_excel)+1), gridExpand = TRUE, stack = TRUE) # On applique le style de pourcentage aux proportions
    }

    setColWidths(wb, "Test statistique", widths = 20, cols = 1:ncol(test_stat_excel)) # Largeur des colonnes
    hs2 <- createStyle(fontColour = "#ffffff", fgFill = "grey15",  # Style de la première ligne
                       halign = "center", textDecoration = "Bold",
                       fontName = "Arial Narrow")
    body2 <- createStyle (fontName = "Arial Narrow") # Style des cellules du tableau

    addStyle(wb, "Test statistique", hs2, cols = 1:ncol(test_stat_excel), rows = 1) # On applique le style à la première ligne
    addStyle(wb, "Test statistique", firstC, cols = 1, rows = 2:(nrow(test_stat_excel)+1), gridExpand = TRUE, stack = TRUE) # On applique le style à la première colonne (sans la première ligne)
    addStyle(wb, "Test statistique", body2, cols = 2:ncol(test_stat_excel), rows = 2:(nrow(test_stat_excel)+1), gridExpand = TRUE, stack = TRUE) # On applique le style aux reste des cellules

    saveWorkbook(wb, export_path, overwrite = TRUE)
  }

  return(res)
}
