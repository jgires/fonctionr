#' quali_distrib : fonction pour réaliser facilement un graphique et une analyse d'une distribution d'une variable qualitative d'un sondage complexe
#'
#' @param data
#' @param quali_var
#' @param facet_var
#' @param filter_exp
#' @param ...
#' @param na.rm
#' @param fill
#' @param show_n
#' @param show_value
#' @param reorder
#' @param scale
#' @param dodge
#' @param prop_method
#' @param unit
#' @param error_bar
#' @param caption
#' @param title
#' @param subtitle
#' @param ylab
#' @param show_labs
#' @param font
#' @param digits
#' @param wrap_width
#' @param export_path
#'
#' @return
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

quali_distrib <- function(data, # Données en format srvyr
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
                          error_bar = T,
                          caption = NULL,
                          title = NULL, # Le titre du graphique
                          subtitle = NULL,
                          ylab = NULL, # Le nom de l'axe de la variable catégorielle
                          show_labs = TRUE,
                          font ="Roboto", # Quelle font par défaut?
                          digits = 0,
                          wrap_width = 25,
                          export_path = NULL) {

  # Petite fonction utile
  `%ni%` = Negate(`%in%`)

  # On ajoute les polices contenues dans le package et on les active
  font_add(family = "Montserrat", regular = paste0(system.file("font", package = "fonctionr"), "/Montserrat-Regular.otf"))
  font_add(family = "Roboto", regular = paste0(system.file("font", package = "fonctionr"), "/Roboto-Regular.ttf"))
  font_add(family = "Gotham Narrow", regular = paste0(system.file("font", package = "fonctionr"), "/GothamNarrow-Book.otf"))
  showtext_auto()

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
  if(all(vars_input_char %in% names(data)) == FALSE){
    stop("Au moins une des variables introduites dans quali_var, filter_exp ou facet n'est pas présente dans data")
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
    table <- data_W %>%
      group_by({{ quali_var }}) %>%
      srvyr::summarize(prop = survey_prop(vartype = "ci", proportion = T, prop_method = prop_method),
                       n_weighted = survey_total(vartype = "ci"), # si l'on met les poids pondéré, je trouve nécessaire et pertinent de mettre leurs IC
                       n = unweighted(n()))
  }
  if (!quo_is_null(quo_facet)) {
    table <- data_W %>%
      group_by({{ facet_var }}, {{ quali_var }}) %>%
      srvyr::summarize(prop = survey_prop(vartype = "ci", proportion = T, prop_method = prop_method),
                       n_weighted = survey_total(vartype = "ci"), # si l'on met les poids pondéré, je trouve nécessaire et pertinent de mettre leurs IC
                       n = unweighted(n()))
  }

  # Faire le graphique ---------------

  # On crée la palette : x fois la couleur selon le nombre de levels
  palette <- c(rep(fill, nlevels(table[[deparse(substitute(quali_var))]])))

  # On calcule la valeur max de la proportion, pour l'écart des geom_text dans le ggplot
  max_ggplot <- max(table$prop, na.rm = TRUE)

  if (reorder == T & quo_is_null(quo_facet)) {
    # On crée un vecteur pour ordonner les levels de quali_var selon prop, en mettant NA en premier (= en dernier sur le graphique ggplot)
    levels <- c(
      NA,
      levels(reorder(
        table[[deparse(substitute(quali_var))]],
        table[["prop"]]
      ))
    )
  }

  if (reorder == F | !quo_is_null(quo_facet)) {
    # On crée un vecteur pour ordonner les levels de quali_var pour mettre NA en premier (= en dernier sur le graphique ggplot)
    levels <- c(
      NA,
      rev(
        levels(
          table[[deparse(substitute(quali_var))]]
        )
      )
    )
  }

  # Dans le vecteur qui ordonne les levels, on a mis un NA => Or parfois pas de missing pour quali_var, même si na.rm = F !
  # On les supprime donc ssi na.rm = F et pas de missing sur la variable quali_var **OU** na.rm = T
  if ((na.rm == F & sum(is.na(table[[deparse(substitute(quali_var))]])) == 0) | na.rm == T)  {
    levels <- levels[!is.na(levels)]
  }

  # Le graphique proprement dit
  graph <- table %>%
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
    scale_y_continuous(
      labels = function(x) { paste0(x * scale, unit) },
      limits = function(x) { c(min(x), max(x)) },
      expand = expansion(mult = c(.01, .05))
      ) +
    scale_x_discrete(
      labels = function(x) str_wrap(x, width = wrap_width),
      limits = levels
      ) +
    theme_minimal() +
    theme(
       panel.grid.minor.y = element_blank(),
       panel.grid.minor.x = element_line(color = "#dddddd"),
       panel.grid.major.y = element_blank(),
       panel.grid.major.x = element_line(color = "#dddddd"),
       axis.line = element_line(color = "black"),
       axis.ticks = element_line(color = "black"),
       legend.position = "none",
       text = element_text(family = font),
       axis.text = element_text(color = "black")
       )+
    coord_flip() +
    labs(title = title,
         subtitle = subtitle,
         caption = caption)

  # Ajouter les axes au besoins
  if(show_labs == TRUE){
    graph <- graph +
      labs(x = NULL, # Pour cette fonction, x est vide dans tous les cas (à voir si c'est adapté dans tous les cas)
           y = ifelse(is.null(ylab),
                      paste0("Distribution : ", deparse(substitute(quali_var))),
                      ylab))
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

  # Ajouter les prop au besoin

  # if(show_value == TRUE){
  #   graph <- graph +
  #     geom_text(
  #       aes(y = prop_upp + (0.1 * max_ggplot), ###j'ai l'impression que le 1, c'est bon, mais c'est à vérifier
  #           label = paste(round(prop * scale, digits = digits),
  #                         unit),
  #           family = font),
  #       color = "black")
  # }

  if (show_value == TRUE) { # MODIF DE JOEL ----------------------
    graph <- graph +
      geom_text(
        aes(
          y = (prop) + (0.01 * max_ggplot),
          label = paste0(round(prop * scale,
                               digits = digits),
                         unit),
          family = font),
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
          label = paste0("n=", n),
          family = font),
        size = 3,
        alpha = 0.7,
        hjust = 0, # Justifié à droite
        vjust = 0.4
      )
  }

  # Ajouter les facets au besoin
  if (!quo_is_null(quo_facet)) {
    graph <- graph +
      facet_wrap(vars({{ facet_var }}))
  }

  # Retourner les résultat
  res <- list()
  res$tab <- table
  res$graph <- graph

  if (!is.null(export_path)) {
    # L'export en excel

    # Pour être intégré au fichier excel, le graphique doit être affiché => https://ycphs.github.io/openxlsx/reference/insertPlot.html
    print(graph)

    # On simplifie le tableau à exporter
    tab_excel <- table %>% select(-n_weighted_low, -n_weighted_upp)

    # On crée ici manuellement le dataframe pour le test, celui-ci n'étant pas encore implémenté
    test_stat_excel <- data.frame(Parameter = c("test.error"),
                                  Value = "Test pas encore implémenté pour quali_distrib()",
                                  row.names = NULL)

    # Je formate un fichier Excel dans lequel j'exporte les résultats

    wb <- createWorkbook() # On crée l'objet dans lequel on va formater toutes les infos en vue d'un export en fichier Excel
    addWorksheet(wb, "Résultats") # On ajoute une feuille pour les résultats
    addWorksheet(wb, "Graphique") # On ajoute une feuille pour le graphique
    addWorksheet(wb, "Test statistique") # On ajoute une feuille pour le résultat du test stat

    writeData(wb, "Résultats", tab_excel, keepNA = TRUE, na.string = "NA") # On écrit les résultats en gardant les NA
    insertPlot(wb,"Graphique", dpi = 80, width = 12, height = 8)
    writeData(wb, "Test statistique", test_stat_excel) # On écrit le résultat du test stat

    setColWidths(wb, "Résultats", widths = 20, cols = 1:ncol(tab_excel)) # Largeur des colonnes
    hs <- createStyle(fontColour = "#ffffff", fgFill = "sienna3",  # Style de la première ligne
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
      addStyle(wb, "Résultats", firstC, cols = 1:2, rows = 2:(nrow(tab_excel)+1), gridExpand = TRUE, stack = TRUE) # On applique le style à la première colonne (sans la première ligne)
      addStyle(wb, "Résultats", percent, cols = 3:5, rows = 2:(nrow(tab_excel)+1), gridExpand = TRUE, stack = TRUE) # On applique le style de pourcentage aux proportions
    }
    if (quo_is_null(quo_facet)) {
      addStyle(wb, "Résultats", firstC, cols = 1, rows = 2:(nrow(tab_excel)+1), gridExpand = TRUE, stack = TRUE) # On applique le style à la première colonne (sans la première ligne)
      addStyle(wb, "Résultats", percent, cols = 2:4, rows = 2:(nrow(tab_excel)+1), gridExpand = TRUE, stack = TRUE) # On applique le style de pourcentage aux proportions
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

