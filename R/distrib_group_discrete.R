#' distrib_group_discrete
#'
#' Function describe the distribution of a discrete variable in different groups. It can use complex survey data. It produces a table, a graphic and a statistical test.
#'
#' @param data A dataframe or an object from the survey package or an object from the srvyr package.
#' @param group A variable defining groups be compared.
#' @param quali_var The discrete variable that is studied.
#' @param facet_var A variable defining the faceting group.
#' @param filter_exp An expression that filters the data, preserving the design.
#' @param prop_method Type of proportion method to use. See svyciprop in survey package for details. Default is the beta method.
#' @param ... All options possible in as_survey_design in srvyr package.
#' @param title Title of the graphic.
#' @param subtitle Subtitle of the graphic.
#' @param xlab X label on the graphic. As coord_flip() is used in the graphic, xlab refers to the x label on the graphic, after the coord_flip(), and not to the x variable in the data.
#' @param ylab Y label on the graphic. As coord_flip() is used in the graphic, xlab refers to the x label on the graphic, after the coord_flip(), and not to the x variable in the data.
#' @param legend_lab Legend (fill) label on the graphic.
#' @param caption Caption of the graphic.
#' @param show_labs TRUE if you want to show axes, titles, caption and legend labels. FALSE if you do not want to show any label on axes, titles, caption and legend. Default is TRUE.
#' @param show_value TRUE if you want to show the proportion in each category of each group on the graphic. FALSE if you do not want to show the proportion.
#' @param unit Unit showed in the graphic. Default is percent.
#' @param digits Numbers of digits showed on the values labels on the graphic. Default is 0.
#' @param dodge Width of the bar, between 0 and 1.
#' @param pretty_pal Color palette used on the graphic. The palettes from the packages MetBrewer, MoMAColors and PrettyCols are available.
#' @param direction Direction of the palette color. Default is 1. The opposite direction is -1.
#' @param wrap_width Number of characters before going to the line for the labels of the groups. Default is 25.
#' @param wrap_width_leg Number of characters before going to the line for the labels of the categories. Default is 25.
#' @param legend_ncol Number of colomns in the legend. Default is 4.
#' @param font Font used in the graphic. Available fonts, included in the package itself, are "Roboto", "Montserrat" and "Gotham Narrow". Default is "Roboto".
#' @param na.rm TRUE if you want to remove the NAs in quali_var, group and facet_var. FALSE if you want to create NA categories for quali_var, group and facet_var. Default is TRUE.
#' @param export_path Path to export the results in an xlsx file. The file includes two sheets : the table and the graphic.
#'
#' @return A list that contains a table, a graphic and a statistical test
#' @import rlang
#' @import survey
#' @import srvyr
#' @import dplyr
#' @import scales
#' @import MetBrewer
#' @import PrettyCols
#' @import ggplot2
#' @importFrom stats as.formula
#' @import forcats
#' @import stringr
#' @import openxlsx
#' @import broom
#' @import MoMAColors
#' @export
#'
#' @examples
#'
distrib_group_discrete <- function(data,
                                   group,
                                   quali_var,
                                   facet_var = NULL,
                                   filter_exp = NULL,
                                   prop_method = "beta",
                                   ...,
                                   title = NULL, # Le titre du graphique
                                   subtitle = NULL,
                                   xlab = NULL, # Le nom de l'axe de la variable catégorielle
                                   ylab = NULL,
                                   legend_lab = NULL,
                                   caption = NULL,
                                   show_labs = TRUE,
                                   show_value = TRUE,
                                   unit = "",
                                   scale = 100,
                                   digits = 0,
                                   dodge = 0.9,
                                   pretty_pal = "Hokusai1",
                                   direction = 1,
                                   wrap_width = 25,
                                   wrap_width_leg = 25,
                                   legend_ncol = 4,
                                   font ="Roboto",
                                   na.rm = T,
                                   export_path = NULL) {

  # Un check impératif
  if((missing(data) | missing(group) | missing(quali_var)) == TRUE){
    stop("Les arguments data, group et quali_var doivent être remplis")
  }

  # Check des autres arguments
  check_character(arg = list(prop_method, unit, caption, title, subtitle, xlab, ylab, legend_lab, font, pretty_pal, export_path))
  check_logical(arg = list(show_labs, show_value, na.rm))
  check_numeric(arg = list(digits, dodge, direction, wrap_width, wrap_width_leg, legend_ncol))

  # Petite fonction utile
  `%ni%` <- Negate(`%in%`)

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

  # On convertit d'abord en objet srvyr
  data_W <- convert_to_srvyr(data, ...)

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
        n_sample = unweighted(n()),
        n_weighted = survey_total(vartype = c("ci"))
      )
  }
  if(!quo_is_null(quo_facet)){
    tab <- data_W %>%
      group_by({{ facet_var }}, {{ group }}, {{ quali_var }}) %>%
      summarise(
        prop = survey_prop(proportion = T, prop_method = prop_method, vartype = c("ci")),
        n_sample = unweighted(n()),
        n_weighted = survey_total(vartype = c("ci"))
      )
  }

  # On crée la palette avecle package met.brewer
  if(pretty_pal %in% c("Archambault","Austria","Benedictus","Cassatt1","Cassatt2","Cross","Degas","Demuth",
                       "Derain","Egypt","Gauguin","Greek","Hiroshige","Hokusai1",
                       "Hokusai2","Hokusai3","Homer1","Homer2","Ingres","Isfahan1","Isfahan2",
                       "Java","Johnson","Juarez","Kandinsky","Klimt","Lakota","Manet",
                       "Monet","Moreau","Morgenstern","Nattier","Navajo","NewKingdom","Nizami",
                       "OKeeffe1","OKeeffe2","Paquin","Peru1","Peru2","Pillement","Pissaro",
                       "Redon","Renoir","Signac","Tam","Tara","Thomas","Tiepolo","Troy",
                       "Tsimshian","VanGogh1","VanGogh2","VanGogh3","Veronese","Wissing" )){
  palette <- as.character(met.brewer(name = pretty_pal, n = nlevels(as.factor(tab[[deparse(substitute(quali_var))]])), type = "continuous", direction = direction))
  }

  #ou la crée avec le package MoMAColors
  if(pretty_pal %in% c("Abbott","Alkalay1","Alkalay2","Althoff","Andri","Avedon","Budnitz",
                       "Clay","Connors","Dali","Doughton","Ernst","Exter","Flash",
                       "Fritsch","Kippenberger","Klein","Koons","Levine1","Levine2","Liu",
                       "Lupi","Ohchi","OKeeffe","Palermo","Panton","Picabia","Picasso",
                       "Rattner","Sidhu","Smith","ustwo","VanGogh","vonHeyl","Warhol" )){
    palette <- as.character(moma.colors(palette_name = pretty_pal, n = nlevels(as.factor(tab[[deparse(substitute(quali_var))]])), type = "continuous", direction = direction))
  }

  # On crée la palette avecle package PrettyCols
  if(pretty_pal %in% c("Blues","Purples","Tangerines","Greens","Pinks","Teals",
                       "Yellows","Reds","PurpleGreens","PinkGreens","TangerineBlues","PurpleTangerines",
                       "PurplePinks","TealGreens","PurpleYellows","RedBlues","Bold","Dark",
                       "Light","Neon","Summer","Autumn","Winter","Rainbow",
                       "Beach","Fun","Sea","Bright","Relax","Lucent",
                       "Lively","Joyful")){
    palette <- as.character(prettycols(name = pretty_pal, n = nlevels(as.factor(tab[[deparse(substitute(quali_var))]])), type = "continuous", direction = direction))
  }

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

  # On charge et active les polices
  load_and_active_fonts()

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
    theme_fonctionr(font = font) +
    theme(
      legend.position = "bottom"
    ) +
    scale_fill_manual(values = palette,
                      labels = function(x) str_wrap(x, width = wrap_width_leg),
                      na.value = "grey") +
    scale_y_continuous(
      labels = function(x) { paste0(x * scale, unit) },
      expand = expansion(mult = c(.01, .05))
    ) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = wrap_width),
                     limits = levels) +
    guides(fill = guide_legend(ncol = legend_ncol)) +
    labs(title = title,
         subtitle = subtitle) +
    coord_flip()

  # Pour caption
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
  # Ce n'est pas un khi2 s'il y a des facets
  if (!quo_is_null(quo_facet)) {
    graph <- graph +
      labs(
        caption = caption
      )
  }

  # Ajouter les axes
  if(show_labs == TRUE){
    graph <- graph +
      labs(y = ifelse(is.null(xlab),
                      paste0("Distribution : ", deparse(substitute(quali_var))),
                      xlab))
    if(!is.null(ylab)){
      graph <- graph +
        labs(x = ylab)
    }
    if(!is.null(legend_lab)){
      graph <- graph +
        labs(fill = legend_lab)
    }
  }

  # Masquer les axes si show_labs == FALSE
  if(show_labs == FALSE){
    graph <- graph +
      labs(x = NULL,
           y = NULL,
           fill = NULL)
  }

  if (!quo_is_null(quo_facet)) {
    graph <- graph +
      facet_wrap(vars({{ facet_var }})) +
      theme(panel.spacing.x = unit(1, "lines"))
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
        size = 3.5,
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

    # J'exporte les résultats en Excel
    export_excel(tab_excel = tab,
                 graph = graph,
                 test_stat_excel = test_stat_excel,
                 facet_null = quo_is_null(quo_facet),
                 export_path = export_path,
                 percent_fm = TRUE,
                 fgFill = "mediumseagreen",
                 bivariate = TRUE)
  }

  return(res)
}


#' @rdname distrib_group_discrete
#' @export
distrib_group_d <- function(...) {
  distrib_group_discrete(...)
}
