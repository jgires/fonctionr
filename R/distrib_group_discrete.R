#' distrib_group_discrete
#'
#' Function describe the distribution of a discrete variable in different groups. It can use complex survey data. It produces a table, a graphic and a statistical test.
#'
#' @param data A dataframe or an object from the survey package or an object from the srvyr package.
#' @param group A variable defining groups to be compared.
#' @param quali_var The discrete variable that is described in the different groups.
#' @param facet A variable defining the faceting group.
#' @param filter_exp An expression that filters the data, preserving the design.
#' @param ... All options possible in as_survey_design in srvyr package.
#' @param na.rm.group TRUE if you want to remove the NAs in quali_var, group and facet. FALSE if you want to create NA categories for quali_var, group and facet. Default is TRUE.
#' @param prop_method Type of proportion method used to compute confidence intervals. See svyciprop in survey package for details. Default is the beta method.
#' @param show_value TRUE if you want to show the proportion in each category of each group on the graphic. FALSE if you do not want to show the proportions. Proportions of 2 percent or less are never showed on the graphic. Default is TRUE.
#' @param show_lab TRUE if you want to show axes, titles, caption and legend labels. FALSE if you do not want to show any label on axes, titles, caption and legend. Default is TRUE.
#' @param scale Denominator of the proportion. Default is 100 to interprets numbers as percentages.
#' @param digits Numbers of digits showed on the values labels on the graphic. Default is 0.
#' @param unit Unit showed in the graphic. Default is no unit.
#' @param dec Decimal mark shown on the graphic. Default is ",".
#' @param pretty_pal Color palette used on the graphic. The palettes from the packages MetBrewer, MoMAColors and PrettyCols are available.
#' @param direction Direction of the palette color. Default is 1. The opposite direction is -1.
#' @param dodge Width of the bar, between 0 and 1. Default is 0.9.
#' @param font Font used in the graphic. Available fonts, included in the package itself, are "Roboto", "Montserrat" and "Gotham Narrow". Default is "Roboto".
#' @param wrap_width_y Number of characters before going to the line for the labels of the groups. Default is 25.
#' @param wrap_width_leg Number of characters before going to the line for the labels of the categories of quali_var. Default is 25.
#' @param legend_ncol Number of colomns in the legend. Default is 4.
#' @param title Title of the graphic.
#' @param subtitle Subtitle of the graphic.
#' @param xlab X label on the graphic. As coord_flip() is used in the graphic, xlab refers to the x label on the graphic, after the coord_flip(), and not to the x variable in the data. If xlab = NULL, the X label on the graphic wil be Distribution : " + quali_var. To show no X label, use xlab = "".
#' @param ylab Y label on the graphic. As coord_flip() is used in the graphic, ylab refers to the y label on the graphic, after the coord_flip(), and not to the y variable in the data. If ylab = NULL, Y label on the graphic will be group. To show no Y label, use ylab = "".
#' @param legend_lab Legend (fill) label on the graphic. If legend_lab = NULL, legend label on the graphic will be quali_var. To show no legend label, use legend_lab = "".
#' @param caption Caption of the graphic.
#' @param export_path Path to export the results in an xlsx file. The file includes three sheets : the table, the graphic and the statistical test.
#'
#' @return A list that contains a table, a graphic and a statistical test
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
#' # Recoding eusilc$pl030 into eusilc$pl030_rec
#' eusilc$pl030_rec <- NA
#' eusilc$pl030_rec[eusilc$pl030 == "1"] <- "Working full time"
#' eusilc$pl030_rec[eusilc$pl030 == "2"] <- "Working part time"
#' eusilc$pl030_rec[eusilc$pl030 == "3"] <- "Unemployed"
#' eusilc$pl030_rec[eusilc$pl030 == "4"] <- "Student"
#' eusilc$pl030_rec[eusilc$pl030 == "5"] <- "Retired"
#' eusilc$pl030_rec[eusilc$pl030 == "6"] <- "Permanently disabled"
#' eusilc$pl030_rec[eusilc$pl030 == "7"] <- "Fulfilling domestic tasks"
#'
#' # Computation, taking sample design into account
#' eusilc_dist_d <- distrib_group_d(
#' eusilc,
#' group = pb220a,
#' quali_var = pl030_rec,
#' strata = db040,
#' ids = db030,
#' weight = rb050,
#' title = "Distribution of socio-economic status according to nationality",
#' subtitle = "Example with austrian SILC data from 'laeken' package"
#' )
#' eusilc_dist_d$graph
#' eusilc_dist_d$tab
#'
distrib_group_discrete <- function(data,
                                   group,
                                   quali_var,
                                   facet = NULL,
                                   filter_exp = NULL,
                                   ...,
                                   na.rm.group = T,
                                   # na.rm.facet = T,
                                   # na.var = T,
                                   prop_method = "beta",
                                   show_value = TRUE,
                                   show_lab = TRUE,
                                   scale = 100,
                                   digits = 0,
                                   unit = "",
                                   dec = ",",
                                   pretty_pal = "Hokusai1",
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
                                   export_path = NULL) {

  # Un check impératif
  if((missing(data) | missing(group) | missing(quali_var)) == TRUE){
    stop("Les arguments data, group et quali_var doivent être remplis")
  }

  # Un check pour voir si quali_var n'a pas qu'un unique level => sinon aucun intérêt à l'analyse
  if(nlevels(droplevels(as.factor(data[[deparse(substitute(quali_var))]]))) == 1){
    stop(paste(deparse(substitute(quali_var)), "ne possède qu'un level"))
  }

  # Check des autres arguments
  check_arg(
    arg = list(
      prop_method = prop_method,
      unit = unit,
      dec = dec,
      pretty_pal = pretty_pal,
      font = font,
      title = title,
      subtitle = subtitle,
      xlab = xlab,
      ylab = ylab,
      legend_lab = legend_lab,
      caption = caption,
      export_path = export_path
    ),
    type = "character"
  )
  check_arg(
    arg = list(
      na.rm.group = na.rm.group,
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
      wrap_width_y = wrap_width_y,
      wrap_width_leg = wrap_width_leg,
      legend_ncol = legend_ncol
    ),
    type = "numeric"
  )

  # Petite fonction utile
  `%ni%` <- Negate(`%in%`)

  # On crée une quosure de facet & filter_exp => pour if statements dans la fonction (voir ci-dessous)
  # Solution trouvée ici : https://rpubs.com/tjmahr/quo_is_missing
  quo_facet <- enquo(facet)
  quo_filter <- enquo(filter_exp)

  # On procède d'abord à un test : il faut que toutes les variables entrées soient présentes dans data => sinon stop et erreur
  # On crée un vecteur string qui contient toutes les variables entrées
  vars_input_char <- c(as.character(substitute(quali_var)), as.character(substitute(group)))
  # On ajoute facet si non-NULL
  if(!quo_is_null(quo_facet)){
    vars_input_char <- c(vars_input_char, as.character(substitute(facet)))
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
      stop("Au moins une des variables introduites dans group, quali_var, filter_exp ou facet n'est pas présente dans data")
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
      stop("Au moins une des variables introduites dans group, quali_var, filter_exp ou facet n'est pas présente dans data")
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

  # On supprime les NA des 2 variables si na.rm.group == T
  if(na.rm.group == T){
    data_W <- data_W %>%
      filter(!is.na({{ group }}) & !is.na({{ quali_var }}))
    # idem sur la variable de facet si non-NULL
    if(!quo_is_null(quo_facet)){
      data_W <- data_W %>%
        filter(!is.na({{ facet }}))
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
        "{{ facet }}" := droplevels(as.factor({{ facet }}))) # droplevels pour éviter qu'un level soit encodé alors qu'il n'a pas d'effectifs (pb pour le test khi2)
  }

  # Ici je crée une copie des données dans data_W_NA
  # L'idée est de recoder les NA des 2 variables croisées en level "NA", pour que le khi2 s'applique aussi aux NA
  # Voir si simplification possible pour ne pas créer 2 objets : data_W & data_W_NA => cela implique de changer la suite : à voir car le fait d'avoir les NA en missing réel est pratique
  if(na.rm.group == F){
    data_W_NA <- data_W %>%
      # Idée : fct_na_value_to_level() pour ajouter un level NA encapsulé dans un droplevels() pour le retirer s'il n'existe pas de NA
      mutate("{{ group }}" := droplevels(forcats::fct_na_value_to_level({{ group }}, "NA")),
             "{{ quali_var }}" := droplevels(forcats::fct_na_value_to_level({{ quali_var }}, "NA"))
      )
    if(!quo_is_null(quo_facet)){
      data_W_NA <- data_W_NA %>% # On repart de data_W_NA => on enlève séquentiellement les NA de group puis facet
        mutate("{{ facet }}" := droplevels(forcats::fct_na_value_to_level({{ facet }}, "NA"))
        )
    }
  }

  # On réalise les tests statistiques
  # NOTE : pour l'instant uniquement lorsque pas de facet => pour facet mon idée c'est une analyse loglinéaire => pas bien pigé avec survey
  if(quo_is_null(quo_facet)){
    quali_var_fmla <- as.character(substitute(quali_var))
    group_fmla <- as.character(substitute(group))
    fmla <- stats::as.formula(paste("~", group_fmla, "+", quali_var_fmla))
    if(na.rm.group == F){
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
    if(na.rm.group == T){
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
  if (quo_is_null(quo_facet)) {
    data_W <- data_W %>%
      group_by({{ group }}, {{ quali_var }})
  }
  if (!quo_is_null(quo_facet)) {
    data_W <- data_W %>%
      group_by({{ facet }}, {{ group }}, {{ quali_var }})
  }
  tab <- data_W %>%
    summarise(
      prop = survey_prop(proportion = T, prop_method = prop_method, vartype = c("ci")),
      n_sample = unweighted(n()),
      n_weighted = survey_total(vartype = c("ci"))
    )

  # On crée la palette avecle package met.brewer
  if(pretty_pal %in% names(MetBrewer::MetPalettes)){
  palette <- as.character(MetBrewer::met.brewer(name = pretty_pal, n = nlevels(as.factor(tab[[deparse(substitute(quali_var))]])), type = "continuous", direction = direction))
  }
  #ou la crée avec le package MoMAColors
  if(pretty_pal %in% names(MoMAColors::MoMAPalettes)){
    palette <- as.character(MoMAColors::moma.colors(palette_name = pretty_pal, n = nlevels(as.factor(tab[[deparse(substitute(quali_var))]])), type = "continuous", direction = direction))
  }
  # On crée la palette avecle package PrettyCols
  if(pretty_pal %in% names(PrettyCols::PrettyColsPalettes)){
    palette <- as.character(PrettyCols::prettycols(name = pretty_pal, n = nlevels(as.factor(tab[[deparse(substitute(quali_var))]])), type = "continuous", direction = direction))
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

  # Dans le vecteur qui ordonne les levels, on a mis un NA => Or parfois pas de missing pour le groupe, même si na.rm.group.group = F !
  # On les supprime donc ssi na.rm.group = F et pas de missing sur la variable de groupe **OU** na.rm.group = T
  if ((na.rm.group == F & sum(is.na(tab[[deparse(substitute(group))]])) == 0) | na.rm.group == T)  {
    levels <- levels[!is.na(levels)]
  }

  # On charge et active les polices
  load_and_active_fonts()

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
    theme_fonctionr(font = font) +
    theme(
      legend.position = "bottom"
    ) +
    scale_fill_manual(values = palette,
                      labels = function(x) stringr::str_wrap(x, width = wrap_width_leg),
                      na.value = "grey") +
    scale_y_continuous(
      labels = function(x) { paste0(x * scale, unit) },
      expand = expansion(mult = c(.01, .05))
    ) +
    scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = wrap_width_y),
                     limits = levels) +
    guides(fill = guide_legend(ncol = legend_ncol)) +
    labs(title = title,
         subtitle = subtitle) +
    coord_flip()

  # Pour caption

  if (!is.null(caption)) { # Permet de passer à la ligne par rapport au test stat
    caption <- paste0("\n", caption)
  }

  if (quo_is_null(quo_facet)) {
    if (inherits(test.stat, "htest")) { # Condition sur inherits car si le test a réussi => test.stat est de class "htest", sinon "character"
      graph <- graph +
        labs(
          caption = paste0(
            "Khi2 d'indépendance : ", scales::pvalue(test.stat$p.value, add_p = T),
            caption
          )
        )
    }
    if (inherits(test.stat, "character")) { # Condition sur inherits car si le test a réussi => test.stat est de class "htest", sinon "character"
      graph <- graph +
        labs(
          caption = paste0(
            "Khi2 d'indépendance : conditions non remplies",
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
  if(show_lab == TRUE){
    # X ---
    if(any(is.null(xlab), xlab != "")){
      graph <- graph +
        labs(y = ifelse(is.null(xlab),
                        paste0("Distribution : ", deparse(substitute(quali_var))),
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
  }

  # Masquer les axes si show_lab == FALSE
  if(show_lab == FALSE){
    graph <- graph +
      labs(x = NULL,
           y = NULL,
           fill = NULL)
  }

  # Création des facets si facet
  if (!quo_is_null(quo_facet)) {
    graph <- graph +
      facet_wrap(vars({{ facet }})) +
      theme(panel.spacing.x = unit(1, "lines"))
  }

  # Ajouter les valeurs calculées
  if (show_value == TRUE) {
    graph <- graph +
      geom_text(
        aes(
          label = ifelse(prop > 0.02,
                         paste0(stringr::str_replace(round(prop * scale,
                                                           digits = digits),
                                                     "[.]",
                                                     dec),
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
    # Pour faceting, test pas encore implémenté => on crée un data.frame à la main
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
