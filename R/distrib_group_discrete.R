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
#' @param na.rm.facet TRUE if you want to remove observations with NA on the facet variable. FALSE if you want to create a facet with the NA value for the facet variable. Default is TRUE.
#' @param na.rm.var TRUE if you want to remove observations with NA on the discrete variable. FALSE if you want to create a modality with the NA value for the discrete variable. Default is TRUE.
#' @param prop_method Type of proportion method used to compute confidence intervals. See svyciprop in survey package for details. Default is the beta method.
#' @param show_value TRUE if you want to show the proportion in each category of each group on the graphic. FALSE if you do not want to show the proportions. Proportions of 2 percent or less are never showed on the graphic. Default is TRUE.
#' @param show_lab TRUE if you want to show axes, titles, caption and legend labels. FALSE if you do not want to show any label on axes, titles, caption and legend. Default is TRUE.
#' @param scale Denominator of the proportion. Default is 100 to interprets numbers as percentages.
#' @param digits Numbers of digits showed on the values labels on the graphic. Default is 0.
#' @param unit Unit showed in the graphic. Default is no unit.
#' @param dec Decimal mark shown on the graphic. Default is ",".
#' @param pal Color palette used on the graphic. The palettes from the packages MetBrewer, MoMAColors and PrettyCols are available.
#' @param direction Direction of the palette color. Default is 1. The opposite direction is -1.
#' @param dodge Width of the bar, between 0 and 1. Default is 0.9.
#' @param font Font used in the graphic. See load_and_active_fonts() for available fonts.
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
                                   na.rm.facet = T,
                                   na.rm.var = T,
                                   prop_method = "beta",
                                   show_value = TRUE,
                                   show_lab = TRUE,
                                   scale = 100,
                                   digits = 0,
                                   unit = "",
                                   dec = ",",
                                   pal = "Hokusai1",
                                   direction = 1,
                                   dodge = 0.9,
                                   font ="Roboto",
                                   wrap_width_y = 25,
                                   wrap_width_leg = 25,
                                   legend_ncol = 4,
                                   title = NULL,
                                   subtitle = NULL,
                                   xlab = NULL,
                                   ylab = NULL,
                                   legend_lab = NULL,
                                   caption = NULL,
                                   export_path = NULL) {


  # 1. CHECKS DES ARGUMENTS --------------------

  # Un check imperatif
  if((missing(data) | missing(group) | missing(quali_var)) == TRUE){
    stop("Les arguments data, group et quali_var doivent etre remplis")
  }

  # Un check pour voir si quali_var n'a pas qu'un unique level => sinon aucun interet a l'analyse
  if(nlevels(droplevels(as.factor(data[[deparse(substitute(quali_var))]]))) == 1){
    stop(paste(deparse(substitute(quali_var)), "ne possede qu'un level"))
  }

  # Check des autres arguments
  check_arg(
    arg = list(
      prop_method = prop_method,
      unit = unit,
      dec = dec,
      pal = pal,
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
      na.rm.facet = na.rm.facet,
      na.rm.var = na.rm.var,
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

  # On cree une quosure de facet & filter_exp => pour if statements dans la fonction (voir ci-dessous)
  # Solution trouvee ici : https://rpubs.com/tjmahr/quo_is_missing
  quo_facet <- enquo(facet)
  quo_filter <- enquo(filter_exp)

  # On procede d'abord a un test : il faut que toutes les variables entrees soient presentes dans data => sinon stop et erreur
  # On cree un vecteur string qui contient toutes les variables entrees
  vec_quali_var <- all.vars(substitute(quali_var))
  names(vec_quali_var) <- rep("quali_var", length(vec_quali_var)) # On cree un vecteur nomme pour la fonction check_input ci-dessous
  # On ajoute group
  vec_group <- c(group = as.character(substitute(group)))
  vars_input_char <- c(vec_quali_var, vec_group)
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
  # Ici le check a proprement parler
  check_input(data,
              vars_input_char)

  # Un check sur quali_var
  if(length(vec_quali_var) != 1){
    stop("quali_var ne doit comprendre qu'une seule variable")
  }


  # 2. PROCESSING DES DONNEES --------------------

  # On convertit d'abord en objet srvyr
  data_W <- convert_to_srvyr(data, ...)

  # On ne garde que les colonnes entrees en input
  data_W <- data_W |>
    select(all_of(unname(vars_input_char)))

  # On filtre si filter est non NULL
  if(!quo_is_null(quo_filter)){
    data_W <- data_W |>
      filter({{ filter_exp }})
  }

  # On supprime les NA de group si na.rm.group == T
  if(na.rm.group == T){
    data_W <- data_W |>
      filter(!is.na({{ group }}))
  }
  # On supprime les NA de quali_var si na.rm.var == T
  if(na.rm.var == T){
    data_W <- data_W |>
      filter(!is.na({{ quali_var }}))
  }
  # idem sur la variable de facet si non-NULL
  if (na.rm.facet == T) {
    if(!quo_is_null(quo_facet)){
      data_W <- data_W |>
        filter(!is.na({{ facet }}))
    }
  }

  # On convertit en facteurs si pas facteurs
  data_W <- data_W |>
    mutate(
      "{{ quali_var }}" := droplevels(as.factor({{ quali_var }})), # droplevels pour eviter qu'un level soit encode alors qu'il n'a pas d'effectifs (pb pour le test khi2)
      "{{ group }}" := droplevels(as.factor({{ group }}))
    )
  # On convertit egalement la variable de facet en facteur si facet non-NULL
  if(!quo_is_null(quo_facet)){
    data_W <- data_W |>
      mutate(
        "{{ facet }}" := droplevels(as.factor({{ facet }}))) # droplevels pour eviter qu'un level soit encode alors qu'il n'a pas d'effectifs (pb pour le test khi2)
  }


  # 3. TEST STATISTIQUE --------------------

  # Ici je remplace les NA pour les groupes / facet par une valeur "NA"
  # L'idee est de recoder les NA des 2 variables group et facet en level "NA", pour que le test stat s'applique aussi aux NA
  if (na.rm.group == F) {
    data_W <- data_W |>
      # Idee : fct_na_value_to_level() pour ajouter un level NA encapsule dans un droplevels() pour le retirer s'il n'existe pas de NA
      mutate(
        "{{ group }}" := droplevels(forcats::fct_na_value_to_level({{ group }}, "NA")),
      )
  }
  # idem sur quali_var
  if (na.rm.var == F) {
    data_W <- data_W |>
      mutate(
        "{{ quali_var }}" := droplevels(forcats::fct_na_value_to_level({{ quali_var }}, "NA"))
      )
  }
  # idem sur la variable de facet si non-NULL
  if (na.rm.facet == F) {
    if (!quo_is_null(quo_facet)) {
      data_W <- data_W |>
        mutate("{{ facet }}" := droplevels(forcats::fct_na_value_to_level({{ facet }}, "NA")))
    }
  }

  # On realise les tests statistiques
  # NOTE : pour l'instant uniquement lorsque pas de facet => pour facet mon idee c'est une analyse loglineaire => pas bien pige avec survey
  if(quo_is_null(quo_facet)){
    quali_var_fmla <- as.character(substitute(quali_var))
    group_fmla <- as.character(substitute(group))
    fmla <- stats::as.formula(paste("~", group_fmla, "+", quali_var_fmla))
    # On utilise un tryCatch pour bypasser le test s'il produit une erreur => possible lorsque les conditions ne sont pas remplies
    test.stat <- tryCatch(
      expr = {
        svychisq(fmla, data_W)
      },
    # test.stat devient un vecteur string avec 1 chaine de caracteres si erreur du test
      error = function(e){
        "Conditions non remplies"
      }
    )
  }

  # Ici je remets les NA pour les groupes / quali_var / facet => Le fait d'avoir les NA en missing reel est pratique pour construire le graphique ggplot !
  if (na.rm.group == F) {
    data_W <- data_W |>
      mutate(
        "{{ group }}" := droplevels(forcats::fct_na_level_to_value({{ group }}, "NA")),
      )
  }
  # idem sur quali_var
  if (na.rm.var == F) {
    data_W <- data_W |>
      mutate(
        "{{ quali_var }}" := droplevels(forcats::fct_na_level_to_value({{ quali_var }}, "NA"))
      )
  }
  # idem sur la variable de facet si non-NULL
  if (na.rm.facet == F) {
    if (!quo_is_null(quo_facet)) {
      data_W <- data_W |>
        mutate("{{ facet }}" := droplevels(forcats::fct_na_level_to_value({{ facet }}, "NA")))
    }
  }


  # 4. CALCUL DES FREQUENCES RELATIVES --------------------

  # On calcule les frequences relatives par groupe
  if (quo_is_null(quo_facet)) {
    data_W <- data_W |>
      group_by({{ group }}, {{ quali_var }})
  }
  if (!quo_is_null(quo_facet)) {
    data_W <- data_W |>
      group_by({{ facet }}, {{ group }}, {{ quali_var }})
  }
  tab <- data_W |>
    summarise(
      prop = survey_prop(proportion = T, prop_method = prop_method, vartype = c("ci")),
      n_sample = unweighted(n()),
      n_weighted = survey_total(vartype = c("ci"))
    ) |>
    ungroup()


  # 5. CREATION DU GRAPHIQUE --------------------

  # On cree la palette avec le package MetBrewer
  if(pal %in% names(MetBrewer::MetPalettes)){
    palette <- as.character(MetBrewer::met.brewer(name = pal, n = nlevels(as.factor(tab[[deparse(substitute(quali_var))]])), type = "continuous", direction = direction))

  # On cree la palette avec le package MoMAColors
  } else if(pal %in% names(MoMAColors::MoMAPalettes)){
    palette <- as.character(MoMAColors::moma.colors(palette_name = pal, n = nlevels(as.factor(tab[[deparse(substitute(quali_var))]])), type = "continuous", direction = direction))

  # On cree la palette avecle package PrettyCols
  } else if(pal %in% names(PrettyCols::PrettyColsPalettes)){
    palette <- as.character(PrettyCols::prettycols(palette = pal, n = nlevels(as.factor(tab[[deparse(substitute(quali_var))]])), type = "continuous", direction = direction))

  # On cree la palette avec la fonction interne official_pal()
  } else if(pal %in% official_pal(list_pal_names = T)){
    palette <- as.character(official_pal(inst = pal, n = nlevels(as.factor(tab[[deparse(substitute(quali_var))]])), direction = direction))

  } else {
    palette <- as.character(MetBrewer::met.brewer(name = "Hokusai1", n = nlevels(as.factor(tab[[deparse(substitute(quali_var))]])), type = "continuous", direction = direction))
    warning("La palette indiquee dans pal n'existe pas : la palette par defaut est utilisee")
  }

  # On cree un vecteur pour ordonner les levels de group pour mettre NA en premier (= en dernier sur le graphique ggplot)
  levels <- c(
    NA,
    rev(
      levels(
        tab[[deparse(substitute(group))]]
        )
    )
  )

  # Dans le vecteur qui ordonne les levels, on a mis un NA => Or parfois pas de missing pour le groupe, meme si na.rm.group.group = F !
  # On les supprime donc ssi na.rm.group = F et pas de missing sur la variable de groupe **OU** na.rm.group = T
  if ((na.rm.group == F & sum(is.na(tab[[deparse(substitute(group))]])) == 0) | na.rm.group == T)  {
    levels <- levels[!is.na(levels)]
  }

  # On cree le graphique
  graph <- tab |>
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
    scale_fill_manual(
      values = palette,
      labels = function(x) stringr::str_wrap(x, width = wrap_width_leg),
      na.value = "grey"
    ) +
    scale_y_continuous(
      labels = function(x) {
        paste0(x * scale, unit)
      },
      expand = expansion(mult = c(.01, .01))
    ) +
    scale_x_discrete(
      labels = function(x) stringr::str_wrap(x, width = wrap_width_y),
      limits = levels
    ) +
    guides(fill = guide_legend(ncol = legend_ncol)) +
    labs(
      title = title,
      subtitle = subtitle
    ) +
    coord_flip()

  # Pour caption

  if (!is.null(caption)) { # Permet de passer a la ligne par rapport au test stat
    caption <- paste0("\n", stringr::str_wrap(caption, width = 100))
  }

  if (quo_is_null(quo_facet)) {
    if (inherits(test.stat, "htest")) { # Condition sur inherits car si le test a reussi => test.stat est de class "htest", sinon "character"
      graph <- graph +
        labs(
          caption = paste0(
            "Khi2 d'ind","\u00e9","pendance : ", scales::pvalue(test.stat$p.value, add_p = T),
            caption
          )
        )
    }
    if (inherits(test.stat, "character")) { # Condition sur inherits car si le test a reussi => test.stat est de class "htest", sinon "character"
      graph <- graph +
        labs(
          caption = paste0(
            "Khi2 d'ind","\u00e9","pendance : conditions non remplies",
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

  # Creation des facets si facet
  if (!quo_is_null(quo_facet)) {
    graph <- graph +
      facet_wrap(vars({{ facet }})) +
      theme(panel.spacing.x = unit(1, "lines"))
  }

  # Ajouter les valeurs calculees
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


  # 6. RESULTATS --------------------

  # On cree l'objet final
  res <- list()
  res$tab <- tab
  res$graph <- graph
  # Pour l'instant, test uniquement si pas de facet
  if (quo_is_null(quo_facet)) {
    res$test.stat <- test.stat
  }

  if (!is.null(export_path)) {
    # L'export en excel

    # Pour etre integre au fichier excel, le graphique doit etre affiche => https://ycphs.github.io/openxlsx/reference/insertPlot.html
    print(graph)

    # On transforme le test stat en dataframe
    if (quo_is_null(quo_facet)) { # Pour l'instant, test uniquement si pas de facet
      if(all(test.stat != "Conditions non remplies")){
      test_stat_excel <- test.stat |>
        broom::tidy() |>
        t() |>
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
    # Pour faceting, test pas encore implemente => on cree un data.frame a la main
    if (!quo_is_null(quo_facet)) {
      test_stat_excel <- data.frame(Parameter = c("test.error"),
                                    Value = "Test pas encore implemente avec le faceting",
                                    row.names = NULL)
    }

    # J'exporte les resultats en Excel
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
