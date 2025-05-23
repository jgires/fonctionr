#' distrib_discrete
#'
#' Function describe the distribution of a discrete variable from complex survey data. It produces a table and a graphic.
#'
#' @param data A dataframe or an object from the survey package or an object from the srvyr package.
#' @param quali_var The discrete variable that is described.
#' @param facet A variable defining the faceting group.
#' @param filter_exp An expression that filters the data, preserving the design.
#' @param ... All options possible in as_survey_design in srvyr package.
#' @param na.rm.facet TRUE if you want to remove observations with NA on the facet variable. FALSE if you want to create a facet with the NA value for the facet variable. Default is TRUE.
#' @param na.rm.var TRUE if you want to remove observations with NA on the discrete variable. FALSE if you want to create a modality with the NA value for the discrete variable. Default is TRUE.
#' @param probs Vector of probabilities for H0 of the statistical test, in the correct order (will be rescaled to sum to 1). If probs = NULL, no statistical test is performed. Default is NULL.
#' @param prop_method Type of proportion method to use to compute confidence intervals. See svyciprop in survey package for details. Default is the beta method.
#' @param reorder TRUE if you want to reorder the categories according to their proportion. NA value, in case if na.rm.group = FALSE, is not included in the reorder. Default is FALSE.
#' @param show_ci TRUE if you want to show the error bars on the graphic. FALSE if you do not want to show the error bars. Default is TRUE.
#' @param show_n TRUE if you want to show on the graphic the number of individuals in the sample in each modality of quali_var. FALSE if you do not want to show this number. Default is FALSE.
#' @param show_value TRUE if you want to show the proportion of each category on the graphic. FALSE if you do not want to show the proportion. Default is TRUE.
#' @param show_labs TRUE if you want to show axes, titles and caption labels. FALSE if you do not want to show any label on axes and titles. Default is TRUE.
#' @param scale Denominator of the proportion. Default is 100 to interprets numbers as percentages.
#' @param digits Numbers of digits showed on the values labels on the graphic. Default is 0.
#' @param unit Unit showed in the graphic. Default is percent.
#' @param dec Decimal mark shown on the graphic. Depends on lang: "," for fr and nl ; "." for en.
#' @param pal Colour of the bars. NA bar, in case if na.rm.group = FALSE, and total bar are always in grey.
#' @param dodge Width of the bar, between 0 and 1. Default is 0.9.
#' @param font Font used in the graphic. See load_and_active_fonts() for available fonts.
#' @param wrap_width_y Number of characters before going to the line in the labels of the categories. Default is 25.
#' @param title Title of the graphic.
#' @param subtitle Subtitle of the graphic.
#' @param xlab X label on the graphic. As coord_flip() is used in the graphic, xlab refers to the X label on the graphic, after the coord_flip(), and not to the x variable in the data. If xlab = NULL, X label on the graphic will be "Distribution (total : 100 percent)". To show no X label, use xlab = "".
#' @param ylab Y label on the graphic. As coord_flip() is used in the graphic, ylab refers to the Y label on the graphic, after the coord_flip(), and not to the Y variable in the data. If ylab = NULL, Y label on the graphic will be quali_var. To show no Y label, use ylab = "".
#' @param caption Caption in the graphic.
#' @param lang The language of the indications on the chart. Possibilities: "fr", "nl", "en". Default is "fr".
#' @param theme Theme of the graphic. IWEPS adds y axis lines and ticks.
#' @param export_path Path to export the results in an xlsx file. The file includes three sheets : the table, the graphic and the statistical test (if probs is not NULL).
#'
#' @return A list that contains a table, a graphic and a statistical test
#' @import rlang
#' @import ggplot2
#' @import survey
#' @import srvyr
#' @import dplyr
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
#' eusilc_dist_group_d <- distrib_d(
#' eusilc,
#' pl030_rec,
#' strata = db040,
#' ids = db030,
#' weight = rb050,
#' title = "Distribution of socio-economic status",
#' subtitle = "Example with austrian SILC data from 'laeken' package"
#' )
#'
#' # Results in graph form
#' eusilc_dist_group_d$graph
#'
#' # Results in table format
#' eusilc_dist_group_d$tab
#'
distrib_discrete <- function(data,
                             quali_var,
                             facet = NULL,
                             filter_exp = NULL,
                             ...,
                             na.rm.facet = TRUE,
                             na.rm.var = TRUE,
                             probs = NULL,
                             prop_method = "beta",
                             reorder = FALSE,
                             show_ci = TRUE,
                             show_n = FALSE,
                             show_value = TRUE,
                             show_labs = TRUE,
                             scale = 100,
                             digits = 0,
                             unit = "%",
                             dec = NULL,
                             pal = "sienna2",
                             dodge = 0.9,
                             font ="Roboto",
                             wrap_width_y = 25,
                             title = NULL,
                             subtitle = NULL,
                             xlab = NULL,
                             ylab = NULL,
                             lang = "fr",
                             caption = NULL,
                             theme = NULL,
                             export_path = NULL) {


  # 1. CHECKS DES ARGUMENTS --------------------

  # Un check imperatif
  if((missing(data) | missing(quali_var)) == TRUE){
    stop("Les arguments data et quali_var doivent etre remplis")
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
      lang = lang,
      caption = caption,
      theme = theme,
      export_path = export_path
    ),
    type = "character"
  )
  check_arg(
    arg = list(
      na.rm.facet = na.rm.facet,
      na.rm.var = na.rm.var,
      show_n = show_n,
      show_labs = show_labs,
      show_value = show_value,
      reorder = reorder,
      show_ci = show_ci
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
  check_arg(
    arg = list(
      probs = probs
    ),
    type = "numeric",
    short = F
  )

  # Check que les arguments avec choix precis sont les bons
  lang <- tolower(lang)
  match.arg(lang, choices = c("fr", "nl", "en"))

  # Petite fonction utile
  `%ni%` <- Negate(`%in%`)

  # On cree une quosure de facet & filter_exp => pour if statements dans la fonction (voir ci-dessous)
  # Solution trouvee ici : https://rpubs.com/tjmahr/quo_is_missing
  quo_facet <- enquo(facet)
  quo_filter <- enquo(filter_exp)

  # On procede d'abord a un test : il faut que toutes les variables entrees soient presentes dans data => sinon stop et erreur
  # On cree un vecteur string qui contient toutes les variables entrees
  vec_quali_var <- all.vars(substitute(quali_var))
  names(vec_quali_var) <- rep("quali_var", length(vec_quali_var))
  vars_input_char <- vec_quali_var
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

  # Dictionnaire
  if(lang == "fr"){
    if(is.null(dec)){
      dec <- ","
    }
    lang_khi2_ad <- paste0("Khi2 d'ad","\u00e9","quation : ")
    lang_distrib <- "Distribution (total : 100%)"
  }
  if(lang == "nl"){
    if(is.null(dec)){
      dec <- ","
    }
    lang_khi2_ad <- "Chikwadraat goodness of fit: "
    lang_distrib <- "Distributie (totaal: 100%)"
  }
  if(lang == "en"){
    if(is.null(dec)){
      dec <- "."
    }
    lang_khi2_ad <- "Chi-square goodness of fit: "
    lang_distrib <- "Distribution (total: 100%)"
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
  # On supprime les NA de quali_var si na.rm.var == T
  if(na.rm.var == T){
    data_W <- data_W |>
      filter(!is.na({{ quali_var }}))
    }
  # idem sur la variable de facet si non-NULL
  if(na.rm.facet == T){
    if(!quo_is_null(quo_facet)){
      data_W <- data_W |>
        filter(!is.na({{ facet }}))
    }
  }

  # On convertit en facteurs si pas facteurs
  data_W <- data_W |>
    mutate(
      "{{ quali_var }}" := droplevels(as.factor({{ quali_var }})) # droplevels pour eviter qu'un level soit encode alors qu'il n'a pas d'effectifs (pb pour le test khi2)
    )
  # On convertit egalement la variable de facet en facteur si facet non-NULL
  if(!quo_is_null(quo_facet)){
    data_W <- data_W |>
      mutate(
        "{{ facet }}" := droplevels(as.factor({{ facet }}))) # droplevels pour eviter qu'un level soit encode alors qu'il n'a pas d'effectifs (pb pour le test khi2)
  }


  # 3. TEST STATISTIQUE --------------------

  # Je recode les NA des 2 variables quali_var et facet en level "NA", pour que le test stat s'applique aussi aux NA
  if (na.rm.var == F) {
    data_W <- data_W |>
      # Idee : fct_na_value_to_level() pour ajouter un level NA encapsule dans un droplevels() pour le retirer s'il n'existe pas de NA
      mutate("{{ quali_var }}" := droplevels(forcats::fct_na_value_to_level({{ quali_var }}, "NA")))
  }
  if (na.rm.facet == T) {
    if (!quo_is_null(quo_facet)) {
      data_W <- data_W |> # On enleve sequentiellement les NA de quali_var puis facet
        mutate("{{ facet }}" := droplevels(forcats::fct_na_value_to_level({{ facet }}, "NA")))
    }
  }

  # On realise un test khi2 d'adequation sur quali_var
  if(!is.null(probs)){ # Uniquement si probs est non null
    if(quo_is_null(quo_facet)){ # Uniquement sans facet (pour le moment)
      quali_var_fmla <- as.character(substitute(quali_var))
      fmla <- stats::as.formula(paste("~", quali_var_fmla))
      test.stat <- svygofchisq(fmla, data_W, p = probs)
    }
  }

  # Ici je remets les NA pour quali_var / facet => Le fait d'avoir les NA en missing reel est pratique pour construire le graphique ggplot !
  if (na.rm.var == F) {
    data_W <- data_W |>
      mutate("{{ quali_var }}" := droplevels(forcats::fct_na_level_to_value({{ quali_var }}, "NA")))
  }
  if (na.rm.facet == T) {
    if (!quo_is_null(quo_facet)) {
      data_W <- data_W |>
        mutate("{{ facet }}" := droplevels(forcats::fct_na_level_to_value({{ facet }}, "NA")))
    }
  }


  # 4. CALCUL DE LA DISTRIBUTION --------------------

  # On calcule la distribution
  if (quo_is_null(quo_facet)) {
    data_W <- data_W |>
      group_by({{ quali_var }})
    }
  if (!quo_is_null(quo_facet)) {
    data_W <- data_W |>
      group_by({{ facet }}, {{ quali_var }})
    }
  tab <- data_W |>
    summarize(
      prop = survey_prop(vartype = "ci", proportion = T, prop_method = prop_method),
      n_sample = unweighted(n()),
      n_weighted = survey_total(vartype = "ci")
    ) |>
    ungroup()


  # 5. CREATION DU GRAPHIQUE --------------------

  # On cree la palette : x fois la couleur selon le nombre de levels
  if(all(isColor(pal)) == TRUE){
    palette <- c(rep(pal, nlevels(tab[[deparse(substitute(quali_var))]])))
  } else { # Si la couleur n'est pas valide => on met la couleur par defaut
    palette <- c(rep("sienna2", nlevels(tab[[deparse(substitute(quali_var))]])))
    warning("La couleur indiquee dans pal n'existe pas : la couleur par defaut est utilisee")
  }

  # On calcule la valeur max de la proportion, pour l'ecart des geom_text dans le ggplot
  max_ggplot <- max(tab$prop, na.rm = TRUE)

  if (reorder == T ) {
    # On cree un vecteur pour ordonner les levels de quali_var selon prop, en mettant NA en premier (= en dernier sur le graphique ggplot)
    levels <- c(
      NA,
      levels(reorder(
        tab[[deparse(substitute(quali_var))]],
        tab[["prop"]],
        FUN = "median",
        decreasing = T
      ))
    )
  }

  if (reorder == F) {
    # On cree un vecteur pour ordonner les levels de quali_var pour mettre NA en premier (= en dernier sur le graphique ggplot)
    levels <- c(
      NA,
      rev(
        levels(
          tab[[deparse(substitute(quali_var))]]
        )
      )
    )
  }

  # Dans le vecteur qui ordonne les levels, on a mis un NA => Or parfois pas de missing pour quali_var, meme si na.rm.var = F !
  # On les supprime donc ssi na.rm.var = F et pas de missing sur la variable quali_var **OU** na.rm.var = T
  if ((na.rm.var == F & sum(is.na(tab[[deparse(substitute(quali_var))]])) == 0) | na.rm.var == T)  {
    levels <- levels[!is.na(levels)]
  }

  # Le graphique proprement dit

  # Pour caption
  # Permet de passer a la ligne par rapport au test stat
  if (!is.null(caption) & !is.null(probs) & quo_is_null(quo_facet)) {
    caption <- paste0("\n", stringr::str_wrap(caption, width = 100))
  }

  graph <- tab |>
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
      labels = function(x) stringr::str_wrap(x, width = wrap_width_y),
      limits = levels
      ) +
    theme_fonctionr(font = font,
                    theme = theme) +
    theme(
      legend.position = "none"
    ) +
    coord_flip() +
    labs(title = title,
         subtitle = subtitle,
         caption = if (!is.null(probs) & quo_is_null(quo_facet)) paste0(
           lang_khi2_ad, scales::pvalue(test.stat$p.value, add_p = T),
           caption) else stringr::str_wrap(caption, width = 100)
         )

  # Ajouter les axes au besoin
  if(show_labs == TRUE){
    # X ---
    if(any(is.null(xlab), xlab != "")){
      graph <- graph +
        labs(#x = NULL, # Pour cette fonction, x est vide dans tous les cas (a voir si c'est adapte dans tous les cas)
             y = ifelse(is.null(xlab),
                        lang_distrib,
                        xlab))
    }
    if(all(!is.null(xlab), xlab == "")){
      graph <- graph +
        labs(#x = NULL, # Pour cette fonction, x est vide dans tous les cas (a voir si c'est adapte dans tous les cas)
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

  # Ajouter les IC si show_ci == T
  if (show_ci == T) {
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

  # Ajouter les valeurs calculees
  if (show_value == TRUE) {
    graph <- graph +
      geom_text(
        aes(
          y = (prop) + (0.01 * max_ggplot),
          label = paste0(stringr::str_replace(round(prop * scale,
                                                    digits = digits),
                                              "[.]",
                                              dec),
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
        hjust = 0, # Justifie a droite
        vjust = 0.4
      )
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


  # 6. RESULTATS --------------------

  # On cree l'objet final
  res <- list()
  res$tab <- tab
  res$graph <- graph
  # Pour l'instant, test uniquement si pas de facet
  if(!is.null(probs) & quo_is_null(quo_facet)){
    res$test.stat <- test.stat
  }

  if (!is.null(export_path)) {
    # L'export en excel

    # On transforme le test stat en dataframe
    if (!is.null(probs) & quo_is_null(quo_facet)) {
      test_stat_excel <- test.stat |>
        broom::tidy() |>
        t() |>
        as.data.frame()
      test_stat_excel$names <- rownames(test_stat_excel)
      test_stat_excel <- test_stat_excel[, c(2,1)]
      names(test_stat_excel)[1] <- "Parameter"
      names(test_stat_excel)[2] <- "Value"
    }
    # Pour faceting, test pas encore implemente => on cree un data.frame a la main
    if (!is.null(probs) & !quo_is_null(quo_facet)) {
      test_stat_excel <- data.frame(Parameter = c("test.error"),
                                    Value = "Test pas encore implemente avec le faceting",
                                    row.names = NULL)
    }
    # si test pas demande
    if (is.null(probs)) {
      test_stat_excel <- data.frame(Parameter = c("test.error"),
                                    Value = "Test non realise",
                                    row.names = NULL)
    }

    # J'exporte les resultats en Excel
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
