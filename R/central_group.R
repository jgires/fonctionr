#' central_group
#'
#' Function to compare means or medians among different groups based on complex survey data. It produces a list containing a table, including the confidence intervals of the indicators, a ready-to-be published ggplot graphic and a statistical test. In case of mean comparison, the statistical test is a Wald test (using survey::regTermTest). In case of median comparison the statistical test is a Kruskal Wallis test (using survey::svyranktest(test = "KruskalWallis")). Exporting the results to an Excell file is possible. The confidence intervals and the statistical test are taking into account the complex survey design. In case of facets, the statistical test is computed on the total means or medians between facets (and not within facets). In case of second group (group.fill), no statistical test is computed.
#'
#' @name central_group
#'
#' @param data A dataframe or an object from the survey package or an object from the srvyr package.
#' @param group A variable defining groups to be compared.
#' @param quanti_exp An expression defining the quantitative variable from which the mean/median is computed. Notice that if any observations with NA in at least one of the variable in quanti_exp are excluded for the computation of the indicators.
#' @param type "mean" to compute mean by group ; "median" to compute median by group.
#' @param group.fill A variable defining a second variable of groups to be compared.
#' @param facet A variable defining the faceting group.
#' @param filter_exp An expression filtering the data, preserving the design.
#' @param ... All options possible in as_survey_design in srvyr package.
#' @param na.rm.group TRUE if you want to remove observations with NA on the group and the group.fill variables. FALSE if you want to create a group with the NA values for the group variable and a group.fill with the NA values for the group.fill variable. Default is TRUE.
#' @param na.rm.facet TRUE if you want to remove observations with NA on the facet variable. FALSE if you want to create a facet with the NA values for the facet variable. Default is TRUE.
#' @param total TRUE if you want to compute a total, FALSE if you don't. The default is TRUE.
#' @param reorder TRUE if you want to reorder the groups according to the mean/median. NA value, in case if na.rm.group = FALSE, is not included in the reorder. In case of facets, the groups are reordered based on each median group. Default is FALSE.
#' @param show_ci TRUE if you want to show the error bars on the graphic. FALSE if you don't want to show the error bars. Default is TRUE.
#' @param show_n TRUE if you want to show on the graphic the number of observations in the sample in each group. FALSE if you don't want to show this number. Default is FALSE.
#' @param show_value TRUE if you want to show the mean/median in each group on the graphic. FALSE if you don't want to show the mean/median. Default is TRUE.
#' @param show_labs TRUE if you want to show axes and legend (in case of a group.fill) labels. FALSE if you don't want to show any labels on axes and legend. Default is TRUE.
#' @param total_name Name of the total displayed on the graphic. Default is "Total" in French and in English and "Totaal" in Dutch.
#' @param digits Number of decimal places displayed on the values labels on the graphic. Default is 0.
#' @param unit Unit displayed on the graphic. Default is none.
#' @param dec Decimal mark displayed on the graphic. Default depends on lang: "," for fr and nl ; "." for en.
#' @param col Color of the bars if there is no group.fill. col must be a R color or an hexadecimal color code. Default color used depends on type : "deeppink3" for mean and "mediumorchid3" for median. The colors of total and NA group (in case of na.rm.group == FALSE) are always "grey40" and "grey". If there is a group.fill, col has no effect and pal argument should be used instead.
#' @param pal Colors of the bars if there is a group.fill. pal must be vector of R colors or hexadecimal colors or a palette from packages MetBrewer or PrettyCols or a palette from fonctionr. Default is "Peppers" from PrettyCols. The color of NA group.fill (in case of na.rm.group == FALSE) andt of the total are always "grey" and "grey40". If there is no group.fill, pal has no effect and col argument should be used instead.
#' @param direction Direction of the palette color. Default is 1. The opposite direction is -1. If there is no group.fill, this argument has no effect.
#' @param desaturate Numeric specifying the amount of desaturation where 1 corresponds to complete desaturation (no colors, grey layers only), 0 to no desaturation, and values in between to partial desaturation. Default is 0. It affects only the palette (pal, if there is a second group) and not the monocolor (col, if there is no second group).See desaturate function from colorspace package for details. If desaturate and lighten/darken arguments are used, lighten/darken is applied in a second time (i.e. on the color transformed by desaturate).
#' @param lighten Numeric specifying the amount of lightening. Negative numbers cause darkening. Value shoud be ranged between -1 (black) and 1 (white). Default is 0. It doesn't affect the color of NAs (in case of na.rm.group = FALSE). It affects only the palette (pal, if there is a second group) and not the monocolor (col, if there is no second group). See colorspace::lighten for details. If both argument ligthen and darken are used (not advised), darken is applied in a second time (i.e. on the color transformed by lighten).
#' @param darken Numeric specifying the amount of lightening. Negative numbers cause lightening. Value shoud be ranged between -1 (white) and 1 (black). Default is 0. It doesn't affect the color of NAs (in case of na.rm.group = FALSE). It affects only the palette (pal, if there is a second group) and not the monocolor (col, if there is no second group). See colorspace::darken for details. If both argument ligthen and darken are used (not advised), darken is applied in a second time (i.e. on the color transformed by lighten).#'
#' @param dodge Width of the bar. Default is 0.9 to let a small space between bars. A value of 1 leads to no space betweens bars. Values higher than 1 are not advised because they cause an overlaping of the bars. dodge doesn't affect the spaces between second groups (group.fill). There is always no space between second groups.
#' @param font Font used in the graphic. See load_and_active_fonts() for available fonts. Default is "Roboto".
#' @param wrap_width_y Number of characters before going to the line for the labels of the groups. Default is 25.
#' @param wrap_width_leg Number of characters before going to the line for the labels of the group.fill. Default is 25.
#' @param legend_ncol Number of colomns in the legend. Default is 4.
#' @param title Title of the graphic.
#' @param subtitle Subtitle of the graphic.
#' @param xlab X label on the graphic. As coord_flip() is used in the graphic, xlab refers to the x label on the graphic, after the coord_flip(), and not to the x variable in the data. Default (xlab = NULL) displays, for type = "mean", "Moyenne :" (if lang == "fr"), "Mean:" (if lang == "en" ) or "Gemiddelde:" (if lang == "nl"), or, for type = "median", "Médiane :" (if lang == "fr"), "Median:" (if lang == "en" ) or "Mediaan:" (if lang == "nl"), followed by the quanti_exp argument. To show no X label, use xlab = "".
#' @param ylab Y label on the graphic. As coord_flip() is used in the graphic, ylab refers to the y label on the graphic, after the coord_flip(), and not to the y variable in the data. Default (ylab = NULL) displays the name of the group variable. To show no Y label, use ylab = "".
#' @param legend_lab Legend (fill) label on the graphic. If legend_lab = NULL, legend label on the graphic will be group.fill. To show no legend label, use legend_lab = "".
#' @param caption Caption of the graphic. This caption goes under de default caption showing the result of the Chi-Square test. There is no way of not showing the result of the chi-square test as a caption.
#' @param lang Language of the indications on the graphic. Possibilities are "fr" (french), "nl" (dutch) and "en" (english). Default is "fr".
#' @param theme Theme of the graphic. Default is "fonctionr". "IWEPS" adds y axis lines and ticks. NULL uses the default grey ggplot2 theme.
#' @param coef_font A multiplier factor for font size of all fonts on the graphic. Default is 1. Usefull when exporting the graphic for a publication (e.g. in a Quarto document).
#' @param export_path Path to export the results in an xlsx file. The file includes three (without group.fill) or two sheets (with a group.fill): the table, the graphic and the statistical test result.
#'
#' @return A list that contains a table, a ggplot graphic and, in most cases, a statistical test.
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
#' # Creation of age categories
#' eusilc$age_cat <- cut(eusilc$age,
#' breaks = 6,
#' include.lowest = TRUE)
#'
#' # Calculation of income means by age category with fonctionr, taking sample design into account
#' eusilc_mean <- mean_group(
#'   eusilc,
#'   group = age_cat,
#'   quanti_exp = eqIncome / 12,
#'   strata = db040,
#'   ids = db030,
#'   weight = rb050,
#'   title = "Mean of equivalised income in household by age of individuals",
#'   subtitle = "Example with austrian SILC data from 'laeken' package"
#'   )
#'
#' # Results in graph form
#' eusilc_mean$graph
#'
#' # Results in table format
#' eusilc_mean$tab
#'
central_group <- function(data,
                          group,
                          quanti_exp,
                          type,
                          group.fill = NULL,
                          facet = NULL,
                          filter_exp = NULL,
                          ...,
                          na.rm.group = TRUE,
                          na.rm.facet = TRUE,
                          total = TRUE,
                          reorder = FALSE,
                          show_ci = TRUE,
                          show_n = FALSE,
                          show_value = TRUE,
                          show_labs = TRUE,
                          total_name = NULL,
                          digits = 0,
                          unit = "",
                          dec = NULL,
                          col = NULL,
                          pal = "Peppers",
                          direction = 1,
                          desaturate = 0,
                          lighten = 0,
                          darken = 0,
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
                          lang = "fr",
                          theme = "fonctionr",
                          coef_font = 1,
                          export_path = NULL) {

  # Les arguments par defaut
  formals.args <- formals()
  # On enregistre le call
  call <- match.call()
  # On cree un vecteur avec le nom des arguments definis explicitement par l'utilisateur (sans le nom de la fonction)
  user.args <- names(call[-1])

  # On cree une liste avec les noms et valeurs des arguments definis dans fonctionr_options()
  list_opt_fonctionr <- options()[names(options()) == "fonctionr.options"]
  # On ne retient que les options definies dans fonctionr_options() MAIS qui ne sont pas definies par l'utilisateur et qui sont bien utilisees dans cette fonction (pour ne pas creer d'objets pour rien, source potentielle d'erreur)
  list_opt_fonctionr$fonctionr.options <- list_opt_fonctionr$fonctionr.options[!(names(list_opt_fonctionr$fonctionr.options) %in% user.args) & names(list_opt_fonctionr$fonctionr.options) %in% names(formals.args)]

  # NOTE : on fait la suite SSI il y a des options qui remplissent cette condition
  if(length(list_opt_fonctionr$fonctionr.options > 0)){

    warning(
      "Parametres actifs dans fonctionr_options(): ",
      paste(
        names(list_opt_fonctionr$fonctionr.options),
        collapse = ", "
      )
    )

    # On cree des objets avec les valeurs definies dans la liste pour toutes ces options (= on remplace les arguments par defaut de la fonction)
    for(x in names(list_opt_fonctionr$fonctionr.options)) assign(x, list_opt_fonctionr$fonctionr.options[[x]])
  }


  # 1. CHECKS DES ARGUMENTS --------------------

  # Un check imperatif
  if(missing(type) == TRUE){
    stop("L'argument type doit etre rempli")
  }
  if((missing(data) | missing(group) | missing(quanti_exp)) == TRUE){
    stop("Les arguments data, group et quanti_exp doivent etre remplis")
  }

  # Check des autres arguments
  check_arg(
    arg = list(
      type = type,
      total_name = total_name,
      unit = unit,
      dec = dec,
      col = col,
      # pal = pal, # Je supprime pour pouvoir generer automatiquement des palettes dans l'argument avec des fonctions
      font = font,
      title = title,
      subtitle = subtitle,
      xlab = xlab,
      ylab = ylab,
      legend_lab = legend_lab,
      caption = caption,
      lang = lang,
      theme = theme,
      export_path = export_path
    ),
    type = "character"
  )
  check_arg(
    arg = list(
      na.rm.group = na.rm.group,
      na.rm.facet = na.rm.facet,
      total = total,
      reorder = reorder,
      show_ci = show_ci,
      show_n = show_n,
      show_value = show_value,
      show_labs = show_labs
    ),
    type = "logical"
  )
  check_arg(
    arg = list(
      digits = digits,
      direction = direction,
      desaturate = desaturate,
      lighten = lighten,
      darken = darken,
      dodge = dodge,
      wrap_width_y = wrap_width_y,
      wrap_width_leg = wrap_width_leg,
      legend_ncol = legend_ncol,
      coef_font = coef_font
    ),
    type = "numeric"
  )

  # Check que les arguments avec choix precis sont les bons
  match.arg(type, choices = c("mean", "median"))
  lang <- tolower(lang)
  match.arg(lang, choices = c("fr", "nl", "en"))

  # Petite fonction utile
  `%ni%` <- Negate(`%in%`)

  # On cree une quosure de group.fill, facet & filter_exp => pour if statements dans la fonction (voir ci-dessous)
  # Solution trouvee ici : https://rpubs.com/tjmahr/quo_is_missing
  quo_group.fill <- enquo(group.fill)
  quo_facet <- enquo(facet)
  quo_filter <- enquo(filter_exp)

  # On procede d'abord a un test : il faut que toutes les variables entrees soient presentes dans data => sinon stop et erreur
  # On cree un vecteur string qui contient toutes les variables entrees
  # Solution trouvee ici : https://stackoverflow.com/questions/63727729/r-how-to-extract-object-names-from-expression

  # On detecte d'abord les variables entrees dans l'expression pour calculer la moyenne/mediane
  vec_quanti_exp <- all.vars(substitute(quanti_exp))
  names(vec_quanti_exp) <- rep("quanti_exp", length(vec_quanti_exp)) # On cree un vecteur nomme pour la fonction check_input ci-dessous
  # On ajoute groupe
  vec_group <- c(group = as.character(substitute(group)))
  vars_input_char <- c(vec_quanti_exp, vec_group)
  # On ajoute group.fill si non-NULL
  if(!quo_is_null(quo_group.fill)){
    vec_group.fill <- c(group.fill = as.character(substitute(group.fill)))
    vars_input_char <- c(vars_input_char, vec_group.fill)
  }
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

  # Dictionnaire
  if(lang == "fr"){
    if(is.null(total_name)){
      total_name <- "Total"
    }
    if(is.null(dec)){
      dec <- ","
    }
    lang_anova <- "Test de Wald : "
    lang_kruskal <- "Kruskal Wallis : "
    lang_mean <- "Moyenne : "
    lang_median <- paste0("M","\u00e9","diane : ")
  }
  if(lang == "nl"){
    if(is.null(total_name)){
      total_name <- "Totaal"
    }
    if(is.null(dec)){
      dec <- ","
    }
    lang_anova <- "Wald-test: "
    lang_kruskal <- "Kruskal Wallis: "
    lang_mean <- "Gemiddelde: "
    lang_median <- "Mediaan: "
  }
  if(lang == "en"){
    if(is.null(total_name)){
      total_name <- "Total"
    }
    if(is.null(dec)){
      dec <- "."
    }
    lang_anova <- "Wald test: "
    lang_kruskal <- "Kruskal Wallis: "
    lang_mean <- "Mean: "
    lang_median <- "Median: "
  }


  # 2. PROCESSING DES DONNEES --------------------

  # On convertit d'abord en objet srvyr
  data_W <- convert_to_srvyr(data, ...)

  # On ne garde que les colonnes entrees en input
  data_W <- data_W |>
    select(all_of(unname(vars_input_char)))

  # On filtre si filter est non NULL
  if(!quo_is_null(quo_filter)){

    # On calcule les effectifs avant filtre
    before <- data_W |>
      summarise(n=unweighted(n()))

    data_W <- data_W |>
      filter({{ filter_exp }})

    # On calcule les effectifs apres filtre
    after <- data_W |>
      summarise(n=unweighted(n()))
    # On affiche le nombre de lignes supprimees (pour verification)
    message(paste0(before[[1]] - after[[1]]), " observations removed by filter_exp")

  }

  # On supprime les NA sur facet sifacet non-NULL et na.rm.facet = T
  if (na.rm.facet == T) {
    if(!quo_is_null(quo_facet)){

      # message avec le nombre d'exclus pour facet
      message(paste0(data_W |>
                       filter(is.na({{facet}})) |>
                       summarise(n = unweighted(n())), " observations removed due to missing facet"))

      data_W <- data_W |>
        filter(!is.na({{ facet }}))

    }
  }


  # On supprime les NA sur group + group.fill si na.rm.group = T
  if (na.rm.group == T) {

    # message avec le nombre d'exclus pour group
    message(paste0(data_W |>
                     filter(is.na({{group}})) |>
                     summarise(n = unweighted(n())), " observations removed due to missing group"))

    data_W <- data_W |>
      filter(!is.na({{ group }}))


    if(!quo_is_null(quo_group.fill)){

      # message avec le nombre d'exclus pour group.fill
      message(paste0(data_W |>
                       filter(is.na({{group.fill}})) |>
                       summarise(n = unweighted(n())), " observations removed due to missing group.fill"))

      data_W <- data_W |>
        filter(!is.na({{ group.fill }}))

    }
  }

  # On supprime les NA sur la/les variable(s) quanti dans tous les cas, sinon ambigu => de cette facon les n par groupe sont toujours les effectifs pour lesquels la/les variable(s) quanti sont non missing (et pas tout le groupe : ca on s'en fout)
  # On les affiche via message (pour verification)
  message("Variable(s) detectee(s) dans quanti_exp : ", paste(vec_quanti_exp, collapse = ", "))
  # On calcule les effectifs avant filtre
  before <- data_W |>
    summarise(n=unweighted(n()))
  # On filtre via boucle => solution trouvee ici : https://dplyr.tidyverse.org/articles/programming.html#loop-over-multiple-variables
  for (var in vec_quanti_exp) {
    data_W <- data_W |>
      filter(!is.na(.data[[var]]))
  }
  # On calcule les effectifs apres filtre
  after <- data_W |>
    summarise(n=unweighted(n()))
  # On affiche le nombre de lignes supprimees (pour verification)
  message(paste0(before[[1]] - after[[1]]), " observations removed due to missing value(s) for the variable(s) in quanti_exp")

  # On convertit la variable de groupe en facteur si pas facteur
  data_W <- data_W |>
    mutate(
      "{{ group }}" := droplevels(as.factor({{ group }})), # droplevels pour eviter qu'un level soit encode alors qu'il n'a pas d'effectifs (pb pour le test stat potentiel)
      quanti_exp_flattened = {{ quanti_exp }} # On recalcule quanti_exp dans une variable unique si c'est une expression a la base => necessaire pour les tests stat ci-dessous
      )

  # On enregistre les labels originaux pour si total = T
  levels_origin_group <- levels(data_W$variables[[deparse(substitute(group))]])

  # On convertit egalement la variable de group.fill en facteur si facet non-NULL
  if(!quo_is_null(quo_group.fill)){
    data_W <- data_W |>
      mutate(
        "{{ group.fill }}" := droplevels(as.factor({{ group.fill }}))) # droplevels pour eviter qu'un level soit encode alors qu'il n'a pas d'effectifs (pb pour le test khi2)
  }

  # On convertit egalement la variable de facet en facteur si facet non-NULL
  if(!quo_is_null(quo_facet)){
    data_W <- data_W |>
      mutate(
        "{{ facet }}" := droplevels(as.factor({{ facet }}))) # droplevels pour eviter qu'un level soit encode alors qu'il n'a pas d'effectifs (pb pour le test khi2)
  }


  # 3. TEST STATISTIQUE --------------------

  # Uniquement si pas de group.fill (A PREVOIR PLUS TARD ?)
  if(quo_is_null(quo_group.fill)){

    # Ici je remplace les NA pour les groupes / facet par une valeur "NA"
    # L'idee est de recoder les NA des 2 variables group et facet en level "NA", pour que le test stat s'applique aussi aux NA
    if(na.rm.group == F){
      data_W <- data_W |>
        # Idee : fct_na_value_to_level() pour ajouter un level NA encapsule dans un droplevels() pour le retirer s'il n'existe pas de NA
        mutate("{{ group }}" := droplevels(forcats::fct_na_value_to_level({{ group }}, "NA"))
        )
    }
    if (na.rm.facet == F) {
      # idem sur la variable de facet si non-NULL
      if(!quo_is_null(quo_facet)){
        data_W <- data_W |> # On enleve sequentiellement les NA de group puis facet
          mutate("{{ facet }}" := droplevels(forcats::fct_na_value_to_level({{ facet }}, "NA"))
          )
      }
    }

    # On realise les tests statistiques
    # Solutions trouvees ici :
    # https://stackoverflow.com/questions/27261232/passing-argument-to-lm-in-r-within-function
    # https://stackoverflow.com/questions/72384740/passing-data-variables-to-r-formulas
    # https://stackoverflow.com/questions/52856711/use-function-arguments-in-lm-formula-within-function-environment
    # Pour regTermTest => explique par Lumley himself : https://stackoverflow.com/questions/72843411/one-way-anova-using-the-survey-package-in-r
    quanti_exp_fmla <- "quanti_exp_flattened" # Un string car on a cree la variable "en dur" dans la fonction
    if(quo_is_null(quo_facet)){
      group_fmla <- as.character(substitute(group))
      fmla <- stats::as.formula(paste(quanti_exp_fmla, "~", group_fmla))
      fmla2 <- stats::as.formula(paste("~", group_fmla))
    }
    # Avec facet : prevoir une boucle pour chacune des modalite de facet => A FAIRE PLUS TARD
    if(!quo_is_null(quo_facet)){
      group_fmla <- as.character(substitute(facet))
      fmla <- stats::as.formula(paste(quanti_exp_fmla, "~", group_fmla))
      fmla2 <- stats::as.formula(paste("~", group_fmla))
    }

    if(type == "mean"){
      model <- svyglm(fmla, design = data_W)
      test.stat <- regTermTest(model, fmla2)
      test.stat[["call"]] <- paste(quanti_exp_fmla, " ~ ", group_fmla)
    }
    if(type == "median"){
      test.stat <- svyranktest(fmla, design = data_W, test = "KruskalWallis")
    }
    # /!\ NOTE : ca fonctionne mais j'ai peur d'utiliser eval => solution precedente choisie, qui a tout de meme le pb de ne pas garder la formule dans le call
    # if(type == "median"){
    #   if(na.rm.group == T){
    #     eval(substitute(test.stat <- svyranktest(quanti_exp ~ group, design = data_W, test = "KruskalWallis")))
    #   }
    #   if(na.rm.group == F){
    #     eval(substitute(test.stat <- svyranktest(quanti_exp ~ group, design = data_W_NA, test = "KruskalWallis")))
    #   }
    # }

    # Ici je remets les NA pour les groupes / facet => Le fait d'avoir les NA en missing reel est pratique pour construire le graphique ggplot !
    if(na.rm.group == F){
      data_W <- data_W |>
        mutate("{{ group }}" := droplevels(forcats::fct_na_level_to_value({{ group }}, "NA"))
        )
    }
    if (na.rm.facet == F) {
      # idem sur la variable de facet si non-NULL
      if(!quo_is_null(quo_facet)){
        data_W <- data_W |> # On enleve sequentiellement les NA de group puis facet
          mutate("{{ facet }}" := droplevels(forcats::fct_na_level_to_value({{ facet }}, "NA"))
          )
      }
    }

  }


  # 4. CALCUL DES MOYENNES/MEDIANES --------------------

  # On definit le grouping
  # Si facet
  if (!quo_is_null(quo_facet)) {
    data_W <- data_W |>
      group_by({{ facet }})
  }
  # Group (dans tous les cas)
  data_W <- data_W |>
    group_by({{ group }}, .add = TRUE)
  # Si group.fill
  if (!quo_is_null(quo_group.fill)) {
    data_W <- data_W |>
      group_by({{ group.fill }}, .add = TRUE)
  }

  # On calcule l'indicateur par groupe (mean ou median selon la fonction appelee)
  # Si pas de total
  if(total == FALSE) {
    tab <- data_W |>
      summarise( # pas cascade si total == F
        indice = if (type == "median") {
          survey_median({{ quanti_exp }}, na.rm = T, vartype = "ci")
        } else if (type == "mean") survey_mean({{ quanti_exp }}, na.rm = T, vartype = "ci"),
        n_sample = unweighted(n()), # On peut faire n(), car les NA ont ete supprimes partout dans l'expression (precedemment dans la boucle) => plus de NA
        n_weighted = survey_total(vartype = "ci")
      ) |>
      ungroup()
  }
  # Si total
  if(total == TRUE) {
    tab <- data_W |>
      summarise(
        indice = if (type == "median") {
          survey_median({{ quanti_exp }}, na.rm = T, vartype = "ci")
        } else if (type == "mean") survey_mean({{ quanti_exp }}, na.rm = T, vartype = "ci"),
        n_sample = unweighted(n()), # On peut faire n(), car les NA ont ete supprimes partout dans l'expression (precedemment dans la boucle) => plus de NA
        n_weighted = survey_total(vartype = "ci")
      ) |>
      ungroup()

    # On calcule les proportions pour le total
    if (quo_is_null(quo_facet)) { # On refait un grouping mais sans group (=> pour le total)
      data_W <- data_W |>
        group_by({{ group.fill }})
    }
    if (!quo_is_null(quo_facet)) {
      data_W <- data_W |>
        group_by({{ facet }}, {{ group.fill }})
    }

    tab_tot <- data_W |>
      summarise(
        indice = if (type == "median") {
          survey_median({{ quanti_exp }}, na.rm = T, vartype = "ci")
        } else if (type == "mean") survey_mean({{ quanti_exp }}, na.rm = T, vartype = "ci"),
        n_sample = unweighted(n()), # On peut faire n(), car les NA ont ete supprimes partout dans l'expression (precedemment dans la boucle) => plus de NA
        n_weighted = survey_total(vartype = "ci")
      ) |>
      ungroup() |>
      mutate("{{ group }}" := total_name)

    # On joint les resultats par groupe et pour le total
    tab <- bind_rows(tab, tab_tot) |>
      mutate("{{ group }}" := factor({{ group }}, levels = c(levels_origin_group, total_name))) # On recree un facteur avec l'ordre original + total
  }


  # 5. CREATION DU GRAPHIQUE --------------------

  # On cree la palette

  # La palette est differente selon qu'il y a group.fill (1 palette) ou non (1 couleur)
  if(quo_is_null(quo_group.fill)) {
    # On cree la palette : avec le total au debut (en gris fonce) puis x fois la col selon le nombre de levels - 1 (le total etant deja un niveau)
    # Si couleur introduite par l'utilisateur
    if(!is.null(col) & all(isColor(col)) == TRUE){
      palette <- c(rep(col, nlevels(tab[[deparse(substitute(group))]]) - 1), "grey40")
    # Si col est NULL ou n'est pas valide => on met la couleur par defaut
    } else {
      # Warning uniquement si une couleur fausse a ete entree
      if(!is.null(col) & all(isColor(col)) == FALSE){
        warning("col n'est pas valide : la couleur par defaut est utilisee")
      }
      if(type == "mean"){
        col <- "deeppink3"
      }
      if(type == "median"){
        col <- "mediumorchid3"
      }
      palette <- c(rep(col, nlevels(tab[[deparse(substitute(group))]]) - 1), "grey40")
    }
    # Si pas de total, alors pas de gris mais tout en col (indiquee par l'utilisateur ou par defaut si n'existe pas)
    if(total == FALSE) {
      palette[palette == "grey40"] <- col
    }
  }

  if(!quo_is_null(quo_group.fill)) {
    palette <- create_palette(
      pal = pal,
      levels_palette = nlevels(as.factor(tab[[deparse(substitute(group.fill))]])),
      direction = -1*direction,
      name_function = "central_group",
      desaturate = desaturate,
      lighten = lighten,
      darken = darken
    )
  }

  # On calcule la valeur max de l'indice, pour l'ecart des geom_text dans le ggplot
  max_ggplot <- max(tab$indice, na.rm = TRUE)

  # On cree un vecteur pour ordonner les levels de group selon mean, en mettant Total et NA en premier (= en dernier sur le graphique ggplot)
  if (reorder == T) {
    levels <- c(
      total_name,
      NA,
      levels(reorder(
        tab[[deparse(substitute(group))]],
        tab[["indice"]],
        FUN = "median",
        decreasing = T
      ))[levels(reorder(
        tab[[deparse(substitute(group))]],
        tab[["indice"]],
        FUN = "median",
        decreasing = T
      )) != total_name]
    )
  }

  # On cree un vecteur pour ordonner les levels de group pour mettre Total et NA en premier (= en dernier sur le graphique ggplot)
  if (reorder == F) {
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

  # Dans le vecteur qui ordonne les levels, on a mis un NA => Or parfois pas de missing pour le groupe, meme si na.rm.group = F !
  # On les supprime donc ssi na.rm.group = F et pas de missing sur la variable de groupe **OU** na.rm.group = T
  if ((na.rm.group == F & sum(is.na(tab[[deparse(substitute(group))]])) == 0) | na.rm.group == T)  {
    levels <- levels[!is.na(levels)]
  }

  # Pour enlever le level "Total" si group.fill (car pas calcule) ou total == F
  if(total == FALSE) {
    levels <- levels[levels != total_name]
  }

  # On cree le graphique

  if (quo_is_null(quo_group.fill)) { # Si pas de group.fill
    graph <- tab |>
      ggplot(aes(
        x = {{ group }},
        y = indice,
        fill = {{ group }}
      ))
  }
  if (!quo_is_null(quo_group.fill)) { # Si group.fill
    graph <- tab |>
      mutate("{{ group.fill }}" := forcats::fct_rev({{ group.fill }})) |>
      ggplot(aes(
        x = {{ group }},
        y = indice,
        fill = {{ group.fill }}
      ))
  }

  graph <- graph +
    geom_bar(
      width = dodge,
      stat = "identity",
      position = "dodge"
    ) +
    theme_fonctionr(
      font = font,
      theme = theme,
      display = "ggtext",
      coef_font = coef_font
    ) +
    theme(
      legend.position = if (quo_is_null(quo_group.fill)) "none" else "bottom"
    ) +
    scale_fill_manual(
      # if statement car palette differente si group.fill ou non
      # Si non group.fill, les couleurs de la palette sont associees aux levels avec un named vector (pour eviter les erreurs) => le named vector ne comprend pas l'eventuel groupe NA, dont la couleur est geree par na.value (argument ci-dessous)
      values = if (quo_is_null(quo_group.fill)) stats::setNames(rev(palette), levels[!is.na(levels)]) else palette,
      na.value = "grey",
      labels = function(x) stringr::str_wrap(x, width = wrap_width_leg)
    ) +
    scale_x_discrete(
      # fonction interne relabel_ggtext() pour compatibilite avec ggtext
      labels = ~relabel_ggtext(x = ., wrap_width = wrap_width_y, total_name = total_name),
      limits = levels
    ) +
    guides(
      fill = guide_legend(
        ncol = legend_ncol,
        # avec group.fill : pour accorder l'ordre des couleurs sur le graphique et la legende
        reverse = TRUE
      )
    ) +
    labs(
      title = title,
      subtitle = subtitle
    ) +
    coord_flip()

  # Autre design pour la barre du total (si total = T)
  if(total == TRUE) {
    if(!quo_is_null(quo_group.fill)) {
      graph <- graph +
        geom_bar(
          aes(
            x = {{ group }},
            y = ifelse({{ group }} == total_name, indice, NA),
            color = {{ group.fill }}
          ),
          fill = "white",
          linewidth = .8,
          alpha = .8,
          width = dodge,
          stat = "identity",
          position = "dodge"
        ) +
        scale_colour_manual(
          values = palette,
          guide = "none"
        )
    }
    if (show_value == TRUE) {
      graph<-graph  +
        geom_text(
          aes(
            y = ifelse({{ group }} == total_name, indice + (0.01 * max_ggplot), NA),
            label = paste0(stringr::str_replace(round(indice,
                                                      digits = digits),
                                                "[.]",
                                                dec),
                           unit),
            family = font),
          size = coef_font * fonctionr_font_size(type = "normal"),
          vjust = ifelse(show_ci == T,
                         -0.5,
                         0.5),
          hjust = 0,
          color = "grey10",
          fontface = "bold",
          alpha = 0.9,
          # position = position_stack(vjust = .5))
          position = position_dodge(width = dodge))
    }

  }

  # Le resultat du test stat => uniquement si non group.fill
  if (quo_is_null(quo_group.fill)) {

    # Pour caption
    if (!is.null(caption)) { # Permet de passer a la ligne par rapport au test stat
      caption <- paste0("\n", stringr::str_wrap(caption, width = 100))
    }

    if (type == "mean") {
      graph <- graph +
        labs(
          caption = paste0(
            lang_anova, scales::pvalue(test.stat$p[1], add_p = T),
            caption
          )
        )
    }
    if (type == "median") {
      graph <- graph +
        labs(
          caption = paste0(
            lang_kruskal, scales::pvalue(test.stat$p.value[1], add_p = T),
            caption
          )
        )
    }
  }
  if (!quo_is_null(quo_group.fill)) {
    graph <- graph +
      labs(
        caption = stringr::str_wrap(caption, width = 100)
      )
  }

  # Ajouter les axes
  if(show_labs == TRUE){
    # X ---
    if(any(is.null(xlab), xlab != "")){
      if (type == "mean") {
        graph <- graph +
          labs(y = ifelse(is.null(xlab),
                          paste0(lang_mean, deparse(substitute(quanti_exp))),
                          xlab))
      }
      if (type == "median") {
        graph <- graph +
          labs(y = ifelse(is.null(xlab),
                          paste0(lang_median, deparse(substitute(quanti_exp))),
                          xlab))
      }
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

  # Masquer les axes si show_labs == FALSE
  if(show_labs == FALSE){
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
        labels = function(x) { paste0(x, unit) },
        limits = function(x) { c(min(x), max(x)) },
        expand = expansion(mult = c(.01, .2))
      )
  }

  # scale_y si pas de facet
  if (quo_is_null(quo_facet)) {
    graph <- graph +
      scale_y_continuous(
        labels = function(x) { paste0(x, unit) },
        limits = function(x) { c(min(x), max(x)) },
        expand = expansion(mult = c(.01, .1))
      )
  }

  # Ajouter les IC si show_ci == T
  if (show_ci == T) {
    graph <- graph +
      geom_errorbar(aes(ymin = indice_low,
                        ymax = indice_upp),
                    width = dodge * 0.05,
                    colour = "black",
                    alpha = 0.5,
                    linewidth = 0.5,
                    position = position_dodge(width = dodge)
      )
  }

  # Ajouter les valeurs calculees
  if (show_value == TRUE){
    graph <- graph  +
      geom_text(
        aes(
          y = ifelse({{ group }} != total_name|is.na({{ group }}), indice + (0.01 * max_ggplot), NA),
          label = paste0(stringr::str_replace(round(indice,
                                                    digits = digits),
                                              "[.]",
                                              dec),
                         unit),
          family = font),
        size = coef_font * fonctionr_font_size(type = "normal"),
        vjust = ifelse(show_ci == T,
                       -0.5,
                       0.5),
        hjust = 0,
        color = "black",
        alpha = 0.9,
        # position = position_stack(vjust = .5))
        position = position_dodge(width = dodge))
    }

  # Ajouter le nombre d'individus au besoin
  if (show_n == TRUE) {
    graph <- graph +
      geom_text(
        aes(
          y = 0 + (0.01 * max_ggplot), # Pour ajouter des labels avec les effectifs en dessous des barres
          label = paste0("n=", n_sample),
          family = font),
        size = coef_font * fonctionr_font_size(type = "little"),
        alpha = 0.7,
        hjust = 0, # Justifie a droite
        vjust = 0.4,
        position = position_dodge(width = dodge)
      )
  }


  # 6. RESULTATS --------------------

  # Dans un but de lisibilite, on renomme les indices "mean" ou "median" selon la fonction appelee
  if (type == "mean") {
    tab <- tab |>
      rename(mean = indice,
             mean_low = indice_low,
             mean_upp = indice_upp)
  }

  if (type == "median") {
    tab <- tab |>
      rename(median = indice,
             median_low = indice_low,
             median_upp = indice_upp)
  }

  # On cree l'objet final
  res <- list()
  res$tab <- tab
  res$graph <- graph
  if (quo_is_null(quo_group.fill)) { # Pas de test stat si group.fill
    res$test.stat <- test.stat
  }

  if (!is.null(export_path)) {
    # L'export en excel

    # On transforme le test stat en dataframe
    if (quo_is_null(quo_group.fill)) {
      if (type == "median") {
        test_stat_excel <- test.stat |>
          broom::tidy() |>
          t() |>
          as.data.frame()
        test_stat_excel$names <- rownames(test_stat_excel)
        test_stat_excel <- test_stat_excel[, c(2,1)]
        names(test_stat_excel)[1] <- "Parameter"
        names(test_stat_excel)[2] <- "Value"
      }
      # broom::tidy() ne fonctionne pas sur regTermTest => je le fais a la main
      if (type == "mean") {
        test_stat_excel <- data.frame(Parameter = c("df", "ddf", "statistic", "p.value", "method"),
                                      Value = c(test.stat$df, test.stat$ddf, test.stat$Ftest, test.stat$p, "Wald test"),
                                      row.names = NULL)
      }
    }
    # Si group.fill, test pas encore implemente => on cree un data.frame a la main
    if (!quo_is_null(quo_group.fill)) {
      test_stat_excel <- data.frame(Parameter = c("test.error"),
                                    Value = "Test pas encore implemente avec group.fill",
                                    row.names = NULL)
    }

    # J'exporte les resultats en Excel
    export_excel(tab_excel = tab,
                 graph = graph,
                 test_stat_excel = test_stat_excel,
                 facet_null = quo_is_null(quo_facet),
                 export_path = export_path,
                 percent_fm = FALSE,
                 fgFill = "mediumorchid3",
                 bivariate = !quo_is_null(quo_group.fill))
  }

  return(res)
}


#' @rdname central_group
#' @export
median_group <- function(..., type = "median") {
  central_group(..., type = type)
}


#' @rdname central_group
#' @export
mean_group <- function(..., type = "mean") {
  central_group(..., type = type)
}
