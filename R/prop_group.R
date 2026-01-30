#' prop_group
#'
#' Function to compare a proportion among different groups based on complex survey data. It produces a list containing a table, including the confidence intervals of the indicators, a ready-to-be published ggplot graphic and a Chi-Square statistical test. Exporting those results to an Excell file is possible. The confidence intervals and the statistical test are taking into account the complex survey design. In case of facets, the Chi-square test is computed on the total proportion between facets (and not within facets). In case of second group (group.fill), no Chi-square test is computed.
#'
#' @param data A dataframe or an object from the survey package or an object from the srvyr package.
#' @param group A variable defining the groups to be compared.
#' @param prop_exp An expression defining the proportion to be computed. Notice that if na.prop is "rm" any is.na() is not allowed in this argument. The removal of NA's is done before the computation of the proportion. Thus any function that takes into account NA's (e.g. 'in') will not work as designed in this argument, unless na.prop is set to "include".
#' @param group.fill A variable defining a second variable of groups to be compared.
#' @param facet A variable defining the faceting groups.
#' @param filter_exp An expression filtering the data, preserving the design.
#' @param ... All options possible in as_survey_design in srvyr package.
#' @param na.rm.group TRUE if you want to remove observations with NA on the group and the group.fill variables. FALSE if you want to create a group with the NA values for the group variable and a group.fill with the NA values for the group.fill variable.
#' @param na.rm.facet TRUE if you want to remove observations with NA on the facet variable. FALSE if you want to create a facet with the NA values for the facet variable.
#' @param na.prop "rm" to remove observations with NA on one of the variables used in prop_exp before computing the proportions, "include" to compute the proportions with the NA's in the denominators. Default is "rm". If na.prop is set to "rm" the function 'is.na()' is not allowed in prop_exp.
#' @param total TRUE if you want to compute a total, FALSE if you don't. The default is TRUE.
#' @param prop_method Type of proportion method used. See svyciprop() in survey package for details and possible values. Default is the beta method.
#' @param reorder TRUE if you want to reorder the groups according to the proportion. NA value, in case if na.rm.group = FALSE, is not included in the reorder.
#' @param show_ci TRUE if you want to show the error bars on the graphic. FALSE if you don't want to show the error bars. Default is TRUE.
#' @param show_n TRUE if you want to show on the graphic the number of individuals in the sample in each group. FALSE if you don't want to show this number. Default is FALSE.
#' @param show_value TRUE if you want to show the proportions in each group on the graphic. FALSE if you don't want to show the proportion. Default is TRUE.
#' @param show_labs TRUE if you want to show axes and legend (in case of a group.fill) labels. FALSE if you don't want to show any labels on axes and legend. Default is TRUE.
#' @param total_name Name of the total shown on the graphic Default is "Total" in french and tin englis and "Totaal" in Dutch.
#' @param scale Denominator of the proportions. Default is 100 to interpret proporitons as percentages.
#' @param digits Number of digits shown on the values labels on the graphic. Default is 0.
#' @param unit Unit shown in the graphic. Default is percent.
#' @param dec Decimal mark shown on the graphic. Default depends on lang: "," for fr and nl ; "." for en.
#' @param col Color of the bars if there is no group.fill. col must be a R color or an hexadecimal color code. Default is "deepskyblue3". The colors of total and NA group (in case of na.rm.group == FALSE) are always "grey40" and "grey". If there is a group.fill, col has no effect and pal argument should be used instead.
#' @param pal Colors of the bars if there is a group.fill. pal must be vector of R colors or hexadecimal colors or a palette from packages MetBrewer or PrettyCols or a palette from fonctionr. Default is "Coast" from PrettyCols. The color of NA group.fill (in case of na.rm.group == FALSE) are is always "grey". If there is no group.fill, pal has no effect and col argument should be used instead.
#' @param direction Direction of the palette color. Default is 1. The opposite direction is -1. If there is no group.fill, this argument has no effect.
#' @param desaturate Numeric specifying the amount of desaturation where 1 corresponds to complete desaturation (no colors, grey layers only), 0 to no desaturation, and values in between to partial desaturation. Default is 0. It affects only the palette (pal, if there is a second group) and not the monocolor (col, if there is no second group).See desaturate function from colorspace package for details. If desaturate and lighten/darken arguments are used lighten/darken applies in a second time (i.e. on the color transformed by desaturate).
#' @param lighten Numeric specifying the amount of lightening. Negative numbers cause darkening. Value shoud be ranged between -1 (black) and 1 (white). Default is 0. It doesn't affect the color of NAs (in case of na.rm.group = FALSE). It affects only the palette (pal, if there is a second group) and not the monocolor (col, if there is no second group). See lighten function from colorspace package for details. If both argument ligthen and darken are used (not advised), darken applies in a second time (i.e. on the color transformed by lighten).
#' @param darken Numeric specifying the amount of lightening. Negative numbers cause lightening. Value shoud be ranged between -1 (white) and 1 (black). Default is 0. It doesn't affect the color of NAs (in case of na.rm.group = FALSE). It affects only the palette (pal, if there is a second group) and not the monocolor (col, if there is no second group). See darken function from colorspace package for details. If both argument ligthen and darken are used (not advised), darken applies in a second time (i.e. on the color transformed by lighten).
#' @param dodge Width of the bar. Default is 0.9 to let a small space between bars. A value of 1 leads to no space betweens bars. Values higher than 1 are not advised because it causes an overlaping of the bars. dodge doesn't affect the spaces between second groups (group.fill). There is always no space between second groups.
#' @param font Font used in the graphic. See load_and_active_fonts() for available fonts. Default is "Roboto".
#' @param wrap_width_y Number of characters before going to the line for the labels of the groups. Default is 25.
#' @param wrap_width_leg Number of characters before going to the line for the labels of the group.fill. Default is 25.
#' @param legend_ncol Number of colomns in the legend. Default is 4.
#' @param title Title of the graphic.
#' @param subtitle Subtitle of the graphic.
#' @param xlab X label on the graphic. As coord_flip() is used in the graphic, xlab refers to the x label on the graphic, after the coord_flip(), and not to the x variable in the data. Default (xlab = NULL) displays "Proportion :" (if lang == "fr"), "Proportion:" (if lang == "en" ) or "Aandeel:" (if lang == "nl"), followed by the prop_exp argument. To show no X label, use xlab = "".
#' @param ylab Y label on the graphic. As coord_flip() is used in the graphic, ylab refers to the y label on the graphic, after the coord_flip(), and not to the y variable in the data. Default (ylab = NULL) displays the name of the group variable. To show no Y label, use ylab = "".
#' @param legend_lab Legend (fill) label on the graphic. Default (legend_lab = NULL) displays the name of the group.fill variable. To show no legend label, use legend_lab = "".
#' @param caption Caption of the graphic. This caption goes under de default caption showing the result of the Chi-Square test. There is no way of not showing the result of the chi-square test as a caption.
#' @param lang Language of the indications on the graphic. Possibilities are  "fr" (french), "nl" (dutch) and "en" (english). Default is "fr".
#' @param theme Theme of the graphic. Default is "fonctionr". "IWEPS" adds y axis lines and ticks. NULL uses the default grey ggplot2 theme.
#' @param coef_font A multiplier factor for font size of all fonts on the graphic. Default is 1. Usefull when exporting the graphic for a publication (e.g. in a Quarto document).
#' @param export_path Path to export the results in an xlsx file. The file includes three (without group.fill) or two sheets (with a group.fill): the table, the graphic and the Chi-Square statistical test result.
#'
#' @return A list that contains a table, a ggplot graphic and, in most cases, a Chi-square statistical test.
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
#' eusilc_prop <- prop_group(
#' eusilc,
#' group = pl030_rec,
#' prop_exp = py090n > 0,
#' strata = db040,
#' ids = db030,
#' weight = rb050,
#' title = "% of ind. receiving unemployment benefits in their hh",
#' subtitle = "Example with austrian SILC data from 'laeken' package"
#' )
#'
#' # Results in graph form
#' eusilc_prop$graph
#'
#' # Results in table format
#' eusilc_prop$tab
#'
prop_group <- function(data,
                       group,
                       prop_exp,
                       group.fill = NULL,
                       facet = NULL,
                       filter_exp = NULL,
                       ...,
                       na.rm.group = T,
                       na.rm.facet = T,
                       na.prop = "rm",
                       total = TRUE,
                       prop_method = "beta",
                       reorder = F,
                       show_ci = T,
                       show_n = FALSE,
                       show_value = TRUE,
                       show_labs = TRUE,
                       total_name = NULL,
                       scale = 100,
                       digits = 0,
                       unit = "%",
                       dec = NULL,
                       col = "deepskyblue3",
                       pal = "Coast",
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

  # start_time <- Sys.time

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

  # Check des arguments necessaires
  if((missing(data) | missing(group) | missing(prop_exp)) == TRUE){
    stop("data, group and prop_exp arguments must be filled in")
  }

  # Check des autres arguments
  check_arg(
    arg = list(
      na.prop = na.prop,
      prop_method = prop_method,
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
      reorder = reorder,
      total = total,
      show_ci = show_ci,
      show_n = show_n,
      show_value = show_value,
      show_labs = show_labs
    ),
    type = "logical"
  )
  check_arg(
    arg = list(
      scale = scale,
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
  match.arg(na.prop, choices = c("rm", "include"))
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

  # On detecte d'abord les variables entrees dans l'expression pour calculer la proportion
  vec_prop_exp <- all.vars(substitute(prop_exp))
  names(vec_prop_exp) <- rep("prop_exp", length(vec_prop_exp)) # On cree un vecteur nomme pour la fonction check_input ci-dessous
  # On ajoute group
  vec_group <- c(group = as.character(substitute(group)))
  vars_input_char <- c(vec_prop_exp, vec_group)
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

  # On convertit d'abord en objet srvyr
  # NOTE : on le fait a ce moment du script car on a besoin d'avoir un objet srvyr pour faire le mutate a l'etape d'apres !
  data_W <- convert_to_srvyr(data, ...)

  # Test que prop_exp est OK : uniquement des valeurs 0-1 / T-F ou NA
  data_W <- data_W |>
    mutate(fonctionr_test_prop_exp = {{ prop_exp }})
  if (!all(data_W$variables[["fonctionr_test_prop_exp"]] %in% c(0,1,NA))) stop(paste("prop_exp must be an expression that produces TRUE-FALSE or be a binary variable (0-1/TRUE-FALSE)"), call. = FALSE)

  if(na.prop == "rm"){
    # Si na.prop == "rm", l'expression ne peut pas contenir la fonction is.na() => il est utile de calculer la proportion de NA, mais vu qu'on supprime les NA dans la suite (voir plus loin), ca ne marche pas !
    # On regarde donc si la fonction is.na() est utilisee dans l'expression, et on bloque si c'est le cas
    names_expression <- all.names(substitute(prop_exp))
    if("is.na" %in% names_expression){
      stop("is.na() is detected in prop_exp. prop_group() does not allow the proportion of missing values to be calculated when na.prop == 'rm'")
    }
  }

  # Dictionnaire
  if(lang == "fr"){
    if(is.null(total_name)){
      total_name <- "Total"
    }
    if(is.null(dec)){
      dec <- ","
    }
    lang_khi2 <- paste0("Khi2 d'ind","\u00e9","pendance : ")
    lang_khi2_error <- paste0("Khi2 d'ind","\u00e9","pendance : conditions non remplies")
    lang_prop <- "Proportion : "
  }
  if(lang == "nl"){
    if(is.null(total_name)){
      total_name <- "Totaal"
    }
    if(is.null(dec)){
      dec <- ","
    }
    lang_khi2 <- "Chi-kwadraat van onafhankelijkheid: "
    lang_khi2_error <- "Chi-kwadraat van onafhankelijkheid: voorwaarden niet vervuld"
    lang_prop <- "Aandeel: "
  }
  if(lang == "en"){
    if(is.null(total_name)){
      total_name <- "Total"
    }
    if(is.null(dec)){
      dec <- "."
    }
    lang_khi2 <- "Chi-square of independence: "
    lang_khi2_error <- "Chi-square of independence: conditions not met"
    lang_prop <- "Proportion: "
  }


  # 2. PROCESSING DES DONNEES --------------------

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
    message(paste0(before[[1]] - after[[1]]), " observations excluded by filter_exp")

  }
  # On supprime les NA sur group + group.fill si na.rm.group = T
  if (na.rm.group == T) {

    # On calcule les effectifs avant filtre
    before <- data_W |>
      summarise(n=unweighted(n()))

    data_W <- data_W |>
      filter(!is.na({{ group }}))

    # On calcule les effectifs apres filtre
    after <- data_W |>
      summarise(n=unweighted(n()))
    # On affiche le nombre de lignes supprimees (pour verification)
    message(paste0(before[[1]] - after[[1]]), " observations excluded with missing group")

    if(!quo_is_null(quo_group.fill)){

      # On calcule les effectifs avant filtre
      before <- data_W |>
        summarise(n=unweighted(n()))

      data_W <- data_W |>
        filter(!is.na({{ group.fill }}))

      # On calcule les effectifs apres filtre
      after <- data_W |>
        summarise(n=unweighted(n()))
      # On affiche le nombre de lignes supprimees (pour verification)
      message(paste0(before[[1]] - after[[1]]), " observations excluded with missing group.fill")

    }
  }
  # idem sur la variable de facet si non-NULL
  if (na.rm.facet == T) {
    if(!quo_is_null(quo_facet)){

      # On calcule les effectifs avant filtre
      before <- data_W |>
        summarise(n=unweighted(n()))

      data_W <- data_W |>
        filter(!is.na({{ facet }}))

      # On calcule les effectifs apres filtre
      after <- data_W |>
        summarise(n=unweighted(n()))
      # On affiche le nombre de lignes supprimees (pour verification)
      message(paste0(before[[1]] - after[[1]]), " observations excluded with missing facet")

    }
  }

  # On supprime les NA sur la/les variable(s) de l'expression si na.prop == "rm" => de cette facon les n par groupe sont toujours les effectifs pour lesquels la/les variable(s) de l'expression sont non missing (et pas tout le groupe : ca on s'en fout)
  if(na.prop == "rm"){
    # On affiche les variables entrees dans l'expression via message (pour verification) => presentes dans vec_prop_exp cree au debut
    message("Variable(s) detected in prop_exp: ", paste(vec_prop_exp, collapse = ", "))
    # On calcule les effectifs avant filtre
    before <- data_W |>
      summarise(n=unweighted(n()))
    # On filtre via boucle => solution trouvee ici : https://dplyr.tidyverse.org/articles/programming.html#loop-over-multiple-variables
    for (var in vec_prop_exp) {
      data_W <- data_W |>
        filter(!is.na(.data[[var]]))
    }
    # On calcule les effectifs apres filtre
    after <- data_W |>
      summarise(n=unweighted(n()))
    # On affiche le nombre de lignes supprimees (pour verification)
    message(paste0(before[[1]] - after[[1]]), " observations excluded with missing value(s) for the variable(s) in prop_exp")

    # On convertit la variable de groupe en facteur si pas facteur
    # On cree egalement une variable binaire liee a la proportion pour le khi2
    data_W <- data_W |>
      mutate(
        "{{ group }}" := droplevels(as.factor({{ group }})), # droplevels pour eviter qu'un level soit encode alors qu'il n'a pas d'effectifs (pb pour le test khi2)
        fonctionr_express_bin = {{ prop_exp }}
      )
  }

  # Si na.prop == "include", alors on transforme les NA en 0, pour inclure tout l'echantillon au denominateur
  if(na.prop == "include"){
    data_W <- data_W |>
      mutate(
        "{{ group }}" := droplevels(as.factor({{ group }})), # droplevels pour eviter qu'un level soit encode alors qu'il n'a pas d'effectifs (pb pour le test khi2)
        fonctionr_express_bin = ifelse(!is.na({{ prop_exp }}),
                                       {{ prop_exp }},
                                       0)
      )
  }

  # On enregistre les labels originaux pour si total = T
  levels_origin_group <- levels(data_W$variables[[deparse(substitute(group))]])

  # On convertit egalement la variable de group.fill en facteur si facet non-NULL
  if(!quo_is_null(quo_group.fill)){
    data_W <- data_W |>
      mutate(
        "{{ group.fill }}" := droplevels(as.factor({{ group.fill }}))) # droplevels pour eviter qu'un level soit encode alors qu'il n'a pas d'effectifs (pb pour le test khi2)
  }

  # idem pour facet
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
    if (na.rm.group == F) {
      data_W <- data_W |>
        # Idee : fct_na_value_to_level() pour ajouter un level NA encapsule dans un droplevels() pour le retirer s'il n'existe pas de NA
        mutate("{{ group }}" := droplevels(forcats::fct_na_value_to_level({{ group }}, "NA")))
    }
    # idem sur la variable de facet si non-NULL
    if (na.rm.facet == F) {
      if (!quo_is_null(quo_facet)) {
        data_W <- data_W |> # On enleve sequentiellement les NA de group puis facet
          mutate("{{ facet }}" := droplevels(forcats::fct_na_value_to_level({{ facet }}, "NA")))
      }
    }

    # On realise les tests statistiques
    # Ici un test khi2 sur une variable binaire "fonctionr_express_bin" oui/non pour l'expression
    if(quo_is_null(quo_facet)){
      group_fmla <- as.character(substitute(group))
      fmla <- stats::as.formula(paste("~", group_fmla, "+", "fonctionr_express_bin"))
    }
    # Avec facet : prevoir une boucle pour chacune des modalite de facet => A FAIRE PLUS TARD
    if(!quo_is_null(quo_facet)){
      facet_fmla <- as.character(substitute(facet))
      fmla <- stats::as.formula(paste("~", facet_fmla, "+", "fonctionr_express_bin"))
    }

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


  # 4. CALCUL DES PROPORTIONS --------------------

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

  # On calcule les proportions par groupe
  # Si pas de total
  if(total == FALSE) {
    tab <- data_W |>
      summarise( # pas cascade si total == F
        prop = survey_mean(fonctionr_express_bin, na.rm = T, proportion = T, prop_method = prop_method, vartype = "ci"),
        n_sample = unweighted(n()), # On peut faire n(), car avec na.prop == "rm", les NA ont ete supprimes partout dans l'expression et avec "include", ils ont ete transformes en 0 => plus de NA
        n_true_weighted = survey_total({{ prop_exp }}, na.rm = T, vartype = "ci"),
        n_tot_weighted = survey_total(vartype = "ci")
      ) |>
      ungroup()
  }
  # Si total
  if(total == TRUE) {
    tab <- data_W |>
      summarise(
        prop = survey_mean(fonctionr_express_bin, na.rm = T, proportion = T, prop_method = prop_method, vartype = "ci"),
        n_sample = unweighted(n()), # On peut faire n(), car avec na.prop == "rm", les NA ont ete supprimes partout dans l'expression et avec "include", ils ont ete transformes en 0 => plus de NA
        n_true_weighted = survey_total({{ prop_exp }}, na.rm = T, vartype = "ci"),
        n_tot_weighted = survey_total(vartype = "ci")
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
        prop = survey_mean(fonctionr_express_bin, na.rm = T, proportion = T, prop_method = prop_method, vartype = "ci"),
        n_sample = unweighted(n()), # On peut faire n(), car avec na.prop == "rm", les NA ont ete supprimes partout dans l'expression et avec "include", ils ont ete transformes en 0 => plus de NA
        n_true_weighted = survey_total({{ prop_exp }}, na.rm = T, vartype = "ci"),
        n_tot_weighted = survey_total(vartype = "ci")
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
      col <- "deepskyblue3" # Alors col == "deepskyblue3"
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
      name_function = "prop_group",
      desaturate = desaturate,
      lighten = lighten,
      darken = darken
    )
  }

  # On calcule la valeur max de la proportion, pour l'ecart des geom_text dans le ggplot
  max_ggplot <- max(tab$prop, na.rm = TRUE)

  # On cree un vecteur pour ordonner les levels de group selon prop, en mettant Total et NA en premier (= en dernier sur le graphique ggplot)
  if (reorder == T) {
    levels <- c(
      total_name,
      NA,
      levels(reorder(
        tab[[deparse(substitute(group))]],
        tab[["prop"]],
        FUN = "median",
        decreasing = T
      ))[levels(reorder(
        tab[[deparse(substitute(group))]],
        tab[["prop"]],
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
  # Pour enlever le level "Total" si total == F
  if(total == FALSE) {
    levels <- levels[levels != total_name]
  }

  # On cree le graphique

  if (quo_is_null(quo_group.fill)) { # Si pas de group.fill
    graph <- tab |>
      ggplot(aes(
        x = {{ group }},
        y = prop,
        fill = {{ group }}
      ))
  }
  if (!quo_is_null(quo_group.fill)) { # Si group.fill
    graph <- tab |>
      mutate("{{ group.fill }}" := forcats::fct_rev({{ group.fill }})) |>
      ggplot(aes(
        x = {{ group }},
        y = prop,
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
            y = ifelse({{ group }} == total_name, prop, NA),
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
      graph <- graph +
        geom_text(
          aes(
            y = ifelse({{ group }} == total_name, (prop) + (0.01 * max_ggplot), NA),
            label = paste0(stringr::str_replace(round(prop * scale,
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
          position = position_dodge(width = dodge)
        )
    }
  }

  # Le resultat du test stat => uniquement si non group.fill
  if (quo_is_null(quo_group.fill)) {

    # Pour caption
    if (!is.null(caption)) { # Permet de passer a la ligne par rapport au test stat
      caption <- paste0("\n", stringr::str_wrap(caption, width = 100))
    }

    if (inherits(test.stat, "htest")) { # Condition sur inherits car si le test a reussi => test.stat est de class "htest", sinon "character"
      graph <- graph +
        labs(
          caption = paste0(
            lang_khi2, scales::pvalue(test.stat$p.value, add_p = T),
            caption
          )
        )
    }
    if (inherits(test.stat, "character")) { # Condition sur inherits car si le test a reussi => test.stat est de class "htest", sinon "character"
      graph <- graph +
        labs(
          caption = paste0(
            lang_khi2_error,
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
      graph <- graph +
        labs(y = ifelse(is.null(xlab),
                        paste0(lang_prop, deparse(substitute(prop_exp))),
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

  # Masquer les axes si show_labs == FALSE
  if(show_labs == FALSE){
    graph <- graph +
      labs(x = NULL,
           y = NULL,
           fill = NULL)
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

  # Ajouter les valeurs calculees
  if (show_value == TRUE) {
    graph <- graph +
      geom_text(
        aes(
          y = ifelse({{ group }} != total_name|is.na({{ group }}), (prop) + (0.01 * max_ggplot), NA),
          label = paste0(stringr::str_replace(round(prop * scale,
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
        size = coef_font * fonctionr_font_size(type = "little"),
        alpha = 0.7,
        hjust = 0, # Justifie a droite
        vjust = 0.4,
        position = position_dodge(width = dodge)
      )
  }


  # 6. RESULTATS --------------------

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
                 percent_fm = TRUE,
                 fgFill = "skyblue3",
                 bivariate = !quo_is_null(quo_group.fill))
  }

  # end_time <- Sys.time()
  # message(paste("Processing time:", round(end_time - start_time, 2), "sec"))

  return(res)

}
