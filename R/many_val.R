#' many_val
#'
#' Function to compute the proportions of a set of several binary variables or means or medians of a set of quantitative variables, based on complex survey data. It produces  a list containing a table, including the confidence intervals of the indicators and a ready-to-be published ggplot graphic. Exporting the results to an Excell file is possible. The confidence intervals are taking into account the complex survey design.
#'
#' @param data A dataframe or an object from the survey package or an object from the srvyr package.
#' @param list_vars A vector containing the names of the dummy/quantitative variables on which to compute the proportions/means/medians.
#' @param type "prop" to compute proportions ; "mean" to compute means ; "median" to compute medians.
#' @param list_vars_lab A vector containing the labels of the dummy/quantitative variables to be displayed on the graphic and in the table of result. Default uses the variable names in list_vars.
#' @param facet A variable defining the faceting group.
#' @param filter_exp An expression filtering the data, preserving the design.
#' @param ... All options possible in as_survey_design in srvyr package.
#' @param na.rm.facet TRUE if you want to remove observations with NA on the facet variable. FALSE if you want to create a facet with the NA values for the facet variable. Default is TRUE.
#' @param na.vars The treatment of NA values in variables (list_vars). "rm" removes NA seperately in each individual variable, "rm.all" removes every individual that has at least one NA in one variable. Default is "rm".
#' @param prop_method Type of proportion method used to compute confidence intervals. See survey::svyciprop() for details. Default is beta method. This argument is only used in case of type = "prop".
#' @param reorder TRUE if you want to reorder the variables according to the proportions/means/medians. Default is FALSE.
#' @param show_ci TRUE if you want to show the error bars on the graphic. FALSE if you don't want to show the error bars. Default is TRUE.
#' @param show_n TRUE if you want to show on the graphic the number of observations in the sample for each variable. The number can varie if na.vars = "rm". FALSE if you do not want to show this number. Default is FALSE.
#' @param show_value TRUE if you want to show the proportions/means/median for each variable on the graphic. FALSE if you do not want to show the proportions/means/medians. Default is TRUE.
#' @param show_labs TRUE if you want to show axes labels. FALSE if you do not want to show any labels on axes. Default is TRUE.
#' @param scale Denominator of the proportions. Default is 100 to interpret numbers as percentages. This argument is only used in case of type = "prop".
#' @param digits Number of decimal places displayed on the values labels on the graphic. Default is 0.
#' @param unit Unit displayed on the graphic. Default is percent for type = "prop" and no unit for type = "mean" or "median".
#' @param dec Decimal mark displayed on the graphic. Default depends on lang: "," for fr and nl ; "." for en.
#' @param col Color of the bars if the user wants a monocolor graph. col must be a R color or an hexadecimal color code. As pal has a priority over col, if the user wants to use col, he must not use simultaneously the pal argument (even pal = NULL).
#' @param pal Colors of the bars if the user wants the bars to have different colors. pal must be vector of R colors or hexadecimal colors or a palette from packages MetBrewer or PrettyCols or a palette from fonctionr. Default is "Egypt" from MetBrewer. pal has a priority over col.
#' @param direction Direction of the palette color. Default is 1. The opposite direction is -1.
#' @param desaturate Numeric specifying the amount of desaturation where 1 corresponds to complete desaturation (no colors, grey layers only), 0 to no desaturation, and values in between to partial desaturation. Default is 0. It affects only the palette (pal) and not the monocolor (col). See colorspace::desaturate for details. If desaturate and lighten/darken arguments are used, lighten/darken is applied in a second time (i.e. on the color transformed by desaturate).
#' @param lighten Numeric specifying the amount of lightening. Negative numbers cause darkening. Value shoud be ranged between -1 (black) and 1 (white). Default is 0. It affects only the palette (pal) and not the monocolor (col). See colorspace::lighten for details. If both argument ligthen and darken are used (not advised), darken is applied in a second time (i.e. on the color transformed by lighten).
#' @param darken Numeric specifying the amount of lightening. Negative numbers cause lightening. Value shoud be ranged between -1 (white) and 1 (black). Default is 0. It affects only the palette (pal) and not the monocolor (col). See colorspace::darken for details. If both argument ligthen and darken are used (not advised), darken is applied in a second time (i.e. on the color transformed by lighten).
#' @param dodge Width of the bars. Default is 0.9 to let a small space between bars. A value of 1 leads to no space betweens bars. Values higher than 1 are not advised because they cause an overlaping of the bars.
#' @param font Font used in the graphic. See load_and_active_fonts() for available fonts. Default is "Roboto".
#' @param wrap_width_y Number of characters before going to the line for the labels of the groups. Default is 25.
#' @param title Title of the graphic.
#' @param subtitle Subtitle of the graphic.
#' @param xlab X label on the graphic. As coord_flip() is used in the graphic, xlab refers to the x label on the graphic, after the coord_flip(), and not to the x variable in the data. Default (xlab = NULL) displays, for type = prop, "Proportion :" (if lang == "fr"), "Proportion:" (if lang == "en" ) or "Aandeel:" (if lang == "nl"), or, for type = "mean", "Moyenne :" (if lang == "fr"), "Mean:" (if lang == "en" ) or "Gemiddelde:" (if lang == "nl"), or, for type = "median", "Médiane :" (if lang == "fr"), "Median:" (if lang == "en" ) or "Mediaan:" (if lang == "nl"),  followed by the labels of the variables (list_vars_lab). To show no X label, use xlab = "".
#' @param ylab Y label on the graphic. As coord_flip() is used in the graphic, ylab refers to the y label on the graphic, after the coord_flip(), and not to the y variable in the data. Default (ylab = NULL) displays no Y label.
#' @param caption Caption of the graphic.
#' @param lang Language of the indications on the graphic. Possibilities are  "fr" (french), "nl" (dutch) and "en" (english). Default is "fr".
#' @param theme Theme of the graphic. Default is "fonctionr". "IWEPS" adds y axis lines and ticks. NULL uses the default grey ggplot2 theme.
#' @param coef_font A multiplier factor for font size of all fonts on the graphic. Default is 1. Usefull when exporting the graphic for a publication (e.g. in a Quarto document).
#' @param export_path Path to export the results in an xlsx file. The file includes two sheets: the table and the graphic.
#'
#' @return A list that contains a table and a ggplot graphic.
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
#' facet = rb090,
#' strata = db040,
#' ids = db030,
#' weight = rb050,
#' title = "Proportion of workers and Autrian according to gender",
#' subtitle = "Example with austrian SILC data from 'laeken' package"
#' )
#'
#' # Results in graph form
#' eusilc_many_prop$graph
#'
#' # Results in table format
#' eusilc_many_prop$tab
#'
many_val = function(data,
                    list_vars,
                    type,
                    list_vars_lab = NULL,
                    facet = NULL,
                    filter_exp = NULL,
                    ...,
                    na.rm.facet = T,
                    na.vars = "rm",
                    prop_method = "beta",
                    reorder = FALSE,
                    show_ci = T,
                    show_n = FALSE,
                    show_value = TRUE,
                    show_labs = TRUE,
                    scale = NULL,
                    digits = 0,
                    unit = NULL,
                    dec = NULL,
                    col = NULL,
                    pal = "Egypt",
                    direction = 1,
                    desaturate = 0,
                    lighten = 0,
                    darken = 0,
                    dodge = 0.9,
                    font ="Roboto",
                    wrap_width_y = 25,
                    title = NULL,
                    subtitle = NULL,
                    xlab = NULL,
                    ylab = NULL,
                    caption = NULL,
                    lang = "fr",
                    theme = "fonctionr",
                    coef_font = 1,
                    export_path = NULL){

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
  if(missing(type) == TRUE){
    stop("Argument should be filled in")
  }
  if((missing(data) | missing(list_vars)) == TRUE){
    stop("Arguments data and list_vars should be filled in")
  }

  # Check des autres arguments
  check_arg(
    arg = list(
      type = type,
      na.vars = na.vars,
      prop_method = prop_method,
      unit = unit,
      dec = dec,
      col = col,
      # pal = pal, # Je supprime pour pouvoir generer automatiquement des palettes dans l'argument avec des fonctions
      font = font,
      title = title,
      subtitle = subtitle,
      xlab = xlab,
      ylab = ylab,
      lang = lang,
      caption = caption,
      theme = theme
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
      na.rm.facet = na.rm.facet,
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
      scale = scale,
      digits = digits,
      direction = direction,
      desaturate = desaturate,
      lighten = lighten,
      darken = darken,
      dodge = dodge,
      wrap_width_y = wrap_width_y,
      coef_font = coef_font
    ),
    type = "numeric"
  )

  # Check que les arguments avec choix precis sont les bons
  match.arg(type, choices = c("mean", "median", "prop"))
  match.arg(na.vars, choices = c("rm", "rm.all"))
  lang <- tolower(lang)
  match.arg(lang, choices = c("fr", "nl", "en"))

  # Petite fonction utile
  `%ni%` <- Negate(`%in%`)

  # On cree une quosure de facet & filter_exp => pour if statements dans la fonction (voir ci-dessous)
  # Solution trouvee ici : https://rpubs.com/tjmahr/quo_is_missing
  quo_facet <- enquo(facet)
  quo_filter <- enquo(filter_exp)

  # On transforme les colonnes entrees en un vecteur caractere (plus facile pour le code !)
  vec_list_vars <- all.vars(substitute(list_vars))
  names(vec_list_vars) <- rep("list_vars", length(vec_list_vars))

  # Check que list_vars ne comprend que des variables binaires
  if(type == "prop"){
    check_bin(data = data,
              vec_list_vars = vec_list_vars)
  }

  message("Variables used: ", paste(vec_list_vars, collapse = ", "))

  # On procede d'abord a un test : il faut que toutes les variables entrees soient presentes dans data => sinon stop et erreur
  # On cree un vecteur string qui contient toutes les variables entrees

  # On detecte d'abord les variables entrees dans list_vars (detectees precedemment)
  vars_input_char <- vec_list_vars
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
    if(is.null(dec)){
      dec <- ","
    }
    lang_prop <- "Proportion : "
    lang_mean <- "Moyenne : "
    lang_median <- paste0("M","\u00e9","diane : ")
  }
  if(lang == "nl"){
    if(is.null(dec)){
      dec <- ","
    }
    lang_prop <- "Aandeel: "
    lang_mean <- "Gemiddelde: "
    lang_median <- "Mediaan: "
  }
  if(lang == "en"){
    if(is.null(dec)){
      dec <- "."
    }
    lang_prop <- "Proportion: "
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

  # On supprime les NA sur la/les variable(s) entrees si na.vars == "rm.all" => de cette facon les effectifs sont les memes pour tous les indicateurs.
  if(na.vars == "rm.all"){
    # On calcule les effectifs avant filtre
    before <- data_W |>
      summarise(n=unweighted(n()))
    # On filtre via boucle => solution trouvee ici : https://dplyr.tidyverse.org/articles/programming.html#loop-over-multiple-variables
    for (var in vec_list_vars) {
      data_W <- data_W |>
        filter(!is.na(.data[[var]]))
    }
    # On calcule les effectifs apres filtre
    after <- data_W |>
      summarise(n=unweighted(n()))
    # On affiche le nombre de lignes supprimees (pour verification)
    message(paste0(before[[1]] - after[[1]]), " observations removed due to missing in at least one of the variables")
  }
  else{
    message("With na.vars = 'rm', observations removed differ between variables")
  }


  # On convertit la variable de facet en facteur si facet non-NULL
  if (!quo_is_null(quo_facet)) {
    data_W <- data_W |>
      mutate(
        "{{ facet }}" := droplevels(as.factor({{ facet }})) # droplevels pour eviter qu'un level soit encode alors qu'il n'a pas d'effectifs (pb pour le test khi2)
        )
  }


  # 3. CALCUL DES INDICATEURS --------------------

  # Si facet => on groupe deja data (economie de code)
  if (!quo_is_null(quo_facet)) {
    data_W <- data_W |>
      group_by({{ facet }})
  }
  tab <- tibble()
  # On calcule les proportions
  if(type == "prop"){
    for (i in vec_list_vars) {
      tab_i <- data_W |>
        summarise(
          list_col = i,
          indice = survey_mean(.data[[i]], na.rm = T, proportion = T, prop_method = prop_method, vartype = "ci"),
          n_sample = unweighted(sum(!is.na(.data[[i]]))), # Ici on calcule les effectifs non NA (sum(!is.na(x))) car les NA ne sont pas supprimes au prealable des variables de vec_list_vars si na.vars = "rm"
          n_true_weighted = survey_total(.data[[i]], na.rm = T, vartype = "ci"),
          n_tot_weighted = survey_total(!is.na(.data[[i]]), vartype = "ci") # Ici on calcule les effectifs non NA (survey_total(!is.na(x))) car les NA ne sont pas supprimes au prealable des variables de vec_list_vars si na.vars = "rm"
        )

      tab <- rbind(tab, tab_i)
    }
  }
  # On calcule les moyennes/medianes
  if(type == "median" | type == "mean"){
    for (i in vec_list_vars) {
      tab_i <- data_W |>
        summarise(
          list_col = i,
          indice = if (type == "median") {
            survey_median(.data[[i]], na.rm = T, vartype = "ci")
          } else if (type == "mean") survey_mean(.data[[i]], na.rm = T, vartype = "ci"),
          n_sample = unweighted(sum(!is.na(.data[[i]]))), # Ici on calcule les effectifs non NA (sum(!is.na(x))) car les NA ne sont pas supprimes au prealable des variables de vec_list_vars si na.vars = "rm"
          n_weighted = survey_total(!is.na(.data[[i]]), vartype = "ci") # Ici on calcule les effectifs non NA (survey_total(!is.na(x))) car les NA ne sont pas supprimes au prealable des variables de vec_list_vars si na.vars = "rm"
        )

      tab <- rbind(tab, tab_i)
    }
  }
  tab <- tab |>
    ungroup()

  # On remplace list_vars par les labels list_vars_lab
  if (!is.null(list_vars_lab)) {

    # verifier que list_vars a une meme longueur que list_vars_lab
    # si non, message avec erreur...
    if (length(vec_list_vars) != length(list_vars_lab)) {
      message("The number of labels is not equal to the number of variables: labels (list_vars_lab) are not used")

    # si oui, on remplace dans tab$list_col le nom des variables par les labels definis par l'utilisateur dans list_vars_lab
    } else {

      for (i in seq_along(vec_list_vars)) {
        tab[["list_col"]][tab[["list_col"]] == vec_list_vars[i]] <- list_vars_lab[i]
      }
      # On definit l'ordre tel qu'il est entre par l'utilisateur (pour ggplot)
      tab$list_col <- factor(tab$list_col, levels = list_vars_lab)
    }
  }
  # On definit l'ordre tel qu'il est entre par l'utilisateur (pour ggplot)
  if (is.null(list_vars_lab)) {
    tab$list_col <- factor(tab$list_col, levels = vec_list_vars)
  }


  # 4. CREATION DU GRAPHIQUE --------------------

  # Si reorder = T, on cree un vecteur pour ordonner les levels
  if (reorder == T) {
    levels <- levels(reorder(
      tab[["list_col"]],
      tab[["indice"]],
      FUN = "median",
      decreasing = T
    ))
  }

  # Si reorder = F, l'ordre = celui rentre en input
  if (reorder == F) {
    if (length(vec_list_vars) == length(list_vars_lab)) {
      levels <- rev(list_vars_lab)
    } else {
      levels <- rev(vec_list_vars)
    }
  }

  # On cree la palette
  # Particulier a many_val() et make_surface() : possibilite d'une palette unicolore

  # Si :
  # * col est defini (= option ou utilisateur)
  # * + pal n'est pas indique par l'utilisateur (dans user.args car pal est defini par defaut, sinon col ne peut jamais etre applique !)
  # * + pal n'est pas dans les options (names(list_opt_fonctionr$fonctionr.options))
  # => Autrement dit, col est prioritaire seulement si pal n'est pas la.
  # OU
  # * col est indique par l'utilisateur
  # * + pal n'est pas indique par l'utilisateur
  # => Dans ce cas, col de l'utilisateur est prioritaire sur pal des options
  if((!is.null(col) & !"pal" %in% user.args & !"pal" %in% names(list_opt_fonctionr$fonctionr.options)) | ("col" %in% user.args & !"pal" %in% user.args)){
    # On cree la palette
    if(all(isColor(col)) == TRUE){
      palette <- rep(col, nlevels(tab[["list_col"]]))
    # Si col est pas valide => on met la couleur par defaut
    } else {
      if(all(isColor(col)) == FALSE){ # Warning uniquement si une couleur fausse a ete entree
        warning("col n'est pas valide : la couleur par defaut est utilisee")
      }
      col <- "indianred4" # Alors col == "indianred4"
      palette <- rep(col, nlevels(tab[["list_col"]]))
    }
  } else {
    palette <- create_palette(
      pal = pal,
      # /!\ NOTE : on met unique() car avec facet il y a les modalites en double !
      levels_palette = nlevels(tab[["list_col"]]),
      direction = direction,
      name_function = "many_val",
      desaturate = desaturate,
      lighten = lighten,
      darken = darken
    )
  }

  # On calcule la valeur max de la proportion, pour l'ecart des geom_text dans le ggplot
  max_ggplot <- max(tab$indice, na.rm = TRUE)

  # On definit le nom de l'indicateur (proportion, mediane ou moyenne) et l'echelle qui seront affichees dans le graphique ggplot
  if(type == "prop"){
    # Si l'echelle n'est pas definie par l'utilisateur => echelle = 100
    if(is.null(scale)){
      scale <- 100
    }
    # Si l'unite n'est pas definie par l'utilisateur => unite = "%"
    if(is.null(unit)){
      unit <- "%"
    }
    type_ggplot <- lang_prop
  }
  # Par contre, pour la mediane et la moyenne => echelle = 1 (equivalence avec la variable entree)
  if(type == "median"){
    if(is.null(scale)){
      scale <- 1
    }
    type_ggplot <- lang_median
  }
  if(type == "mean"){
    if(is.null(scale)){
      scale <- 1
    }
    type_ggplot <- lang_mean
  }

  # On cree le graphique

  graph <- tab |>
    ggplot(aes(
      x = list_col,
      y = indice,
      fill = list_col
      )
    ) +
    geom_bar(
      width = dodge,
      stat = "identity",
      position = "dodge"
    ) +
    theme_fonctionr(
      font = font,
      theme = theme,
      coef_font = coef_font
    ) +
    scale_fill_manual(
      values = palette
    ) +
    scale_x_discrete(
      labels = function(x) stringr::str_wrap(x, width = wrap_width_y),
      limits = levels
    )+
    labs(
      title = title,
      subtitle = subtitle,
      caption = stringr::str_wrap(caption, width = 100)
    ) +
    guides(
      fill="none"
    ) +
    coord_flip()

  # Ajouter les axes
  if(show_labs == TRUE){
      # X ---
      if(any(is.null(xlab), xlab != "")){
        graph <- graph +
          labs(y = ifelse(is.null(xlab),
                          paste0(type_ggplot, paste(vec_list_vars, collapse = ", ")),
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
            labs(x = NULL) # Il y avait "Indicateurs", mais c'est moche et inutile => possibilite de mettre ce qu'on veut
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
      geom_errorbar(aes(ymin = indice_low, ymax = indice_upp),
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
          y = (indice) + (0.01 * max_ggplot),
          label = paste0(stringr::str_replace(round(indice * scale,
                                                    digits = digits),
                                              "[.]",
                                              ","),
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


  # 5. RESULTATS --------------------

  # Dans un but de lisibilite, on renomme les indices "mean" ou "median" selon la fonction appelee
  if (type == "prop") {
    tab <- tab |>
      rename(prop = indice,
             prop_low = indice_low,
             prop_upp = indice_upp)
  }
  if (type == "median") {
    tab <- tab |>
      rename(median = indice,
             median_low = indice_low,
             median_upp = indice_upp)
  }
  if (type == "mean") {
    tab <- tab |>
      rename(mean = indice,
             mean_low = indice_low,
             mean_upp = indice_upp)
  }

  # On cree l'objet final
  res <- list()
  res$tab <- tab
  res$graph <- graph

  if (!is.null(export_path)) {
    # L'export en excel

    # Pour many_prop, test pas encore implemente => on cree un data.frame a la main
    test_stat_excel <- data.frame(Parameter = c("test.error"),
                                  Value = "Test pas encore implemente dans many_prop",
                                  row.names = NULL)

    # J'exporte les resultats en Excel
    export_excel(tab_excel = tab,
                 graph = graph,
                 test_stat_excel = test_stat_excel,
                 facet_null = quo_is_null(quo_facet),
                 export_path = export_path,
                 percent_fm = ifelse(type == "prop", TRUE, FALSE),
                 fgFill = "mediumseagreen",
                 bivariate = FALSE)
  }

  return(res)
}


#' @rdname many_val
#' @export
many_prop <- function(..., type = "prop") {
  many_val(..., type = type)
}


#' @rdname many_val
#' @export
many_median <- function(..., type = "median") {
  many_val(..., type = type)
}


#' @rdname many_val
#' @export
many_mean <- function(..., type = "mean") {
  many_val(..., type = type)
}
