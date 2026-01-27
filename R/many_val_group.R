#' many_val_group
#'
#' Function to compare de proportions/means/medians of a set of several binary/continuous variables between different groups. It can use complex survey data. It produces a table and a graphic.
#'
#' @param data A dataframe or an object from the survey package or an object from the srvyr package.
#' @param group A variable defining groups to be compared.
#' @param list_vars A vector containing names of the dummy variables on which to compute the proportions
#' @param type "mean" to compute means by group ; "median" to compute medians by group ; "prop" to compute proportions by group.
#' @param list_vars_lab Names of the variables
#' @param facet A variable defining the faceting group.
#' @param filter_exp An expression that filters the data, preserving the design.
#' @param ... All options possible in as_survey_design in srvyr package.
#' @param na.rm.group TRUE if you want to remove observations with NA on the group variable or NA on the facet variable. FALSE if you want to create a group with the NA value for the group variable and a facet with the NA value for the facet variable. NA in the variables included in prop_exp are not affected in this argument. All the observation with a NA in the variables included in prop_exp are excluded.
#' @param na.rm.facet TRUE if you want to remove observations with NA on the group variable or NA on the facet variable. FALSE if you want to create a group with the NA value for the group variable and a facet with the NA value for the facet variable. NA in the variables included in prop_exp are not affected in this argument. All the observation with a NA in the variables included in prop_exp are excluded.
#' @param na.vars The treatment of NA values in variables. "rm" removes NA only in each individual variable, "rm.all" removes every individual that has at least one NA in one variable.
#' @param total TRUE if you want to calculate a total, FALSE if you don't. The default is TRUE
#' @param prop_method Type of proportion method to use. See svyciprop in survey package for details. Default is the beta method.
#' @param position Position adjustment for the ggplot. Default is "dodge". Other possible values are "flip" and "stack". "dodge" means that groups are on the y axe and variables are in differents colors, "flip" means that variables are on the y axe and groups are in differents colors, and "stack" means that groups are on the y axe and variables are stacking with differents colors. The latter is usefull when the variables are component of a broader sum varible (e.g. different sources of income).
#' @param show_ci TRUE if you want to show the error bars on the graphic. FALSE if you do not want to show the error bars.
#' @param show_n TRUE if you want to show on the graphic the number of individuals in the sample in each group. FALSE if you do not want to show this number. Default is FALSE.
#' @param show_value TRUE if you want to show the proportion in each group on the graphic. FALSE if you do not want to show the proportion.
#' @param show_labs TRUE if you want to show axes, titles and caption labels. FALSE if you do not want to show any label on axes and titles. Default is TRUE.
#' @param total_name Name of the total bar on the graphic. Default is Total.
#' @param scale Denominator of the proportion. Default is 100 to interprets numbers as percentages.
#' @param digits Numbers of digits showed on the values labels on the graphic. Default is 0.
#' @param unit Unit showed in the graphic. Default is percent.
#' @param dec Decimal mark shown on the graphic. Depends on lang: "," for fr and nl ; "." for en.
#' @param pal Color palette used on the graphic. Palettes from fonctionr and the MetBrewer and PrettyCols packages are available.
#' @param direction Direction of the palette color. Default is 1. The opposite direction is -1.
#' @param desaturate Numeric specifying the amount of desaturation where 1 corresponds to complete desaturation, 0 to no desaturation, and values in between to partial desaturation.
#' @param lighten Numeric specifying the amount of lightening. Negative numbers cause darkening.
#' @param darken Numeric specifying the amount of lightening. Negative numbers cause lightening.
#' @param dodge Width of the bar, between 0 and 1.
#' @param font Font used in the graphic. See load_and_active_fonts() for available fonts.
#' @param wrap_width_y Number of characters before going to the line. Applies to the labels of the groups. Default is 25.
#' @param wrap_width_leg Number of characters before going to the line. Applies to the labels of the legend. Default is 25.
#' @param legend_ncol Number maximum of colomn in the legend. Default is 4.
#' @param title Title of the graphic.
#' @param subtitle Subtitle of the graphic.
#' @param xlab X label on the graphic. As coord_flip() is used in the graphic, xlab refers to the x label on the graphic, after the coord_flip(), and not to the x variable in the data.
#' @param ylab Y label on the graphic. As coord_flip() is used in the graphic, xlab refers to the x label on the graphic, after the coord_flip(), and not to the x variable in the data.
#' @param legend_lab Legend (fill) label on the graphic.
#' @param lang The language of the indications on the chart. Possibilities: "fr", "nl", "en". Default is "fr".
#' @param caption Caption of the graphic.
#' @param theme Theme of the graphic. IWEPS adds y axis lines and ticks.
#' @param coef_font A multiplier factor for font size. Default is 1. Usefull when exporting the plot for a publication (for instance with a Quarto document).
#' @param export_path Path to export the results in an xlsx file. The file includes two sheets : the table and the graphic.
#'
#' @return A list that contains a table and a graphic
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
#' eusilc_many_mean_group <- many_mean_group(
#' eusilc,
#' group = rb090,
#' list_vars = c(py010n,py050n,py090n,py100n),
#' list_vars_lab = c("Wage","Self-employement income","unemployement benefit","pension"),
#' strata = db040,
#' ids = db030,
#' weight = rb050,
#' title = "Average incomes according to gender",
#' subtitle = "Example with austrian SILC data from 'laeken' package"
#' )
#'
#' # Results in graph form
#' eusilc_many_mean_group$graph
#'
#' # Results in table format
#' eusilc_many_mean_group$tab
#'
many_val_group = function(data,
                          group,
                          list_vars,
                          type,
                          list_vars_lab = NULL,
                          facet = NULL,
                          filter_exp = NULL,
                          ...,
                          na.rm.group = T,
                          na.rm.facet = T,
                          na.vars = "rm",
                          total = TRUE,
                          prop_method = "beta",
                          position = "dodge",
                          show_ci = T,
                          show_n = FALSE,
                          show_value = TRUE,
                          show_labs = TRUE,
                          total_name = NULL,
                          scale = NULL,
                          digits = 0,
                          unit = NULL,
                          dec = NULL,
                          pal = "Egypt",
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
                          lang = "fr",
                          caption = NULL,
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
    stop("L'argument type doit etre rempli")
  }
  if((missing(data) | missing(group) | missing(list_vars)) == TRUE){
    stop("Les arguments data, group, list_vars doivent etre remplis")
  }

  # Check des autres arguments
  check_arg(
    arg = list(
      type = type,
      na.vars = na.vars,
      prop_method = prop_method,
      position = position,
      total_name = total_name,
      unit = unit,
      dec = dec,
      # pal = pal, # Je supprime pour pouvoir generer automatiquement des palettes dans l'argument avec des fonctions
      font = font,
      title = title,
      subtitle = subtitle,
      xlab = xlab,
      ylab = ylab,
      legend_lab = legend_lab,
      lang = lang,
      caption = caption,
      theme = theme
    ),
    type = "character"
  )
  check_arg(
    arg = list(
      list_vars_lab = list_vars_lab),
    short = F,
    type = "character"
  )
  check_arg(
    arg = list(
      na.rm.group = na.rm.group,
      na.rm.facet = na.rm.facet,
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
  match.arg(type, choices = c("mean", "median", "prop"))
  match.arg(position, choices = c("dodge", "stack", "flip"))
  match.arg(na.vars, choices = c("rm", "rm.all"))
  lang <- tolower(lang)
  match.arg(lang, choices = c("fr", "nl", "en"))

  if(total == TRUE & position == "flip"){
    total <- FALSE
    warning("La position 'flip' n'accepte pas de total : celui-ci est donc desactive")
  }

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

  message("Variable(s) entrees : ", paste(vec_list_vars, collapse = ", "))

  # On procede d'abord a un test : il faut que toutes les variables entrees soient presentes dans data => sinon stop et erreur
  # On cree un vecteur string qui contient toutes les variables entrees

  # On ajoute group aux variables entrees dans list_vars (detectees precedemment)
  vec_group <- c(group = as.character(substitute(group)))
  vars_input_char <- c(vec_list_vars, vec_group)
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
    lang_prop <- "Proportion : "
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
    lang_prop <- "Aandeel: "
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
    data_W <- data_W |>
      filter({{ filter_exp }})
  }
  # On supprime les NA sur le groupe si na.rm.group = T
  if (na.rm.group == T) {
    data_W <- data_W |>
      filter(!is.na({{ group }}))
  }
  # idem sur la variable de facet si non-NULL
  if (na.rm.facet == T) {
    if(!quo_is_null(quo_facet)){
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
    message(paste0(before[[1]] - after[[1]]), " lignes supprimees avec valeur(s) manquante(s) pour le(s) variable(s) entrees")
  }

  # On convertit la variable de groupe en facteur si pas facteur
  data_W <- data_W |>
    mutate(
      "{{ group }}" := droplevels(as.factor({{ group }})) # droplevels pour eviter qu'un level soit encode alors qu'il n'a pas d'effectifs (pb pour le test khi2)
    )
  # On convertit egalement la variable de facet en facteur si facet non-NULL
  if(!quo_is_null(quo_facet)){
    data_W <- data_W |>
      mutate(
        "{{ facet }}" := droplevels(as.factor({{ facet }}))) # droplevels pour eviter qu'un level soit encode alors qu'il n'a pas d'effectifs (pb pour le test khi2)
  }


  # 3. CALCUL DES INDICATEURS --------------------

  # Si facet
  if (!quo_is_null(quo_facet)) {
    data_W <- data_W |>
      group_by({{ facet }})
  }
  # Group (dans tous les cas)
  data_W <- data_W |>
    group_by({{ group }}, .add = TRUE)

  tab <- tibble()

  # Si total et position pas egale a flip (car pas de total dans ce cas => changer par la suite ?)
  if(total == TRUE) {

    # On calcule les proportions par groupe
    if(type == "prop"){
      for(i in vec_list_vars) {
        tab_i <- data_W |>
          cascade(
            list_col = i,
            indice = survey_mean(.data[[i]], na.rm = T, proportion = T, prop_method = prop_method, vartype = "ci"),
            n_sample = unweighted(sum(!is.na(.data[[i]]))), # Ici on calcule les effectifs non NA (sum(!is.na(x))) car les NA ne sont pas supprimes au prealable des variables de vec_list_vars si na.vars = "rm"
            n_true_weighted = survey_total(.data[[i]] == 1, na.rm = T, vartype = "ci"),
            n_tot_weighted = survey_total(!is.na(.data[[i]]), vartype = "ci"), # Ici on calcule les effectifs non NA (survey_total(!is.na(x))) car les NA ne sont pas supprimes au prealable des variables de vec_list_vars si na.vars = "rm"
            .fill = total_name # Le total
          )

        tab <- rbind(tab, tab_i)
      }
    }
    # On calcule les moyennes/medianes par groupe
    if(type == "median" | type == "mean"){
      for(i in vec_list_vars) {
        tab_i <- data_W |>
          cascade(
            list_col = i,
            indice = if (type == "median") {
              survey_median(.data[[i]], na.rm = T, vartype = "ci")
            } else if (type == "mean") survey_mean(.data[[i]], na.rm = T, vartype = "ci"),
            n_sample = unweighted(sum(!is.na(.data[[i]]))), # Ici on calcule les effectifs non NA (sum(!is.na(x))) car les NA ne sont pas supprimes au prealable des variables de vec_list_vars si na.vars = "rm"
            n_weighted = survey_total(!is.na(.data[[i]]), vartype = "ci"), # Ici on calcule les effectifs non NA (survey_total(!is.na(x))) car les NA ne sont pas supprimes au prealable des variables de vec_list_vars si na.vars = "rm"
            .fill = total_name # Le total
          )

        tab <- rbind(tab, tab_i)
      }
    }

    # On supprime la facet qui est le total => pas utile
    if(!quo_is_null(quo_facet)) {
      tab <- tab |>
        filter({{ facet }} != total_name | is.na({{ facet }})) |>
        mutate("{{ facet }}" := droplevels(as.factor({{ facet }}))) # Pour enlever le level "Total"
    }
  }

  # Si pas total et position egale a flip (car pas de total dans ce cas => changer par la suite ?)
  if(total != TRUE) {

    # On calcule les proportions par groupe
    if(type == "prop"){
      for(i in vec_list_vars) {
        tab_i <- data_W |>
          summarise(
            list_col = i,
            indice = survey_mean(.data[[i]], na.rm = T, proportion = T, prop_method = prop_method, vartype = "ci"),
            n_sample = unweighted(sum(!is.na(.data[[i]]))), # Ici on calcule les effectifs non NA (sum(!is.na(x))) car les NA ne sont pas supprimes au prealable des variables de vec_list_vars si na.vars = "rm"
            n_true_weighted = survey_total(.data[[i]] == 1, na.rm = T, vartype = "ci"),
            n_tot_weighted = survey_total(!is.na(.data[[i]]), vartype = "ci"), # Ici on calcule les effectifs non NA (survey_total(!is.na(x))) car les NA ne sont pas supprimes au prealable des variables de vec_list_vars si na.vars = "rm"
          )

        tab <- rbind(tab, tab_i)
      }
    }
    # On calcule les moyennes/medianes par groupe
    if(type == "median" | type == "mean"){
      for(i in vec_list_vars) {
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
  }

  if(position == "stack") {
    # Procedure pour indiquer quelles valeurs afficher dans les cellules

    # Si facet
    if (!quo_is_null(quo_facet)) {
      tab <- tab |>
        group_by({{ facet }})
    }
    # Group (dans tous les cas)
    tab <- tab |>
      group_by({{ group }}, .add = TRUE)

    # L'idee est de ne pas afficher les valeurs lorsque la barre est inferieure a 10% de la barre la plus longue de tout le graphique
    tab <- tab |>
      mutate(longest = sum(indice, na.rm = TRUE)) |>
      ungroup() |>
      mutate(longest = max(longest),
             show_value_stack = ifelse(indice/longest > .1, TRUE, FALSE))
  }

  tab <- tab |>
    ungroup()

  # On remplace list_vars par les labels list_vars_lab
  if (!is.null(list_vars_lab)) {

    # verifier que list_vars a une meme longueur que list_vars_lab
    # si non, message avec erreur...
    if (length(vec_list_vars) != length(list_vars_lab)) {
      warning("Le nombre de labels n'est pas egal au nombre de variables : les labels ne sont pas pris en compte")

      # On cree un facteur avec l'ordre tel qu'il est entre par l'utilisateur (pour ggplot)
      tab$list_col <- factor(tab$list_col, levels = rev(vec_list_vars))

      # si oui, on remplace dans tab$list_col le nom des variables par les labels definis par l'utilisateur dans list_vars_lab
    } else {

      for (i in seq_along(vec_list_vars)) {
        tab[["list_col"]][tab[["list_col"]] == vec_list_vars[i]] <- list_vars_lab[i]
      }
      # On definit l'ordre tel qu'il est entre par l'utilisateur (pour ggplot)
      tab$list_col <- factor(tab$list_col, levels = rev(list_vars_lab))
    }
  }
  # On cree un facteur avec l'ordre tel qu'il est entre par l'utilisateur (pour ggplot)
  if (is.null(list_vars_lab)) {
    tab$list_col <- factor(tab$list_col, levels = rev(vec_list_vars))
  }


  # 4. CREATION DU GRAPHIQUE --------------------

  # On cree la palette

  # Le nombre de couleurs de la palette selon que la position est flip (couleurs = groupes) ou non (couleurs = indicateurs)
  if(position == "flip"){
    column_fill <- deparse(substitute(group))
  } else {
    column_fill <- "list_col"
  }

  palette <- create_palette(
    pal = pal,
    # /!\ NOTE : on met unique() car avec facet il y a les modalites en double !
    levels_palette = nlevels(tab[[column_fill]]),
    direction = -1*direction,
    name_function = "make_surface",
    desaturate = desaturate,
    lighten = lighten,
    darken = darken
  )

  # On calcule la valeur max de la proportion, pour l'ecart des geom_text dans le ggplot
  max_ggplot <- max(tab$indice, na.rm = TRUE)

  # On cree un vecteur pour ordonner les levels de group pour mettre NA en premier (= en dernier sur le graphique ggplot)
  levels <- c(
    total_name,
    NA,
    rev(
      levels(
        tab[[deparse(substitute(group))]]
      )[levels(
        tab[[deparse(substitute(group))]]
      ) != total_name]
    )
  )

  # Dans le vecteur qui ordonne les levels, on a mis un NA => Or parfois pas de missing pour le groupe, meme si na.rm.group = F !
  # On les supprime donc ssi na.rm.group = F et pas de missing sur la variable de groupe **OU** na.rm.group = T
  if ((na.rm.group == F & sum(is.na(tab[[deparse(substitute(group))]])) == 0) | na.rm.group == T)  {
    levels <- levels[!is.na(levels)]
  }
  # Pour enlever le level "Total" si total == F
  if(total == FALSE) {
    levels <- levels[levels != total_name]
  }

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

  # NOTE : la position = flip n'utilise pas levels et n'a pas de total
  graph <- tab |>
    mutate("{{ group }}" := forcats::fct_rev({{ group }})) |>
    ggplot(aes(
      x = if(position == "dodge"|position == "stack") {{ group }} else list_col,
      y = indice,
      fill = if(position == "dodge"|position == "stack") list_col else {{ group }}
    )) +
    geom_bar(
      width = dodge,
      stat = "identity",
      position = if(position == "stack") "stack" else "dodge"
    ) +
    theme_fonctionr(
      font = font,
      theme = theme,
      display = "ggtext",
      coef_font = coef_font
    ) +
    theme(
      legend.position = "bottom"
    ) +
    scale_fill_manual(
      values = palette,
      labels = function(x) stringr::str_wrap(x, width = wrap_width_leg),
      na.value = "grey"
    ) +
    scale_x_discrete(
      # fonction interne relabel_ggtext() pour compatibilite avec ggtext
      labels = ~relabel_ggtext(x = ., wrap_width = wrap_width_y, total_name = total_name),
      limits = if(position == "stack"|position == "dodge") levels else NULL
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      caption = stringr::str_wrap(caption, width = 100)
    ) +
    guides(
      fill = guide_legend(ncol = legend_ncol, reverse = TRUE)
    ) +
    coord_flip()

  # Autre design pour la barre du total (si total = T)
  if (total == TRUE) {
  graph <- graph +
    # annotate("rect", xmin = .5, xmax = 1.5, ymin = -Inf, ymax = Inf, fill = "black", alpha = 0.05) +
    # geom_vline(xintercept = 1.5, linetype = "dotted", color = "grey50", linewidth = .1) +
    geom_bar(
      aes(
        x = {{ group }},
        y = ifelse({{ group }} == total_name, indice, NA),
        group = list_col,
        color = list_col
      ),
      fill = "white",
      linewidth = .8,
      alpha = .8,
      width = dodge,
      stat = "identity",
      position = if(position == "stack") "stack" else "dodge"
    ) +
    scale_colour_manual(
      values = palette,
      guide = "none"
    )
    if (show_value == TRUE) { # Peut-etre ici une redondance => voir si simplification possible ?
      graph <- graph +
        geom_text(
          aes(
            # Pas flip dans les conditions sur position, car le total n'existe pas en position = flip
            y = if (position == "dodge") (ifelse({{ group }} == total_name, indice, NA)) + (0.01 * max_ggplot) else ifelse({{ group }} == total_name, indice, NA),
            label = if (position == "stack") {
              ifelse(show_value_stack == TRUE, paste0(
                stringr::str_replace(
                  round(indice * scale,
                    digits = digits
                  ),
                  "[.]",
                  dec
                ),
                unit
              ), NA)
            } else {
              paste0(
                stringr::str_replace(
                  round(indice * scale,
                    digits = digits
                  ),
                  "[.]",
                  dec
                ),
                unit
              )
            },
            family = font
          ),
          size = coef_font * fonctionr_font_size(type = "little"),
          vjust = if (position == "dodge") ifelse(show_ci == T, -0.25, 0.5) else 0.4,
          hjust = if (position == "dodge") "left" else "center",
          color = "grey10",
          fontface = "bold",
          alpha = 0.9,
          # position = position_stack(vjust = .5))
          position = if (position == "dodge") position_dodge(width = dodge) else position_stack(vjust = .5)
        )
    }
}

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
      # ici bricolage car ggplot affiche la condition ecrite pour l'axe x => on remet le nom de la variable
      if(is.null(ylab) & position == "flip"){
        graph <- graph +
          labs(x = NULL)
      }
      if(is.null(ylab) & (position != "flip")){
        graph <- graph +
          labs(x = deparse(substitute(group)))
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

  # LEGEND ---
  if(all(!is.null(legend_lab), legend_lab != "")){
    graph <- graph +
      labs(fill = stringr::str_wrap(legend_lab, wrap_width_leg))
  }
  if(all(!is.null(legend_lab), legend_lab == "")){
    graph <- graph +
      labs(fill = NULL)
  }
  if(is.null(legend_lab)){
    graph <- graph +
      labs(fill = NULL)
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
  if (show_ci == T & position != "stack") {
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
          y = if (position == "dodge"|position == "flip") (ifelse({{ group }} != total_name|is.na({{ group }}), indice, NA)) + (0.01 * max_ggplot) else ifelse({{ group }} != total_name|is.na({{ group }}), indice, NA),
          label = if (position == "stack") {
            ifelse(show_value_stack == TRUE, paste0(
              stringr::str_replace(
                round(indice * scale,
                      digits = digits
                ),
                "[.]",
                dec
              ),
              unit
            ), NA)
          } else {
            paste0(
              stringr::str_replace(
                round(indice * scale,
                      digits = digits
                ),
                "[.]",
                dec
              ),
              unit
            )
          },
          family = font),
        size = coef_font * fonctionr_font_size(type = "little"),
        vjust = if (position == "dodge"|position == "flip") ifelse(show_ci == T, -0.25, 0.5) else 0.4,
        hjust = if (position == "dodge"|position == "flip") "left" else "center",
        color = if (position == "dodge"|position == "flip") "black" else "white",
        alpha = 0.9,
        # position = position_stack(vjust = .5))
        position = if (position == "dodge"|position == "flip") position_dodge(width = dodge) else position_stack(vjust = .5)
      )
  }

  # Ajouter le nombre d'individus au besoin
  if (show_n == TRUE) {
    graph <- graph +
      geom_text(
        aes(
          y = if (position == "dodge"|position == "flip") 0 + (0.01 * max_ggplot) else indice, # Pour ajouter des labels avec les effectifs en dessous des barres
          label = paste0("n=", n_sample),
          family = font),
        size = coef_font * fonctionr_font_size(type = "little"),
        alpha = 0.7,
        hjust = 0, # Justifie a droite
        vjust = 0.4,
        position = if (position == "dodge"|position == "flip") position_dodge(width = dodge) else position_stack(vjust = 0)
      )
  }


  # 5. RESULTATS --------------------

  if(position == "stack") {
    tab <- tab |>
      select(-longest, -show_value_stack)
  }

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

    # Pour many_val_group, test pas encore implemente => on cree un data.frame a la main
    test_stat_excel <- data.frame(Parameter = c("test.error"),
                                    Value = "Test pas encore implemente dans many_val_group",
                                    row.names = NULL)

    # J'exporte les resultats en Excel
    export_excel(tab_excel = tab,
                 graph = graph,
                 test_stat_excel = test_stat_excel,
                 facet_null = quo_is_null(quo_facet),
                 export_path = export_path,
                 percent_fm = ifelse(type == "prop", TRUE, FALSE),
                 fgFill = "mediumvioletred",
                 bivariate = TRUE)
  }

  return(res)
}


#' @rdname many_val_group
#' @export
many_prop_group <- function(..., type = "prop") {
  many_val_group(..., type = type)
}


#' @rdname many_val_group
#' @export
many_median_group <- function(..., type = "median") {
  many_val_group(..., type = type)
}


#' @rdname many_val_group
#' @export
many_mean_group <- function(..., type = "mean") {
  many_val_group(..., type = type)
}
