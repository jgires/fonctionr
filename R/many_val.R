#' many_val
#'
#' Function to compute de proportions of a set of several binary variables. It can use complex survey data. It produces a table and a graphic.
#'
#' @param data A dataframe or an object from the survey package or an object from the srvyr package.
#' @param list_vars A vector containing names of the dummy variables on which to compute the proportions
#' @param type "mean" to compute means ; "median" to compute medians ; "prop" to compute proportions.
#' @param list_vars_lab Names of the variables
#' @param facet A variable defining the faceting group.
#' @param filter_exp An expression that filters the data, preserving the design.
#' @param ... All options possible in as_survey_design in srvyr package.
#' @param na.rm.facet TRUE if you want to remove observations with NA on the group variable or NA on the facet variable. FALSE if you want to create a group with the NA value for the group variable and a facet with the NA value for the facet variable. NA in the variables included in prop_exp are not affected in this argument. All the observation with a NA in the variables included in prop_exp are excluded.
#' @param na.vars The treatment of NA values in variables. "rm" removes NA only in each individual variable, "rm.all" removes every individual that has at least one NA in one variable.
#' @param prop_method Type of proportion method to use. See svyciprop in survey package for details. Default is the beta method.
#' @param reorder TRUE if you want to reorder the variables according to the proportion.
#' @param show_ci TRUE if you want to show the error bars on the graphic. FALSE if you do not want to show the error bars.
#' @param show_n TRUE if you want to show on the graphic the number of individuals in the sample in each group. FALSE if you do not want to show this number. Default is FALSE.
#' @param show_value TRUE if you want to show the proportion in each group on the graphic. FALSE if you do not want to show the proportion.
#' @param show_labs TRUE if you want to show axes, titles and caption labels. FALSE if you do not want to show any label on axes and titles. Default is TRUE.
#' @param scale Denominator of the proportion. Default is 100 to interprets numbers as percentages.
#' @param digits Numbers of digits showed on the values labels on the graphic. Default is 0.
#' @param unit Unit showed in the graphic. Default is percent.
#' @param dec Decimal mark shown on the graphic. Depends on lang: "," for fr and nl ; "." for en.
#' @param pal Color palette used on the graphic. The palettes from the packages MetBrewer, MoMAColors and PrettyCols are available.
#' @param direction Direction of the palette color. Default is 1. The opposite direction is -1.
#' @param desaturate Numeric specifying the amount of desaturation where 1 corresponds to complete desaturation, 0 to no desaturation, and values in between to partial desaturation.
#' @param lighten Numeric specifying the amount of lightening. Negative numbers cause darkening.
#' @param darken Numeric specifying the amount of lightening. Negative numbers cause lightening.
#' @param dodge Width of the bar, between 0 and 1.
#' @param font Font used in the graphic. See load_and_active_fonts() for available fonts.
#' @param wrap_width_y Number of characters before going to the line. Applies to the labels of the groups. Default is 25.
#' @param title Title of the graphic.
#' @param subtitle Subtitle of the graphic.
#' @param xlab X label on the graphic. As coord_flip() is used in the graphic, xlab refers to the x label on the graphic, after the coord_flip(), and not to the x variable in the data.
#' @param ylab Y label on the graphic. As coord_flip() is used in the graphic, xlab refers to the x label on the graphic, after the coord_flip(), and not to the x variable in the data.
#' @param lang The language of the indications on the chart. Possibilities: "fr", "nl", "en". Default is "fr".
#' @param caption Caption of the graphic.
#' @param theme Theme of the graphic. IWEPS adds y axis lines and ticks.
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
                    lang = "fr",
                    caption = NULL,
                    theme = NULL,
                    export_path = NULL){


  # 1. CHECKS DES ARGUMENTS --------------------

  # Check des arguments necessaires
  if(missing(type) == TRUE){
    stop("L'argument type doit etre rempli")
  }
  if((missing(data) | missing(list_vars)) == TRUE){
    stop("Les arguments data et list_vars doivent etre remplis")
  }

  # Check des autres arguments
  check_arg(
    arg = list(
      type = type,
      na.vars = na.vars,
      prop_method = prop_method,
      unit = unit,
      dec = dec,
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
      wrap_width_y = wrap_width_y
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

  message("Variable(s) entrees : ", paste(vec_list_vars, collapse = ", "))

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
    data_W <- data_W |>
      filter({{ filter_exp }})
  }
  # On supprime les NA sur la variable de facet si non-NULL
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
      message("Le nombre de labels n'est pas egal au nombre de variables")

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

  # On cree la palette avec le package met.brewer
  if(pal %in% names(MetBrewer::MetPalettes)){
    palette <- as.character(MetBrewer::met.brewer(name = pal, n = nlevels(tab[["list_col"]]), type = "continuous", direction = direction))

  # On cree avec le package MoMAColors
  } else if(pal %in% names(MoMAColors::MoMAPalettes)){
    palette <- as.character(MoMAColors::moma.colors(palette_name = pal, n = nlevels(tab[["list_col"]]), type = "continuous", direction = direction))

  # On cree la palette avec le package PrettyCols
  } else if(pal %in% names(PrettyCols::PrettyColsPalettes)){
    palette <- as.character(PrettyCols::prettycols(palette = pal, n = nlevels(tab[["list_col"]]), type = "continuous", direction = direction))

  # On cree la palette avec la fonction interne official_pal
  } else if(pal %in% official_pal(list_pal_names = T)){
    palette <- as.character(official_pal(inst = pal, n = nlevels(tab[["list_col"]]), direction = direction))

  # On cree une palette unicolore
  } else if(all(isColor(pal)) == TRUE){
    palette <- rep(pal, nlevels(tab[["list_col"]]))

  } else { # Si la couleur/palette n'est pas valide => on met la palette par defaut
    palette <- as.character(MetBrewer::met.brewer(name = "Egypt", n = nlevels(tab[["list_col"]]), type = "continuous", direction = 1))
    warning("La couleur ou palette indiquee dans pal n'existe pas : la palette par defaut est utilisee")
  }

  # Pour modifier la palette (desaturer, eclaircir, foncer)
  if(desaturate != 0){
    palette <- colorspace::desaturate(palette, desaturate)
  }
  if(lighten != 0){
    palette <- colorspace::lighten(palette, lighten)
  }
  if(darken != 0){
    palette <- colorspace::darken(palette, darken)
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
    )) +
    geom_bar(
      width = dodge,
      stat = "identity",
      position = "dodge"
    ) +
    theme_fonctionr(font = font,
                    theme = theme) +
    scale_fill_manual(
      values = palette
    ) +
    scale_x_discrete(
      labels = function(x) stringr::str_wrap(x, width = wrap_width_y),
      limits = levels
    )+
    labs(title = title,
         subtitle = subtitle,
         caption = stringr::str_wrap(caption, width = 100)
    ) +
    guides(fill="none") +
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
