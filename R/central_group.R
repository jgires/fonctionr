#' central_group
#'
#' Function to compare means or medians in different groups from complex survey data. It produces a table, a graphic and a statistical test.
#'
#' @name central_group
#'
#' @param data A dataframe or an object from the survey package or an object from the srvyr package.
#' @param group A variable defining groups to be compared.
#' @param quanti_exp An expression that define the variable from which the mean/median is computed.
#' @param type "mean" to compute mean by group ; "median" to compute median by group.
#' @param group.fill A variable defining a second variable of groups to be compared.
#' @param facet A variable defining the faceting group.
#' @param filter_exp An expression that filters the data, preserving the design.
#' @param ... All options possible in as_survey_design in srvyr package.
#' @param na.rm.group TRUE if you want to remove observations with NA on the group variable or NA on the facet variable, if applicable. FALSE if you want to create a group with the NA value for the group variable and a facet with the NA value for the facet variable. NA in the variables included in quanti_exp are not affected in this argument. All the observation with a NA in the variables included in quanti_exp are always excluded. Default is TRUE.
#' @param na.rm.facet TRUE if you want to remove observations with NA on the group variable or NA on the facet variable. FALSE if you want to create a group with the NA value for the group variable and a facet with the NA value for the facet variable. NA in the variables included in prop_exp are not affected in this argument. All the observation with a NA in the variables included in prop_exp are excluded.
#' @param total TRUE if you want to calculate a total, FALSE if you don't. The default is TRUE
#' @param reorder TRUE if you want to reorder the groups according to the mean/median. NA value, in case if na.rm.group = FALSE, is not included in the reorder.
#' @param show_ci TRUE if you want to show the error bars on the graphic. FALSE if you do not want to show the error bars. Default is TRUE.
#' @param show_n TRUE if you want to show on the graphic the number of individuals in the sample in each group. FALSE if you do not want to show this number. Default is FALSE.
#' @param show_value TRUE if you want to show the mean/median of each group on the graphic. FALSE if you do not want to show the mean/median. Default is TRUE.
#' @param show_labs TRUE if you want to show axes, titles and caption labels. FALSE if you do not want to show any label on axes and titles. Default is TRUE.
#' @param total_name Name of the total bar on the graphic. Default is Total.
#' @param digits Numbers of digits showed on the value labels on the graphic. Default is 0.
#' @param unit Unit showed on the graphic. Default is no unit.
#' @param dec Decimal mark shown on the graphic. Depends on lang: "," for fr and nl ; "." for en.
#' @param pal If group.fill is empty, pal must be a vector containing a single color to define the color of the bars. If a variable is specified in group.fill, pal is the color palette used on the graph to differentiate its different modalities. Palettes from the MetBrewer, MoMAColors and PrettyCols packages are available. The NA bar, if na.rm.group = FALSE, and the total bar are always in gray.
#' @param direction Direction of the palette color. Default is 1. The opposite direction is -1.
#' @param desaturate Numeric specifying the amount of desaturation where 1 corresponds to complete desaturation, 0 to no desaturation, and values in between to partial desaturation.
#' @param lighten Numeric specifying the amount of lightening. Negative numbers cause darkening.
#' @param darken Numeric specifying the amount of lightening. Negative numbers cause lightening.
#' @param dodge Width of the bar, between 0 and 1.Default is 0.9.
#' @param font Font used in the graphic. See load_and_active_fonts() for available fonts.
#' @param wrap_width_y Number of characters before going to the line in the labels of the groups. Default is 25.
#' @param wrap_width_leg Number of characters before going to the line for the labels of the categories of group.fill. Default is 25.
#' @param legend_ncol Number of colomns in the legend. Default is 4.
#' @param title Title of the graphic.
#' @param subtitle Subtitle of the graphic.
#' @param xlab X label on the graphic. As coord_flip() is used in the graphic, xlab refers to the X label on the graphic, after the coord_flip(), and not to the x variable in the data. If xlab = NULL, X label on the graphic will be "Moyenne : " + quanti_exp or "Medianne : " + quanti_exp. To show no X label, use xlab = "".
#' @param ylab Y label on the graphic. As coord_flip() is used in the graphic, ylab refers to the Y label on the graphic, after the coord_flip(), and not to the y variable in the data. If ylab = NULL, Y label on the graphic will be group. To show no Y label, use ylab = "".
#' @param legend_lab Legend (fill) label on the graphic. If legend_lab = NULL, legend label on the graphic will be group.fill. To show no legend label, use legend_lab = "".
#' @param caption Caption of the graphic.
#' @param lang The language of the indications on the chart. Possibilities: "fr", "nl", "en". Default is "fr".
#' @param theme Theme of the graphic. IWEPS adds y axis lines and ticks.
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
                          na.rm.group = T,
                          na.rm.facet = T,
                          total = TRUE,
                          reorder = F,
                          show_ci = T,
                          show_n = FALSE,
                          show_value = TRUE,
                          show_labs = TRUE,
                          total_name = NULL,
                          digits = 0,
                          unit = "",
                          dec = NULL,
                          pal = NULL,
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
                          theme = NULL,
                          export_path = NULL) {

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
      legend_ncol = legend_ncol
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
    data_W <- data_W |>
      filter({{ filter_exp }})
  }
  # On supprime les NA sur group + group.fill si na.rm.group = T
  if (na.rm.group == T) {
    data_W <- data_W |>
      filter(!is.na({{ group }}))

    if(!quo_is_null(quo_group.fill)){
      data_W <- data_W |>
        filter(!is.na({{ group.fill }}))
    }
  }
  # idem sur la variable de facet si non-NULL
  if (na.rm.facet == T) {
    if(!quo_is_null(quo_facet)){
      data_W <- data_W |>
        filter(!is.na({{ facet }}))
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
  message(paste0(before[[1]] - after[[1]]), " lignes supprimees avec valeur(s) manquante(s) pour le(s) variable(s) de quanti_exp")

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

  # La palette est differente selon qu'il y a group.fill (1 palette) ou non (1 couleur)
  if(quo_is_null(quo_group.fill)) {
    # On cree la palette : avec le total au debut (en gris fonce) puis x fois le pal selon le nombre de levels - 1 (le total etant deja un niveau)
    # Si couleur introduite par l'utilisateur
    if(!is.null(pal) & all(isColor(pal)) == TRUE){
      palette <- c(rep(pal, nlevels(tab[[deparse(substitute(group))]]) - 1), "grey40")
    } else { # Si pal est NULL ou n'est pas valide => on met la couleur par defaut
      if(!is.null(pal) & all(isColor(pal)) == FALSE){ # Warning uniquement si une couleur fausse a ete entree
        warning("La couleur indiquee dans pal n'existe pas : la couleur par defaut est utilisee")
      }
      if(type == "mean"){
        pal <- "deeppink3"
      }
      if(type == "median"){
        pal <- "mediumorchid3"
      }
      palette <- c(rep(pal, nlevels(tab[[deparse(substitute(group))]]) - 1), "grey40")
    }
    # Si pas de total, alors pas de gris mais tout en pal (indiquee par l'utilisateur ou par defaut si n'existe pas)
    if(total == FALSE) {
      palette[palette == "grey40"] <- pal
    }
  }

  if(!quo_is_null(quo_group.fill)) {
    # On cree la palette avec le package MetBrewer
    if(!is.null(pal) & all(pal %in% names(MetBrewer::MetPalettes))){
      palette <- as.character(MetBrewer::met.brewer(name = pal, n = nlevels(as.factor(tab[[deparse(substitute(group.fill))]])), type = "continuous", direction = direction))

      # On cree la palette avec le package MoMAColors
    } else if(!is.null(pal) & all(pal %in% names(MoMAColors::MoMAPalettes))){
      palette <- as.character(MoMAColors::moma.colors(palette_name = pal, n = nlevels(as.factor(tab[[deparse(substitute(group.fill))]])), type = "continuous", direction = direction))

      # On cree la palette avecle package PrettyCols
    } else if(!is.null(pal) & all(pal %in% names(PrettyCols::PrettyColsPalettes))){
      palette <- as.character(PrettyCols::prettycols(palette = pal, n = nlevels(as.factor(tab[[deparse(substitute(group.fill))]])), type = "continuous", direction = direction))

      # On cree la palette avec la fonction interne official_pal()
    } else if(!is.null(pal) & all(pal %in% official_pal(list_pal_names = T))){
      palette <- as.character(official_pal(inst = pal, n = nlevels(as.factor(tab[[deparse(substitute(group.fill))]])), direction = direction))

    } else if(is.null(pal)) {
      palette <- as.character(PrettyCols::prettycols(palette = "Peppers", n = nlevels(as.factor(tab[[deparse(substitute(group.fill))]])), type = "continuous", direction = direction))

    } else {
      palette <- as.character(PrettyCols::prettycols(palette = "Peppers", n = nlevels(as.factor(tab[[deparse(substitute(group.fill))]])), type = "continuous", direction = direction))
      warning("La palette indiquee dans pal n'existe pas : la palette par defaut est utilisee")
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

  # GGTEXT start ---------------

  # On transforme les choses pour rendre compatible avec ggtext, pour mettre le total en gras et le NA en "NA" (string)
  levels[is.na(levels)] <- "NA"

  if(total == TRUE) {
    levels[levels == total_name] <- paste0("**", total_name, "**")

    # Il faut changer les modalites de la variable de groupe (total avec ** et "NA" en string)
    graph <- tab |>
      mutate(
        "{{ group }}" := case_when(
          {{ group }} == total_name ~ paste0("**", total_name, "**"),
          is.na({{ group }}) ~ "NA",
          .default = {{ group }}
        )
      )

    # On renomme le total (n'est plus utilise que pour le graphique)
    total_name <- paste0("**", total_name, "**")

  }

  if(total == FALSE) {

    # Il faut changer les modalites de la variable de groupe ("NA" en string)
    graph <- tab |>
      mutate(
        "{{ group }}" := case_when(
          is.na({{ group }}) ~ "NA",
          .default = {{ group }}
        )
      )
  }

  # Si une modalite "NA" a ete ajoutee (en transformant le vrai NA en "NA" string)
  # alors il faut ajouter une couleur "NA" a la palette (car ggplot n'appliquera pas la couleur defaut au NA qui n'en est plus un)
  # @@@ Il s'agit d'un bricolage du a l'utilisation tardive de ggtext(). Ce pourrait etre largement optimise => on laisse comme ca pour le test @@@
  if (quo_is_null(quo_group.fill) & "NA" %in% levels) { # Ne s'applique pas si group.fill
    if(total == TRUE) {
      # Si un total, juste avant le total (car NA toujours avant le total)
      palette <- c(utils::head(palette, -1),
                   "grey",
                   utils::head(rev(palette), 1))
    }
    else {
      # Si pas de total, juste a la fin (car NA toujours a la fin)
      palette <- c(palette,
                   "grey")
    }
  }
  # GGTEXT end ---------------

  # On cree le graphique

  if (quo_is_null(quo_group.fill)) { # Si pas de group.fill
    graph <- graph |>
      ggplot(aes(
        x = {{ group }},
        y = indice,
        fill = {{ group }}
      ))
  }
  if (!quo_is_null(quo_group.fill)) { # Si group.fill
    graph <- graph |>
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
    theme_fonctionr(font = font,
                    theme = theme,
                    display = "ggtext") +
    theme(
      legend.position = if (quo_is_null(quo_group.fill)) "none" else "bottom"
    ) +
    scale_fill_manual(
      # if statement car palette differente si group.fill ou non
      # Si non group.fill, les couleurs de la palette sont associees aux levels avec un vecteur nomme (pour eviter les erreurs)
      values = if (quo_is_null(quo_group.fill)) stats::setNames(rev(palette), levels) else palette,
      na.value = "grey",
      labels = function(x) stringr::str_wrap(x, width = wrap_width_leg)
    ) +
    scale_x_discrete(
      labels = function(x) stringr::str_replace_all(stringr::str_wrap(x, width = wrap_width_y), "\n", "<br>"),
      limits = levels
    ) +
    guides(fill = guide_legend(ncol = legend_ncol)) +
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
          size = 3.5,
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
        size = 3.5,
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
        size = 3,
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
