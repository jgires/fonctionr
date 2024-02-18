#' central_group
#'
#' Function to compare means or medians in different groups from complex survey data. It produces a table, a graphic and a statistical test.
#'
#' @name central_group
#'
#' @param data A dataframe or an object from the survey package or an object from the srvyr package.
#' @param group A variable defining groups be compared.
#' @param quanti_exp An expression that define the mean/mediam to be computed.
#' @param type "mean" to compute means by group ; "median" to compute medians by group.
#' @param facet_var A variable defining the faceting group.
#' @param filter_exp An expression that filters the data, preserving the design.
#' @param ... All options possible in as_survey_design in srvyr package.
#' @param na.rm.group TRUE if you want to remove observations with NA on the group variable or NA on the facet variable. FALSE if you want to create a group with the NA value for the group variable and a facet with the NA value for the facet variable. NA in the variables included in quanti_exp are not affected in this argument. All the observation with a NA in the variables included in quanti_exp are excluded.
#' @param reorder TRUE if you want to reorder the groups according to the mean/median. NA value, in case if na.rm.group = FALSE, is not included in the reorder.
#' @param show_ci TRUE if you want to show the error bars on the graphic. FALSE if you do not want to show the error bars.
#' @param show_n TRUE if you want to show on the graphic the number of individuals in the sample in each group. FALSE if you do not want to show this number. Default is FALSE.
#' @param show_value TRUE if you want to show the mean/median in each group on the graphic. FALSE if you do not want to show the mean/median
#' @param show_lab TRUE if you want to show axes, titles and caption labels. FALSE if you do not want to show any label on axes and titles. Default is TRUE.
#' @param total_name Name of the total bar on the graphic. Default is Total.
#' @param digits Numbers of digits showed on the values labels on the graphic. Default is 0.
#' @param unit Unit showed in the graphic. Default is no unit.
#' @param dec Decimal mark shown on the graphic. Default is ","
#' @param fill Colour of the bars. NA bar, in case if na.rm.group = FALSE, and total bar are always in grey.
#' @param dodge Width of the bar, between 0 and 1.
#' @param font Font used in the graphic. Available fonts, included in the package itself, are "Roboto", "Montserrat" and "Gotham Narrow". Default is "Roboto".
#' @param wrap_width_y Number of characters before before going to the line. Applies to the labels of the groups. Default is 25.
#' @param title Title of the graphic.
#' @param subtitle Subtitle of the graphic.
#' @param xlab X label on the graphic. As coord_flip() is used in the graphic, xlab refers to the x label on the graphic, after the coord_flip(), and not to the x variable in the data.
#' @param ylab Y label on the graphic. As coord_flip() is used in the graphic, xlab refers to the x label on the graphic, after the coord_flip(), and not to the x variable in the data.
#' @param caption Caption of the graphic.
#' @param export_path Path to export the results in an xlsx file. The file includes two sheets : the table and the graphic.
#'
#' @return A list that contains a table, a graphic and a statistical test
#' @import rlang
#' @import survey
#' @import srvyr
#' @import dplyr
#' @import ggplot2
#' @import forcats
#' @import stringr
#' @import openxlsx
#' @import broom
#' @import showtext
#' @import sysfonts
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
#' eusilc_mean$graph
#'
central_group <- function(data,
                          group,
                          quanti_exp,
                          type,
                          facet_var = NULL,
                          filter_exp = NULL,
                          ...,
                          na.rm.group = T,
                          # na.rm.facet = T,## à completer
                          reorder = F,
                          show_ci = T,
                          show_n = FALSE,
                          show_value = TRUE,
                          show_lab = TRUE,
                          total_name = "Total",
                          digits = 0,
                          unit = "",
                          dec = ",",
                          fill = NULL,
                          dodge = 0.9,
                          font ="Roboto",
                          wrap_width_y = 25,
                          title = NULL,
                          subtitle = NULL,
                          xlab = NULL,
                          ylab = NULL,
                          caption = NULL,
                          export_path = NULL) {

  # Un check impératif
  if(missing(type) == TRUE){
    stop("L'argument type doit être rempli")
  }
  if((missing(data) | missing(group) | missing(quanti_exp)) == TRUE){
    stop("Les arguments data, group et quanti_exp doivent être remplis")
  }

  # Check des autres arguments
  check_arg(
    arg = list(
      type = type,
      total_name = total_name,
      unit = unit,
      dec = dec,
      fill = fill,
      font = font,
      title = title,
      subtitle = subtitle,
      xlab = xlab,
      ylab = ylab,
      caption = caption,
      export_path = export_path
    ),
    type = "character"
  )
  check_arg(
    arg = list(
      na.rm.group = na.rm.group,
      reorder = reorder,
      show_ci = show_ci,
      show_n = show_n,
      show_value = show_value,
      show_lab = show_lab
    ),
    type = "logical"
  )
  check_arg(
    arg = list(
      digits = digits,
      dodge = dodge,
      wrap_width_y = wrap_width_y
    ),
    type = "numeric"
  )

  # Check que les arguments avec choix précis sont les bons
  match.arg(type, choices = c("mean", "median"))

  # Petite fonction utile
  `%ni%` <- Negate(`%in%`)

  # On crée une quosure de facet_var & filter_exp => pour if statements dans la fonction (voir ci-dessous)
  # Solution trouvée ici : https://rpubs.com/tjmahr/quo_is_missing
  quo_facet <- enquo(facet_var)
  quo_filter <- enquo(filter_exp)

  # On procède d'abord à un test : il faut que toutes les variables entrées soient présentes dans data => sinon stop et erreur
  # On détecte d'abord les variables entrées dans l'expression pour calculer la moyenne/médiane
  # Solution trouvée ici : https://stackoverflow.com/questions/63727729/r-how-to-extract-object-names-from-expression
  vars_expression <- all.vars(substitute(quanti_exp))
  # On crée ensuite un vecteur string qui contient toutes les variables entrées
  vars_input_char <- c(as.character(vars_expression), as.character(substitute(group)))
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
      stop("Au moins une des variables introduites dans group, quanti_exp, filter_exp ou facet_var n'est pas présente dans data")
    }
    # Check du design. Solution trouvée ici : https://stackoverflow.com/questions/70652685/how-to-set-aliases-for-function-arguments-in-an-r-package
    vars_survey <- as.character(substitute(...()))[names(as.list(substitute(...()))) %in% c("strata", "ids", "weight", "weights", "probs", "variables", "fpc")]
    if(all(vars_survey %in% names(data)) == FALSE){
      stop("Au moins une des variables du design n'est pas présente dans data")
    }
  }
  # Si objet sondage
  if(any(class(data) %in% c("survey.design2","survey.design","tbl_svy","svyrep.design"))){
    if(all(vars_input_char %in% names(data[["variables"]])) == FALSE){
      stop("Au moins une des variables introduites dans group, quanti_exp, filter_exp ou facet_var n'est pas présente dans data")
    }
  }

  # Couleur selon médiane ou moyenne
  if(type == "mean" & is.null(fill)){
    fill <- "deeppink3"
  }
  if(type == "median" & is.null(fill)){
    fill <- "mediumorchid3"
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

  # On supprime les NA sur le groupe si na.rm.group = T
  if (na.rm.group == T) {
    data_W <- data_W %>%
      filter(!is.na({{ group }}))
    # idem sur la variable de facet si non-NULL
    if(!quo_is_null(quo_facet)){
      data_W <- data_W %>%
        filter(!is.na({{ facet_var }}))
    }
  }

  # On supprime les NA sur la/les variable(s) quanti dans tous les cas, sinon ambigu => de cette façon les n par groupe sont toujours les effectifs pour lesquels la/les variable(s) quanti sont non missing (et pas tout le groupe : ça on s'en fout)
  # On détecte les variables entrées dans quanti_exp pour calculer la moyenne/médiane
  # Solution trouvée ici : https://stackoverflow.com/questions/63727729/r-how-to-extract-object-names-from-expression
  vars_quanti_expression <- all.vars(substitute(quanti_exp))
  # On les affiche via message (pour vérification)
  message("Variable(s) détectée(s) dans quanti_exp : ", paste(vars_quanti_expression, collapse = ", "))
  # On calcule les effectifs avant filtre
  before <- data_W %>%
    summarise(n=unweighted(n()))
  # On filtre via boucle => solution trouvée ici : https://dplyr.tidyverse.org/articles/programming.html#loop-over-multiple-variables
  for (var in vars_quanti_expression) {
    data_W <- data_W %>%
      filter(!is.na(.data[[var]]))
  }
  # On calcule les effectifs après filtre
  after <- data_W %>%
    summarise(n=unweighted(n()))
  # On affiche le nombre de lignes supprimées (pour vérification)
  message(paste0(before[[1]] - after[[1]]), " lignes supprimées avec valeur(s) manquante(s) pour le(s) variable(s) de quanti_exp")

  # On convertit la variable de groupe en facteur si pas facteur
  data_W <- data_W %>%
    mutate(
      "{{ group }}" := droplevels(as.factor({{ group }})), # droplevels pour éviter qu'un level soit encodé alors qu'il n'a pas d'effectifs (pb pour le test stat potentiel)
      quanti_exp_flattened = {{ quanti_exp }} # On recalcule quanti_exp dans une variable unique si c'est une expression à la base => nécessaire pour les tests stat ci-dessous
      )

  # On convertit également la variable de facet en facteur si facet non-NULL
  if(!quo_is_null(quo_facet)){
    data_W <- data_W %>%
      mutate(
        "{{ facet_var }}" := droplevels(as.factor({{ facet_var }}))) # droplevels pour éviter qu'un level soit encodé alors qu'il n'a pas d'effectifs (pb pour le test khi2)
  }

  # Ici je crée une copie des données dans data_W_NA
  # L'idée est de recoder les NA des 2 variables croisées en level "NA", pour que le test stat s'applique aussi aux NA
  # Voir si simplification possible pour ne pas créer 2 objets : data_W & data_W_NA => cela implique de changer la suite : à voir car le fait d'avoir les NA en missing réel est pratique
  if(na.rm.group == F){
    data_W_NA <- data_W %>%
      # Idée : fct_na_value_to_level() pour ajouter un level NA encapsulé dans un droplevels() pour le retirer s'il n'existe pas de NA
      mutate("{{ group }}" := droplevels(fct_na_value_to_level({{ group }}, "NA"))
      )
    if(!quo_is_null(quo_facet)){
      data_W_NA <- data_W_NA %>% # On repart de data_W_NA => on enlève séquentiellement les NA de group puis facet_var
        mutate("{{ facet_var }}" := droplevels(fct_na_value_to_level({{ facet_var }}, "NA"))
        )
    }
  }

  # On réalise les tests statistiques
  # Solutions trouvées ici :
  # https://stackoverflow.com/questions/27261232/passing-argument-to-lm-in-r-within-function
  # https://stackoverflow.com/questions/72384740/passing-data-variables-to-r-formulas
  # https://stackoverflow.com/questions/52856711/use-function-arguments-in-lm-formula-within-function-environment
  # Pour regTermTest => expliqué par Lumley himself : https://stackoverflow.com/questions/72843411/one-way-anova-using-the-survey-package-in-r
  quanti_exp_fmla <- "quanti_exp_flattened" # Un string car on a créé la variable "en dur" dans la fonction
  if(quo_is_null(quo_facet)){
    group_fmla <- as.character(substitute(group))
    fmla <- stats::as.formula(paste(quanti_exp_fmla, "~", group_fmla))
    fmla2 <- stats::as.formula(paste("~", group_fmla))
  }
  # Avec facet : prévoir une boucle pour chacune des modalité de facet_var => A FAIRE PLUS TARD
  if(!quo_is_null(quo_facet)){
    group_fmla <- as.character(substitute(facet_var))
    fmla <- stats::as.formula(paste(quanti_exp_fmla, "~", group_fmla))
    fmla2 <- stats::as.formula(paste("~", group_fmla))
  }

  if(type == "mean"){
    if(na.rm.group == T & type == "mean"){
      model <- svyglm(fmla, design = data_W)
      test.stat <- regTermTest(model, fmla2)
      test.stat[["call"]] <- paste(quanti_exp_fmla, " ~ ", group_fmla)
    }
    if(na.rm.group == F & type == "mean"){
      model <- svyglm(fmla, design = data_W_NA)
      test.stat <- regTermTest(model, fmla2)
      test.stat[["call"]] <- paste(quanti_exp_fmla, " ~ ", group_fmla)
    }
  }
  # /!\ NOTE : ça fonctionne mais j'ai peur d'utiliser eval => solution précédente choisie, qui a tout de même le pb de ne pas garder la formule dans le call
  # if(type == "median"){
  #   if(na.rm.group == T){
  #     eval(substitute(test.stat <- svyranktest(quanti_exp ~ group, design = data_W, test = "KruskalWallis")))
  #   }
  #   if(na.rm.group == F){
  #     eval(substitute(test.stat <- svyranktest(quanti_exp ~ group, design = data_W_NA, test = "KruskalWallis")))
  #   }
  # }
  if(type == "median"){
    if(na.rm.group == T){
      test.stat <- svyranktest(fmla, design = data_W, test = "KruskalWallis")
    }
    if(na.rm.group == F){
      test.stat <- svyranktest(fmla, design = data_W_NA, test = "KruskalWallis")
    }
  }

  # On calcule l'indicateur par groupe (mean ou median selon la fonction appelée)
  if(quo_is_null(quo_facet)){
    if (type == "mean") {
      tab <- data_W %>%
        group_by({{ group }}) %>%
        cascade(
          indice = survey_mean({{ quanti_exp }}, na.rm = T, vartype = "ci"),
          n_sample = unweighted(n()),
          n_weighted = survey_total(vartype = "ci"),
          .fill = total_name, # Le total
        )
    }
    if (type == "median") {
      tab <- data_W %>%
        group_by({{ group }}) %>%
        cascade(
          indice = survey_median({{ quanti_exp }}, na.rm = T, vartype = "ci"),
          n_sample = unweighted(n()),
          n_weighted = survey_total(vartype = "ci"),
          .fill = total_name, # Le total
        )
    }
  }
  if(!quo_is_null(quo_facet)){
    if (type == "mean") {
      tab <- data_W %>%
        group_by({{ facet_var }}, {{ group }}) %>%
        cascade(
          indice = survey_mean({{ quanti_exp }}, na.rm = T, vartype = "ci"),
          n_sample = unweighted(n()),
          n_weighted = survey_total(vartype = "ci"),
          .fill = total_name, # Le total
        ) %>%
        filter({{ facet_var }} != total_name | is.na({{ facet_var }}))
    }
    if (type == "median") {
      tab <- data_W %>%
        group_by({{ facet_var }}, {{ group }}) %>%
        cascade(
          indice = survey_median({{ quanti_exp }}, na.rm = T, vartype = "ci"),
          n_sample = unweighted(n()),
          n_weighted = survey_total(vartype = "ci"),
          .fill = total_name, # Le total
        ) %>%
        filter({{ facet_var }} != total_name | is.na({{ facet_var }}))
    }
  }

  # On crée la palette : avec le total au début (en gris foncé) puis x fois le bleu selon le nombre de levels - 1 (le total étant déjà un niveau)
  palette <- c(rep(fill, nlevels(tab[[deparse(substitute(group))]]) - 1), "grey40")

  # On calcule la valeur max de l'indice, pour l'écart des geom_text dans le ggplot
  max_ggplot <- max(tab$indice, na.rm = TRUE)

  if (reorder == T) {
    # On crée un vecteur pour ordonner les levels de group selon mean, en mettant Total et NA en premier (= en dernier sur le graphique ggplot)
    levels <- c(
      total_name,
      NA,
      levels(reorder(
        tab[[deparse(substitute(group))]],
        tab[["indice"]],
        FUN = median,
        decreasing = T
      ))[levels(reorder(
        tab[[deparse(substitute(group))]],
        tab[["indice"]],
        FUN = median,
        decreasing = T
      )) != total_name]
    )
  }

  if (reorder == F) {
    # On crée un vecteur pour ordonner les levels de group pour mettre Total et NA en premier (= en dernier sur le graphique ggplot)
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

  # Dans le vecteur qui ordonne les levels, on a mis un NA => Or parfois pas de missing pour le groupe, même si na.rm.group = F !
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
      y = indice,
      fill = {{ group }}
    )) +
    geom_bar(
      width = dodge,
      stat = "identity",
      position = "dodge"
    ) +
    theme_fonctionr(font = font) +
    theme(
      legend.position = "none"
    ) +
    scale_fill_manual(
      values = palette,
      na.value = "grey"
    ) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = wrap_width_y),
                     limits = levels) +
    labs(title = title,
         subtitle = subtitle
    ) +
    coord_flip()


  # Pour caption

  if (!is.null(caption)) { # Permet de passer à la ligne par rapport au test stat
    caption <- paste0("\n", caption)
  }

  if (type == "mean") {
    graph <- graph +
      labs(
        caption = paste0(
          "GLM : ", scales::pvalue(test.stat$p[1], add_p = T),
          caption
        )
      )
  }
  if (type == "median") {
    graph <- graph +
      labs(
        caption = paste0(
          "Kruskal Wallis : ", scales::pvalue(test.stat$p.value[1], add_p = T),
          caption
        )
      )
  }

  # Ajouter les axes
  if(show_lab == TRUE){
    # X ---
    if(any(is.null(xlab), xlab != "")){
      if (type == "mean") {
        graph <- graph +
          labs(y = ifelse(is.null(xlab),
                          paste0("Moyenne : ", deparse(substitute(quanti_exp))),
                          xlab))
      }
      if (type == "median") {
        graph <- graph +
          labs(y = ifelse(is.null(xlab),
                          paste0("Médiane : ", deparse(substitute(quanti_exp))),
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
  }

  # Masquer les axes si show_lab == FALSE
  if(show_lab == FALSE){
    graph <- graph +
      labs(x = NULL,
           y = NULL)
  }

  # Ajouter les facets au besoin + scale_y si facet
  if (!quo_is_null(quo_facet)) {
    graph <- graph +
      facet_wrap(vars({{ facet_var }})) +
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

  # Ajouter les valeurs calculées
  if (show_value == TRUE){
    graph<-graph  +
      geom_text(
        aes(
          y = indice + (0.01 * max_ggplot),
          label = paste0(str_replace(round(indice,
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
        hjust = 0, # Justifié à droite
        vjust = 0.4
      )
  }

  # Dans un but de lisibilité, on renomme les indices "mean" ou "median" selon la fonction appelée
  if (type == "mean") {
    tab <- tab %>%
      rename(mean = indice,
             mean_low = indice_low,
             mean_upp = indice_upp)
  }

  if (type == "median") {
    tab <- tab %>%
      rename(median = indice,
             median_low = indice_low,
             median_upp = indice_upp)
  }

  # On crée l'objet final
  res <- list()
  res$tab <- tab
  res$graph <- graph
  res$test.stat <- test.stat

  if (!is.null(export_path)) {
    # L'export en excel

    # Pour être intégré au fichier excel, le graphique doit être affiché => https://ycphs.github.io/openxlsx/reference/insertPlot.html
    print(graph)

    # On transforme le test stat en dataframe
    if (type == "median") {
      test_stat_excel <- test.stat %>%
        broom::tidy() %>%
        t() %>%
        as.data.frame()
      test_stat_excel$names <- rownames(test_stat_excel)
      test_stat_excel <- test_stat_excel[, c(2,1)]
      names(test_stat_excel)[1] <- "Parameter"
      names(test_stat_excel)[2] <- "Value"
    }
    # broom::tidy() ne fonctionne pas sur regTermTest => je le fais à la main
    if (type == "mean") {
      test_stat_excel <- data.frame(Parameter = c("df", "ddf", "statistic", "p.value", "method"),
                                    Value = c(test.stat$df, test.stat$ddf, test.stat$Ftest, test.stat$p, "Wald test"),
                                    row.names = NULL)
    }

    # J'exporte les résultats en Excel
    export_excel(tab_excel = tab,
                 graph = graph,
                 test_stat_excel = test_stat_excel,
                 facet_null = quo_is_null(quo_facet),
                 export_path = export_path,
                 percent_fm = FALSE,
                 fgFill = "mediumorchid3",
                 bivariate = FALSE)
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
