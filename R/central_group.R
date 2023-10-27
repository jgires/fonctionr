#' central_group : fonction pour calculer facilement des tendances centrales par groupe
#' @name central_group
#'
#' @param data
#' @param group
#' @param quanti_exp
#' @param facet_var
#' @param filter_exp
#' @param ...
#' @param unit
#' @param caption
#' @param digits
#' @param show_n
#' @param dodge
#' @param reorder
#' @param error_bar
#' @param type
#' @param fill
#' @param na.rm.group
#' @param total_name
#' @param wrap_width
#' @param export_path
#'
#' @return
#' @import rlang
#' @import survey
#' @import srvyr
#' @import dplyr
#' @import ggplot2
#' @import scales
#' @importFrom stats as.formula
#' @import forcats
#' @import stringr
#' @import openxlsx
#' @export
#'
#' @examples
central_group <- function(data,
                          group,
                          quanti_exp,
                          facet_var = NULL,
                          filter_exp = NULL,
                          ...,
                          unit = "",
                          caption = NULL,
                          digits = 1,
                          show_n = FALSE,
                          dodge = 0.9,
                          reorder = F,
                          error_bar = T,
                          type,
                          fill = NULL,
                          na.rm.group = T,
                          total_name = "Total",
                          wrap_width = 25,
                          export_path = NULL) {

  # Petite fonction utile
  `%ni%` = Negate(`%in%`)

  # On crée une quosure de facet_var & filter_exp => pour if statements dans la fonction (voir ci-dessous)
  # Solution trouvée ici : https://rpubs.com/tjmahr/quo_is_missing
  quo_facet <- enquo(facet_var)
  quo_filter <- enquo(filter_exp)

  if(type == "mean" & is.null(fill)){
    fill <- "deeppink3"
  }
  if(type == "median" & is.null(fill)){
    fill <- "mediumorchid3"
  }

  # On convertit d'abord en objet srvyr
  # Si objet survey (avec replicates ou non)
  if(any(class(data) %in% c("survey.design2","survey.design")) & all(class(data) %ni% c("tbl_svy"))){
    message("Input : objet survey")
    data_W <- data %>%
      as_survey_design()
  }
  if(any(class(data) %in% c("svyrep.design")) & all(class(data) %ni% c("tbl_svy"))){
    message("Input : objet survey")
    data_W <- data %>%
      as_survey_rep()
  }
  # Si objet srvyr (avec replicates ou non)
  if(any(class(data) %in% c("tbl_svy"))){
    message("Input : objet srvyr")
    data_W <- data
  }
  # Si data.frame (pas de replicate prévu => A FAIRE A TERME)
  if(any(class(data) %ni% c("survey.design2","survey.design")) & any(class(data) %ni% c("tbl_svy")) & any(class(data) %in% c("data.frame"))){
    message("Input : data.frame")
    data_W <- data %>%
      as_survey_design(...)
  }

  message("Variables du design :", " cluster : ", paste(names(data_W$cluster), collapse = " "), " | strata : ",  paste(names(data_W$strata), collapse = " "), " | weights : ",  paste(names(data_W$allprob), collapse = " "))

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
  # On détecte les variables entrées dans quanti_exp pour calculer la proportion
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
    fmla <- as.formula(paste(quanti_exp_fmla, "~", group_fmla))
    fmla2 <- as.formula(paste("~", group_fmla))
  }
  # Avec facet : prévoir une boucle pour chacune des modalité de facet_var => A FAIRE PLUS TARD
  if(!quo_is_null(quo_facet)){
    group_fmla <- as.character(substitute(facet_var))
    fmla <- as.formula(paste(quanti_exp_fmla, "~", group_fmla))
    fmla2 <- as.formula(paste("~", group_fmla))
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
          n_tot_sample = unweighted(n()),
          n_tot_weighted = survey_total(),
          .fill = total_name, # Le total
        )
    }
    if (type == "median") {
      tab <- data_W %>%
        group_by({{ group }}) %>%
        cascade(
          indice = survey_median({{ quanti_exp }}, na.rm = T, vartype = "ci"),
          n_tot_sample = unweighted(n()),
          n_tot_weighted = survey_total(),
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
          n_tot_sample = unweighted(n()),
          n_tot_weighted = survey_total(),
          .fill = total_name, # Le total
        ) %>%
        filter({{ facet_var }} != total_name | is.na({{ facet_var }}))
    }
    if (type == "median") {
      tab <- data_W %>%
        group_by({{ facet_var }}, {{ group }}) %>%
        cascade(
          indice = survey_median({{ quanti_exp }}, na.rm = T, vartype = "ci"),
          n_tot_sample = unweighted(n()),
          n_tot_weighted = survey_total(),
          .fill = total_name, # Le total
        ) %>%
        filter({{ facet_var }} != total_name | is.na({{ facet_var }}))
    }
  }

  # On crée la palette : avec le total au début (en gris foncé) puis x fois le bleu selon le nombre de levels - 1 (le total étant déjà un niveau)
  palette <- c(rep(fill, nlevels(tab[[deparse(substitute(group))]]) - 1), "grey20")

  # On calcule la valeur max de l'indice, pour l'écart des geom_text dans le ggplot
  max_ggplot <- max(tab$indice, na.rm = TRUE)

  if (reorder == T & quo_is_null(quo_facet)) {
    # On crée un vecteur pour ordonner les levels de group selon mean, en mettant Total et NA en premier (= en dernier sur le graphique ggplot)
    levels <- c(
      total_name,
      NA,
      levels(reorder(
        tab[[deparse(substitute(group))]],
        tab[["indice"]]
      ))[levels(reorder(
        tab[[deparse(substitute(group))]],
        tab[["indice"]]
      )) != total_name]
    )
  }

  if (reorder == F | !quo_is_null(quo_facet)) {
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
    theme_minimal() +
    theme(
      panel.grid.minor.y = element_blank(),
      panel.grid.minor.x = element_line(color = "#dddddd"),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = "#dddddd"),
      legend.position = "none"
    ) +
    scale_fill_manual(
      values = palette,
      na.value = "grey"
    ) +
    scale_y_continuous(
      limits = function(x) {
        c(min(x), max(x))
      },
      expand = expansion(mult = c(.01, .05)),
      labels = function(x) {
        paste0(x, unit)
      }
    ) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = wrap_width),
                     limits = levels) +
    coord_flip()

  if (!quo_is_null(quo_facet)) {
    graph <- graph +
      facet_wrap(vars({{ facet_var }}))
  }

  if (type == "mean") {
    graph <- graph +
      labs(y = paste0("mean : ", deparse(substitute(quanti_exp))))
  }

  if (type == "median") {
    graph <- graph +
      labs(y = paste0("median : ", deparse(substitute(quanti_exp))))
  }

  if (type == "mean") {
    graph <- graph +
      labs(
        caption = paste0(
          caption,
          "\n",
          "GLM : ", pvalue(test.stat$p[1], add_p = T)
        )
      )
  }
  if (type == "median") {
    graph <- graph +
      labs(
        caption = paste0(
          "Kruskal Wallis : ", pvalue(test.stat$p.value[1], add_p = T),
          "\n",
          caption
        )
      )
  }

  if (error_bar == T) {
    graph <- graph +
      geom_errorbar(aes(ymin = indice_low,
                        ymax = indice_upp),
                    width = dodge * 0.25,
                    colour = "black",
                    alpha = 0.5,
                    linewidth = 0.5,
                    position = position_dodge(width = dodge)
      )
  }

  graph <- graph +
    geom_text(
      aes(
        y = indice - (0.01 * max_ggplot),
        label = paste0(round(indice,
                             digits = digits
        ), unit)
      ),
      vjust = 0.4,
      hjust = 1,
      color = "white",
      alpha = 0.9,
      position = position_dodge(width = dodge)
    )

  if (show_n == TRUE) {
    graph <- graph +
      geom_text(
        aes(
          y = 0 + (0.01 * max_ggplot), # Pour ajouter des labels avec les effectifs en dessous des barres
          label = paste0("n=", n_tot_sample)
        ),
        alpha = 0.7,
        hjust = 0
      ) # Justifié à droite
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
    write.xlsx(tab, export_path)
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
