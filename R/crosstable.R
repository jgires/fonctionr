#' crosstable : fonction pour calculer facilement des profils lignes par groupe
#'
#' @param data
#' @param group
#' @param var_distrib
#' @param facet_var
#' @param filter_exp
#' @param ...
#' @param caption
#' @param show_prop
#' @param unit
#' @param digits
#' @param dodge
#' @param pretty_pal
#' @param direction
#' @param wrap_width
#' @param na.rm
#'
#' @return
#' @import rlang
#' @import survey
#' @import srvyr
#' @import dplyr
#' @import MetBrewer
#' @import ggplot2
#' @import scales
#' @importFrom stats as.formula
#' @import forcats
#' @import stringr
#' @import openxlsx
#' @export
#'
#' @examples
crosstable <- function(data,
                       group,
                       var_distrib,
                       facet_var = NULL,
                       filter_exp = NULL,
                       prop_method = "beta",#possibilité de choisir la methode d'ajustement des IC, car empiriqument, j'ai eu des problèmes avec logit-----
                       ...,
                       caption = NULL,
                       show_prop = TRUE,
                       unit = "",
                       digits = 0,
                       dodge = 0.9,
                       pretty_pal = "Hokusai1",
                       direction = 1,
                       wrap_width = 25,
                       legend_nmax_mod_row = 4,#j'ai ajouté une variable indiquant le nombre de modalités maximale par ligne dans la légende, mais ce n'est pas le nom idéal -------
                       na.rm = T) {

  # Petite fonction utile
  `%ni%` = Negate(`%in%`)

  # On crée une quosure de facet_var & filter_exp => pour if statements dans la fonction (voir ci-dessous)
  # Solution trouvée ici : https://rpubs.com/tjmahr/quo_is_missing
  quo_facet <- enquo(facet_var)
  quo_filter <- enquo(filter_exp)

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

  # On supprime les NA des 2 variables si na.rm == T
  if(na.rm == T){
    data_W <- data_W %>%
      filter(!is.na({{ group }}) & !is.na({{ var_distrib }}))
    # idem sur la variable de facet si non-NULL
    if(!quo_is_null(quo_facet)){
      data_W <- data_W %>%
        filter(!is.na({{ facet_var }}))
    }
  }

  # On convertit en facteurs si pas facteurs
  data_W <- data_W %>%
    mutate(
      "{{ var_distrib }}" := droplevels(as.factor({{ var_distrib }})), # droplevels pour éviter qu'un level soit encodé alors qu'il n'a pas d'effectifs (pb pour le test khi2)
      "{{ group }}" := droplevels(as.factor({{ group }}))
    )
  # On convertit également la variable de facet en facteur si facet non-NULL
  if(!quo_is_null(quo_facet)){
    data_W <- data_W %>%
      mutate(
        "{{ facet_var }}" := droplevels(as.factor({{ facet_var }}))) # droplevels pour éviter qu'un level soit encodé alors qu'il n'a pas d'effectifs (pb pour le test khi2)
  }

  # Ici je crée une copie des données dans data_W_NA
  # L'idée est de recoder les NA des 2 variables croisées en level "NA", pour que le khi2 s'applique aussi aux NA
  # Voir si simplification possible pour ne pas créer 2 objets : data_W & data_W_NA => cela implique de changer la suite : à voir car le fait d'avoir les NA en missing réel est pratique
  if(na.rm == F){
    data_W_NA <- data_W %>%
      # Idée : fct_na_value_to_level() pour ajouter un level NA encapsulé dans un droplevels() pour le retirer s'il n'existe pas de NA
      mutate("{{ group }}" := droplevels(fct_na_value_to_level({{ group }}, "NA")),
             "{{ var_distrib }}" := droplevels(fct_na_value_to_level({{ var_distrib }}, "NA"))
      )
    if(!quo_is_null(quo_facet)){
      data_W_NA <- data_W_NA %>% # On repart de data_W_NA => on enlève séquentiellement les NA de group puis facet_var
        mutate("{{ facet_var }}" := droplevels(fct_na_value_to_level({{ facet_var }}, "NA"))
        )
    }
  }

  # On réalise les tests statistiques
  # NOTE : pour l'instant uniquement lorsque pas de facet => pour facet mon idée c'est une analyse loglinéaire => pas bien pigé avec survey
  if(quo_is_null(quo_facet)){
    var_distrib_fmla <- as.character(substitute(var_distrib))
    group_fmla <- as.character(substitute(group))
    fmla <- as.formula(paste("~", group_fmla, "+", var_distrib_fmla))
    if(na.rm == F){
      # On utilise un tryCatch pour bypasser le test s'il produit une erreur => possible lorsque les conditions ne sont pas remplies
      test.stat <- tryCatch(
        expr = {
          svychisq(fmla, data_W_NA)
        },
      # test.stat devient un vecteur string avec 1 chaîne de caractères si erreur du test
        error = function(e){
          "Conditions non remplies"
        }
      )
    }
    if(na.rm == T){
      test.stat <- tryCatch(
        expr = {
          svychisq(fmla, data_W)
        },
        error = function(e){
          "Conditions non remplies"
        }
      )
    }
  }

  # On calcule les fréquences relatives par groupe
  if(quo_is_null(quo_facet)){
    tab <- data_W %>%
      group_by({{ group }}, {{ var_distrib }}) %>%
      summarise(
        prop = survey_prop(proportion = T,prop_method ={{prop_method}}, vartype = c("ci")),#j'ai ajouté la possibilité de choisir la méthode de correstion des IC -------
        n = unweighted(n()),
        n_weighted = survey_total()
      )
  }
  if(!quo_is_null(quo_facet)){
    tab <- data_W %>%
      group_by({{ facet_var }}, {{ group }}, {{ var_distrib }}) %>%
      summarise(
        prop = survey_prop(proportion = T,prop_method ={{prop_method}}, vartype = c("ci")),#j'ai ajouté la possibilité de choisir la méthode de correstion des IC -------
        n = unweighted(n()),
        n_weighted = survey_total()
      )
  }

  # On crée la palette avecle package met.brewer
  palette <- as.character(met.brewer(name = pretty_pal, n = nlevels(as.factor(tab[[deparse(substitute(var_distrib))]])), type = "continuous", direction = direction))

  # On crée un vecteur pour ordonner les levels de group pour mettre NA en premier (= en dernier sur le graphique ggplot)
  levels <- c(
    NA,
    rev(
      levels(
        tab[[deparse(substitute(group))]]
        )
    )
  )

  # Dans le vecteur qui ordonne les levels, on a mis un NA => Or parfois pas de missing pour le groupe, même si na.rm.group = F !
  # On les supprime donc ssi na.rm = F et pas de missing sur la variable de groupe **OU** na.rm = T
  if ((na.rm == F & sum(is.na(tab[[deparse(substitute(group))]])) == 0) | na.rm == T)  {
    levels <- levels[!is.na(levels)]
  }

  n_lev<- nlevels(as.factor(tab[[deparse(substitute(var_distrib))]]))###on identifie le nombre de modalités dans la variable

  # On crée le graphique
  graph <- tab %>%
    ggplot(aes(
      x = {{ group }},
      y = prop,
      fill = {{ var_distrib }}
    )) +
    geom_bar(
      width = dodge,
      stat = "identity",
      position = position_stack(reverse = TRUE)
    ) +
    theme_minimal() +
    theme(
      panel.grid.minor.y = element_blank(),
      panel.grid.minor.x = element_line(color = "#dddddd"),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = "#dddddd"),
      legend.position = "bottom"
    ) +
    ylab(paste0("distribution : ", deparse(substitute(var_distrib)))) +
    scale_fill_manual(values = palette,
                      labels = function(x) str_wrap(x, width = 25),
                      na.value = "grey") +
    scale_y_continuous(
      labels = scales::percent,
      expand = expansion(mult = c(.01, .05))
    ) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = wrap_width),
                     limits = levels) +
    guides (fill = guide_legend(nrow = ceiling(n_lev/{{legend_nmax_mod_row}}))) + ##ici, j'ai la ligne qui indique le nombre de modalités maximal par ligne dans la légende
    coord_flip()

  if (!quo_is_null(quo_facet)) {
    graph <- graph +
      facet_wrap(vars({{ facet_var }}))
  }

  if (quo_is_null(quo_facet) & inherits(test.stat, "htest")) { # Condition sur inherits car si le test a réussi => test.stat est de class "htest", sinon "character"
    graph <- graph +
      labs(
        caption = paste0(
          "Khi2 d'indépendance : ", pvalue(test.stat$p.value, add_p = T),
          "\n",
          caption
        )
      )
  }
  if (quo_is_null(quo_facet) & inherits(test.stat,"character")) { # Condition sur inherits car si le test a réussi => test.stat est de class "htest", sinon "character"
    graph <- graph +
      labs(
        caption = paste0(
          "Khi2 d'indépendance : conditions non remplies",
          "\n",
          caption
        )
      )
  }
  # Ce n'est un khi2 s'il y a des facettes
  if (!quo_is_null(quo_facet)) {
    graph <- graph +
      labs(
        caption = caption
      )
  }

  if (show_prop == TRUE) {
    graph <- graph +
      geom_text(
        aes(label = ifelse(prop > 0.02,
                           paste0(round(prop * 100,
                                        digits = digits
                           ), unit),
                           NA
        )),
        color = "white",
        position = position_stack(vjust = .5,
                                  reverse = TRUE)
      )
  }

  # On crée l'objet final
  res <- list()
  res$tab <- tab
  res$graph <- graph
  # Pour l'instant, test uniquement si pas de facet
  if (quo_is_null(quo_facet)) {
    res$test.stat <- test.stat
  }

  return(res)
}
