#' prop_group
#'
#' Function to compare proportions in different groups from complex survey data. It produces a table, a graphic and a statistical test.
#'
#' @param data A dataframe or an object from the survey package or an object from the srvyr package.
#' @param ... All options possible in as_survey_design in srvyr package.
#' @param group A variable defining groups be compared.
#' @param prop_exp An expression that define the proportion to be computed.
#' @param facet A variable defining the faceting group.
#' @param filter_exp An expression that filters the data, preserving the design.
#' @param na.rm.group TRUE if you want to remove observations with NA on the group variable or NA on the facet variable. FALSE if you want to create a group with the NA value for the group variable and a facet with the NA value for the facet variable. NA in the variables included in prop_exp are not affected in this argument. All the observation with a NA in the variables included in prop_exp are excluded.
#' @param na.rm.facet TRUE if you want to remove observations with NA on the group variable or NA on the facet variable. FALSE if you want to create a group with the NA value for the group variable and a facet with the NA value for the facet variable. NA in the variables included in prop_exp are not affected in this argument. All the observation with a NA in the variables included in prop_exp are excluded.
#' @param na.prop "rm" to remove the NA in the variables used in prop_exp before computing the proportions, "include" to compute the proportions with the NA's in the denominators. Default is "rm". When "rm" NA are not allowed in prop_exp
#' @param prop_method Type of proportion method to use. See svyciprop in survey package for details. Default is the beta method.
#' @param reorder TRUE if you want to reorder the groups according to the proportion. NA value, in case if na.rm.group = FALSE, is not included in the reorder.
#' @param show_ci TRUE if you want to show the error bars on the graphic. FALSE if you do not want to show the error bars.
#' @param show_n TRUE if you want to show on the graphic the number of individuals in the sample in each group. FALSE if you do not want to show this number. Default is FALSE.
#' @param show_value TRUE if you want to show the proportion in each group on the graphic. FALSE if you do not want to show the proportion.
#' @param show_lab TRUE if you want to show axes, titles and caption labels. FALSE if you do not want to show any label on axes and titles. Default is TRUE.
#' @param total_name Name of the total shown on the graphic. Default is "Total".
#' @param scale Denominator of the proportion. Default is 100 to interprets numbers as percentages.
#' @param digits Numbers of digits showed on the values labels on the graphic. Default is 0.
#' @param unit Unit showed in the graphic. Default is percent.
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
#' eusilc_prop$graph
#'
prop_group <- function(data,
                       ...,
                       group,
                       prop_exp,
                       facet = NULL,
                       filter_exp = NULL,
                       na.rm.group = T,
                       na.rm.facet = T, # à compléter
                       na.prop = "rm",
                       prop_method = "beta", # Possibilité de choisir la methode d'ajustement des IC, car empiriquement, j'ai eu des problèmes avec logit
                       reorder = F,
                       show_ci = T,
                       show_n = FALSE,
                       show_value = TRUE, # Possibilité de ne pas vouloir avoir les valeurs sur le graphique
                       show_lab = TRUE,
                       total_name = "Total",#où mettre???
                       scale = 100,
                       digits = 0,
                       unit = "%",
                       dec = ",", ### A FAIRE
                       fill = "deepskyblue3",
                       dodge = 0.9,
                       font ="Roboto",
                       wrap_width_y = 25,
                       title = NULL, # Le titre du graphique
                       subtitle = NULL,
                       xlab = NULL, # Le nom de l'axe de la variable catégorielle
                       ylab = NULL,
                       caption = NULL,
                       export_path = NULL) {

  # start_time <- Sys.time()

  # Check des arguments nécessaires
  if((missing(data) | missing(group) | missing(prop_exp)) == TRUE){
    stop("Les arguments data, group et prop_exp doivent être remplis")
  }

  # Check des autres arguments
  check_arg(
    arg = list(
      na.prop = na.prop,
      prop_method = prop_method,
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
      reorder = reorder,
      show_ci = show_ci,
      show_n = show_n,
      show_value = show_value,
      show_lab = show_lab,
      na.rm.group = na.rm.group,
      na.rm.facet = na.rm.facet
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

  # Check que les arguments avec choix précis sont les bons
  match.arg(na.prop, choices = c("rm", "include"))

  # Petite fonction utile
  `%ni%` <- Negate(`%in%`)

  # On crée une quosure de facet & filter_exp => pour if statements dans la fonction (voir ci-dessous)
  # Solution trouvée ici : https://rpubs.com/tjmahr/quo_is_missing
  quo_facet <- enquo(facet)
  quo_filter <- enquo(filter_exp)

  # On procède d'abord à un test : il faut que toutes les variables entrées soient présentes dans data => sinon stop et erreur
  # On détecte d'abord les variables entrées dans l'expression pour calculer la proportion
  # Solution trouvée ici : https://stackoverflow.com/questions/63727729/r-how-to-extract-object-names-from-expression
  vec_prop_exp <- all.vars(substitute(prop_exp))
  names(vec_prop_exp) <- rep("prop_exp", length(vec_prop_exp))
  # On crée ensuite un vecteur string qui contient toutes les variables entrées
  vec_group <- c(group = as.character(substitute(group)))
  vars_input_char <- c(vec_prop_exp, vec_group)
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
  # Ici la contition et le stop à proprement parler
  check_input(data,
              vars_input_char)

  # Test que prop_exp est OK
  data <- data %>%
    mutate(fonctionr_test_prop_exp = {{ prop_exp }})
  if (!all(data[["fonctionr_test_prop_exp"]] %in% c(0,1,NA))) stop(paste("prop_exp doit être une expression produisant des TRUE-FALSE ou être une variable binaire (0-1/TRUE-FALSE)"), call. = FALSE)

  if(na.prop == "rm"){
    # L'expression ne peut pas contenir la fonction is.na() => il est utile de calculer la proportion de NA, mais vu qu'on supprime les NA dans la suite (voir plus loin), ça ne marche pas !
    # On regarde donc si la fonction is.na() est utilisée dans l'expression, et on bloque si c'est le cas
   names_expression <- all.names(substitute(prop_exp))
   if("is.na" %in% names_expression){
     stop("is.na() est détecté dans l'expression : prop_group() ne permet pas de calculer la proportion de valeurs manquantes lorsque na.prop == 'rm'")
   }
  }

  # # On extrait les & ou | dans l'expression => interdit car ça pose problème pour le filtrage des NA sur les variables utilisées dans l'expression si plusieurs variables (voir la partie avec "filter(!is.na(fonctionr_express_bin))")
  # express_check <- str_extract_all((deparse(substitute(prop_exp))), "[\\&\\|]+")[[1]]
  # if(length(express_check) > 0){
  #   if(express_check %in% c("&", "&&", "|", "||")){
  #    stop("L'expression ne peut pas comprendre de conditions multiples")
  #   }
  # }

  # On convertit d'abord en objet srvyr
  data_W <- convert_to_srvyr(data, ...)

  # On ne garde que les colonnes entrées en input
  data_W <- data_W %>%
    select(all_of(unname(vars_input_char)))

  # On filtre si filter est non NULL
  if(!quo_is_null(quo_filter)){
    data_W <- data_W %>%
      filter({{ filter_exp }})
  }

  # /!\ NOTE : méthode de filtrage pas sure ! Remplacée par version avec filtre sur "fonctionr_express_bin" /!\
  # # On choppe la colonne sur laquelle on calcule la proportion dans l'expression => devient un vecteur string
  # var_prop <- str_extract((deparse(substitute(prop_exp))), "\\w+\\b")
  # # Et on filtre le data.frame pour enlever les valeurs manquantes sur cette variable => sinon ambigu : de cette façon les n par groupe sont toujours les effectifs pour lesquels la variable var_prop est non missing (et pas tout le groupe : ça on s'en fout)
  # data <- data %>%
  #   filter(!is.na(var_prop))

  # On supprime les NA sur le groupe si na.rm.group = T
  if (na.rm.group == T) {
    data_W <- data_W %>%
      filter(!is.na({{ group }}))
  }
  if (na.rm.facet == T) {
    # idem sur la variable de facet si non-NULL
    if(!quo_is_null(quo_facet)){
      data_W <- data_W %>%
        filter(!is.na({{ facet }}))
    }
  }

  if(na.prop == "rm"){
    # On supprime les NA sur la/les variable(s) de l'expression dans tous les cas, sinon ambigu => de cette façon les n par groupe sont toujours les effectifs pour lesquels la/les variable(s) de l'expression sont non missing (et pas tout le groupe : ça on s'en fout)
    # On affiche les variables entrées dans l'expression via message (pour vérification) => presentes dans vec_prop_exp créé au début
    message("Variable(s) détectée(s) dans l'expression : ", paste(vec_prop_exp, collapse = ", "))
    # On calcule les effectifs avant filtre
    before <- data_W %>%
      summarise(n=unweighted(n()))
    # On filtre via boucle => solution trouvée ici : https://dplyr.tidyverse.org/articles/programming.html#loop-over-multiple-variables
    for (var in vec_prop_exp) {
      data_W <- data_W %>%
        filter(!is.na(.data[[var]]))
    }
    # On calcule les effectifs après filtre
    after <- data_W %>%
      summarise(n=unweighted(n()))
    # On affiche le nombre de lignes supprimées (pour vérification)
    message(paste0(before[[1]] - after[[1]]), " lignes supprimées avec valeur(s) manquante(s) pour le(s) variable(s) de l'expression")

    # On convertit la variable de groupe en facteur si pas facteur
    # On crée également une variable binaire liée à la proportion pour le khi2
    data_W <- data_W %>%
      mutate(
        "{{ group }}" := droplevels(as.factor({{ group }})), # droplevels pour éviter qu'un level soit encodé alors qu'il n'a pas d'effectifs (pb pour le test khi2)
        fonctionr_express_bin = {{ prop_exp }}
      )
    # On convertit également la variable de facet en facteur si facet non-NULL
    if(!quo_is_null(quo_facet)){
      data_W <- data_W %>%
        mutate(
          "{{ facet }}" := droplevels(as.factor({{ facet }}))) # droplevels pour éviter qu'un level soit encodé alors qu'il n'a pas d'effectifs (pb pour le test khi2)
    }
  }

  if(na.prop == "include"){
    data_W <- data_W %>%
      mutate(
        "{{ group }}" := droplevels(as.factor({{ group }})), # droplevels pour éviter qu'un level soit encodé alors qu'il n'a pas d'effectifs (pb pour le test khi2)
        fonctionr_express_bin = ifelse(!is.na({{ prop_exp }}),
                             {{ prop_exp }},
                             0)
      )
  }

  # /!\ NOTE : méthode de filtrage remplacée par version avec filtre sur vars_expression avec boucle for (voir précédent) /!\
  # # Méthode plus sure de filtrage des NA sur la variable sur laquelle est calculée la proportion
  # express_na <- data_W %>%
  #   summarise(na_express = sum(is.na(fonctionr_express_bin)))
  # message(paste0(express_na[[1]]), " valeur(s) manquante(s) supprimée(s) dans l'expression")
  # data_W <- data_W %>%
  #   filter(!is.na(fonctionr_express_bin))

  # Ici je crée une copie des données dans data_W_NA
  # L'idée est de recoder les NA des 2 variables group et facet en level "NA", pour que le test stat s'applique aussi aux NA
  # Voir si simplification possible pour ne pas créer 2 objets : data_W & data_W_NA => cela implique de changer la suite : à voir car le fait d'avoir les NA en missing réel est pratique
  if(na.rm.group == F|na.rm.facet == F){
    data_W_NA <- data_W
  }
  if(na.rm.group == F){
    data_W_NA <- data_W_NA %>%
      # Idée : fct_na_value_to_level() pour ajouter un level NA encapsulé dans un droplevels() pour le retirer s'il n'existe pas de NA
      mutate("{{ group }}" := droplevels(forcats::fct_na_value_to_level({{ group }}, "NA"))
      )
  }
  if (na.rm.facet == F) {
    # idem sur la variable de facet si non-NULL
    if(!quo_is_null(quo_facet)){
      data_W_NA <- data_W_NA %>% # On repart de data_W_NA => on enlève séquentiellement les NA de group puis facet
        mutate("{{ facet }}" := droplevels(forcats::fct_na_value_to_level({{ facet }}, "NA"))
        )
    }
  }

  # On réalise les tests statistiques
  # Ici un test khi2 sur une variable binaire "fonctionr_express_bin" oui/non pour l'expression
  if(quo_is_null(quo_facet)){
    group_fmla <- as.character(substitute(group))
    fmla <- stats::as.formula(paste("~", group_fmla, "+", "fonctionr_express_bin"))
  }
  # Avec facet : prévoir une boucle pour chacune des modalité de facet => A FAIRE PLUS TARD
  if(!quo_is_null(quo_facet)){
    facet_fmla <- as.character(substitute(facet))
    fmla <- stats::as.formula(paste("~", facet_fmla, "+", "fonctionr_express_bin"))
  }

  if(na.rm.group == F|na.rm.facet == F){
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
  if(na.rm.group == T & na.rm.facet == T){
    test.stat <- tryCatch(
      expr = {
        svychisq(fmla, data_W)
      },
      error = function(e){
        "Conditions non remplies"
      }
    )
  }

  # On calcule les proportions par groupe
  if(quo_is_null(quo_facet)){
    tab <- data_W %>%
      group_by({{ group }}) %>%
      cascade(
        prop = survey_mean(fonctionr_express_bin, na.rm = T, proportion = T, prop_method = prop_method, vartype = "ci"),
        n_sample = unweighted(n()),
        n_true_weighted = survey_total({{ prop_exp }}, na.rm = T, vartype = "ci"),
        n_tot_weighted = survey_total(vartype = "ci"),
        .fill = total_name, # Le total = colonne "Total"
      )
  }
  # Version avec facet
  if(!quo_is_null(quo_facet)){
    tab <- data_W %>%
      group_by({{ facet }}, {{ group }}) %>%
      cascade(
        prop = survey_mean(fonctionr_express_bin, na.rm = T, proportion = T, prop_method = prop_method, vartype = "ci"),
        n_sample = unweighted(n()),
        n_true_weighted = survey_total({{ prop_exp }}, na.rm = T, vartype = "ci"),
        n_tot_weighted = survey_total(vartype = "ci"),
        .fill = total_name, # Le total = colonne "Total"
      ) %>%
      filter({{ facet }} != total_name | is.na({{ facet }}))
  }

  # On crée la palette : avec le total au début (en gris foncé) puis x fois le bleu selon le nombre de levels - 1 (le total étant déjà un niveau)
  palette <- c(rep(fill, nlevels(tab[[deparse(substitute(group))]]) - 1), "grey40")

  # On calcule la valeur max de la proportion, pour l'écart des geom_text dans le ggplot
  max_ggplot <- max(tab$prop, na.rm = TRUE)

  if (reorder == T) {
    # On crée un vecteur pour ordonner les levels de group selon prop, en mettant Total et NA en premier (= en dernier sur le graphique ggplot)
    levels <- c(
      total_name,
      NA,
      levels(reorder(
        tab[[deparse(substitute(group))]],
        tab[["prop"]],
        FUN = median,
        decreasing = T
      ))[levels(reorder(
        tab[[deparse(substitute(group))]],
        tab[["prop"]],
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

  # Pour caption

  if (!is.null(caption)) { # Permet de passer à la ligne par rapport au test stat
    caption <- paste0("\n", caption)
  }

  graph <- tab %>%
    ggplot(aes(
      x = {{ group }},
      y = prop,
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
    scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = wrap_width_y),
                     limits = levels)+
    labs(title = title,
         subtitle = subtitle
         ) +
    coord_flip()

  # Pour caption

  if (!is.null(caption)) { # Permet de passer à la ligne par rapport au test stat
    caption <- paste0("\n", caption)
  }

  if (inherits(test.stat, "htest")) { # Condition sur inherits car si le test a réussi => test.stat est de class "htest", sinon "character"
    graph <- graph +
      labs(
        caption = paste0(
          "Khi2 d'indépendance : ", scales::pvalue(test.stat$p.value, add_p = T),
          caption
        )
      )
  }
  if (inherits(test.stat, "character")) { # Condition sur inherits car si le test a réussi => test.stat est de class "htest", sinon "character"
    graph <- graph +
      labs(
        caption = paste0(
          "Khi2 d'indépendance : conditions non remplies",
          caption
        )
      )
  }

  # Ajouter les axes
  if(show_lab == TRUE){
    # X ---
    if(any(is.null(xlab), xlab != "")){
      graph <- graph +
        labs(y = ifelse(is.null(xlab),
                        paste0("Proportion : ", deparse(substitute(prop_exp))),
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

  # Ajouter les valeurs calculées
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
        hjust = 0, # Justifié à droite
        vjust = 0.4
      )
  }

  # On crée l'objet final
  res <- list()
  res$tab <- tab
  res$graph <- graph
  res$test.stat <- test.stat

  if (!is.null(export_path)) {
    # L'export en excel

    # # Pour être intégré au fichier excel, le graphique doit être affiché => https://ycphs.github.io/openxlsx/reference/insertPlot.html
    # print(graph)

    # On transforme le test stat en dataframe
    if(all(test.stat != "Conditions non remplies")){
      test_stat_excel <- test.stat %>%
        broom::tidy() %>%
        t() %>%
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

    # J'exporte les résultats en Excel
    export_excel(tab_excel = tab,
                 graph = graph,
                 test_stat_excel = test_stat_excel,
                 facet_null = quo_is_null(quo_facet),
                 export_path = export_path,
                 percent_fm = TRUE,
                 fgFill = "skyblue3",
                 bivariate = FALSE)
  }

  # end_time <- Sys.time()
  # message(paste("Processing time:", round(end_time - start_time, 2), "sec"))

  return(res)

}
