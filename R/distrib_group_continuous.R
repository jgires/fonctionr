#' distrib_group_continuous
#'
#' Function to compare means or medians in different groups from complex survey data. It produces a table, a graphic and a statistical test.
#'
#' @name distrib_continuous
#'
#' @param data A dataframe or an object from the survey package or an object from the srvyr package.
#' @param group A variable defining groups to be compared.
#' @param quanti_exp An expression that define the variable from which the mean/median is computed.
#' @param type "mean" to compute mean by group ; "median" to compute median by group.
#' @param facet A variable defining the faceting group.
#' @param filter_exp An expression that filters the data, preserving the design.
#' @param ... All options possible in as_survey_design in srvyr package.
#' @param na.rm.facet TRUE if you want to remove observations with NA on the group variable or NA on the facet variable. FALSE if you want to create a group with the NA value for the group variable and a facet with the NA value for the facet variable. NA in the variables included in prop_exp are not affected in this argument. All the observation with a NA in the variables included in prop_exp are excluded.
#' @param show_ci TRUE if you want to show the error bars on the graphic. FALSE if you do not want to show the error bars. Default is TRUE.
#' @param show_n TRUE if you want to show on the graphic the number of individuals in the sample in each group. FALSE if you do not want to show this number. Default is FALSE.
#' @param show_value TRUE if you want to show the mean/median of each group on the graphic. FALSE if you do not want to show the mean/median. Default is TRUE.
#' @param show_lab TRUE if you want to show axes, titles and caption labels. FALSE if you do not want to show any label on axes and titles. Default is TRUE.
#' @param digits Numbers of digits showed on the value labels on the graphic. Default is 0.
#' @param unit Unit showed on the graphic. Default is no unit.
#' @param dec Decimal mark shown on the graphic. Default is ",".
#' @param pal
#' @param font Font used in the graphic. Available fonts, included in the package itself, are "Roboto", "Montserrat" and "Gotham Narrow". Default is "Roboto".
#' @param title Title of the graphic.
#' @param subtitle Subtitle of the graphic.
#' @param xlab X label on the graphic. As coord_flip() is used in the graphic, xlab refers to the X label on the graphic, after the coord_flip(), and not to the x variable in the data. If xlab = NULL, X label on the graphic will be "Moyenne : " + quanti_exp or "Médianne : " + quanti_exp. To show no X label, use xlab = "".
#' @param ylab Y label on the graphic. As coord_flip() is used in the graphic, ylab refers to the Y label on the graphic, after the coord_flip(), and not to the y variable in the data. If ylab = NULL, Y label on the graphic will be group. To show no Y label, use ylab = "".
#' @param caption Caption of the graphic.
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
#'
distrib_group_continuous <- function(data,
                               group,
                               quanti_exp,
                               type = "median",
                               facet = NULL,
                               filter_exp = NULL,
                               ...,
                               na.rm.group = TRUE,
                               na.rm.facet = TRUE,
                               quantiles = seq(.1, .9, .1),
                               moustache_probs = c(.95, .8, .5),
                               bw = 1,
                               resolution = 512,
                               height = .8,
                               limits = NULL,
                               reorder = FALSE,
                               show_center = TRUE,
                               show_ci = TRUE,
                               show_ci_area = FALSE,
                               show_quant_lines = FALSE,
                               show_moustache = TRUE,
                               show_n = FALSE,
                               show_value = TRUE,
                               show_lab = TRUE,
                               digits = 0,
                               unit = "",
                               dec = ",",
                               pal = c("skyblue4", "skyblue"),
                               pal_moustache = c("#EB9BA0", "#FAD7B1"),
                               color = NA,
                               font ="Roboto",
                               wrap_width_y = 25,
                               title = NULL,
                               subtitle = NULL,
                               xlab = NULL,
                               ylab = NULL,
                               caption = NULL,
                               export_path = NULL) {

  # 1. CHECKS DES ARGUMENTS --------------------

  # On crée une quosure de facet & filter_exp => pour if statements dans la fonction (voir ci-dessous)
  # Solution trouvée ici : https://rpubs.com/tjmahr/quo_is_missing
  quo_facet <- enquo(facet)
  quo_filter <- enquo(filter_exp)

  # On procède d'abord à un test : il faut que toutes les variables entrées soient présentes dans data => sinon stop et erreur
  # On crée un vecteur string qui contient toutes les variables entrées
  # Solution trouvée ici : https://stackoverflow.com/questions/63727729/r-how-to-extract-object-names-from-expression

  # On détecte d'abord les variables entrées dans l'expression pour calculer la moyenne/médiane
  vec_quanti_exp <- all.vars(substitute(quanti_exp))
  names(vec_quanti_exp) <- rep("quanti_exp", length(vec_quanti_exp)) # On crée un vecteur nommé pour la fonction check_input ci-dessous
  vars_input_char <- vec_quanti_exp
  # On ajoute groupe
  vec_group <- c(group = as.character(substitute(group)))
  vars_input_char <- c(vars_input_char, vec_group)
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
  # Ici le check à proprement parler
  check_input(data,
              vars_input_char)


  # 2. PROCESSING DES DONNEES --------------------

  # On convertit d'abord data en objet srvyr
  data_W <- convert_to_srvyr(data, ...)

  # # On ne garde que les colonnes entrées en input
  # data_W <- data_W %>%
  #   select(all_of(unname(vars_input_char)))

  # On filtre si filter est non NULL
  if(!quo_is_null(quo_filter)){
    data_W <- data_W %>%
      filter({{ filter_exp }})
  }
  # On supprime les NA sur le groupe si na.rm.group = T
  if (na.rm.group == T) {
    data_W <- data_W %>%
      filter(!is.na({{ group }}))
  }
  # On supprimes les NA sur la variable de facet si non-NULL
  if (na.rm.facet == T) {
    if(!quo_is_null(quo_facet)){
      data_W <- data_W %>%
        filter(!is.na({{ facet }}))
    }
  }

  # On supprime les NA sur la/les variable(s) quanti dans tous les cas, sinon ambigu => de cette façon les n par groupe sont toujours les effectifs pour lesquels la/les variable(s) quanti sont non missing (et pas tout le groupe : ça on s'en fout)
  # On les affiche via message (pour vérification)
  message("Variable(s) détectée(s) dans quanti_exp : ", paste(vec_quanti_exp, collapse = ", "))
  # On calcule les effectifs avant filtre
  before <- data_W %>%
    summarise(n=unweighted(n()))
  # On filtre via boucle => solution trouvée ici : https://dplyr.tidyverse.org/articles/programming.html#loop-over-multiple-variables
  for (var in vec_quanti_exp) {
    data_W <- data_W %>%
      filter(!is.na(.data[[var]]))
  }
  # On calcule les effectifs après filtre
  after <- data_W %>%
    summarise(n=unweighted(n()))
  # On affiche le nombre de lignes supprimées (pour vérification)
  message(paste0(before[[1]] - after[[1]]), " lignes supprimées avec valeur(s) manquante(s) pour le(s) variable(s) de quanti_exp")

  # On convertit la variable de groupe en facteur si pas facteur
  # + on recalcule quanti_exp dans une variable unique si c'est une expression à la base => nécessaire pour calculer la densité
  data_W <- data_W %>%
    mutate(
      "{{ group }}" := droplevels(as.factor({{ group }})), # droplevels pour éviter qu'un level soit encodé alors qu'il n'a pas d'effectifs (pb pour le test stat potentiel)
      quanti_exp_flattened = {{ quanti_exp }}
    )
  # On convertit la variable de facet en facteur si facet non-NULL
  if(!quo_is_null(quo_facet)){
    data_W <- data_W %>%
      mutate(
        "{{ facet }}" := droplevels(as.factor({{ facet }}))) # droplevels pour éviter qu'un level soit encodé alors qu'il n'a pas d'effectifs (pb pour le test khi2)
  }


  # 3. CALCUL DE L'INDICE CENTRAL (MEDIANE/MOYENNE) --------------------

  # Si non facet
  if (quo_is_null(quo_facet)) {
    data_W <- data_W %>%
      group_by({{ group }})
  }
  # Si facet
  if (!quo_is_null(quo_facet)) {
    data_W <- data_W %>%
      group_by({{ facet }}, {{ group }})
  }

  # On calcule l'indicateur par groupe (mean ou median selon la fonction appelée)
  if (quo_is_null(quo_facet)) {
    tab <- data_W %>%
      summarise(
        indice = if (type == "median") {
          survey_median({{ quanti_exp }}, na.rm = T, vartype = "ci")
        } else if (type == "mean") survey_mean({{ quanti_exp }}, na.rm = T, vartype = "ci"),
        n_sample = unweighted(n()), # On peut faire n(), car les NA ont été supprimés partout dans l'expression (précédemment dans la boucle) => plus de NA
        n_weighted = survey_total(vartype = "ci")
      )
  }

  if (reorder == T) {
    tab_order <- tab %>%
      arrange(desc(indice)) %>%
      mutate(order = row_number()) %>%
      select(group = {{ group }}, order)
  }


  # 4. CALCUL DE LA DENSITé ET DES QUANTILES --------------------

  # On identifie la variable de pondération inclue dans dotdotdot (...) pour la passer aussi à la densité
  var_weights <- substitute(...())$weights

  # On crée des objets vides pour les remplir dans la boucle ci-dessous
  estQuant_W <- tibble()
  df_dens <- tibble()
  quant_seg <- tibble()
  boxplot_df <- tibble()
  # On crée un vecteur sur lequel on va boucler => ce sont les levels de group (qui est un facteur)
  vec_group.i <- rev(levels(data_W$variables[[deparse(substitute(group))]]))

  for(group.i in seq_along(vec_group.i)){

    # On filtre data pour le group.i
    data_W_group <- data_W %>%
      filter({{ group }} == vec_group.i[group.i])

    # On estime la densité de la variable quanti pour le group.i
    estDensity_group <- stats::density(data_W_group$variables[["quanti_exp_flattened"]],
      n = resolution,
      adjust = bw,
      subdensity = T,
      weights = if (is.null(var_weights)) {
        NULL
      # On introduit la variable de pondération identifiée dans var_weights mais transformée pour que la somme = 1
      } else if (!is.null(var_weights)) data_W_group$variables[[as.character(var_weights)]] / sum(data_W_group$variables[[as.character(var_weights)]]),
      na.rm = T
    )

    # print(sum(data_W_group$variables[[as.character(var_weights)]]/sum(data_W_group$variables[[as.character(var_weights)]])))

    # On calcule les quantiles du group.i avec survey et on les stocke dans un data.frame
    estQuant_W_group <- as.data.frame(svyquantile(~quanti_exp_flattened,
      design = data_W_group,
      quantiles = quantiles,
      ci = T,
      na.rm = T
    )[[1]]) %>%
      tibble::rownames_to_column(var = "probs") %>% # On crée une colonne qui contient le quantile
      mutate(
        group = vec_group.i[group.i], # Pour savoir quel groupe
        level = group.i # Pour savoir quel level (en valeur numérique)
      )

    # On crée un data.frame avec les densités, et on crée les classes de quantiles (à quel quantile x appartient) en croisant x avec le vecteur de quantiles
    df_dens_group <- data.frame(
      group = vec_group.i[group.i],
      level = group.i,
      x = estDensity_group$x,
      y = estDensity_group$y,
      quantFct = findInterval(estDensity_group$x, estQuant_W_group$quantile) + 1,
      segment = NA # Pour pouvoir identifier les quantiles que j'ajoute ci-dessous
    )

    # sum(df_dens_group$y)

    # Il faut qu'il y ait dans l'estimation de la densité les valeurs EXACTES des quantiles, pour que le ggplot puisse couper exactement aux quantiles !
    # => Je les ajoute avec add_case, en estimant y avec approx()
    # Je le fais dans une boucle, pour avoir tous les quantiles.
    # SOLUTION INSPIRéE DE CE CODE : https://stackoverflow.com/questions/74560448/how-fill-geom-ribbon-with-different-colour-in-r
    for (i in seq_along(quantiles)) {
      df_dens_group <- df_dens_group |>
        tibble::add_case(
          group = vec_group.i[group.i],
          level = group.i,
          x = estQuant_W_group$quantile[i],
          y = stats::approx(df_dens_group$x, df_dens_group$y, xout = estQuant_W_group$quantile[i])$y,
          quantFct = i,
          segment = TRUE
        ) %>%
        tibble::add_case(
          group = vec_group.i[group.i],
          level = group.i,
          x = estQuant_W_group$quantile[i],
          y = stats::approx(df_dens_group$x, df_dens_group$y, xout = estQuant_W_group$quantile[i])$y,
          quantFct = i + 1
        )
    }

    # On rbind progressivement les df de densité des différents groupes
    df_dens <- rbind(df_dens, df_dens_group)
    # On rbind progressivement les df de quantiles des différents groupe
    estQuant_W <- rbind(estQuant_W, estQuant_W_group)

    # On fait les calculs nécessaires pour créer les boites à show_moustaches
    if (show_moustache == T) {

      # On calcule les quantiles à partir des proportions indiquées dans moustache_probs
      moustache_quant <- c(0 + ((1-moustache_probs)/2), 1 - ((1-moustache_probs)/2))
      moustache_quant <- moustache_quant[order(moustache_quant)]

      # On calcule les quantiles du group.i avec survey et on les stocke dans un data.frame
      boxplot_group <- as.data.frame(svyquantile(~quanti_exp_flattened,
       design = data_W_group,
       quantiles = moustache_quant, # On indique les quantiles calculés
       ci = F,
       na.rm = T
     )[[1]]) %>%
       mutate(
         group = vec_group.i[group.i],
         level = group.i
       )
      # On rbind progressivement les df de quantiles des différents groupe
      boxplot_df <- rbind(boxplot_df, boxplot_group)
    }
  }

  # On restructure boxplot_df pour ggplot => on calcule xbegin & xend par proportion + par groupe, pour indiquer à quelles valeurs de x commencent et finissent chaque "moustache"
  if (show_moustache == T) {
    boxplot_df <- boxplot_df %>%
      pivot_longer(
        cols = !c(group, level),
        names_to = "probs",
        values_to = "quantile",
        names_transform = as.numeric
      ) %>%
      mutate(
        moustache_prob = ifelse( # On fait l'inverse que précédemment : on retrouve les proportions à partir des quantiles
          probs > .5,
          1 - ((1 - probs) * 2),
          1 - (probs * 2)
          ),
        position = ifelse(
          probs > .5,
          "end",
          "begin"
        )
      )
    if (reorder == T) {
      boxplot_df <- boxplot_df %>%
        left_join(tab_order, by = "group") %>%
        mutate(level = order)
    }
    boxplot_df_begin <- boxplot_df %>% filter(position == "begin") %>% select(group, level, moustache_prob, xbegin = quantile) %>% mutate(across(everything(), as.character)) # Pour la jointure ci-dessous les variables doivent être en caractère
    boxplot_df_end <- boxplot_df   %>% filter(position == "end")   %>% select(group, level, moustache_prob, xend = quantile) %>% mutate(across(everything(), as.character))
    boxplot_df <- boxplot_df_begin %>%
      left_join(boxplot_df_end) %>%
      mutate(
        moustache_prob = as.character(moustache_prob), # On met moustache_prob en caractère pour le fill sur le ggplot
        across(
          c(xbegin, xend, level),
          as.numeric # On remet les variables numériques en numérique
        )
      )

  }

  # On calcule y_ridges, pour ploter chaque densité de groupe à un y différent sur des multiples de 1 (0, 1, 2, 3, ..., n)
  if (reorder == T) {
    df_dens <- df_dens %>%
      left_join(tab_order, by = "group") %>%
      mutate(level = order)
  }
  df_dens <- df_dens %>%
    group_by(group) %>%
    mutate(y_ridges = (y / max(y)) * height) %>%
    ungroup() %>%
    mutate(
      y_ridges = y_ridges + (level - 1),
      quantFct = as.factor(quantFct) # Je transforme l'appartenance au quantile en facteur, pour le ggplot
    )

  # On isole les quantiles avec leurs coordonnées y de densité (pour les afficher avec le ggplot)
  quant_seg <- df_dens %>%
    filter(segment == TRUE)

  # On inclut les levels à tab => nécessaire pour ggplot
  tab_level <- df_dens %>%
    group_by(group) %>%
    summarise(level = first(level)) %>%
    rename("{{ group }}" := group) # Pour la jointure
  tab <- tab %>%
    left_join(tab_level)


  # 5. CREATION DU GRAPHIQUE --------------------

  # La palette divergente => varie selon que le nombre de quantiles soit pair ou impair
  if(length(estQuant_W_group$quantile) %% 2 == 0){
    palette <- grDevices::colorRampPalette(pal)((length(unique(estQuant_W$probs))/2)+1)
    palette <- c(palette, rev(palette)[-1])
  }
  if(length(estQuant_W_group$quantile) %% 2 == 1){
    palette <- grDevices::colorRampPalette(pal)(((length(unique(estQuant_W$probs))+1)/2)+1)
    palette <- c(palette, rev(palette)[-c(1,2)])
    # palette <- c(rev(rev(palette)[-2]), rev(palette)[-1])
  }

  # Palette pour les moustaches, selon le nombre de proportions dans moustache_probs
  if (show_moustache == T) {
    pal_mous_calc <- grDevices::colorRampPalette(pal_moustache)(length(moustache_probs))
  }

  # Les limites de la variable quanti si non indiquée par l'utilisateur => pour ggplot
  if(is.null(limits)){
    lim_min <- min(data_W$variables[["quanti_exp_flattened"]], na.rm = T)
    lim_max <- max(data_W$variables[["quanti_exp_flattened"]], na.rm = T)
    limits <- c(lim_min, lim_max)
  }

  # On calcule la valeur max de la densité, pour l'écart des geom_text dans le ggplot
  max_ggplot <- max(df_dens$y_ridges)

  # Le graphique ggplot

  graph <- ggplot(
    data = df_dens,
  ) +
    geom_ribbon(
      aes(
        x = x, ymin = level - 1, ymax = y_ridges, # ymin à level - 1 car commence à 0
        fill = quantFct,
        group = interaction(group, quantFct) # Ici le groupe doit être l'interaction du groupe et des quantiles pour dessiner correctement les ribbon par groupe
      ),
      alpha = 1
    ) +
    geom_line(
      aes(
        x = x,
        y = y_ridges,
        group = group
      ),
      color = color,
      linewidth = .7
    ) +
    scale_x_continuous(
      labels = scales::label_dollar(
        accuracy = 1/10^digits, # On transforme le nombre de digits en un format compatible avec accuracy
        prefix = "",
        suffix = unit,
        big.mark = "",
        decimal.mark = dec),
      limits = limits,
      expand = expansion(mult = c(.01, .05))
    ) +
    scale_y_continuous(
      breaks = unique(df_dens$level - .8),
      labels = stringr::str_wrap(unique(df_dens$group), width = wrap_width_y),
      expand = expansion(mult = c(0.005, 0.05))
    ) +
    scale_fill_manual(
      values = palette,
      guide = "none"
    ) +
    theme_fonctionr(font = font) +
    theme(
      legend.position = "bottom"
    ) +
    labs(title = title,
         subtitle = subtitle
    )

  # Ajouter les segments des quantiles
  if (show_quant_lines == T) {
    graph <- graph +
      geom_segment(
        data = quant_seg,
        aes(x = x,
            y = level - 1,
            yend = y_ridges),
        linetype = "dotted"
      )
  }

  # Ajouter les moustaches
  if (show_moustache == T) {
    graph <- graph +
      ggnewscale::new_scale_fill() + # Ici je suis obligé de réinitialiser une nouvelle palette avec le package ggnewscale => je vois pas d'autre moyen facile
      geom_rect(
        data = boxplot_df,
        aes(
          xmin = xbegin, xmax = xend, ymin = level - 1.1, ymax = level - .9,
          fill = moustache_prob
        ),
        alpha = 1
      ) +
      scale_fill_manual(
        values = pal_mous_calc,
        name = "Proportion d'observations"
      )
  }

  # Pour caption

  if (!is.null(caption)) { # Permet de passer à la ligne par rapport au test stat
    caption <- paste0("\n", caption)
  }

  graph <- graph +
    labs(
      caption = paste0(
        "Test stat à implémenter !",
        caption
      )
    )

  # # Ajouter le nombre d'individus au besoin
  # if (show_n == TRUE) {
  #   graph <- graph +
  #     geom_text(
  #       data = quantile_n,
  #       aes(
  #         y = 0 + (0.01 * max_ggplot),
  #         x = coord_x + (0.001 * limits[2]), # Pour ajouter des labels avec les effectifs
  #         label = paste0("n=", n),
  #         family = font),
  #       size = 3,
  #       alpha = 0.5,
  #       hjust = 0, # Justifié à droite
  #       vjust = 1,
  #       angle = 90 # pour incliner
  #     )
  # }

  # # Ajouter l'aire des CI
  # if (show_ci_area == T) {
  #   graph <- graph +
  #     geom_ribbon(
  #       data = central,
  #       aes(
  #         x = x, ymin = 0, ymax = y
  #       ),
  #       alpha = .25
  #     )
  # }

  # Ajouter les limites des IC
  if (show_ci == T) {
    graph <- graph +
      geom_errorbarh(
        data = tab,
        aes(
          xmin = indice_low,
          xmax = indice_upp,
          y = level - 1
        ),
        alpha = 0.5,
        height = .2
      )

    # graph <- graph +
    #   geom_segment(
    #     data = central[central$central == "indice_low" | central$central == "indice_upp", ],
    #     aes(x = x,
    #         y = 0,
    #         yend = y),
    #     linetype = "dashed",
    #     alpha = .4
    #   )
  }

  # Ajouter la tendance centrale
  if (show_center == T) {
    graph <- graph +
      geom_point(
        data = tab,
        aes(x = indice,
            y = level - 1)
      )

    # graph <- graph +
    #   geom_segment(
    #     data = central[central$central == "indice", ],
    #     aes(x = x,
    #         y = 0,
    #         yend = y),
    #     linewidth = 1,
    #     alpha = .6
    #   )
  }

  # Ajouter les axes
  if(show_lab == TRUE){
    # X ---
    if(any(is.null(xlab), xlab != "")){
      graph <- graph +
        labs(x = ifelse(is.null(xlab),
                        paste0(deparse(substitute(quanti_exp))),
                        xlab))
    }
    if(all(!is.null(xlab), xlab == "")){
      graph <- graph +
        labs(x = NULL)
    }

    # Y ---
    if(any(is.null(ylab), ylab != "")){
      if(!is.null(ylab)){
        graph <- graph +
          labs(y = ylab)
      }
      if(is.null(ylab)){
        graph <- graph +
          labs(y = paste0(deparse(substitute(group))))
      }
    }
    if(all(!is.null(ylab), ylab == "")){
      graph <- graph +
        labs(y = NULL)
    }
  }

  # Masquer les axes si show_lab == FALSE
  if(show_lab == FALSE){
    graph <- graph +
      labs(x = NULL,
           y = NULL)
  }


  # 6. RESULTATS --------------------

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
  res$dens <- df_dens[, c("group", "x", "y", "quantFct")]
  res$tab <- tab#[, !names(tab) %in% c("level")]
  res$quant <- estQuant_W
  res$graph <- graph
  res$moustache <- boxplot_df[, !names(boxplot_df) %in% c("level")]

  return(res)

}