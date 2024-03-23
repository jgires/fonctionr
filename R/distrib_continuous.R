#' distrib_continuous
#'
#' Function to compare means or medians in different groups from complex survey data. It produces a table, a graphic and a statistical test.
#'
#' @name distrib_continuous
#'
#' @param data A dataframe or an object from the survey package or an object from the srvyr package.
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
distrib_continuous <- function(data,
                          quanti_exp,
                          type = "median",
                          facet = NULL,
                          quantiles = seq(.1, .9, .1),
                          filter_exp = NULL,
                          ...,
                          na.rm.facet = T,
                          limits = NULL,
                          bw = 1,
                          resolution = 1024,
                          show_ci_lines = T,
                          show_ci_area = F,
                          show_center = T,
                          show_segments = F,
                          show_n = FALSE,
                          show_value = TRUE,
                          show_lab = TRUE,
                          digits = 0,
                          unit = "",
                          dec = ",",
                          pal = c("#c94d33", "#ffe3bf"),
                          border = NA,
                          font ="Roboto",
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

  # On recalcule quanti_exp dans une variable unique si c'est une expression à la base => nécessaire pour calculer la densité
  data_W <- data_W %>%
    mutate(
      quanti_exp_flattened = {{ quanti_exp }}
    )
  # On convertit la variable de facet en facteur si facet non-NULL
  if(!quo_is_null(quo_facet)){
    data_W <- data_W %>%
      mutate(
        "{{ facet }}" := droplevels(as.factor({{ facet }}))) # droplevels pour éviter qu'un level soit encodé alors qu'il n'a pas d'effectifs (pb pour le test khi2)
  }


  # 3. CALCUL DE LA DENSITé ET DES QUANTILES --------------------

  # On identifie la variable de pondération inclue dans dotdotdot (...) pour la passer aussi à la densité
  var_weights <- substitute(...())$weights

  # On estime la densité de la variable quanti
  estDensity <- stats::density(data_W$variables[["quanti_exp_flattened"]],
    n = resolution,
    adjust = bw,
    subdensity = T,
    weights = if (is.null(var_weights)) {
      NULL
    } else if (!is.null(var_weights)) data_W$variables[[as.character(var_weights)]], # On introduit la variable de pondération identifiée dans var_weights
    na.rm = T
  )

  # On calcule les quantiles avec survey et on les stocke dans un vecteur
  estQuant_W <- as.data.frame(svyquantile(~quanti_exp_flattened,
    design = data_W,
    quantiles = quantiles,
    ci = T,
    na.rm = T
  )[[1]])

  # On crée un data.frame, et on crée les groupes de quantiles (à quel quantile x appartient) en croisant x avec le vecteur de quantiles
  df_dens <- data.frame(
    x = estDensity$x,
    y = estDensity$y / sum(estDensity$y),
    quantFct = findInterval(estDensity$x, estQuant_W$quantile) + 1,
    segment = NA # Pour pouvoir identifier les quantiles que j'ajoute ci-dessous
  )

  # sum(df_dens$y)

  # Il faut qu'il y ait dans l'estimation de la densité les valeurs EXACTES des quantiles, pour que le ggplot coupe exactement aux quantiles !
  # => Je les ajoute avec add_case, en estimant y avec approx()
  # Je le fais dans une boucle, pour avoir tous les quantiles.
  # SOLUTION INSPIRéE DE CE CODE : https://stackoverflow.com/questions/74560448/how-fill-geom-ribbon-with-different-colour-in-r
  for(i in seq_along(quantiles)){
    df_dens <- df_dens |>
      tibble::add_case(
        x = estQuant_W$quantile[i],
        y = stats::approx(df_dens$x, df_dens$y, xout = estQuant_W$quantile[i])$y,
        quantFct = i,
        segment = TRUE
      ) %>%
      tibble::add_case(
        x = estQuant_W$quantile[i],
        y = stats::approx(df_dens$x, df_dens$y, xout = estQuant_W$quantile[i])$y,
        quantFct = i+1
      )
  }

  # On isolé les quantiles avec leurs coordonnées y de densité (pour les afficher avec le ggplot)
  quant_seg <- df_dens %>%
    filter(segment == TRUE)

  # Je transforme l'appartenance au quantile en facteur, pour le ggplot
  df_dens <- df_dens %>%
    mutate(quantFct = as.factor(quantFct))

  if (show_n == TRUE) {
    # Les effectifs par quantile
    quantile_n <- data_W %>%
      mutate(quantFct = cut({{ quanti_exp }},
        include.lowest = T,
        right = F,
        breaks = c(-Inf, estQuant_W$quantile, Inf)
      )) %>%
      group_by(quantFct) %>%
      summarise(n = unweighted(n()))

    # On joint les coordonnées x du début des classes de quantile aux effectifs par quantile
    vec_mean_quant <- c(min(data_W$variables[["quanti_exp_flattened"]], na.rm = T), estQuant_W$quantile)
    quantile_n$coord_x <- vec_mean_quant

    # Si l'utilisateur indique des limites, alors on supprime les effectifs de toutes les classes de quantile pas affichées en entier
    if(!is.null(limits)){
      quantile_n <- quantile_n %>%
        filter(coord_x > limits[1]) # Les premières classes si elles sont coupées
      if(max(data_W$variables[["quanti_exp_flattened"]], na.rm = T) > limits[2])
        quantile_n <- quantile_n %>%
          slice(-n()) # Et la dernière si elle coupée
    }
  }


  # 4. CALCUL DE L'INDICE CENTRAL (MEDIANE/MOYENNE) --------------------

  if (!quo_is_null(quo_facet)) {
    data_W <- data_W %>%
      group_by({{ facet }})
  }

  # Calcul de la moyenne ou médiane et ses IC
  tab <- data_W %>%
    summarise(
      indice = if (type == "median") {
        survey_median({{ quanti_exp }}, na.rm = T, vartype = "ci")
      } else if (type == "mean") survey_mean({{ quanti_exp }}, na.rm = T, vartype = "ci"),
      n_sample = unweighted(n()), # On peut faire n(), car les NA ont été supprimés partout dans l'expression (précédemment dans la boucle) => plus de NA
      n_weighted = survey_total(vartype = "ci")
    )

  # On estime la densité de la moyenne ou médiane et ses CI
  df_dens <- df_dens |>
    mutate(central = NA) %>%
    tibble::add_case(
      x = tab$indice,
      y = stats::approx(df_dens$x, df_dens$y, xout = tab$indice)$y,
      central = "indice"
    ) %>%
    tibble::add_case(
      x = tab$indice_low,
      y = stats::approx(df_dens$x, df_dens$y, xout = tab$indice_low)$y,
      central = "indice_low"
    ) %>%
    tibble::add_case(
      x = tab$indice_upp,
      y = stats::approx(df_dens$x, df_dens$y, xout = tab$indice_upp)$y,
      central = "indice_upp"
    )

  # On identifie toutes les valeurs de densité comprises dans les IC => permet de créer une région sur le ggplot
  central <- df_dens |>
    filter(x >= tab$indice_low & x <= tab$indice_upp)


  # 5. CREATION DU GRAPHIQUE --------------------

  # La palette divergente => varie selon que le nombre de quantiles soit pair ou impair
  if(length(estQuant_W$quantile) %% 2 == 0){
    palette <- grDevices::colorRampPalette(pal)((length(estQuant_W$quantile)/2)+1)
    palette <- c(palette, rev(palette)[-1])
  }
  if(length(estQuant_W$quantile) %% 2 == 1){
    palette <- grDevices::colorRampPalette(pal)(((length(estQuant_W$quantile)+1)/2)+1)
    palette <- c(palette, rev(palette)[-c(1,2)])
    # palette <- c(rev(rev(palette)[-2]), rev(palette)[-1])
  }

  # Les limites de la variable quanti si non indiquée par l'utilisateur => pour ggplot
  if(is.null(limits)){
    lim_min <- min(data_W$variables[["quanti_exp_flattened"]], na.rm = T)
    lim_max <- max(data_W$variables[["quanti_exp_flattened"]], na.rm = T)
    limits <- c(lim_min, lim_max)
  }

  # On calcule la valeur max de la densité, pour l'écart des geom_text dans le ggplot
  max_ggplot <- max(df_dens$y)

  # Le graphique ggplot

  graph <- ggplot(
    data = df_dens[is.na(df_dens$central), ] # Sans les valeurs centrales, sinon ggplot les plot avec couleur NA
  ) +
    geom_line(
      aes(
        x = x,
        y = y
      ),
      color = border
    ) +
    geom_ribbon(
      aes(
        x = x, ymin = 0, ymax = y, fill = quantFct
      ),
      alpha = 1
    ) +
    scale_x_continuous(
      breaks = c(limits[1], estQuant_W$quantile, limits[2]),
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
      expand = expansion(mult = c(0.005, 0.05))
    ) +
    scale_fill_manual(
      values = palette
    ) +
    theme_fonctionr(font = font) +
    theme(
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "#dddddd"),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      legend.position = 'none'
    ) +
    labs(title = title,
         subtitle = subtitle
    )

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

  # Ajouter le nombre d'individus au besoin
  if (show_n == TRUE) {
    graph <- graph +
      geom_text(
        data = quantile_n,
        aes(
          y = 0 + (0.01 * max_ggplot),
          x = coord_x + (0.001 * limits[2]), # Pour ajouter des labels avec les effectifs
          label = paste0("n=", n),
          family = font),
        size = 3,
        alpha = 0.5,
        hjust = 0, # Justifié à droite
        vjust = 1,
        angle = 90 # pour incliner
      )
  }

  # Ajouter les segments des quantiles
  if (show_segments == T) {
    graph <- graph +
      geom_segment(
        data = quant_seg,
        aes(x = x,
            y = 0,
            yend = y),
        linetype = "dotted"
      )
  }

  # Ajouter l'aire des CI
  if (show_ci_area == T) {
    graph <- graph +
      geom_ribbon(
        data = central,
        aes(
          x = x, ymin = 0, ymax = y
        ),
        alpha = .25
      )
    }

  # Ajouter les limites des IC
  if (show_ci_lines == T) {
    # graph <- graph +
    #   geom_errorbarh(
    #     data = tab,
    #     aes(
    #       xmin = indice_low,
    #       xmax = indice_upp,
    #       y = 0
    #     ),
    #     alpha = 0.5,
    #     height = max(df_dens$y) / 25
    #   )

    graph <- graph +
      geom_segment(
        data = central[central$central == "indice_low" | central$central == "indice_upp", ],
        aes(x = x,
            y = 0,
            yend = y),
        linetype = "dashed",
        alpha = .4
      )
  }

  # Ajouter la tendance centrale
  if (show_center == T) {
    # graph <- graph +
    #   geom_point(
    #     data = tab,
    #     aes(x = indice,
    #         y = 0)
    #   )

    graph <- graph +
      geom_segment(
        data = central[central$central == "indice", ],
        aes(x = x,
            y = 0,
            yend = y),
        linewidth = 1,
        alpha = .6
      )
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
          labs(y = "Densité")
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
  res$dens <- df_dens[,c("x", "y", "quantFct")]
  res$tab <- tab
  res$quant <- estQuant_W
  res$graph <- graph

  return(res)

}
