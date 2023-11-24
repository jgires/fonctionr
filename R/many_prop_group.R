#' many_prop_group
#'
#' @param data A dataframe or an object from the survey package or an object from the srvyr package.
#' @param group A variable defining groups be compared.
#' @param bin_vars A vector containing names of the dummy variables on which to compute the proportions
#' @param facet_var A variable defining the faceting group.
#' @param filter_exp An expression that filters the data, preserving the design.
#' @param prop_method Type of proportion method to use. See svyciprop in survey package for details. Default is the beta method.
#' @param ... All options possible in as_survey_design in srvyr package.
#' @param unit Unit showed in the graphic. Default is percent.
#' @param caption Caption of the graphic.
#' @param title Title of the graphic.
#' @param subtitle Subtitle of the graphic.
#' @param xlab X label on the graphic. As coord_flip() is used in the graphic, xlab refers to the x label on the graphic, after the coord_flip(), and not to the x variable in the data.
#' @param ylab Y label on the graphic. As coord_flip() is used in the graphic, xlab refers to the x label on the graphic, after the coord_flip(), and not to the x variable in the data.
#' @param scale Denominator of the proportion. Default is 100 to interprets numbers as percentages.
#' @param digits Numbers of digits showed on the values labels on the graphic. Default is 0.
#' @param show_labs TRUE if you want to show axes, titles and caption labels. FALSE if you do not want to show any label on axes and titles. Default is TRUE.
#' @param show_n TRUE if you want to show on the graphic the number of individuals in the sample in each group. FALSE if you do not want to show this number. Default is FALSE.
#' @param show_value TRUE if you want to show the proportion in each group on the graphic. FALSE if you do not want to show the proportion.
#' @param dodge Width of the bar, between 0 and 1.
#' @param pretty_pal Color palette used on the graphic. The palettes from the packages MetBrewer, MoMAColors and PrettyCols are available.
#' @param direction Direction of the palette color. Default is 1. The opposite direction is -1.
#' @param error_bar TRUE if you want to show the error bars on the graphic. FALSE if you do not want to show the error bars.
#' @param na.rm.group TRUE if you want to remove observations with NA on the group variable or NA on the facet variable. FALSE if you want to create a group with the NA value for the group variable and a facet with the NA value for the facet variable. NA in the variables included in prop_exp are not affected in this argument. All the observation with a NA in the variables included in prop_exp are excluded.
#' @param font Font used in the graphic. Available fonts, included in the package itself, are "Roboto", "Montserrat" and "Gotham Narrow". Default is "Roboto".
#' @param wrap_width Number of characters before before going to the line. Applies to the labels of the groups. Default is 25.
#'
#' @return
#' @import rlang
#' @import survey
#' @import srvyr
#' @import dplyr
#' @import ggplot2
#' @import forcats
#' @import stringr
#' @import openxlsx
#' @export
#'
#' @examples
many_prop_group = function(data,
                           group,
                           bin_vars,
                           facet_var = NULL,
                           filter_exp = NULL,
                           prop_method = "beta", # Possibilité de choisir la methode d'ajustement des IC, car empiriquement, j'ai eu des problèmes avec logit
                           ...,
                           unit = "%",
                           caption = NULL,
                           title = NULL, # Le titre du graphique
                           subtitle = NULL,
                           xlab = NULL, # Le nom de l'axe de la variable catégorielle
                           ylab = NULL,
                           scale = 100,
                           digits = 0,
                           show_labs = TRUE,
                           show_n = FALSE,
                           show_value = TRUE, # Possibilité de ne pas vouloir avoir les valeurs sur le graphique
                           dodge = 0.9,
                           pretty_pal = "Hokusai1",
                           direction = 1,
                           error_bar = T,
                           na.rm.group = T,
                           font ="Roboto",
                           wrap_width = 25){

  # On transforme les colonnes binarisée en un vecteur charactère (plus facile pour le code !)
  vec_bin_vars <- all.vars(substitute(bin_vars))
  message(vec_bin_vars)

  # Petite fonction utile
  `%ni%` <- Negate(`%in%`)

  # On crée une quosure de facet_var & filter_exp => pour if statements dans la fonction (voir ci-dessous)
  # Solution trouvée ici : https://rpubs.com/tjmahr/quo_is_missing
  quo_facet <- enquo(facet_var)
  quo_filter <- enquo(filter_exp)

  # On convertit d'abord en objet srvyr
  data_W <- convert_to_srvyr(data, ...)

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

  # On convertit la variable de groupe en facteur si pas facteur
  data_W <- data_W %>%
    mutate(
      "{{ group }}" := droplevels(as.factor({{ group }})) # droplevels pour éviter qu'un level soit encodé alors qu'il n'a pas d'effectifs (pb pour le test khi2)
    )
  # On convertit également la variable de facet en facteur si facet non-NULL
  if(!quo_is_null(quo_facet)){
    data_W <- data_W %>%
      mutate(
        "{{ facet_var }}" := droplevels(as.factor({{ facet_var }}))) # droplevels pour éviter qu'un level soit encodé alors qu'il n'a pas d'effectifs (pb pour le test khi2)
  }

  # On calcule les proportions par groupe
  tab <- tibble()
  if(quo_is_null(quo_facet)){
    for(i in vec_bin_vars) {
      tab_i <- data_W %>%
        group_by({{ group }}) %>%
        summarise(
          bin_col = i,
          prop = survey_mean(.data[[i]], na.rm = T, proportion = T, prop_method = prop_method, vartype = "ci"),
          n_tot_sample = unweighted(n()),
          n_tot_weighted = survey_total()
        )

      tab <- rbind(tab, tab_i)
    }
  }
  # Version avec facet
  if(!quo_is_null(quo_facet)){
    for(i in vec_bin_vars) {
      tab_i <- data_W %>%
        group_by({{ facet_var }}, {{ group }}) %>%
        summarise(
          bin_col = i,
          prop = survey_mean(.data[[i]], na.rm = T, proportion = T, prop_method = prop_method, vartype = "ci"),
          n_tot_sample = unweighted(n()),
          n_tot_weighted = survey_total()
        )

      tab <- rbind(tab, tab_i)
    }
  }

  # On crée la palette avecle package met.brewer
  if(pretty_pal %in% c("Archambault","Austria","Benedictus","Cassatt1","Cassatt2","Cross","Degas","Demuth",
                       "Derain","Egypt","Gauguin","Greek","Hiroshige","Hokusai1",
                       "Hokusai2","Hokusai3","Homer1","Homer2","Ingres","Isfahan1","Isfahan2",
                       "Java","Johnson","Juarez","Kandinsky","Klimt","Lakota","Manet",
                       "Monet","Moreau","Morgenstern","Nattier","Navajo","NewKingdom","Nizami",
                       "OKeeffe1","OKeeffe2","Paquin","Peru1","Peru2","Pillement","Pissaro",
                       "Redon","Renoir","Signac","Tam","Tara","Thomas","Tiepolo","Troy",
                       "Tsimshian","VanGogh1","VanGogh2","VanGogh3","Veronese","Wissing" )){
    palette <- as.character(met.brewer(name = pretty_pal, n = nlevels(as.factor(tab[["bin_col"]])), type = "continuous", direction = direction))
  }

  #ou la crée avec le package MoMAColors
  if(pretty_pal %in% c("Abbott","Alkalay1","Alkalay2","Althoff","Andri","Avedon","Budnitz",
                       "Clay","Connors","Dali","Doughton","Ernst","Exter","Flash",
                       "Fritsch","Kippenberger","Klein","Koons","Levine1","Levine2","Liu",
                       "Lupi","Ohchi","OKeeffe","Palermo","Panton","Picabia","Picasso",
                       "Rattner","Sidhu","Smith","ustwo","VanGogh","vonHeyl","Warhol" )){
    palette <- as.character(moma.colors(palette_name = pretty_pal, n = nlevels(as.factor(tab[["bin_col"]])), type = "continuous", direction = direction))
  }

  # On crée la palette avecle package PrettyCols
  if(pretty_pal %in% c("Blues","Purples","Tangerines","Greens","Pinks","Teals",
                       "Yellows","Reds","PurpleGreens","PinkGreens","TangerineBlues","PurpleTangerines",
                       "PurplePinks","TealGreens","PurpleYellows","RedBlues","Bold","Dark",
                       "Light","Neon","Summer","Autumn","Winter","Rainbow",
                       "Beach","Fun","Sea","Bright","Relax","Lucent",
                       "Lively","Joyful")){
    palette <- as.character(prettycols(name = pretty_pal, n = nlevels(as.factor(tab[["bin_col"]])), type = "continuous", direction = direction))
  }

  # On calcule la valeur max de la proportion, pour l'écart des geom_text dans le ggplot
  max_ggplot <- max(tab$prop, na.rm = TRUE)

  # On crée le graphique

  # On charge et active les polices
  load_and_active_fonts()

  graph <- tab %>%
    ggplot(aes(
      x = {{ group }},
      y = prop,
      fill = bin_col
    )) +
    geom_bar(
      width = dodge,
      stat = "identity",
      position = "dodge"
    ) +
    theme_minimal() +
    theme(
      panel.grid.minor.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = "#dddddd"),
      text = element_text(family = font),
      axis.line = element_line(color = "black"),
      axis.ticks = element_blank(),
      #axis.ticks = element_line(color = "black"),
      axis.text = element_text(color = "black"),
      legend.position = "bottom",
      plot.margin = margin(10, 15, 10, 10)
    ) +
    scale_fill_manual(
      values = palette,
      na.value = "grey"
    ) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = wrap_width))+
    labs(title = title,
         subtitle = subtitle,
         caption = caption
    ) +
    coord_flip()

  # Ajouter les axes
  if(show_labs == TRUE){
    graph <- graph +
      labs(y = ifelse(is.null(xlab),
                      paste0("Proportion : ", paste(vec_bin_vars, collapse = ", ")),
                      xlab))
    if(!is.null(ylab)){
      graph <- graph +
        labs(x = ylab)
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
      facet_wrap(vars({{ facet_var }})) +
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

  if (error_bar == T) {
    graph <- graph +
      geom_errorbar(aes(ymin = prop_low, ymax = prop_upp),
                    width = dodge * 0.05,
                    colour = "black",
                    alpha = 0.5,
                    linewidth = 0.5,
                    position = position_dodge(width = dodge)
      )
  }

  if (show_value == TRUE) {
    graph <- graph +
      geom_text(
        aes(
          y = (prop) + (0.01 * max_ggplot),
          label = paste0(round(prop * scale,
                               digits = digits),
                         unit),
          family = font),
        size = 3,
        vjust = ifelse(error_bar == T,
                       -0.25,
                       0.5),
        hjust = 0,
        color = "black",
        alpha = 0.9,
        # position = position_stack(vjust = .5))
        position = position_dodge(width = dodge)
      )
  }

  if (show_n == TRUE) {
    graph <- graph +
      geom_text(
        aes(
          y = 0 + (0.01 * max_ggplot), # Pour ajouter des labels avec les effectifs en dessous des barres
          label = paste0("n=", n_tot_sample),
          family = font),
        size = 3,
        alpha = 0.7,
        hjust = 0, # Justifié à droite
        vjust = 0.4,
        position = position_dodge(width = dodge)
      )
  }

  # On crée l'objet final
  res <- list()
  res$tab <- tab
  res$graph <- graph

  return(res)
}
