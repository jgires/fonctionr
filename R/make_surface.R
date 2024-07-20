#' make_surface
#'
#'
#' @param tab dataframe with the variables to be ploted.
#' @param var The variable in tab with the labels of the indicators to be ploted.
#' @param value The variable in tab with the values of the indicator to be ploted.
#' @param error_low The variable in tab that is the lower bound of the confidence interval. If either error_low or error_upp is NULL error bars are not shown on the graphic.
#' @param error_upp The variable in tab that is the upper bound of the confidence interval. If either error_low or error_upp is NULL error bars are not shown on the graphic.
#' @param facet A variable in tab defining the faceting group, if applicable. Default is NULL.
#' @param pvalue The p-value to show in the caption. It can a numeric value or the pvalue object from a statsistical test.
#' @param reorder TRUE if you want to reorder the values. NA value is not included in the reorder.
#' @param compare TRUE to display a square representing the smallest value. When facets are enabled, this is the smallest value per facet category.
#' @param space The space between the squares. The unit is that of the indicator.
#' @param position The position of the squares: "mid" for center alignment, "bottom" for bottom alignment.
#' @param show_ci TRUE if you want to show the CI on the graphic. The bounds of the confidence intervals are displayed as dotted squares around the result. FALSE if you do not want to show them. Default is TRUE.
#' @param name_total Name of the var label that may contain the total. When indicated, it is not displayed on the graph.
#' @param digits Numbers of digits showed on the values labels on the graphic. Default is 0.
#' @param unit The unit showd on the plot. Default is percent.
#' @param pal Color palette used on the graphic. The palettes from the packages MetBrewer, MoMAColors and PrettyCols are available.
#' @param direction Direction of the palette color. Default is 1. The opposite direction is -1.
#' @param bg Color of the background.
#' @param font Font used in the graphic. See load_and_active_fonts() for available fonts.
#' @param wrap_width_lab Number of characters before going to the line for the labels of the categories of var. Default is 20.
#' @param title Title of the graphic.
#' @param subtitle Subtitle of the graphic.
#' @param caption Caption of the graphic.
#'
#' @return A ggplot graphic.
#' @import dplyr
#' @import ggplot2
#' @import rlang
#' @export
#'
#' @examples
make_surface <- function(tab,
                         var,
                         value,
                         error_low = NULL,
                         error_upp = NULL,
                         facet = NULL,
                         pvalue = NULL,
                         reorder = F,
                         compare = F,
                         space = NULL,
                         position = "mid",
                         show_ci = TRUE,
                         name_total = "Total",
                         digits = 0,
                         unit = NULL,
                         pal = "Kandinsky",
                         direction = 1,
                         bg = "snow2",
                         font = "Roboto",
                         wrap_width_lab = 20,
                         title = NULL,
                         subtitle = NULL,
                         caption = NULL) {

  # 1. CHECKS DES ARGUMENTS --------------------

  # Check des arguments necessaires
  if((missing(tab) | missing(var) | missing(value)) == TRUE){
    stop("Les arguments tab, var et value doivent etre remplis")
  }

  # Check des autres arguments
  check_arg(
    arg = list(
      position = position,
      name_total = name_total,
      unit = unit,
      pal = pal,
      bg = bg,
      font = font,
      title = title,
      subtitle = subtitle,
      caption = caption
    ),
    type = "character"
  )
  check_arg(
    arg = list(
      reorder = reorder,
      compare = compare,
      show_ci = show_ci
    ),
    type = "logical"
  )
  check_arg(
    arg = list(
      pvalue = pvalue,
      space = space,
      digits = digits,
      direction = direction,
      wrap_width_lab = wrap_width_lab
    ),
    type = "numeric"
  )

  # Check que les arguments avec choix precis sont les bons
  match.arg(position, choices = c("mid", "bottom"))

  # On cree des quosures => pour if statements dans la fonction (voir ci-dessous)
  # Solution trouvee ici : https://rpubs.com/tjmahr/quo_is_missing
  quo_facet <- enquo(facet)
  quo_low <- enquo(error_low)
  quo_up <- enquo(error_upp)

  # Check des arguments necessaires
  if((show_ci == T) & (quo_is_null(quo_low) | quo_is_null(quo_up))){
    message(paste0("Vous n'avez pas indiqu", "\u00e9", " les variables avec les IC : ceux-ci sont desactiv", "\u00e9", "s"))
    show_ci <- FALSE
  }


  # 2. PROCESSING DES DONNEES --------------------

  # On enleve le total
  tab <- tab |>
    filter(
      {{ var }} != name_total
    )

  # On convertit la variable categorielle en facteur si pas facteur
  tab <- tab |>
    mutate(
      "{{ var }}" := droplevels(as.factor({{ var }})) # droplevels pour eviter qu'un level soit encode alors qu'il n'a pas d'effectifs
    )

  # On reordonne si reorder == T
  # Si pas de facets
  if (reorder == T & quo_is_null(quo_facet)) {
    tab <- tab |>
      mutate(
        "{{ var }}" := forcats::fct_reorder({{ var }}, {{ value }}) # Necessaire pour que la palette soit ordonnee comme l'ordre cree par arrange()
      ) |>
      arrange({{ value }}) # Il est necessaire de trier le tableau, puisque le petit algorithme que j'ai ecrit pour creer les positions des geom_tile pour le ggplot s'execute dans l'ordre du tableau !
  }

  # Si facets
  if (reorder == T & !quo_is_null(quo_facet)) {
    tab <- tab |>
      group_by({{ var}}) |>
      mutate(
        the_medians = median({{ value }}, na.rm = TRUE) # On calcule la mediane par groupe (entre les facettes) => utilise pour la palette
        ) |>
      # Il est necessaire de trier le tableau, puisque le petit algorithme que j'ai ecrit pour creer les positions des geom_tile pour le ggplot s'execute dans l'ordre du tableau !
      # On ordonne par valeur => ordre different par facette, c'est voulu !
      arrange({{ facet }}, {{ value }}) |>
      ungroup()

    # Pour creer un ordre de couleurs pour {{ var }}
    tab_levels <- tab |>
      distinct({{ var }}, the_medians) |> # On cree un ordre selon la mediane par groupe => utile pour avoir un ordre de palette coherent
      arrange(the_medians) |>
      select(order = 1)

    # On reordonne le facteur selon la mediane en introduisant le vecteur cree ci-dessus
    tab <- tab |>
      mutate(
        "{{ var }}" := factor({{ var }}, levels = tab_levels$order)
      )

  }

  # Si CI pas affiches (ou affiches mais variables pas indiquees)
  if (show_ci == F | (show_ci == T & (quo_is_null(quo_low) | quo_is_null(quo_up)))) {
    tab <- tab |>
      mutate(
        indice_sqrt = sqrt({{ value }}) # La valeur a la racine carree (car la valeur en surface = racine carree X racine carree)
      )
  }

  # Si CI affiches ET variables indiquees
  if (show_ci == T & !quo_is_null(quo_low) & !quo_is_null(quo_up)) {
    tab <- tab |>
      mutate(
        indice_sqrt = sqrt({{ error_upp }}) # Si les CI sont actives, la base du calcul des coordonnees = l'intervalle de confiance superieur, car il dessine les plus grandes surfaces
      )
  }


  # 3. CREATION DES POSITIONS POUR LES SURFACES --------------------

  # On calcule un espace par defaut, si rien n'est indique
  if (is.null(space)) {
    space <- .15 * min(tab$indice_sqrt, na.rm = T)
  }

  # S'il y a des facets
  if (!quo_is_null(quo_facet)) {
    facet_vec <- as.character(unique(tab[[deparse(substitute(facet))]])) # Vecteur qui liste les modalites de la variable des facettes
  }
  # Si pas de de facets
  if (quo_is_null(quo_facet)) {
    facet_vec <- 1   # Dans ce cas, on met une valeur factice unique a facet_vec pour que la boucle ne fasse qu'un tour ! (economie de code : le meme pour facets et non facets)
  }

  res <- tibble()
  for(i in facet_vec){

    if (!quo_is_null(quo_facet)) {
      temp <- tab |>
        filter({{facet}} == i) # On filtre pour la facet i (et on fait comme ca chaque facet)
    }
    if (quo_is_null(quo_facet)) {
      temp <- tab
    }

    # L'algorithme pour creer les positions des geom_tile pour le ggplot
    temp$xmin <- NA
    temp$xmax <- NA
    temp$xmin[1] <- 0
    temp <- temp |> tibble::add_row() # On ajoute une ligne

    for (i in seq_along(utils::head(temp, -1)[[deparse(substitute(var))]])) {
      temp$xmin[i + 1] <- temp$xmin[i] + temp$indice_sqrt[i] # On calcule les coord xmin et xmax de chaque surface, sur base de indice_sqrt
      temp$xmax[i] <- temp$xmin[i + 1]

      temp$xmin[i + 1] <- temp$xmin[i + 1] + space
    }
    temp <- utils::head(temp, -1) # On supprime la ligne ajoutee
    temp$xmean <- (temp$xmin + temp$xmax) / 2 # On calcule la valeur centrale en faisant la moyenne de xmin et xmax
    temp <- temp |>
      mutate(compare = sqrt(min({{ value }})),  # On calcule le min (pour la comparaison graphique) => automatiquement fait PAR FACET si facet = non-NULL
      row_num = row_number() - 1) # On determine le numero de la ligne - 1 (pour la justification des facettes ci-dessous)

    res <- rbind(res, temp) # On aggrege les resultats par facette
  }
  tab <- res

  # On aligne les facets => Justification de chaque ligne
  if (!quo_is_null(quo_facet)) {
    xmax_facet <- max(tab$xmax) # la valeur max de la facet => definit la largeur max et donc la justification necessaire des autres facets
    tab <- tab |>
      group_by({{facet}}) |>
      mutate(diff = abs(max(xmax) - xmax_facet), # On calcule l'ecart entre la facet max et la facet actuelle
             incr_unit = diff / sum(row_num), # On calcule une incrementation minimale
             row_coef = row_num * (sum(row_num) / max(row_num)), # On calcule un coefficient pour que coef X incrementation augmente progressivement par groupe pour arriver a diff
             xmin = xmin + (incr_unit*row_coef), # On recalcule xmin, xmax et xmean
             xmax = xmax + (incr_unit*row_coef),
             xmean = (xmin + xmax) / 2
      ) |>
      ungroup()
  }
  print(tab)


  # 4. CREATION DU GRAPHIQUE --------------------

  # On cree la palette avec le package MetBrewer
  # /!\ NOTE : on met unique() car avec facet il y a les modalites en double !
  if(pal %in% names(MetBrewer::MetPalettes)){
    palette <- as.character(MetBrewer::met.brewer(name = pal, n = length(unique(tab[[deparse(substitute(var))]])), type = "continuous", direction = direction))

    # On cree la palette avec le package MoMAColors
  } else if(pal %in% names(MoMAColors::MoMAPalettes)){
    palette <- as.character(MoMAColors::moma.colors(palette_name = pal, n = length(unique(tab[[deparse(substitute(var))]])), type = "continuous", direction = direction))

    # On cree la palette avecle package PrettyCols
  } else if(pal %in% names(PrettyCols::PrettyColsPalettes)){
    palette <- as.character(PrettyCols::prettycols(palette = pal, n = length(unique(tab[[deparse(substitute(var))]])), type = "continuous", direction = direction))

    # On cree la palette avec la fonction interne official_pal()
  } else if(pal %in% official_pal(list_pal_names = T)){
    palette <- as.character(official_pal(inst = pal, n = length(unique(tab[[deparse(substitute(var))]])), direction = direction))

  } else {
    palette <- as.character(MetBrewer::met.brewer(name = "Kandinsky", n = length(unique(tab[[deparse(substitute(var))]])), type = "continuous", direction = direction))
    warning("La palette indiquee dans pal n'existe pas : la palette par defaut est utilisee")
  }

  # On cree le graphique

  # On charge les polices
  load_and_active_fonts()

  graph <- tab |>
    ggplot(
      aes(
        color = {{ var }}
      )
    ) +
    geom_tile( # Le fond blanc
      aes(
        x = xmean,
        y = if (position == "mid") {
          0
        } else if (position == "bottom") indice_sqrt / 2,
        width = sqrt({{ value }}),
        height = sqrt({{ value }})
      ),
      fill = "white",
      linewidth = NA
    )

  # Comparaison avec la surface minimale
  if (compare == T) {
    graph <- graph +
      geom_tile(
        aes(
          x = if (position == "mid") {
            xmean
          } else if (position == "bottom" & show_ci == T) { xmin + (compare/2) + (sqrt({{ error_upp }}) - sqrt({{ value }}))/2
          } else if (position == "bottom" & show_ci == F) xmin + (compare/2),
          y = if (position == "mid") {
            0
          } else if (position == "bottom" & show_ci == T) { compare / 2 + (sqrt({{ error_upp }}) - sqrt({{ value }}))/2
          } else if (position == "bottom" & show_ci == F) compare / 2,
          width = compare,
          height = compare
        ),
        alpha = .1,
        fill = "black",
        linewidth = NA,
      )
  }

  graph <- graph +
    geom_tile( # Le contour de couleur des surfaces
      aes(
        x = xmean,
        y = if (position == "mid") {
          0
        } else if (position == "bottom") indice_sqrt / 2,
        width = sqrt({{ value }}),
        height = sqrt({{ value }})
      ),
      fill = NA,
      linewidth = .75
    ) +
    scale_color_manual(values = palette) +
    scale_x_continuous(expand = c(0.01, 0.01)) +
    theme_void() +
    theme(
      legend.position = "none",
      plot.background = element_rect(fill = bg, color = NA),
      text = element_text(family = font),
      plot.caption = element_text(
        color = "grey30"
      )
    ) +
    # guides(fill="none") +
    labs(
      title = title,
      subtitle = subtitle
    ) +
    coord_fixed(ratio = 1)

  # Pour caption

  if (!is.null(caption) & !is.null(pvalue)) { # Permet de passer a la ligne par rapport au test stat
    caption <- paste0("\n", stringr::str_wrap(caption, width = 100))
  }
  if (!is.null(pvalue)) {
    graph <- graph +
      labs(
        caption = paste0(
          "H0 : ", scales::pvalue(pvalue, add_p = T),
          caption
        )
      )
  }
  if (is.null(pvalue)) {
    graph <- graph +
      labs(
        caption = stringr::str_wrap(caption, width = 100)
        )
  }

  # Ajouter les facets au besoin + scale_y si facet
  if (!quo_is_null(quo_facet)) {
    graph <- graph +
      facet_wrap(vars({{ facet }}), ncol = 1)
  }

  # Les labels
  graph <- graph +
    geom_text(
      aes(
        x = xmean,
        y = if (position == "mid") {
          0
        } else if (position == "bottom") indice_sqrt / 2,
        label = if (show_ci == TRUE) {
          paste0(
            stringr::str_wrap({{ var }}, wrap_width_lab), "\n", stringr::str_wrap(paste0(round({{ value }}, digits), unit, " (", round({{ error_low }}, digits), ";", round({{ error_upp }}, digits), ")"), wrap_width_lab)
          )
        } else if (show_ci == FALSE) paste0(stringr::str_wrap({{ var }}, wrap_width_lab), "\n", stringr::str_wrap(paste0(round({{ value }}, digits), unit), wrap_width_lab))
      ),
      family = font
    )

  # Les IC si show_ci = T
  if (show_ci == T) {
    graph <- graph +
      geom_tile(
        aes(
          x = xmean,
          y = if (position == "mid") {
            0
          } else if (position == "bottom") indice_sqrt / 2,
          width = indice_sqrt,
          height = indice_sqrt
        ),
        alpha = .3,
        fill = NA,
        linewidth = .5,
        linetype = "longdash"
      ) +
      geom_tile(
        aes(
          x = xmean,
          y = if (position == "mid") {
            0
          } else if (position == "bottom") indice_sqrt / 2,
          width = sqrt({{ error_low }}),
          height = sqrt({{ error_low }})
        ),
        alpha = .3,
        fill = NA,
        linewidth = .5,
        linetype = "longdash"
      )
  }

  return(graph)
}
