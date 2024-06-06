#' make_surface
#'
#'
#' @param tab
#' @param var
#' @param value
#' @param facet A variable in tab defining the faceting group, if applicable. Default is NULL.
#' @param error_low
#' @param error_upp
#' @param pvalue
#' @param compare
#' @param reorder
#' @param show_ci
#' @param space
#' @param position
#' @param digits
#' @param unit
#' @param pal
#' @param bg
#' @param direction
#' @param title
#' @param subtitle
#' @param caption
#' @param wrap_width_lab
#'
#' @return
#' @import dplyr
#' @import ggplot2
#' @import rlang
#' @export
#'
#' @examples
make_surface <- function(tab,
                         var,
                         value,
                         facet = NULL,
                         error_low = NULL,
                         error_upp = NULL,
                         pvalue = NULL,
                         name_total = "Total",
                         compare = F,
                         reorder = F,
                         show_ci = TRUE,
                         space = NULL,
                         position = "mid",
                         digits = 0,
                         unit = NULL,
                         pal = "Kandinsky",
                         bg = "snow2",
                         direction = 1,
                         title = NULL,
                         subtitle = NULL,
                         caption = NULL,
                         wrap_width_lab = 20) {

  # 1. CHECKS DES ARGUMENTS --------------------

  # Check des arguments necessaires
  if((missing(tab) | missing(var) | missing(value)) == TRUE){
    stop("Les arguments tab, var et value doivent etre remplis")
  }

  # On cree des quosures => pour if statements dans la fonction (voir ci-dessous)
  # Solution trouvee ici : https://rpubs.com/tjmahr/quo_is_missing
  quo_facet <- enquo(facet)
  quo_low <- enquo(error_low)
  quo_up <- enquo(error_upp)


  # 2. PROCESSING DES DONNEES --------------------

  # On enleve le total
  tab <- tab %>%
    filter(
      {{ var }} != name_total
    )

  # On convertit la variable categorielle en facteur si pas facteur
  tab <- tab %>%
    mutate(
      "{{ var }}" := droplevels(as.factor({{ var }})) # droplevels pour eviter qu'un level soit encode alors qu'il n'a pas d'effectifs
    )

  # On reordonne si reorder == T
  if (reorder == T & quo_is_null(quo_facet)) {
    tab <- tab %>%
      mutate(
        "{{ var }}" := forcats::fct_reorder({{ var }}, {{ value }}) # Je pense que c'est necessaire pour le ggplot et la palette ?
      ) %>%
      arrange({{ value }}) # Il est necessaire de trier le tableau, puisque le petit algorithme que j'ai ecrit pour creer les positions des geom_tile pour le ggplot s'execute dans l'ordre du tableau !
  }

  if (reorder == T & !quo_is_null(quo_facet)) {
    message("Attention : reorder n'est pas disponible avec les facets")
  }

  # Si CI pas affiches (ou affiches mais variables pas indiquees)
  if (show_ci == F | (show_ci == T & (quo_is_null(quo_low) | quo_is_null(quo_up)))) {
    tab <- tab %>%
      mutate(
        indice_sqrt = sqrt({{ value }}) # La valeur a la racine carree (car la valeur en surface = racine carree X racine carree)
      )
  }

  # Si CI affiches ET variables indiquees
  if (show_ci == T & !quo_is_null(quo_low) & !quo_is_null(quo_up)) {
    tab <- tab %>%
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
      temp <- tab %>%
        filter({{facet}} == i) # On filtre pour la facet i (et on fait comme ca chaque facet)
    }
    if (quo_is_null(quo_facet)) {
      temp <- tab
    }

    # L'algorithme pour creer les positions des geom_tile pour le ggplot
    temp$xmin <- NA
    temp$xmax <- NA
    temp$xmin[1] <- 0
    temp <- temp %>% tibble::add_row() # On ajoute une ligne

    for (i in seq_along(utils::head(temp, -1)[[deparse(substitute(var))]])) {
      temp$xmin[i + 1] <- temp$xmin[i] + temp$indice_sqrt[i] # On calcule les coord xmin et xmax de chaque surface, sur base de indice_sqrt
      temp$xmax[i] <- temp$xmin[i + 1]

      temp$xmin[i + 1] <- temp$xmin[i + 1] + space
    }
    temp <- utils::head(temp, -1) # On supprime la ligne ajoutee
    temp$xmean <- (temp$xmin + temp$xmax) / 2 # On calcule la valeur centrale en faisant la moyenne de xmin et xmax
    temp <- temp %>%
      mutate(compare = sqrt(min({{ value }})),  # On calcule le min (pour la comparaison graphique) => automatiquement fait PAR FACET si facet = non-NULL
      row_num = row_number() - 1) # On determine le numero de la ligne - 1 (pour la justification des facettes ci-dessous)

    res <- rbind(res, temp) # On aggrege les resultats par facette
  }
  tab <- res

  # On aligne les facets => Justification de chaque ligne
  if (!quo_is_null(quo_facet)) {
    xmax_facet <- max(tab$xmax) # la valeur max de la facet => definit la largeur max et donc la justification necessaire des autres facets
    tab <- tab %>%
      group_by({{facet}}) %>%
      mutate(diff = abs(max(xmax) - xmax_facet), # On calcule l'ecart entre la facet max et la facet actuelle
             incr_unit = diff / sum(row_num), # On calcule une incrementation minimale
             row_coef = row_num * (sum(row_num) / max(row_num)), # On calcule un coefficient pour que coef X incrementation augmente progressivement par groupe pour arriver a diff
             xmin = xmin + (incr_unit*row_coef), # On recalcule xmin, xmax et xmean
             xmax = xmax + (incr_unit*row_coef),
             xmean = (xmin + xmax) / 2
      ) %>%
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
    palette <- as.character(PrettyCols::prettycols(name = pal, n = length(unique(tab[[deparse(substitute(var))]])), type = "continuous", direction = direction))

    # On cree la palette avec la fonction interne official_pal()
  } else if(pal %in% c("OBSS", "IBSA")){
    palette <- as.character(official_pal(inst = pal, n = length(unique(tab[[deparse(substitute(var))]])), direction = direction))

  } else {
    palette <- as.character(MetBrewer::met.brewer(name = "Kandinsky", n = length(unique(tab[[deparse(substitute(var))]])), type = "continuous", direction = direction))
    warning("La palette indiquee dans pal n'existe pas : la palette par defaut est utilisee")
  }

  # On cree le graphique

  graph <- tab %>%
    ggplot(
      aes(
        color = {{ var }}
      )
    ) +
    geom_tile(
      aes(
        x = xmean,
        y = if (position == "mid") {
          0
        } else if (position == "bottom") indice_sqrt / 2,
        width = sqrt({{ value }}),
        height = sqrt({{ value }})
      ),
      fill = "white",
      linewidth = .75
    ) +
    scale_color_manual(values = palette) +
    scale_x_continuous(expand = c(0.01, 0.01)) +
    theme_void() +
    theme(
      legend.position = "none",
      plot.background = element_rect(fill = bg, color = NA)
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

  # Comparaison avec la surface minimale
  if (compare == T) {
    graph <- graph +
      geom_tile(
        aes(
          x = xmean,
          y = if (position == "mid") {
            0
          } else if (position == "bottom") indice_sqrt / 2,
          width = compare,
          height = compare
        ),
        alpha = .1,
        fill = "black",
        linewidth = NA,
      )
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
      )
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
