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

  # On convertit la variable categorielle en facteur si pas facteur
  tab <- tab %>%
    mutate(
      "{{ var }}" := droplevels(as.factor({{ var }})) # droplevels pour eviter qu'un level soit encode alors qu'il n'a pas d'effectifs
    )

  # On reordonne si reorder == T
  if (reorder == T & quo_is_null(quo_facet)) {
    tab <- tab %>%
      mutate(
        "{{ var }}" := forcats::fct_reorder({{ var }}, {{ value }})
      ) %>%
      arrange({{ value }}) # Il est necessaire de trier le tableau, puisque le petit algorithme que j'ai ecrit pour creer les positions des geom_tile pour le ggplot s'execute dans l'ordre du tableau !
  }

  if (reorder == T & !quo_is_null(quo_facet)) {
    message("Note : reorder n'est pas disponible avec les facets")
  }

  if (show_ci == F | (show_ci == T & (quo_is_null(quo_low) | quo_is_null(quo_up)))) {
    tab <- tab %>%
      mutate(
        indice_sqrt = sqrt({{ value }}) # La valeur a la racine carree (car la valeur en surface = racine carree X racine carree)
      )
  }

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

  # Si pas de de facets
  if (quo_is_null(quo_facet)) {

    # L'algorithme pour creer les positions des geom_tile pour le ggplot
    tab$xmin <- NA
    tab$xmax <- NA
    tab$xmin[1] <- 0
    tab <- tab %>% tibble::add_row() # On ajoute une ligne

    for (i in seq_along(utils::head(tab, -1)[[deparse(substitute(var))]])) {
      tab$xmin[i + 1] <- tab$xmin[i] + tab$indice_sqrt[i]
      tab$xmax[i] <- tab$xmin[i + 1]

      tab$xmin[i + 1] <- tab$xmin[i + 1] + space
    }
    tab <- utils::head(tab, -1) # On supprime la ligne ajoutee
    tab$xmean <- (tab$xmin + tab$xmax) / 2
  }

  # S'il y a des facets
  if (!quo_is_null(quo_facet)) {

    facet_vec <- as.character(unique(tab[[deparse(substitute(facet))]]))

    res <- tibble()
    for(i in facet_vec){
      temp <- tab %>%
        filter({{facet}} == i)

      # L'algorithme pour creer les positions des geom_tile pour le ggplot
      temp$xmin <- NA
      temp$xmax <- NA
      temp$xmin[1] <- 0
      temp <- temp %>% tibble::add_row() # On ajoute une ligne

      for (i in seq_along(utils::head(temp, -1)[[deparse(substitute(var))]])) {
        temp$xmin[i + 1] <- temp$xmin[i] + temp$indice_sqrt[i]
        temp$xmax[i] <- temp$xmin[i + 1]

        temp$xmin[i + 1] <- temp$xmin[i + 1] + space
      }
      temp <- utils::head(temp, -1) # On supprime la ligne ajoutee
      temp$xmean <- (temp$xmin + temp$xmax) / 2

      res <- rbind(res, temp)
    }
    tab <- res
  }


  # 4. CREATION DU GRAPHIQUE --------------------

  # On cree la palette avec le package MetBrewer
  if(pal %in% names(MetBrewer::MetPalettes)){
    palette <- as.character(MetBrewer::met.brewer(name = pal, n = length(tab[[deparse(substitute(var))]]), type = "continuous", direction = direction))

    # On cree la palette avec le package MoMAColors
  } else if(pal %in% names(MoMAColors::MoMAPalettes)){
    palette <- as.character(MoMAColors::moma.colors(palette_name = pal, n = length(tab[[deparse(substitute(var))]]), type = "continuous", direction = direction))

    # On cree la palette avecle package PrettyCols
  } else if(pal %in% names(PrettyCols::PrettyColsPalettes)){
    palette <- as.character(PrettyCols::prettycols(name = pal, n = length(tab[[deparse(substitute(var))]]), type = "continuous", direction = direction))

    # On cree la palette avec la fonction interne official_pal()
  } else if(pal %in% c("OBSS", "IBSA")){
    palette <- as.character(official_pal(inst = pal, n = length(tab[[deparse(substitute(var))]]), direction = direction))

  } else {
    palette <- as.character(MetBrewer::met.brewer(name = "Kandinsky", n = length(tab[[deparse(substitute(var))]]), type = "continuous", direction = direction))
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
          width = sqrt(min({{ value }})),
          height = sqrt(min({{ value }}))
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
