#' make_surface
#'
#' @param data
#' @param IC
#' @param space
#' @param digits
#' @param unit
#' @param pal
#' @param direction
#' @param reorder
#' @param title
#' @param subtitle
#' @param position
#' @param last_del
#' @param caption
#' @param wrap_lab
#'
#' @return
#' @export
#'
#' @examples
make_surface <- function(data,
                           IC = TRUE,
                           space = NULL,
                           reorder = F,
                           position = "mid",
                           digits = 0,
                           unit = NULL,
                           pal = "Kandinsky",
                           direction = 1,
                           last_del = 1,
                           title = NULL,
                           subtitle = NULL,
                           caption = NULL,
                           wrap_lab = 20) {

  data <- head(data, -last_del)

  if (reorder == T) {
    data[[1]] <- reorder(data[[1]], data[[2]])
    data <- data[order(data[[2]]), ]
  }

  if (IC == F) {
    data$label <- paste0(stringr::str_wrap(data[[1]], wrap_lab), "\n", round(data[[2]], digits), unit)
  }

  if (IC == T) {
    data$label <- paste0(stringr::str_wrap(data[[1]], wrap_lab), "\n", round(data[[2]], digits), unit, " (", round(data[[3]], digits), ";", round(data[[4]], digits), ")")
  }

  if (IC == F) {
    data$indice_sqrt <- sqrt(data[[2]])
  }
  if (IC == T) {
    data$indice_sqrt <- sqrt(data[[4]])
  }

  if (is.null(space)) {
    space <- .15 * min(data$indice_sqrt, na.rm = T)
  }

  data$xmin <- NA
  data$xmax <- NA
  data$xmin[1] <- 0
  data <- data %>% tibble::add_row() # On ajoute une ligne

  for (i in seq_along(head(data, -1)[[1]])) {
    data$xmin[i + 1] <- data$xmin[i] + data$indice_sqrt[i]
    data$xmax[i] <- data$xmin[i + 1]

    data$xmin[i + 1] <- data$xmin[i + 1] + space
  }
  data <- head(data, -1) # On supprime la dernière ligne
  data$xmean <- (data$xmin + data$xmax) / 2

  names(data)[1] <- "group"
  names(data)[2] <- "indice"
  names(data)[3] <- "indice_low"
  names(data)[4] <- "indice_upp"

  print(data)

  graph <- data %>%
    ggplot(
      aes(
        color = group
      )
    ) +
    # geom_hline(
    #   yintercept = c(sqrt(space_quant$tab$indice[[1]]) / 2, -sqrt(space_quant$tab$indice[[1]]) / 2),
    #   alpha = .2
    # ) +
    geom_tile(
      aes(
        x = xmean,
        y = if (position == "mid") { 0
        } else if (position == "bottom") indice_sqrt / 2,
        width = sqrt(indice),
        height = sqrt(indice)
      ),
      fill = "white",
      linewidth = .75
    ) +
    geom_text(
      aes(
        x = xmean,
        y = if (position == "mid") { 0
        } else if (position == "bottom") indice_sqrt / 2,
        label = label
      )
    ) +
    scale_color_manual(values = MetBrewer::met.brewer(pal, length(data$group), direction = direction)) +
    scale_x_continuous(expand = c(0.01, 0.01)) +
    theme_void() +
    theme(
      legend.position = "none",
      plot.background = element_rect(fill = "snow2", color = NA)
    ) +
    # guides(fill="none") +
    labs(
      title = title,
      subtitle = subtitle,
      caption = caption
    ) +
    coord_fixed(ratio = 1)

  if(IC == T){
    graph <- graph +
      geom_tile(
        aes(
          x = xmean,
          y = if (position == "mid") { 0
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
          y = if (position == "mid") { 0
          } else if (position == "bottom") indice_sqrt / 2,
          width = sqrt(indice_low),
          height = sqrt(indice_low)
        ),
        alpha = .3,
        fill = NA,
        linewidth = .5,
        linetype = "longdash"
      )
  }

  return(graph)
}
