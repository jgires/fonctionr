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
                          type,
                          facet = NULL,
                          quantiles = c(.25, .5, .75),
                          filter_exp = NULL,
                          ...,
                          na.rm.facet = T,
                          show_ci = T,
                          show_n = FALSE,
                          show_value = TRUE,
                          show_lab = TRUE,
                          digits = 0,
                          unit = "",
                          dec = ",",
                          pal = NULL,
                          font ="Roboto",
                          title = NULL,
                          subtitle = NULL,
                          xlab = NULL,
                          ylab = NULL,
                          caption = NULL,
                          export_path = NULL) {

  # On convertit d'abord en objet srvyr
  data_W <- convert_to_srvyr(data, ...)

  # On recalcule quanti_exp dans une variable unique si c'est une expression à la base => nécessaire pour calculer la densité
  data_W <- data_W %>%
    mutate(
      quanti_exp_flattened = {{ quanti_exp }}
    )

  var_weights <- as.character(substitute(...()))[names(as.list(substitute(...()))) %in% c("weights", "weight")]

  # Estimated probability density
  estDensity <- density(data_W$variables[["quanti_exp_flattened"]],
    adjust = 1,
    n = 4096,
    weights = data_W$variables[[var_weights]],
    na.rm = T
  )

  estQuant_W <- as.numeric(svyquantile(~quanti_exp_flattened,
    design = data_W,
    quantiles = quantiles,
    ci = F,
    na.rm = T
  )[[1]])

  df_dens <- data.frame(
    x = estDensity$x,
    y = estDensity$y / sum(estDensity$y),
    quantFct = as.factor(findInterval(estDensity$x, estQuant_W) + 1)
  )

  # sum(df_dens$y)

  ggplot(
    data = df_dens
  ) +
    geom_line(
      aes(
        x = x,
        y = y
      )
    ) +
    geom_area(
      aes(
        x = x, y = y, fill = quantFct
      ),
      alpha = 1
    ) +
    scale_x_continuous(
      breaks = estQuant_W
    ) +
    scale_fill_manual(
      name = "",
      values = c("green", "yellow", "orange", "red")
    ) +
    labs(title = "Probability Density") +
    xlab("Numeric variable") +
    ylab("Estimated Density") +
    theme_minimal()

}
