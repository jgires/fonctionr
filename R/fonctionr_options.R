#' fonctionr_options
#'
#' Function to set global options for fonctionr
#'
#' @param font Font used in the graphic. See load_and_active_fonts() for available fonts.
#' @param coef_font A multiplier factor for font size
#' @param lang The language of the indications on the chart. Possibilities: "fr", "nl", "en". Default is "fr".
#' @param reset TRUE erases all the options. Default is FALSE.
#'
#' @export
#'
#' @examples
#' # We set settings for font type and font size
#' fonctionr_options(font = "Montserrat", coef_font = 1.5)
#'
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
#' weight = rb050,
#' title = "% of ind. receiving unemployment benefits in their hh",
#' subtitle = "Example with austrian SILC data from 'laeken' package"
#' )
#'
#' # Results in graph form
#' eusilc_prop$graph
#'
fonctionr_options <- function(font = NULL,
                              coef_font = NULL,
                              lang = NULL,
                              reset = FALSE) {

  if(!is.null(font)){
    options(fonctionr.font = font)
  }
  if(!is.null(coef_font)){
    options(fonctionr.coef_font = coef_font)
  }
  if(!is.null(lang)){
    options(fonctionr.lang = lang)
  }

  if(reset){
    options(fonctionr.font = NULL)
    options(fonctionr.coef_font = NULL)
    options(fonctionr.lang = NULL)
  }

}
