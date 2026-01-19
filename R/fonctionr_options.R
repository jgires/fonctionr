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
