#' fonctionr_options
#'
#' Function to set global options for fonctionr
#'
#' @param total total
#' @param prop_method prop_method
#' @param reorder reorder
#' @param show_ci show_ci
#' @param show_n show_n
#' @param show_value show_value
#' @param show_labs show_labs
#' @param dec dec
#' @param pal pal
#' @param col col
#' @param font Font used in the graphic. See load_and_active_fonts() for available fonts.
#' @param coef_font A multiplier factor for font size
#' @param caption caption
#' @param theme theme
#' @param lang The language of the indications on the chart. Possibilities: "fr", "nl", "en". Default is "fr".
#' @param erase_all TRUE erases all the options. Default is FALSE.
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
fonctionr_options <- function(total = NULL,
                              prop_method = NULL,
                              reorder = NULL,
                              show_ci = NULL,
                              show_n = NULL,
                              show_value = NULL,
                              show_labs = NULL,
                              dec = NULL,
                              pal = NULL,
                              col = NULL,
                              font = NULL,
                              coef_font = NULL,
                              caption = NULL,
                              theme = NULL,
                              lang = NULL,
                              erase_all = FALSE) {

  # On enregistre le call
  call_options <- match.call()
  # On cree une liste avec le nom des options definies par l'utilisateur + leur valeur
  options.args <- as.list(call_options[-1]) # -1 pour enlever le nom de la fonction
  # On enleve erase_all (qu'on ne veut pas ajouter aux options generales)
  options.args <- options.args[names(options.args) != "erase_all"]


  if(erase_all == TRUE){
    # Pour le erase_all : uniquement si pas d'options renseignees dans le call
    if(length(options.args) == 0){
      options(fonctionr.options = NULL)
    }
    # Sinon erreur
    if(length(options.args) > 0){
      stop("Impossible d'effacer et d'activer des options simultanement")
    }
  }
  if(erase_all == FALSE){
    # SSI il y a des options renseignees dans le call => sinon il efface alors qu'on ne fait que demander les options actives
    if(length(options.args) > 0){
      # On ajoute les options de fonctionr_options() aux options generales
      options(fonctionr.options = options.args)
    }
  }

  # On affiche les options actives de fonctionr
  if(length(options()[names(options()) == "fonctionr.options"]) > 0){
    options()[names(options()) == "fonctionr.options"]
  } else {
    message("Aucune option de fonctionr active")
  }

}
