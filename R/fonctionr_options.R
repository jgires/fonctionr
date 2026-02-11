#' fonctionr_options
#'
#' Function to set global options for fonctionr
#'
#' @param na.rm.group na.rm.group
#' @param na.rm.facet na.rm.facet
#' @param na.prop na.prop
#' @param na.vars na.vars
#' @param na.rm.var na.rm.var
#' @param probs probs
#' @param total total
#' @param prop_method prop_method
#' @param quantiles quantiles
#' @param moustache_probs moustache_probs
#' @param bw bw
#' @param resolution resolution
#' @param height height
#' @param limits limits
#' @param reorder reorder
#' @param position position
#' @param show_ci show_ci
#' @param show_mid_point show_mid_point
#' @param show_mid_line show_mid_line
#' @param show_ci_errorbar show_ci_errorbar
#' @param show_ci_lines show_ci_lines
#' @param show_ci_area show_ci_area
#' @param show_quant_lines show_quant_lines
#' @param show_moustache show_moustache
#' @param show_n show_n
#' @param show_value show_value
#' @param show_labs show_labs
#' @param total_name total_name
#' @param scale scale
#' @param digits digits
#' @param unit unit
#' @param dec dec
#' @param col col
#' @param pal pal
#' @param direction direction
#' @param desaturate desaturate
#' @param lighten lighten
#' @param darken darken
#' @param col_density col_density
#' @param col_moustache col_moustache
#' @param col_border col_border
#' @param alpha alpha
#' @param dodge dodge
#' @param font font
#' @param wrap_width_y wrap_width_y
#' @param wrap_width_leg wrap_width_leg
#' @param legend_ncol legend_ncol
#' @param title title
#' @param subtitle subtitle
#' @param xlab xlab
#' @param ylab ylab
#' @param legend_lab legend_lab
#' @param caption caption
#' @param lang lang
#' @param theme theme
#' @param coef_font coef_font
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
fonctionr_options <- function(na.rm.group = NULL,
                              na.rm.facet = NULL,
                              na.prop = NULL,
                              na.vars = NULL,
                              na.rm.var = NULL,
                              probs = NULL,
                              total = NULL,
                              prop_method = NULL,
                              quantiles = NULL,
                              moustache_probs = NULL,
                              bw = NULL,
                              resolution = NULL,
                              height = NULL,
                              limits = NULL,
                              reorder = NULL,
                              position = NULL,
                              show_ci = NULL,
                              show_mid_point = NULL,
                              show_mid_line = NULL,
                              show_ci_errorbar = NULL,
                              show_ci_lines = NULL,
                              show_ci_area = NULL,
                              show_quant_lines = NULL,
                              show_moustache = NULL,
                              show_n = NULL,
                              show_value = NULL,
                              show_labs = NULL,
                              total_name = NULL,
                              scale = NULL,
                              digits = NULL,
                              unit = NULL,
                              dec = NULL,
                              col = NULL,
                              pal = NULL,
                              direction = NULL,
                              desaturate = NULL,
                              lighten = NULL,
                              darken = NULL,
                              col_density = NULL,
                              col_moustache = NULL,
                              col_border = NULL,
                              alpha = NULL,
                              dodge = NULL,
                              font = NULL,
                              wrap_width_y = NULL,
                              wrap_width_leg = NULL,
                              legend_ncol = NULL,
                              title = NULL,
                              subtitle = NULL,
                              xlab = NULL,
                              ylab = NULL,
                              legend_lab = NULL,
                              caption = NULL,
                              lang = NULL,
                              theme = NULL,
                              coef_font = NULL,
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
      stop("Not possible to to delete and activate options simultaneously")
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
    message("No fonctionr option active")
  }

}
