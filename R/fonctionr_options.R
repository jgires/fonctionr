#' fonctionr_options
#'
#' Function to set global options for fonctionr. The arguments defined in the options are only active if the user has not manually specified a value for those arguments within the various functions. Arguments may be shared by multiple functions (if they have the same name) or specific to certain functions.
#'
#' @param na.rm.group `na.rm.group` argument.
#' @param na.rm.facet `na.rm.facet` argument.
#' @param na.prop `na.prop` argument.
#' @param na.vars `na.vars` argument.
#' @param na.rm.var `na.rm.var` argument.
#' @param probs `probs` argument.
#' @param total `total` argument.
#' @param prop_method `prop_method` argument.
#' @param quantiles `quantiles` argument.
#' @param moustache_probs `moustache_probs` argument.
#' @param bw `bw` argument.
#' @param resolution `resolution` argument.
#' @param height `height` argument.
#' @param limits `limits` argument.
#' @param reorder `reorder` argument.
#' @param position `position` argument.
#' @param show_ci `show_ci` argument.
#' @param show_mid_point `show_mid_point` argument.
#' @param show_mid_line `show_mid_line` argument.
#' @param show_ci_errorbar `show_ci_errorbar` argument.
#' @param show_ci_lines `show_ci_lines` argument.
#' @param show_ci_area `show_ci_area` argument.
#' @param show_quant_lines `show_quant_lines` argument.
#' @param show_moustache `show_moustache` argument.
#' @param show_n `show_n` argument.
#' @param show_value `show_value` argument.
#' @param show_labs `show_labs` argument.
#' @param total_name `total_name` argument.
#' @param scale `scale` argument.
#' @param digits `digits` argument.
#' @param unit `unit` argument.
#' @param dec `dec` argument.
#' @param col `col` argument.
#' @param pal `pal` argument.
#' @param direction `direction` argument.
#' @param desaturate `desaturate` argument.
#' @param lighten `lighten` argument.
#' @param darken `darken` argument.
#' @param col_density `col_density` argument.
#' @param col_moustache `col_moustache` argument.
#' @param col_border `col_border` argument.
#' @param alpha `alpha` argument.
#' @param dodge `dodge` argument.
#' @param font `font` argument.
#' @param wrap_width_y `wrap_width_y` argument.
#' @param wrap_width_leg `wrap_width_leg` argument.
#' @param legend_ncol `legend_ncol` argument.
#' @param title `title` argument.
#' @param subtitle `subtitle` argument.
#' @param xlab `xlab` argument.
#' @param ylab `ylab` argument.
#' @param legend_lab `legend_lab` argument.
#' @param caption `caption` argument.
#' @param lang `lang` argument.
#' @param theme `theme` argument.
#' @param coef_font `coef_font` argument.
#' @param parallel `parallel` argument.
#' @param erase_all `TRUE` erases all the options. Default is `FALSE`.
#'
#' @return No return value, called for side effects.
#' @export
#'
#' @examples
#' # We set global settings
#' fonctionr_options(coef_font = 1.5, col = "magenta", caption = "Beautiful caption")
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
#'   eusilc,
#'   group = pl030_rec,
#'   prop_exp = py090n > 0,
#'   weight = rb050,
#'   title = "% of ind. receiving unemployment benefits in their hh",
#'   subtitle = "Example with austrian SILC data from 'laeken' package"
#' )
#'
#' # Results in graph form
#' eusilc_prop$graph
#'
#' # We set back settings to default
#' fonctionr_options(erase_all = TRUE)

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
                              parallel = NULL,
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
