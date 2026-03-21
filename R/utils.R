#' convert_to_srvyr
#'
#' Internal function to convert data into srvyr object
#'
#' @param data A data.frame or an object from the survey package or an object from the srvyr package.
#' @param ... All options possible in as_survey_design() in srvyr package.
#'
#' @import srvyr
#'
#' @noRd
#'
convert_to_srvyr <- function(data, ...) {

  # Petite fonction utile
  `%ni%` = Negate(`%in%`)

  # Si objet survey (avec replicates ou non)
  if(any(class(data) %in% c("survey.design2","survey.design")) & all(class(data) %ni% c("tbl_svy"))){
    message("Input: survey object")
    data_W <- data |>
      as_survey_design()
  }
  if(any(class(data) %in% c("svyrep.design")) & all(class(data) %ni% c("tbl_svy"))){
    message("Input: survey object")
    data_W <- data |>
      as_survey_rep()
  }
  # Si objet srvyr (avec replicates ou non)
  if(any(class(data) %in% c("tbl_svy"))){
    message("Input: srvyr object")
    data_W <- data
  }
  # Si data.frame (pas de replicate prevu => A FAIRE A TERME)
  if(any(class(data) %ni% c("survey.design2","survey.design")) & any(class(data) %ni% c("tbl_svy")) & any(class(data) %in% c("data.frame"))){
    message("Input: data.frame")
    data_W <- data |>
      as_survey_design(...)
  }

  # On determine si replicates ou non (pour message plus bas)
  replicates <- ifelse(any(class(data) %in% c("svyrep.design")), TRUE, FALSE)

  # message("Variables du design :", " cluster : ", paste(names(data_W$cluster), collapse = " "), " | strata : ",  paste(names(data_W$strata), collapse = " "), " | weights : ",  paste(names(data_W$allprob), collapse = " "))

  # On extrait les variables du design
  attr_design <- attr(data_W, "survey_vars") |>
    as.character() |>
    unlist(use.names = TRUE)

  vec_design <- character(0)
  for(i in 1:length(attr_design)){
    vec_name_i <- paste0(names(attr_design)[i], ": ")
    if(i != length(attr_design)){
      vec_var_i <- paste0(attr_design[[i]], ",")
    }
    if(i == length(attr_design)){
      vec_var_i <- attr_design[[i]]
    }
    vec_design <- c(vec_design, vec_name_i, vec_var_i)
  }

  # On affiche les variables du design dans un message
  if(replicates == FALSE){
    message("Sampling design -> ", paste(vec_design, collapse = " "))
  }
  if(replicates == TRUE){
    message("Sampling design -> replicate weights")
  }

  return(data_W)

}


#' load_and_active_fonts
#'
#' Function to load and activate fonctionr's built-in fonts.
#' Available fonts, included in the package itself, are `"Roboto"`, `"Montserrat"`, `"Gotham Narrow"`, and `"Euclid Circular A"`. Default is `"Roboto"`.
#'
#' @return No return value, called for side effects.
#' @export
#'
#' @examples
#' # Loading of fonts from fonctionr. You can now use it in fonctionr !
#' load_and_active_fonts()

load_and_active_fonts <- function(){

  # On ajoute les polices contenues dans le package et on les active
  sysfonts::font_add(family = "Montserrat",
                     regular = paste0(system.file("font", package = "fonctionr"), "/Montserrat-Regular.ttf"),
                     italic = paste0(system.file("font", package = "fonctionr"), "/Montserrat-Italic.ttf"),
                     bold = paste0(system.file("font", package = "fonctionr"), "/Montserrat-Bold.ttf"),
                     bolditalic = paste0(system.file("font", package = "fonctionr"), "/Montserrat-BoldItalic.ttf"))
  sysfonts::font_add(family = "Roboto",
                     regular = paste0(system.file("font", package = "fonctionr"), "/Roboto-Regular.ttf"),
                     italic = paste0(system.file("font", package = "fonctionr"), "/Roboto-Italic.ttf"),
                     bold = paste0(system.file("font", package = "fonctionr"), "/Roboto-Bold.ttf"),
                     bolditalic = paste0(system.file("font", package = "fonctionr"), "/Roboto-BoldItalic.ttf"))
  sysfonts::font_add(family = "Gotham Narrow",
                     regular = paste0(system.file("font", package = "fonctionr"), "/Gotham_Narrow_Book.otf"),
                     italic = paste0(system.file("font", package = "fonctionr"), "/Gotham_Narrow_Book_Italic.otf"),
                     bold = paste0(system.file("font", package = "fonctionr"), "/Gotham_Narrow_Bold.otf"),
                     bolditalic = paste0(system.file("font", package = "fonctionr"), "/Gotham_Narrow_Bold_Italic.otf"))
  sysfonts::font_add(family = "Euclid Circular A",
                     regular = paste0(system.file("font", package = "fonctionr"), "/Euclid_Circular_A_Regular.ttf"),
                     italic = paste0(system.file("font", package = "fonctionr"), "/Euclid_Circular_A_Italic.ttf"),
                     bold = paste0(system.file("font", package = "fonctionr"), "/Euclid_Circular_A_Bold.ttf"),
                     bolditalic = paste0(system.file("font", package = "fonctionr"), "/Euclid_Circular_A_Bold_Italic.ttf"))
  sysfonts::font_add(family = "League Gothic", regular = paste0(system.file("font", package = "fonctionr"), "/LeagueGothic-Regular.otf"))
  showtext::showtext_auto()
}


#' export_excel
#'
#' Internal function to export results of fonctionr's functions into an excel file.
#'
#' @param tab_excel A dataframe with the results calculated by fonctionr's functions.
#' @param fonction fonctionr's functions in which we are - necessary for formatting the Excel table
#' @param type 'type' argument for quantitative variables (median or mean) - necessary for formatting the Excel table
#' @param graph ggplot object showing results of fonctionr's functions.
#' @param test.stat Results of a statistical test on the data.
#' @param test_probs 'probs' argument from distrib_discrete() - necessary for formatting the Excel table
#' @param quantiles A dataframe with quantiles for density functions.
#' @param density A dataframe with density coordinates for density functions.
#' @param group.fill_null A logical vector. TRUE if no group.fill.
#' @param facet_null A logical vector. TRUE if no facet.
#' @param export_path Path to export the results in an xlsx file.
#' @param percent_fm A logical vector. TRUE if results are percentages.
#' @param fgFill Color of first row in exported excel file.
#' @param bivariate A logical vector. TRUE if results are bivariate.
#' @param dens A string vector indicating the type of density result to be exported. Possible values: "uni", "group" or "none" (not a density calculation).
#'
#' @noRd
#'
export_excel <- function(tab_excel,
                         fonction,
                         type = NULL,
                         graph,
                         test.stat,
                         test_probs = NULL,
                         quantiles = NULL,
                         density = NULL,
                         group.fill_null,
                         facet_null,
                         export_path,
                         percent_fm,
                         fgFill,
                         bivariate,
                         dens = "none") {

  # Check pour voir si les packages sont installes, etant en 'Suggests'
  rlang::check_installed(c("openxlsx", "broom"))

  # Pour etre integre au fichier excel, le graphique doit etre affiche => https://ycphs.github.io/openxlsx/reference/insertPlot.html
  print(graph)

  # Fonction pour transformer les resultats du test stat en dataframe
  tidy_test.stat <- function(test.stat){
    test_stat_excel <- test.stat |>
      broom::tidy() |>
      t() |>
      as.data.frame()
    test_stat_excel$names <- rownames(test_stat_excel)
    test_stat_excel <- test_stat_excel[, c(2,1)]
    names(test_stat_excel)[1] <- "Parameter"
    names(test_stat_excel)[2] <- "Value"

    return(test_stat_excel)
  }
  # Fonction pour creer un data.frame a la main en cas de test non realise
  empty_test.stat <- function(){
    df <- data.frame(Parameter = c("none"),
                     Value = "Test non realise",
                     row.names = NULL)
    return(df)
  }

  # On execute la bonne operation selon la fonction
  if(fonction %in% c("prop_group")){
    if(group.fill_null) {
      if(all(test.stat != "Conditions non remplies")){
        test_stat_excel <- tidy_test.stat(test.stat)
      } else {
        test_stat_excel <- data.frame(Parameter = c("test.error"),
                                      Value = test.stat,
                                      row.names = NULL)
      }
    } else {
      test_stat_excel <- empty_test.stat()
    }
  }
  if(fonction %in% c("central_group", "distrib_group_continuous")){
    if(group.fill_null) {
      if(type == "median"){
        test_stat_excel <- tidy_test.stat(test.stat)
      }
      # broom::tidy() ne fonctionne pas sur regTermTest => je le fais a la main
      if(type == "mean") {
        test_stat_excel <- data.frame(Parameter = c("df", "ddf", "statistic", "p.value", "method"),
                                      Value = c(test.stat$df, test.stat$ddf, test.stat$Ftest, test.stat$p, "Wald test"),
                                      row.names = NULL)
      }
    } else {
      test_stat_excel <- empty_test.stat()
    }
  }
  if(fonction %in% c("distrib_group_discrete")){
    if(facet_null) {
      if(all(test.stat != "Conditions non remplies")){
        test_stat_excel <- tidy_test.stat(test.stat)
      } else {
        test_stat_excel <- data.frame(Parameter = c("test.error"),
                                      Value = test.stat,
                                      row.names = NULL)
      }
    } else {
      test_stat_excel <- empty_test.stat()
    }
  }
  if(fonction %in% c("distrib_discrete")){
    if(test_probs & facet_null) {
      test_stat_excel <- tidy_test.stat(test.stat)
    } else {
      test_stat_excel <- empty_test.stat()
    }
  }
  if(fonction %in% c("many_val", "many_val_group", "distrib_continuous")){
    test_stat_excel <- empty_test.stat()
  }

  # Le nom du feuillet avec tab_excel varie selon que c'est la densite ou non
  # sub_dens = astuce pour la mise en gras ou non de la premiere colonne (voir apres)
  name_tab <- "Resultats"
  sub_dens <- 0
  if(dens == "uni" | dens == "group"){
    name_tab <- "Indices centraux"
  }
  if(dens == "uni"){
    sub_dens <- 1
  }

  # Formatage du fichier Excel dans lequel on exporte les resultats

  wb <- openxlsx::createWorkbook() # On cree l'objet dans lequel on va formater toutes les infos en vue d'un export en fichier Excel
  openxlsx::addWorksheet(wb, name_tab) # On ajoute une feuille pour les resultats / indices centraux (densite)
  if(dens != "none"){ # Si densite
    openxlsx::addWorksheet(wb, "Quantiles") # On ajoute une feuille pour les quantiles
    openxlsx::addWorksheet(wb, "Densite") # On ajoute une feuille pour la densite
  }
  openxlsx::addWorksheet(wb, "Graphique") # On ajoute une feuille pour le graphique
  openxlsx::addWorksheet(wb, "Test statistique") # On ajoute une feuille pour le resultat du test stat

  # On ecrit les donnees
  openxlsx::writeData(wb, name_tab, tab_excel, keepNA = TRUE, na.string = "NA") # On ecrit les resultats en gardant les NA
  if(dens != "none"){ # Si densite
    openxlsx::writeData(wb, "Quantiles", quantiles, keepNA = TRUE, na.string = "NA") # On ecrit les quantiles en gardant les NA
    openxlsx::writeData(wb, "Densite", density, keepNA = TRUE, na.string = "NA") # On ecrit la densite en gardant les NA
  }
  openxlsx::insertPlot(wb,"Graphique", dpi = 90, width = 12, height = 7)
  openxlsx::writeData(wb, "Test statistique", test_stat_excel) # On ecrit le resultat du test stat

  # On definit les styles
  openxlsx::setColWidths(wb, name_tab, widths = 20, cols = 1:ncol(tab_excel))
  if(dens != "none"){ # Si densite
    openxlsx::setColWidths(wb, "Quantiles", widths = 20, cols = 1:ncol(quantiles))
    openxlsx::setColWidths(wb, "Densite", widths = 20, cols = 1:ncol(density))
  }
  hs <- openxlsx::createStyle(fontColour = "#ffffff", fgFill = fgFill,  # Style de la premiere ligne
                    halign = "center", textDecoration = "Bold",
                    fontName = "Arial Narrow")
  firstC <- openxlsx::createStyle (halign = "left", textDecoration = "Bold", # Style de la premiere colonne
                         fontName = "Arial Narrow")
  body <- openxlsx::createStyle (halign = "center", # Style des cellules du tableau
                       fontName = "Arial Narrow")
  percent <- openxlsx::createStyle(numFmt = "percentage")

  # On applique le style a la premiere ligne
  openxlsx::addStyle(wb, name_tab, hs, cols = 1:ncol(tab_excel), rows = 1)
  if(dens != "none"){
    openxlsx::addStyle(wb, "Quantiles", hs, cols = 1:ncol(quantiles), rows = 1)
    openxlsx::addStyle(wb, "Densite", hs, cols = 1:ncol(density), rows = 1)
  }

  # On applique le style aux reste des lignes a partir de la 2e colonne (car la 1ere est en gras)
  # SAUF pour la densite univariee ou la 1ere col n'est pas en gras (et donc on fait 2-sub_dens qui est = 1)
  openxlsx::addStyle(wb, name_tab, body, cols = (2-sub_dens):ncol(tab_excel), rows = 2:(nrow(tab_excel)+1), gridExpand = TRUE, stack = TRUE)
  if(dens != "none"){
    # Pour les quantiles 3-sub_dens car il y au moins la 1ere colonne en gras (et les 2 premieres lorsque dens par groupe)
    openxlsx::addStyle(wb, "Quantiles", body, cols = (3-sub_dens):ncol(quantiles), rows = 2:(nrow(quantiles)+1), gridExpand = TRUE, stack = TRUE)
    openxlsx::addStyle(wb, "Densite", body, cols = (2-sub_dens):ncol(density), rows = 2:(nrow(density)+1), gridExpand = TRUE, stack = TRUE)
  }

  # On suit une logique d'elements additifs pour plus ou moins de colonnes pour la mise en forme
  # Selon que la fonction produit un indicateur selon 1 variable ou selon 2 (= bivarie) => c'est le cas de many_val_group et distrib_group_d : il y a alors une colonne en plus a mettre en gras dans la mise en forme ci-dessous !
  bivar_add <- 0
  if (bivariate == TRUE) {
    bivar_add <- 1
  }
  # Meme logique dans le cas ou le resultat varie par modalite de facet => il y a une colonne en plus a mettre en gras
  facet_add <- 0
  if (!facet_null) {
    facet_add <- 1
  }

  # On applique le style aux premieres colonnes (sans la premiere ligne)
  # if statement car pas de 1ere colonne en gras pour densite univariee
  if(dens == "group" | dens == "none"){
    openxlsx::addStyle(wb, name_tab, firstC, cols = 1:(1+facet_add+bivar_add), rows = 2:(nrow(tab_excel)+1), gridExpand = TRUE, stack = TRUE)
  }
  # Particulier a densite
  if(dens != "none"){
    openxlsx::addStyle(wb, "Quantiles", firstC, cols = 1:(1+facet_add+(1-sub_dens)), rows = 2:(nrow(quantiles)+1), gridExpand = TRUE, stack = TRUE)
    # if statement car pas de 1ere colonne en gras pour densite univariee
    if(dens == "group"){
      openxlsx::addStyle(wb, "Densite", firstC, cols = 1:(1+facet_add), rows = 2:(nrow(density)+1), gridExpand = TRUE, stack = TRUE)
    }
  }
  # On applique le style de pourcentage aux proportions
  if (percent_fm == TRUE) {
    openxlsx::addStyle(wb, name_tab, percent, cols = (2+facet_add+bivar_add):(4+facet_add+bivar_add), rows = 2:(nrow(tab_excel)+1), gridExpand = TRUE, stack = TRUE)
  }

  # Styles pour le test stat (a part pour lisibilite)
  openxlsx::setColWidths(wb, "Test statistique", widths = 20, cols = 1:ncol(test_stat_excel)) # Largeur des colonnes
  hs2 <- openxlsx::createStyle(fontColour = "#ffffff", fgFill = "grey15",  # Style de la premiere ligne
                     halign = "center", textDecoration = "Bold",
                     fontName = "Arial Narrow")
  body2 <- openxlsx::createStyle (fontName = "Arial Narrow") # Style des cellules du tableau

  openxlsx::addStyle(wb, "Test statistique", hs2, cols = 1:ncol(test_stat_excel), rows = 1) # On applique le style a la premiere ligne
  openxlsx::addStyle(wb, "Test statistique", firstC, cols = 1, rows = 2:(nrow(test_stat_excel)+1), gridExpand = TRUE, stack = TRUE) # On applique le style a la premiere colonne (sans la premiere ligne)
  openxlsx::addStyle(wb, "Test statistique", body2, cols = 2:ncol(test_stat_excel), rows = 2:(nrow(test_stat_excel)+1), gridExpand = TRUE, stack = TRUE) # On applique le style aux reste des cellules

  # On sauvegarde le fichier excel
  openxlsx::saveWorkbook(wb, export_path, overwrite = TRUE)

}


#' check_arg
#'
#' Internal function to check arguments
#'
#' @param arg List of arguments to check
#' @param short Chek if arguments lenght is not superior to 1
#' @param type Type of check : "character", "logical" or "numeric"
#'
#' @noRd
#'

# Solution trouvee ici pour les messages : https://stackoverflow.com/questions/77432872/how-paste-be-used-as-a-message-with-r-stopifnot

check_arg <- function(arg,
                      short = TRUE,
                      type) {
  for(check_i in seq_along(arg)){
    if(!is.null(arg[[check_i]])){
      if(short == TRUE){
        if (!length(arg[[check_i]]) == 1) stop(paste("Argument", names(arg)[[check_i]], "does not have the correct length (max 1)"), call. = FALSE)
      }
      if(all(!is.na(arg[[check_i]]))){
        if(type == "character"){
          if (!is.character(arg[[check_i]])) stop(paste("Argument", names(arg)[[check_i]], "is not in the correct format (character)"), call. = FALSE)
        }
        if(type == "logical"){
          if (!is.logical(arg[[check_i]])) stop(paste("Argument", names(arg)[[check_i]], "is not in the correct format (logical)"), call. = FALSE)
        }
        if(type == "numeric"){
          if (!is.numeric(arg[[check_i]])) stop(paste("Argument", names(arg)[[check_i]], "is not in the correct format (numeric)"), call. = FALSE)
        }
      }
    }
  }
}


#' check_bin
#'
#' Internal function to check if variables are binarized
#'
#' @param data A dataframe in which to check
#' @param vec_list_vars A vector containing names of columns
#'
#' @noRd
#'

check_bin <- function(data,
                      vec_list_vars) {
  for (var in vec_list_vars) {
    if (!all(data[[var]] %in% c(0,1,NA))) stop(paste("Variable", var, "must be binary (0-1 ou TRUE-FALSE)"), call. = FALSE)
  }
}


#' check_prob
#'
#' Internal function to check if probs are between 0 & 1
#'
#' @param arg List of arguments to check
#'
#' @noRd
#'

check_prob <- function(arg) {
  for(check_i in seq_along(arg)){
    if(!is.null(arg[[check_i]])){
      if(all(!is.na(arg[[check_i]]))){
        min_check <- (min(arg[[check_i]]))
        max_check <- (max(arg[[check_i]]))
        if (!(min_check >= 0 & max_check <= 1)) stop(paste("Argument", names(arg)[[check_i]], "must range between 0 and 1"), call. = FALSE)
      }
    }
  }
}


#' check_input
#'
#' Internal function to check if input variables exist in data
#'
#' @param data A dataframe or survey object in which to check
#' @param vars_input_char A vector containing names of columns
#'
#' @noRd
#'

check_input <- function(data,
                        vars_input_char) {

  # Petite fonction utile
  `%ni%` = Negate(`%in%`)

  # Si data.frame
  if(any(class(data) %ni% c("survey.design2","survey.design")) & any(class(data) %ni% c("tbl_svy")) & any(class(data) %in% c("data.frame"))){
    for (i in seq_along(vars_input_char)) {
      if (!vars_input_char[i] %in% names(data)) stop(paste("Variable", vars_input_char[i], "in", names(vars_input_char[i]) , "is not in data"), call. = FALSE)
    }
  }
  # Si objet sondage
  if(any(class(data) %in% c("survey.design2","survey.design","tbl_svy","svyrep.design"))){
    for (i in seq_along(vars_input_char)) {
      if (!vars_input_char[i] %in% names(data[["variables"]])) stop(paste("Variable", vars_input_char[i], "in", names(vars_input_char[i]) , "is not in data"), call. = FALSE)
    }
  }

  # # DESACTIVE : NE FONCTIONNE PAS !
  # # Check du design. Solution trouvee ici : https://stackoverflow.com/questions/70652685/how-to-set-aliases-for-function-arguments-in-an-r-package
  # vars_survey <- as.character(substitute(...()))[names(as.list(substitute(...()))) %in% c("strata", "ids", "weight", "weights", "probs", "variables", "fpc")]
  # if(all(vars_survey %in% names(data)) == FALSE){
  #   stop("Au moins une des variables du design n'est pas presente dans data")
  # }

}


#' isColor
#'
#' Internal function to check if a string is a valid color
#'
#' @param x A string to check
#'
#' @noRd
#'

# Solution trouvee ici : https://stackoverflow.com/questions/13289009/check-if-character-string-is-a-valid-color-representation
# isColor <- function(x){
#   res <- try(grDevices::col2rgb(x), silent = TRUE)
#   return(!"try-error" %in% class(res))
# }
isColor <- function(x) {
  sapply(x, function(X) {
    tryCatch(is.matrix(grDevices::col2rgb(X)),
             error = function(e) FALSE)
  })
}


#' relabel_ggtext
#'
#' Internal function that formats graph labels to work with ggtext.
#'
#' @param x A string to modify
#' @param wrap_width_y Number of characters before going to the line
#'
#' @noRd
#'

relabel_ggtext <- function(x, wrap_width, total_name = NULL){
  x <- ifelse(is.na(x), '*NA*', x)
  if(!is.null(total_name)){
    x <- ifelse(x == total_name, paste0("**", total_name, "**"), x)
  }
  # NOTE : le passage a la ligne doit etre avant la conversion des caract. speciaux, sinon =/= le bon nombre de caract. !
  x <- stringr::str_wrap(x, width = wrap_width)
  x <- stringr::str_replace_all(x, c(
    "\u0026" = "&amp;", "\u25ca" = "&loz;", "\u00a4" = "&curren;", "\u20ac" = "&euro;",
    "\u00a2" = "&cent;", "\u00a3" = "&pound;", "\u00a5" = "&yen;",
    "\u0192" = "&fnof;", "\u00b0" = "&deg;", "\u00b5" = "&micro;",
    "\u003c" = "&lt;", "\u003e" = "&gt;", "\u2264" = "&le;", "\u2265" = "&ge;",
    "\u2248" = "&asymp;", "\u2260" = "&ne;", "\u2261" = "&equiv;",
    "\u00b1" = "&plusmn;", "\u2212" = "&minus;", "\u00d7" = "&times;",
    "\u00f7" = "&divide;", "\u2044" = "&frasl;", "\u2030" = "&permil;",
    "\u00bc" = "&frac14;", "\u00bd" = "&frac12;", "\u00be" = "&frac34;",
    "\u00b9" = "&sup1;", "\u00b2" = "&sup2;", "\u00b3" = "&sup3;",
    "\u00ba" = "&ordm;", "\u00aa" = "&ordf;", "\u0192" = "&fnof;",
    "\u2032" = "&prime;", "\u2033" = "&Prime;", "\u2202" = "&part;",
    "\u220f" = "&prod;", "\u2211" = "&sum;", "\u221a" = "&radic;",
    "\u221e" = "&infin;", "\u00ac" = "&not;", "\u2229" = "&cap;",
    "\u222b" = "&int;", "\u03b1" = "&alpha;", "\u0391" = "&Alpha;",
    "\u03b2" = "&beta;", "\u0392" = "&Beta;", "\u03b3" = "&gamma;",
    "\u0393" = "&Gamma;", "\u03b4" = "&delta;", "\u0394" = "&Delta;",
    "\u03b5" = "&epsilon;", "\u0395" = "&Epsilon;", "\u03b6" = "&zeta;",
    "\u0396" = "&Zeta;", "\u03b7" = "&eta;", "\u0397" = "&Eta;",
    "\u03b8" = "&theta;", "\u0398" = "&Theta;", "\u03b9" = "&iota;",
    "\u0399" = "&Iota;", "\u03ba" = "&kappa;", "\u039a" = "&Kappa;",
    "\u03bb" = "&lambda;", "\u039b" = "&Lambda;", "\u03bc" = "&mu;",
    "\u039c" = "&Mu;", "\u03bd" = "&nu;", "\u039d" = "&Nu;", "\u03be" = "&xi;",
    "\u039e" = "&Xi;", "\u03bf" = "&omicron;", "\u039f" = "&Omicron;",
    "\u03c0" = "&pi;", "\u03a0" = "&Pi;", "\u03c1" = "&rho;", "\u03a1" = "&Rho;",
    "\u03c3" = "&sigma;", "\u03c2" = "&sigmaf;", "\u03a3" = "&Sigma;",
    "\u03c4" = "&tau;", "\u03a4" = "&Tau;", "\u03c5" = "&upsilon;",
    "\u03a5" = "&Upsilon;", "\u03c6" = "&phi;", "\u03a6" = "&Phi;",
    "\u03c7" = "&chi;", "\u03a7" = "&Chi;", "\u03c8" = "&psi;", "\u03a8" = "&Psi;",
    "\u03c9" = "&omega;", "\u03a9" = "&Omega;"
  ))
  # Le remplacement de "\n" par "<br>" doit se faire apres la conversion des caract. speciaux, sinon marche pas (car < > sont convertis ?)
  x <- stringr::str_replace_all(x, "\n", "<br>")

    return(x)
}


#' official_pal
#'
#' A function that allows you to create the palettes of fonctionr.
#'
#' @param inst Name of the palette.
#' @param n Number of colors.
#' @param direction Direction of the palette color. Default is `1`. The opposite direction is `-1`.
#' @param desaturate Numeric specifying the amount of desaturation where `1` corresponds to complete desaturation, `0` to no desaturation, and values in between to partial desaturation.
#' @param lighten Numeric specifying the amount of lightening. Negative numbers cause darkening.
#' @param darken Numeric specifying the amount of lightening. Negative numbers cause lightening.
#' @param show_pal `TRUE` to display a graph representing the specified color palette.
#' @param font Font used in the graphic. See `load_and_active_fonts()` for available fonts.
#' @param list_pal_names `TRUE` to generate a vector with palette names.
#'
#' @return A vector containing hexadecimals color codes.
#' @export
#'
#' @examples
#' official_pal("OBSS", 8, show_pal = TRUE)
#' official_pal("OBSS_Greens", 8, show_pal = TRUE)
#' official_pal("OBSS_div_mid4", 7, show_pal = TRUE)
#' official_pal("OBSS_div_bi3", 8, show_pal = TRUE)
#' official_pal("IBSA", 4, show_pal = TRUE)
#' official_pal("ULB", 6, show_pal = TRUE)

official_pal <- function(inst,
                         n,
                         direction = 1,
                         desaturate = 0,
                         lighten = 0,
                         darken = 0,
                         show_pal = FALSE,
                         font = "Gotham Narrow",
                         list_pal_names = FALSE
                         ){

  # Liste des palettes
  pal_names <- c("Vivalis",
                 "OBSS",
                 "OBSS_alt1",
                 "OBSS_alt2",
                 "OBSS_alt3",
                 "OBSS_Relax",
                 "OBSS_Autumn",
                 "OBSS_Sweet",
                 "OBSS_Spring",
                 "OBSS_Candy",
                 "OBSS_Candy2",
                 "OBSS_Greens",
                 "OBSS_Greens2",
                 "OBSS_Sea",
                 "OBSS_Sea2",
                 "OBSS_Sunset",
                 "OBSS_Sunset2",
                 "OBSS_Purples",
                 "OBSS_Purples2",
                 "OBSS_Blues",
                 "OBSS_Blues2",
                 "OBSS_Brown",
                 "OBSS_Brown2",
                 "OBSS_div_mid1",
                 "OBSS_div_mid2",
                 "OBSS_div_mid3",
                 "OBSS_div_mid4",
                 "OBSS_div_bi1",
                 "OBSS_div_bi2",
                 "OBSS_div_bi3",
                 "OBSS_div_bi4",
                 "OBSS_highlight1",
                 "OBSS_highlight2",
                 "OBSS_highlight3",
                 "OBSS_old",
                 "IBSA",
                 "ULB",
                 "IEFH",
                 "IEFH_div_bi")

  if(list_pal_names == FALSE){

    # Checks de verification
    if(!inst %in% pal_names){
      stop("This palette doesn't exist")
    }

    if(n < 1){
      stop("n must be greater than 0")
    }

    if(all(direction != -1, direction != 1)){
      stop("Direction not valid. Please use 1 for standard palette or -1 for reversed palette.")
    }

    # Palettes avec degrade simple
    # Palettes qualitatives
    if(inst == "Vivalis"){pal_cols <- c("#ff87a5", "#ffb900", "#00e1af", "#375078")}
    if(inst == "OBSS"){pal_cols <- c("#E65362", "#FCB308", "#26ADA8", "#434E73")}
    if(inst == "OBSS_alt1"){pal_cols <- c("#26ADA8", "#FCB308", "#E65362", "#6F66C9")}
    if(inst == "OBSS_alt2"){pal_cols <- c("#EB7FAE", "#434E73", "#26ADA8", "#FCB308", "#E65362")}
    if(inst == "OBSS_alt3"){pal_cols <- c("#E65362", "#3F7FBF", "#33B8B4", "#FCB308")}
    if(inst == "OBSS_Relax"){pal_cols <- c("#0B7373", "#6bbfa3", "#FCB308", "#c7b0d5", "#585fa8")}
    if(inst == "OBSS_Autumn"){pal_cols <- c("#6E3D10", "#F08E3E", "#DBC5B6", "#434E73")}
    if(inst == "OBSS_Sweet"){pal_cols <- c("#adce6d", "#6bbfa3" ,"#5e9ad3", "#585fa8", "#c7b0d5")}
    if(inst == "OBSS_Spring"){pal_cols <- c("#E65362", "#FCB308", "#238C8C")}
    if(inst == "OBSS_old"){pal_cols <- c("#5e9ad2", "#f18d6e", "#accd6c")}
    # Palettes continues
    if(inst == "OBSS_Candy"){pal_cols <- c("#EDA1C2", "#BF2433")}
    if(inst == "OBSS_Candy2"){pal_cols <- c("#F7CDDD", "#BF2433")}
    if(inst == "OBSS_Greens"){pal_cols <- c("#adce6d", "#066969")}
    if(inst == "OBSS_Greens2"){pal_cols <- c("#D2F09E", "#066969")}
    if(inst == "OBSS_Sea"){pal_cols <- c("#99D4AD", "#0F7D7D", "#303C66")}
    if(inst == "OBSS_Sea2"){pal_cols <- c("#B7F0CC", "#168A8A", "#303C66")}
    if(inst == "OBSS_Sunset"){pal_cols <- c("#FFC012", "#cf4d4a")}
    if(inst == "OBSS_Sunset2"){pal_cols <- c("#FFE26E", "#cf4d4a")}
    if(inst == "OBSS_Purples"){pal_cols <- c("#c7b0d5", "#4236A3")}
    if(inst == "OBSS_Purples2"){pal_cols <- c("#E0C3F0", "#4236A3")}
    if(inst == "OBSS_Blues"){pal_cols <- c("#95BFF0", "#064B9E")}
    if(inst == "OBSS_Blues2"){pal_cols <- c("#BCD7F7", "#064B9E")}
    if(inst == "OBSS_Brown"){pal_cols <- c("#DBC5B6", "#6E3D10")}
    if(inst == "OBSS_Brown2"){pal_cols <- c("#F7E0CF", "#6E3D10")}
    # Palettes divergentes avec point central
    if(inst == "OBSS_div_mid1"){pal_cols <- c("#C2324D", "#EDA1C2", "#DBD2CC", "#adce6d", "#0F7D7D")}
    if(inst == "OBSS_div_mid2"){pal_cols <- c("#C2324D", "#EB7FAE", "#DBD2CC", "#5e9ad3", "#3A456B")}
    if(inst == "OBSS_div_mid3"){pal_cols <- c("#0B7373", "#6bbfa3", "#EBE0BC", "#F08E3E", "#D1455F")}
    if(inst == "OBSS_div_mid4"){pal_cols <- c("#585fa8", "#c7b0d5", "#EBE0BC", "#adce6d", "#0F7D7D")}
    # Palettes d'autres institutions
    if(inst == "IBSA"){pal_cols <- c("#D95A49", "#F0D0C8", "#562821", "#9A9A9A")}
    if(inst == "ULB"){pal_cols <- c("#0a5296", "#a7b9dd")}
    if(inst == "IEFH"){pal_cols <- c("#9d4b95", "#c73a5d", "#cf6430", "#eec128", "#00867f", "#2b407f")}

    # On cree la palette si "pal_cols" existe (= palette simple)
    if(exists("pal_cols")){
      pal_fct <- grDevices::colorRampPalette(pal_cols)
      palette <- pal_fct(n)
    }

    # Palettes composees de 2 palettes
    # Palettes divergentes sans point central
    if(inst == "OBSS_div_bi1"){
      pal_cols1 <- c("#A457B5", "#E06B7D", "#F59C4F")
      pal_cols2 <- c("#33B8B4", "#3F7FBF", "#434E73")
    }
    if(inst == "OBSS_div_bi2"){
      pal_cols1 <- c("#585fa8", "#c7b0d5")
      pal_cols2 <- c("#EDA868", "#B85C11")
    }
    if(inst == "OBSS_div_bi3"){
      pal_cols1 <- c("#0B7373", "#26ADA8")
      pal_cols2 <- c("#EB7FAE", "#AD264E")
    }
    if(inst == "OBSS_div_bi4"){
      pal_cols1 <- c("#564A9E", "#637FDB", "#92C6EB")
      pal_cols2 <- c("#F7D89E", "#F08960", "#C73A54")
    }
    if(inst == "IEFH_div_bi"){
      pal_cols1 <- c("#008671", "#85C4BF")
      pal_cols2 <- c("#C28EBD", "#9d4b95")
    }
    # Palettes avec emphase
    if(inst == "OBSS_highlight1"){
      pal_cols1 <- c("#E65362")
      pal_cols2 <- c("#FCB308", "#26ADA8", "#434E73")
      np1 <- 1 # On determine le nombre de modalites avec emphase dans np1
    }
    if(inst == "OBSS_highlight2"){
      pal_cols1 <- c("#434E73")
      pal_cols2 <- c("#FFC012", "#BF2433")
      np1 <- 1
    }
    if(inst == "OBSS_highlight3"){
      pal_cols1 <- c("#434E73", "#6dac70")
      pal_cols2 <- c("#FFC012", "#BF2433")
      np1 <- 2
      # pb si n plus petit que 3 car np1 devient negatif => np1 est defini a 1 par default
      if (n <= 2) {
        np1 <- 1
      }
    }

    # On cree la palette si palette composee de 2 palettes ("pal_cols1" existe alors)
    if(exists("pal_cols1")){

      # Pour divergent, si n == 2 ou 3, alors on selectionne en partant de la fin pour la 2e palette (coherent car divergent)
      if(stringr::str_detect(inst, "_div_bi")){
        if(n %in% 2:3){pal_cols2 <- rev(pal_cols2)}
      }
      # Pour highlight, on selectionne en partant de la fin pour la 2e palette dans certaines situation (coherent car opposition)
      if(stringr::str_detect(inst, "OBSS_highlight")){
        if(all(np1 == 1 & n == 2)){pal_cols2 <- rev(pal_cols2)}
        if(all(np1 == 2 & n %in% 2:3)){pal_cols2 <- rev(pal_cols2)}
      }

      # On cree les palettes
      pal_fct1 <- grDevices::colorRampPalette(pal_cols1)
      pal_fct2 <- grDevices::colorRampPalette(pal_cols2)

      # pb si n est impair dans les palettes divergentes => asymetrie comme solution (la 1ere couleur prend le dessus)
      if(stringr::str_detect(inst, "_div_bi")){
        if (n %% 2 == 0) {
          np1 <- n/2
        } else {
          np1 <- (n/2)+.5
        }
      }

      # On compose la palette : une avec np1, l'autre avec n-np1 (symetrique ou non selon le cas)
      palette <- c(pal_fct1(np1), pal_fct2(n-np1))

    }

    # Pour alterer la palette (desaturer, eclaircir, foncer)
    palette <- fonctionr_alter_cols(
      cols = palette,
      desaturate = desaturate,
      lighten = lighten,
      darken = darken
      )

    # Inversion de la palette si demande
    if(direction == -1){
      palette <- rev(palette)
    }

    # On retourne les codes hex
    if(show_pal == FALSE){
      return(palette)
    }

    # On affiche graphiquement la palette
    if(show_pal == TRUE){

      # On rend dispo les fonts
      load_and_active_fonts()

      # On affiche avec une fonction piquee de Met Brewer
      print.palette <- function(palette, lab = inst, family = font) {
        n_show <- length(palette)
        old <- graphics::par(mar = c(0.5, 0.5, 0.5, 0.5))
        on.exit(graphics::par(old))

        graphics::image(1:n_show, 1, as.matrix(1:n_show), col = palette,
              ylab = "", xaxt = "n_show", yaxt = "n_show", bty = "n_show")

        graphics::rect(0, 0.92, n_show + 1, 1.08, col = grDevices::rgb(1, 1, 1, 0.8), border = NA)
        graphics::text((n_show + 1) / 2, 1, labels = lab, cex = 2.5, family = family)
      }

      return(print.palette(palette))

    }
  }

  # Si les noms des palettes sont demandes
  if(list_pal_names == TRUE){

    return(pal_names)

  }
}


#' fonctionr_alter_cols
#'
#' Internal function to alter colors
#'
#' @param cols Color(s) to alter
#' @param desaturate Numeric specifying the amount of desaturation where 1 corresponds to complete desaturation, 0 to no desaturation, and values in between to partial desaturation.
#' @param lighten Numeric specifying the amount of lightening. Negative numbers cause darkening.
#' @param darken Numeric specifying the amount of lightening. Negative numbers cause lightening.
#'
#' @noRd
#'

fonctionr_alter_cols <- function(cols,
                                 desaturate,
                                 lighten,
                                 darken) {

  # Check pour voir si le package est installe, etant en 'Suggests'
  if(any(c(desaturate, lighten, darken) != 0)){
    rlang::check_installed("colorspace")

    # Pour alterer la palette (desaturer, eclaircir, foncer)
    # NOTE : dans le if pour eviter de tester les conditions si aucune alteration
    if(desaturate != 0){
      cols <- colorspace::desaturate(cols, desaturate)
    }
    if(lighten != 0){
      cols <- colorspace::lighten(cols, lighten)
    }
    if(darken != 0){
      cols <- colorspace::darken(cols, darken)
    }

  }

  return(cols)
}


#' theme_fonctionr
#'
#' A ggplot theme that is ready to use. It is used by most other functions, but can also be applied to an external ggplot object.
#'
#' @param font Font used in the graphic. See `load_and_active_fonts()` for available fonts.
#' @param theme The optionnal theme you want for the graphic. Available themes: `"fonctionr"` and `“IWEPS”`. Default is `NULL`.
#' @param display The way `theme_fonctionr()` works on the axis texts: like ggplot2 or ggtext.
#' @param grid.lines Specify major grid lines : `"x"`, `"y"` or `"both"`. Default is `"x"`.
#' @param coef_font A multiplier factor for font size.
#'
#' @return No return value, called for side effects.
#' @import ggplot2
#' @export

theme_fonctionr <- function(font = "Roboto",
                            theme = "fonctionr",
                            display = "ggplot",
                            grid.lines = "x",
                            coef_font = 1) {

  theme_fonctionr_base <- function(font_base = font,
                                   display_base = display,
                                   grid.lines_base = grid.lines,
                                   coef_font_base = coef_font){

    theme_fonctionr_def <- theme_minimal(
      base_size = coef_font_base * 11
    ) +
      theme(
        text = element_text(family = font_base),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.line.x = element_line(color = "black"),
        axis.ticks.x = element_line(color = "black"),
        plot.margin = margin(10, 15, 10, 10),
        plot.caption = element_text(
          color = "grey30"
        )
      )
    if (grid.lines_base == "x") {
      theme_fonctionr_def <- theme_fonctionr_def +
        theme(
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(color = "#dddddd")
          )
    }
    if (grid.lines_base == "y") {
      theme_fonctionr_def <- theme_fonctionr_def +
        theme(
          panel.grid.major.y = element_line(color = "#dddddd"),
          panel.grid.major.x = element_blank()
          )
    }
    if (grid.lines_base == "both") {
      theme_fonctionr_def <- theme_fonctionr_def +
        theme(
          panel.grid.major.y = element_line(color = "#dddddd"),
          panel.grid.major.x = element_line(color = "#dddddd")
        )
    }
    if (display_base == "ggplot") {
      theme_fonctionr_def <- theme_fonctionr_def +
        theme(
          axis.text = element_text(color = "black")
          )
    }
    if (display_base == "ggtext") {
      theme_fonctionr_def <- theme_fonctionr_def +
        theme(
          # On definit chaque axe individuellement pour contrer le bug avec ggplot 4 => https://github.com/tidyverse/ggplot2/issues/6752
          # Rechanger vers la formule plus generale (avec heritage) lorsque ggtext a corrige => https://github.com/wilkelab/ggtext/issues/128
          axis.text.y.left = ggtext::element_markdown(color = "black"),
          axis.text.y.right = ggtext::element_markdown(color = "black"),
          axis.text.x.bottom = ggtext::element_markdown(color = "black"),
          axis.text.x.top = ggtext::element_markdown(color = "black")
          )
    }
    return(theme_fonctionr_def)
  }

  load_and_active_fonts()

  if (!is.null(theme)) {
    if (theme == "fonctionr") {
      theme_fonctionr_custom <- theme_fonctionr_base(
        font_base = font,
        display_base = display,
        grid.lines_base = grid.lines,
        coef_font_base = coef_font
      ) +
        theme(
          axis.line.y = element_blank(),
          axis.ticks.y = element_blank()
        )
    }
    if (theme == "IWEPS") {
      theme_fonctionr_custom <- theme_fonctionr_base(
        font_base = font,
        display_base = display,
        grid.lines_base = grid.lines,
        coef_font_base = coef_font
      ) +
        theme(
          axis.line.y = element_line(color = "black"),
          axis.ticks.y = element_line(color = "black")
        )
    }
  # Un theme par defaut qui correspond le + à ggplot, tout en implementant les options de theme_fonctionr()
  } else {
    theme_fonctionr_custom <- theme_gray(
      base_size = coef_font * 11
    ) +
      theme(
        text = element_text(family = font),
        # On definit chaque axe individuellement pour contrer le bug avec ggplot 4 => https://github.com/tidyverse/ggplot2/issues/6752
        # Rechanger vers la formule plus generale (avec heritage) lorsque ggtext a corrige => https://github.com/wilkelab/ggtext/issues/128
        axis.text.y.left = ggtext::element_markdown(color = "black"),
        axis.text.y.right = ggtext::element_markdown(color = "black"),
        axis.text.x.bottom = ggtext::element_markdown(color = "black"),
        axis.text.x.top = ggtext::element_markdown(color = "black")
        )
  }

  return(theme_fonctionr_custom)

}


#' fonctionr_font_size
#'
#' Internal function to define font size among fonctions
#'
#' @param type Several font sizes related to type : "normal" or "little".
#'
#' @noRd
#'

fonctionr_font_size <- function(type = "normal") {

  if(type == "normal"){
    font_size <- 3.5
  }
  if(type == "little"){
    font_size <- 3
  }
  return(font_size)
}


#' fonctionr_ggplot_labs
#'
#' Internal function to define axis labels
#'
#' @param graph The ggplot graph on which the axes must be defined
#' @param type fonctionr's functions in which we are (for specific rules)
#' @param position The position for many_val_group() : "flip" or not
#' @param group The group name
#' @param xlab X label on the graphic
#' @param lang_note_axis_x The annotation to be indicated before the X-axis label
#' @param x_exp The original expression used to calculate the X-axis
#' @param ylab Y label on the graphic
#' @param lang_note_axis_y The annotation to be indicated before the Y-axis label
#' @param legend_lab Legend (fill) label on the graphic
#' @param wrap_width_leg Number of characters before going to the line for the labels the legend
#' @param show_labs TRUE if you want to show axes labels. FALSE if you do not want to show any labels on axes
#'
#' @noRd
#'

fonctionr_ggplot_labs <- function(graph,
                                  type = "default",
                                  position = "default",
                                  group = NULL,
                                  xlab,
                                  lang_note_axis_x = NULL,
                                  x_exp = NULL,
                                  ylab,
                                  lang_note_axis_y = NULL,
                                  legend_lab = NULL,
                                  wrap_width_leg = NULL,
                                  show_labs) {

  # Montrer les noms des axes si show_labs == TRUE
  if(show_labs == TRUE){

    # Pour toutes les fonctions sauf distrib_continuous() et distrib_group_continuous()
    if(!type %in% c("distrib_continuous", "distrib_group_continuous")){
      # X ---
      # Cas ou il faut ecrire un nom pour l'axe x (expression ou defini par l'utilisateur)
      if(any(is.null(xlab), xlab != "")){
        graph <- graph +
          labs(y = ifelse(is.null(xlab),
                          paste0(lang_note_axis_x, x_exp),
                          xlab))

      # Tous les autres cas => il ne faut pas de nom pour l'axe x
      } else {
        graph <- graph +
          labs(y = NULL)
      }

      # Y ---
      # Cas ou il faut ecrire un nom pour l'axe y
      if(any(is.null(ylab), ylab != "")){
        # Si le nom de l'axe y est defini par l'utilisateur
        if(!is.null(ylab)){
          graph <- graph +
            labs(x = ylab)
        }

        # FONCTIONS SPECIFIQUES -----

        # Pour many_val : pas de nom en y car ce sont differents indicateurs
        if(is.null(ylab) & type == "many_val"){
          graph <- graph +
            labs(x = NULL)
        }
        # Pour many_val_group
        # Avec position == "flip" => pas de nom en y car ce sont differents indicateurs
        if(is.null(ylab) & type == "many_val_group" & position == "flip"){
          graph <- graph +
            labs(x = NULL)
        }
        # ggplot affiche la condition ecrite pour l'axe x => on veut la variable de groupe
        if(is.null(ylab) & type == "many_val_group" & (position != "flip")){
          graph <- graph +
            labs(x = group)
        }
        # FIN FONCTIONS SPECIFIQUES -----

      # Tous les autres cas => il ne faut pas de nom pour l'axe y
      } else {
        graph <- graph +
          labs(x = NULL)
      }

      # LEGEND ---
      # Cas ou la legende est definie par l'utilisateur
      if(all(!is.null(legend_lab), legend_lab != "")){
        graph <- graph +
          labs(fill = stringr::str_wrap(legend_lab, wrap_width_leg))
      }
      # Cas ou l'utilisateur ne veut pas de legende
      if(all(!is.null(legend_lab), legend_lab == "")){
        graph <- graph +
          labs(fill = NULL)
      }

    # /!\ Pour distrib_continuous() et distrib_group_continuous() => x et y inverses car pas de coord_flip()
    } else {
      # X ---
      if(any(is.null(xlab), xlab != "")){
        graph <- graph +
          labs(x = ifelse(is.null(xlab),
                          paste0(x_exp),
                          xlab))
      } else {
        graph <- graph +
          labs(x = NULL)
      }

      # Y ---
      if(any(is.null(ylab), ylab != "")){
        if(!is.null(ylab)){
          graph <- graph +
            labs(y = ylab)
        }
        if(is.null(ylab)){
          graph <- graph +
            # Le group ou "densite" selon qu'il y a un groupe ou non
            labs(y = if(type == "distrib_group_continuous") group else lang_note_axis_y)
        }
      } else {
        graph <- graph +
          labs(y = NULL)
      }
    }
  }

  # Masquer les axes si show_labs == FALSE
  if(show_labs == FALSE & type == "default"){
    graph <- graph +
      labs(x = NULL,
           y = NULL,
           fill = NULL)
  }
  if(show_labs == FALSE & type %in% c("many_val", "many_val_group", "distrib_continuous", "distrib_group_continuous")){
    graph <- graph +
      labs(x = NULL,
           y = NULL)
  }

  # Partie specifique pour la legende de many_val_group
  if(type == "many_val_group"){
    # LEGEND ---
    if(all(!is.null(legend_lab), legend_lab != "")){
      graph <- graph +
        labs(fill = stringr::str_wrap(legend_lab, wrap_width_leg))
    } else {
      graph <- graph +
        labs(fill = NULL)
    }
  }

  return(graph)

}


#' create_palette
#'
#' @param pal Value of argument "pal" from the original function
#' @param levels_palette Number of colors needed
#' @param direction Value of argument "direction" from the original function
#' @param name_function Name of the function in which we are
#' @param desaturate Value of argument "desaturate" from the original function
#' @param lighten Value of argument "lighten" from the original function
#' @param darken Value of argument "darken" from the original function
#'
#' @noRd
#'
create_palette <- function(pal,
                           levels_palette,
                           direction,
                           name_function,
                           desaturate,
                           lighten,
                           darken) {

  # Fonction qui change la palette selon la fonction (utilise plus bas)
  palette_function <- function(levels_palette2 = levels_palette,
                               direction2 = direction,
                               name_function2 = name_function){

    if(name_function2 == "prop_group") fonctionr_pal = "OBSS_Relax"
    if(name_function2 == "central_group") fonctionr_pal = "OBSS_Spring"
    if(name_function2 == "make_surface") fonctionr_pal = "OBSS_Autumn"
    if(name_function2 %in% c("many_val_group", "many_val")) fonctionr_pal = "OBSS_alt3"
    if(name_function2 == "distrib_group_discrete") fonctionr_pal = "OBSS"

    palette_created <- as.character(official_pal(inst = fonctionr_pal, n = levels_palette2, direction = direction2))

    return(palette_created)
  }
  # Si pal est rempli par l'utilisateur
  if(!is.null(pal)){
    # Si pal est de longueur 1 (= le nom d'1 palette)
    if(length(pal) == 1){

      # On cree la palette avec le package MetBrewer
      # NOTE : on utilise all() dans la condition car si pal est NULL, la condition donne logical(0)
      if (pal %in% c(
        "Archambault", "Austria", "Benedictus", "Cassatt1", "Cassatt2", "Cross", "Degas", "Demuth", "Derain", "Egypt",
        "Gauguin", "Greek", "Hiroshige", "Hokusai1", "Hokusai2", "Hokusai3", "Homer1", "Homer2", "Ingres", "Isfahan1",
        "Isfahan2", "Java", "Johnson", "Juarez", "Kandinsky", "Klimt", "Lakota", "Manet", "Monet", "Moreau",
        "Morgenstern", "Nattier", "Navajo", "NewKingdom", "Nizami", "OKeeffe1", "OKeeffe2", "Paquin", "Peru1", "Peru2",
        "Pillement", "Pissaro", "Redon", "Renoir", "Signac", "Tam", "Tara", "Thomas", "Tiepolo", "Troy",
        "Tsimshian", "VanGogh1", "VanGogh2", "VanGogh3", "Veronese", "Wissing")
        ){
        # Check pour voir si le package est installe, etant en 'Suggests'
        rlang::check_installed("MetBrewer")
        palette <- as.character(MetBrewer::met.brewer(name = pal, n = levels_palette, type = "continuous", direction = direction))

      # On cree la palette avecle package PrettyCols
      } else if(pal %in% c(
        "Blues", "Purples", "Tangerines", "Greens", "Pinks", "Roses", "Teals",
        "Yellows", "Reds", "Greys", "Aubergines", "Browns", "PurpleGreens", "PinkGreens",
        "TangerineBlues", "PurpleTangerines", "PurplePinks", "TealGreens", "PurpleYellows", "RedBlues", "Bold",
        "Dark", "Light", "Beach", "Fun", "Sea", "Bright", "Relax",
        "Lucent", "Lively", "Joyful", "Coast", "Ocean", "Peppers", "Disco",
        "Prism", "Neon", "Oasis", "Celestial", "Aurora", "Spring", "Summer",
        "Autumn", "Winter", "Rainbow", "Velvet")
        ){
        rlang::check_installed("PrettyCols")
        palette <- as.character(PrettyCols::prettycols(palette = pal, n = levels_palette, type = "continuous", direction = direction))

      # On cree la palette avec la fonction interne official_pal()
      } else if(pal %in% official_pal(list_pal_names = TRUE)){
        palette <- as.character(official_pal(inst = pal, n = levels_palette, direction = direction))

      # Si la couleur/palette n'est pas valide => defaut
      } else {
        palette <- palette_function()
        warning("The specified palette does not exist: the default palette is used", call. = FALSE)
      }
    # Si pal est de longueur > 1 avec uniquement des couleurs valides
    } else if(length(pal) > 1 & all(isColor(pal))){
      # S'il y a le bon nombre de couleurs
      if(length(pal) == levels_palette){
        palette <- pal
        # Pour rendre compatible avec direction, meme si rien ne doit etre change dans la palette !
        if(direction == -1){
          palette <- rev(palette)
        }
      }
      # S'il n'y a PAS le bon nombre de couleurs => defaut
      if(length(pal) != levels_palette){
        palette <- palette_function()
        warning("The specified palette does not contain the correct number of colors: the default palette is used.", call. = FALSE)
      }
    # Si aucun de ces cas => defaut
    } else {
      palette <- palette_function()
      warning("The specified palette is invalid: the default palette is used", call. = FALSE)
    }
  }
  # Si pal est NULL => defaut
  if(is.null(pal)){
    palette <- palette_function()
  }

  # Pour alterer la palette (desaturer, eclaircir, foncer)
  palette <- fonctionr_alter_cols(
    cols = palette,
    desaturate = desaturate,
    lighten = lighten,
    darken = darken
  )

  return(palette)
}


#' count_NA_deleted
#'
#' Internal function to mention how many NA have been deleted
#'
#' @param x Variable on wich to count NA.
#' @param type type of variable to show on the message (group, group.fill, facet...)
#'
#' @noRd
#'

count_NA_deleted <- function(x,
                             type) {

  count <- sum(is.na(x))
  message(count, " observation(s) removed due to missing ", type)

}


#' fonctionr_cores_detect
#'
#' Internal function to detect cores number of the CPU
#'
#' @noRd
#'

fonctionr_cores_detect <- function() {

  # On detecte le nombre de cores du CPU
  n.cores <- parallel::detectCores()

  # On parallelise a n-1 core ssi 3 cores ou plus, sinon 1 core
  if(n.cores >= 3) {
    cores <- n.cores - 1
  } else {
    cores <- 1
  }
  # On cree un cluster avec le nombre de cores definis ci-dessus
  doParallel::registerDoParallel(cores)

  message("Parallel computing enabled with ", cores, " cores")

}


#' fonctionr_filter
#'
#' Internal function to filter rows
#'
#' @param data data in the original function
#' @param fonction type of original function. Used to identify which filter on NA applies: groups, quali_var, quanti_exp...
#' @param filter_exp filter expression in the original function
#' @param na.rm.facet na.rm.facet in the original function, used to identify whether the NA facet should be excluded.
#' @param facet facet variable in the original function
#' @param na.rm.group na.rm.group in the original function, used to identify whether the NA group and group.fill should be excluded.
#' @param group group variable in the original function
#' @param group.fill group.fill variable in the original function
#' @param na.rm.var na.rm.var in the original function, used to identify whether the NA modality in quali_var should be excluded.
#' @param quali_var categorial variable, quali_var, in the original function
#' @param na.prop na.prop in the original function, used to identify whether the NAs in prop_exp should be excluded.
#' @param na.vars na.rm.var in the original function, used to identify whether the obervations with at least one NA in an interest variables should be excluded.
#' @param vec_list_vars list_vars in the original function
#'
#' @import rlang
#' @import srvyr
#'
#' @noRd
#'

fonctionr_filter <- function(data,
                             fonction,
                             filter_exp,
                             na.rm.facet,
                             facet,
                             na.rm.group,
                             group,
                             group.fill,
                             na.rm.var,
                             quali_var,
                             na.prop,
                             na.vars,
                             vec_list_vars
                             ) {

  message("Numbers of observation(s) removed by each filter (one after the other): ")

  # Nombre de lignes initiales avant tout filtre
  n_before <- nrow(data)
  data <- data |>
    mutate(fonctionr_rows_to_keep = TRUE)

  # On filtre si une expression de filtrage est indiquee
  if(!quo_is_null(enquo(filter_exp))){
    data <- data |>
      mutate(
        fonctionr_rows_to_keep = {{ filter_exp }},
        # Quand un resultat du mutate sur filter_exp donne NA, alors il doit etre exclu (car filter les exclut de base => on doit correspondre)
        fonctionr_rows_to_keep = ifelse(is.na(fonctionr_rows_to_keep), FALSE, fonctionr_rows_to_keep)
        )

    n_after <- sum(data$variables$fonctionr_rows_to_keep)
    message(n_before - n_after, " observation(s) removed by filter_exp")
    n_before <- n_after
  }
  # On filtre facet
  if(na.rm.facet == TRUE) {
    if(!quo_is_null(enquo(facet))){
      data <- data |>
        mutate(fonctionr_rows_to_keep = ifelse(fonctionr_rows_to_keep, !is.na({{ facet }}), FALSE))

      n_after <- sum(data$variables$fonctionr_rows_to_keep)
      message(n_before - n_after, " observation(s) removed due to missing facet")
      n_before <- n_after
    }
  }

  # On filtre group # uniquement pour les fonctions ci-dessous (avec un groupe)
  if(fonction %in% c("central_group", "prop_group","distrib_group_discrete","distrib_group_continuous","many_val_group")){
    if(na.rm.group == TRUE) {
        data <- data |>
          mutate(fonctionr_rows_to_keep = ifelse(fonctionr_rows_to_keep, !is.na({{ group }}), FALSE))

        n_after <- sum(data$variables$fonctionr_rows_to_keep)
        message(n_before - n_after, " observation(s) removed due to missing group")
        n_before <- n_after
      }
      # On filtre group.fill # uniquement pour les fonctions ci-dessous (avec un sous-groupe possible)
      if(fonction %in% c("central_group", "prop_group")){
        if(!quo_is_null(enquo(group.fill))){
          data <- data |>
            mutate(fonctionr_rows_to_keep = ifelse(fonctionr_rows_to_keep, !is.na({{ group.fill }}), FALSE))

          n_after <- sum(data$variables$fonctionr_rows_to_keep)
          message(n_before - n_after, " observation(s) removed due to missing group.fill")
          n_before <- n_after
        }
      }
  }

  #filtre quali_var
  if(fonction %in% c("distrib_group_discrete", "distrib_discrete")){
    if(na.rm.var == TRUE) {
      data <- data |>
        mutate(fonctionr_rows_to_keep = ifelse(fonctionr_rows_to_keep, !is.na({{ quali_var }}), FALSE))

      n_after <- sum(data$variables$fonctionr_rows_to_keep)
      message(n_before - n_after, " observation(s) removed due to missing quali_var")
      n_before <- n_after
    }
  }
  if(fonction == "prop_group"){
    if(na.prop == "rm"){
      data <- data |>
        mutate(fonctionr_rows_to_keep = ifelse(fonctionr_rows_to_keep, !is.na(fonctionr_express_bin), FALSE))

      n_after <- sum(data$variables$fonctionr_rows_to_keep)
      message(n_before - n_after, " observation(s) removed due to missing value(s) for the variable(s) in prop_exp")
      n_before <- n_after
    }
  }
  if(fonction %in% c("central_group", "distrib_continuous", "distrib_group_continuous")){
    data <- data |>
      mutate(fonctionr_rows_to_keep = ifelse(fonctionr_rows_to_keep, !is.na(quanti_exp_flattened), FALSE))

    n_after <- sum(data$variables$fonctionr_rows_to_keep)
    message(n_before - n_after, " observation(s) removed due to missing value(s) for the variable(s) in quanti_exp")
    n_before <- n_after
  }
  if(fonction %in% c("many_val_group", "many_val")){
    # On supprime les NA sur la/les variable(s) entrees si na.vars == "rm.all" => de cette facon les effectifs sont les memes pour tous les indicateurs.
    if(na.vars == "rm.all"){

      for (var in vec_list_vars) {
        data <- data |>
          mutate(fonctionr_rows_to_keep = ifelse(fonctionr_rows_to_keep, !is.na(.data[[var]]), FALSE))
      }

      n_after <- sum(data$variables$fonctionr_rows_to_keep)
      message(n_before - n_after, " observation(s) removed due to missing value(s) in at least one of the variables")
      n_before <- n_after
    } else {
      warning("With na.vars = 'rm', observations removed differ between variables", call. = FALSE)
    }
  }

  # Si tout a ete filtre => pas de sens pour la suite
  if(all(data$variables$fonctionr_rows_to_keep == FALSE)){
    stop("All observations have been removed: nothing left to be analyzed!")
  }
  # On filtre SSI au moins 1 ligne doit etre supprimee
  if(any(data$variables$fonctionr_rows_to_keep == FALSE)){
    data <- data |>
      filter(fonctionr_rows_to_keep == TRUE)
  }
  # on supprime la variable de filtre car on n'en a plus besoin
  data <- data |>
    select(-fonctionr_rows_to_keep)

  return(data)

}
