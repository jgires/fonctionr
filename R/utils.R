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
    message("Input: objet survey")
    data_W <- data |>
      as_survey_design()
  }
  if(any(class(data) %in% c("svyrep.design")) & all(class(data) %ni% c("tbl_svy"))){
    message("Input: objet survey")
    data_W <- data |>
      as_survey_rep()
  }
  # Si objet srvyr (avec replicates ou non)
  if(any(class(data) %in% c("tbl_svy"))){
    message("Input: objet srvyr")
    data_W <- data
  }
  # Si data.frame (pas de replicate prevu => A FAIRE A TERME)
  if(any(class(data) %ni% c("survey.design2","survey.design")) & any(class(data) %ni% c("tbl_svy")) & any(class(data) %in% c("data.frame"))){
    message("Input: data.frame")
    data_W <- data |>
      as_survey_design(...)
  }

  # On determine si replicates ou non (pour message plus bas)
  replicates <- FALSE
  if(any(class(data) %in% c("svyrep.design"))){
    replicates <- TRUE
  }

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
#' Available fonts, included in the package itself, are "Roboto", "Montserrat", "Gotham Narrow", "Helvetica Neue", "League Gothic" and "Amatic". Default is "Roboto".
#'
#' @export
#'
#' @examples
#' # Loading of fonts from fonctionr. You can now use it in fonctionr !
#' load_and_active_fonts()
#'
load_and_active_fonts <- function(){

  # On ajoute les polices contenues dans le package et on les active
  sysfonts::font_add(family = "Montserrat", regular = paste0(system.file("font", package = "fonctionr"), "/Montserrat-Regular.otf"))
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
  sysfonts::font_add(family = "Helvetica Neue", regular = paste0(system.file("font", package = "fonctionr"), "/HelveticaNeueRoman.otf"))
  sysfonts::font_add(family = "League Gothic", regular = paste0(system.file("font", package = "fonctionr"), "/LeagueGothic-Regular.otf"))
  sysfonts::font_add(family = "Amatic", regular = paste0(system.file("font", package = "fonctionr"), "/AmaticSC-Regular.ttf"))
  showtext::showtext_auto()
}


#' export_excel
#'
#' Internal function to export results of fonctionr's functions into an excel file.
#'
#' @param tab_excel A dataframe with the results calculated by fonctionr's functions.
#' @param graph ggplot object showing results of fonctionr's functions.
#' @param test_stat_excel A dataframe with results of a statistical test on the data.
#' @param quantiles A dataframe with quantiles for density functions.
#' @param density A dataframe with density coordinates for density functions.
#' @param facet_null A logical vector. TRUE if no facet.
#' @param export_path Path to export the results in an xlsx file.
#' @param percent_fm A logical vector. TRUE if results are percentages.
#' @param fgFill Color of first row in exported excel file.
#' @param bivariate A logical vector. TRUE if results are bivariate.
#' @param dens A string vector indicating the type of density result to be exported. Possible values: "uni", "group" or "none" (not a density calculation).
#'
#' @import openxlsx
#'
#' @noRd
#'
export_excel <- function(tab_excel,
                         graph,
                         test_stat_excel,
                         quantiles = NULL,
                         density = NULL,
                         facet_null = NULL,
                         export_path,
                         percent_fm = NULL,
                         fgFill,
                         bivariate = NULL,
                         dens = "none") {

  # Pour etre integre au fichier excel, le graphique doit etre affiche => https://ycphs.github.io/openxlsx/reference/insertPlot.html
  print(graph)

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

  wb <- createWorkbook() # On cree l'objet dans lequel on va formater toutes les infos en vue d'un export en fichier Excel
  addWorksheet(wb, name_tab) # On ajoute une feuille pour les resultats / indices centraux (densite)
  if(dens != "none"){ # Si densite
    addWorksheet(wb, "Quantiles") # On ajoute une feuille pour les quantiles
    addWorksheet(wb, "Densite") # On ajoute une feuille pour la densite
  }
  addWorksheet(wb, "Graphique") # On ajoute une feuille pour le graphique
  addWorksheet(wb, "Test statistique") # On ajoute une feuille pour le resultat du test stat

  # On ecrit les donnees
  writeData(wb, name_tab, tab_excel, keepNA = TRUE, na.string = "NA") # On ecrit les resultats en gardant les NA
  if(dens != "none"){ # Si densite
    writeData(wb, "Quantiles", quantiles, keepNA = TRUE, na.string = "NA") # On ecrit les quantiles en gardant les NA
    writeData(wb, "Densite", density, keepNA = TRUE, na.string = "NA") # On ecrit la densite en gardant les NA
  }
  insertPlot(wb,"Graphique", dpi = 90, width = 12, height = 7)
  writeData(wb, "Test statistique", test_stat_excel) # On ecrit le resultat du test stat

 # On definit les styles
  setColWidths(wb, name_tab, widths = 20, cols = 1:ncol(tab_excel))
  if(dens != "none"){ # Si densite
    setColWidths(wb, "Quantiles", widths = 20, cols = 1:ncol(quantiles))
    setColWidths(wb, "Densite", widths = 20, cols = 1:ncol(density))
  }
  hs <- createStyle(fontColour = "#ffffff", fgFill = fgFill,  # Style de la premiere ligne
                    halign = "center", textDecoration = "Bold",
                    fontName = "Arial Narrow")
  firstC <- createStyle (halign = "left", textDecoration = "Bold", # Style de la premiere colonne
                         fontName = "Arial Narrow")
  body <- createStyle (halign = "center", # Style des cellules du tableau
                       fontName = "Arial Narrow")
  percent <- createStyle(numFmt = "percentage")

  # On applique le style a la premiere ligne
  addStyle(wb, name_tab, hs, cols = 1:ncol(tab_excel), rows = 1)
  if(dens != "none"){
    addStyle(wb, "Quantiles", hs, cols = 1:ncol(quantiles), rows = 1)
    addStyle(wb, "Densite", hs, cols = 1:ncol(density), rows = 1)
  }

  # On applique le style aux reste des lignes a partir de la 2e colonne (car la 1ere est en gras)
  # SAUF pour la densite univariee ou la 1ere col n'est pas en gras (et donc on fait 2-sub_dens qui est = 1)
  addStyle(wb, name_tab, body, cols = (2-sub_dens):ncol(tab_excel), rows = 2:(nrow(tab_excel)+1), gridExpand = TRUE, stack = TRUE)
  if(dens != "none"){
    # Pour les quantiles 3-sub_dens car il y au moins la 1ere colonne en gras (et les 2 premieres lorsque dens par groupe)
    addStyle(wb, "Quantiles", body, cols = (3-sub_dens):ncol(quantiles), rows = 2:(nrow(quantiles)+1), gridExpand = TRUE, stack = TRUE)
    addStyle(wb, "Densite", body, cols = (2-sub_dens):ncol(density), rows = 2:(nrow(density)+1), gridExpand = TRUE, stack = TRUE)
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
    addStyle(wb, name_tab, firstC, cols = 1:(1+facet_add+bivar_add), rows = 2:(nrow(tab_excel)+1), gridExpand = TRUE, stack = TRUE)
  }
  # Particulier a densite
  if(dens != "none"){
    addStyle(wb, "Quantiles", firstC, cols = 1:(1+facet_add+(1-sub_dens)), rows = 2:(nrow(quantiles)+1), gridExpand = TRUE, stack = TRUE)
    # if statement car pas de 1ere colonne en gras pour densite univariee
    if(dens == "group"){
      addStyle(wb, "Densite", firstC, cols = 1:(1+facet_add), rows = 2:(nrow(density)+1), gridExpand = TRUE, stack = TRUE)
    }
  }
  # On applique le style de pourcentage aux proportions
  if (percent_fm == TRUE) {
    addStyle(wb, name_tab, percent, cols = (2+facet_add+bivar_add):(4+facet_add+bivar_add), rows = 2:(nrow(tab_excel)+1), gridExpand = TRUE, stack = TRUE)
  }

  # Styles pour le test stat (a part pour lisibilite)
  setColWidths(wb, "Test statistique", widths = 20, cols = 1:ncol(test_stat_excel)) # Largeur des colonnes
  hs2 <- createStyle(fontColour = "#ffffff", fgFill = "grey15",  # Style de la premiere ligne
                     halign = "center", textDecoration = "Bold",
                     fontName = "Arial Narrow")
  body2 <- createStyle (fontName = "Arial Narrow") # Style des cellules du tableau

  addStyle(wb, "Test statistique", hs2, cols = 1:ncol(test_stat_excel), rows = 1) # On applique le style a la premiere ligne
  addStyle(wb, "Test statistique", firstC, cols = 1, rows = 2:(nrow(test_stat_excel)+1), gridExpand = TRUE, stack = TRUE) # On applique le style a la premiere colonne (sans la premiere ligne)
  addStyle(wb, "Test statistique", body2, cols = 2:ncol(test_stat_excel), rows = 2:(nrow(test_stat_excel)+1), gridExpand = TRUE, stack = TRUE) # On applique le style aux reste des cellules

  # On sauvegarde le fichier excel
  saveWorkbook(wb, export_path, overwrite = TRUE)

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
                      short = T,
                      type) {
  for(check_i in seq_along(arg)){
    if(!is.null(arg[[check_i]])){
      if(short == T){
        if (!length(arg[[check_i]]) == 1) stop(paste("L'argument", names(arg)[[check_i]], "n'a pas la bonne longueur (max 1)"), call. = FALSE)
      }
      if(all(!is.na(arg[[check_i]]))){
        if(type == "character"){
          if (!is.character(arg[[check_i]])) stop(paste("L'argument", names(arg)[[check_i]], "n'est pas au bon format (caractere)"), call. = FALSE)
        }
        if(type == "logical"){
          if (!is.logical(arg[[check_i]])) stop(paste("L'argument", names(arg)[[check_i]], "n'est pas au bon format (logique)"), call. = FALSE)
        }
        if(type == "numeric"){
          if (!is.numeric(arg[[check_i]])) stop(paste("L'argument", names(arg)[[check_i]], "n'est pas au bon format (numerique)"), call. = FALSE)
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
    if (!all(data[[var]] %in% c(0,1,NA))) stop(paste("La colonne", var, "doit etre binaire (0-1 ou TRUE-FALSE)"), call. = FALSE)
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
        if (!(min_check >= 0 & max_check <= 1)) stop(paste("L'argument", names(arg)[[check_i]], "doit contenir des valeurs entre 0 et 1"), call. = FALSE)
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
      if (!vars_input_char[i] %in% names(data)) stop(paste("La colonne", vars_input_char[i], "introduite dans", names(vars_input_char[i]) , "n'est pas presente dans data"), call. = FALSE)
    }
  }
  # Si objet sondage
  if(any(class(data) %in% c("survey.design2","survey.design","tbl_svy","svyrep.design"))){
    for (i in seq_along(vars_input_char)) {
      if (!vars_input_char[i] %in% names(data[["variables"]])) stop(paste("La colonne", vars_input_char[i], "introduite dans", names(vars_input_char[i]) , "n'est pas presente dans data"), call. = FALSE)
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
  x <- stringr::str_replace_all(stringr::str_wrap(x, width = wrap_width), "\n", "<br>")
  return(x)
}


#' official_pal
#'
#' Function to produce color palettes from different institutions
#'
#' @param inst Name of the palette.
#' @param n Number of colors.
#' @param direction Direction of the palette color. Default is 1. The opposite direction is -1.
#' @param desaturate Numeric specifying the amount of desaturation where 1 corresponds to complete desaturation, 0 to no desaturation, and values in between to partial desaturation.
#' @param lighten Numeric specifying the amount of lightening. Negative numbers cause darkening.
#' @param darken Numeric specifying the amount of lightening. Negative numbers cause lightening.
#' @param show_pal TRUE to display a graph representing the specified color palette.
#' @param font Font used in the graphic. See load_and_active_fonts() for available fonts.
#' @param list_pal_names TRUE to generate a vector with palette names.
#'
#' @return A vector containing hexadecimals color codes
#' @export
#'
#' @examples
#' official_pal("OBSS", 8, show_pal = TRUE)
#' official_pal("OBSS_Greens", 8, show_pal = TRUE)
#' official_pal("OBSS_div_mid4", 7, show_pal = TRUE)
#' official_pal("OBSS_div_bi3", 8, show_pal = TRUE)
#' official_pal("IBSA", 4, show_pal = TRUE)
#' official_pal("ULB", 6, show_pal = TRUE)
#'

official_pal <- function(inst,
                         n,
                         direction = 1,
                         desaturate = 0,
                         lighten = 0,
                         darken = 0,
                         show_pal = F,
                         font = "Gotham Narrow",
                         list_pal_names = F
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
                 "ULB")

  if(list_pal_names == F){

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
      if(stringr::str_detect(inst, "OBSS_div_bi")){
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
      if(stringr::str_detect(inst, "OBSS_div_bi")){
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
    if(desaturate != 0){
      palette <- colorspace::desaturate(palette, desaturate)
    }
    if(lighten != 0){
      palette <- colorspace::lighten(palette, lighten)
    }
    if(darken != 0){
      palette <- colorspace::darken(palette, darken)
    }

    # Inversion de la palette si demande
    if(direction == -1){
      palette <- rev(palette)
    }

    # On retourne les codes hex
    if(show_pal == F){
      return(palette)
    }

    # On affiche graphiquement la palette
    if(show_pal == T){

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
  if(list_pal_names == T){

    return(pal_names)

  }
}


#' theme_fonctionr
#'
#' @param font Font used in the graphic. See load_and_active_fonts() for available fonts.
#' @param theme The optionnal theme you want for the graphic. Available theme: “IWEPS”. Default is NULL
#' @param display The way theme_fonctionr() works on the axis texts: like ggplot2 or ggtext.
#' @param grid.lines Specify major grid lines : "x", "y" or "both". Default is "x".
#' @param coef_font A multiplier factor for font size.
#'
#' @import ggplot2
#' @export
#'
theme_fonctionr <- function(font = "Roboto",
                            theme = NULL,
                            display = "ggplot",
                            grid.lines = "x",
                            coef_font = 1) {
  load_and_active_fonts()

  theme_fonctionr_def <- theme_minimal(
    base_size = coef_font * 11
  ) +
    theme(
      text = element_text(family = font),
      panel.grid.minor.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.line.x = element_line(color = "black"),
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_line(color = "black"),
      plot.margin = margin(10, 15, 10, 10),
      plot.caption = element_text(
        color = "grey30"
      )
    )

  if (grid.lines == "x") {
    theme_fonctionr_def <- theme_fonctionr_def +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "#dddddd")
        )
  }

  if (grid.lines == "y") {
    theme_fonctionr_def <- theme_fonctionr_def +
      theme(
        panel.grid.major.y = element_line(color = "#dddddd"),
        panel.grid.major.x = element_blank()
        )
  }

  if (grid.lines == "both") {
    theme_fonctionr_def <- theme_fonctionr_def +
      theme(
        panel.grid.major.y = element_line(color = "#dddddd"),
        panel.grid.major.x = element_line(color = "#dddddd")
      )
  }

  if (display == "ggplot") {
    theme_fonctionr_def <- theme_fonctionr_def +
      theme(
        axis.text = element_text(color = "black")
        )
  }

  if (display == "ggtext") {
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

  if (!is.null(theme)) {
    if (theme == "IWEPS") {
      theme_fonctionr_def <- theme_fonctionr_def +
        theme(
          axis.line.y = element_line(color = "black"),
          axis.ticks.y = element_line(color = "black")
        )
    }
  }

  return(theme_fonctionr_def)
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

    if(name_function2 == "prop_group"){
      palette_created <- as.character(PrettyCols::prettycols(palette = "Coast", n = levels_palette2, type = "continuous", direction = direction2))
    }
    if(name_function2 == "central_group"){
      palette_created <- as.character(PrettyCols::prettycols(palette = "Peppers", n = levels_palette2, type = "continuous", direction = direction2))
    }
    if(name_function2 == "make_surface"){
      palette_created <- as.character(MetBrewer::met.brewer(name = "Kandinsky", n = levels_palette2, type = "continuous", direction = direction2))
    }
    if(name_function2 %in% c("many_val_group", "many_val")){
      palette_created <- as.character(MetBrewer::met.brewer(name = "Egypt", n = levels_palette2, type = "continuous", direction = direction2))
    }
    if(name_function2 == "distrib_group_discrete"){
      palette_created <- as.character(MetBrewer::met.brewer(name = "Hokusai1", n = levels_palette2, type = "continuous", direction = direction2))
    }

    return(palette_created)
  }
  # Si pal est rempli par l'utilisateur
  if(!is.null(pal)){
    # Si pal est de longueur 1 (= soit 1 couleur soit le nom d'1 palette)
    if(length(pal) == 1){

      # On cree la palette avec le package MetBrewer
      # NOTE : on utilise all() dans la condition car si pal est NULL, la condition donne logical(0)
      if(pal %in% names(MetBrewer::MetPalettes)){
        palette <- as.character(MetBrewer::met.brewer(name = pal, n = levels_palette, type = "continuous", direction = direction))

      # On cree la palette avecle package PrettyCols
      } else if(pal %in% names(PrettyCols::PrettyColsPalettes)){
        palette <- as.character(PrettyCols::prettycols(palette = pal, n = levels_palette, type = "continuous", direction = direction))

      # On cree la palette avec la fonction interne official_pal()
      } else if(pal %in% official_pal(list_pal_names = T)){
        palette <- as.character(official_pal(inst = pal, n = levels_palette, direction = direction))

      # Particulier a many_val() : on cree une palette unicolore (pour que tous les indicateurs aient la meme couleur => pas de sens ailleurs)
      } else if(isColor(pal) & name_function == "many_val"){
        palette <- rep(pal, levels_palette)

      # Si la couleur/palette n'est pas valide => defaut
      } else {
        palette <- palette_function()
        warning("La palette indiquee n'existe pas : la palette par defaut est utilisee")
      }
    # Si pal est de longueur > 1 avec uniquement des couleurs valides
    } else if(length(pal) > 1 & all(isColor(pal))){
      # S'il y a le bon nombre de couleurs
      if(length(pal) == levels_palette){
        palette <- pal
      }
      # S'il n'y a PAS le bon nombre de couleurs => defaut
      if(length(pal) != levels_palette){
        palette <- palette_function()
        warning("La palette indiquee ne contient pas le bon nombre de couleurs : la palette par defaut est utilisee")
      }
    # Si aucun de ces cas => defaut
    } else {
      palette <- palette_function()
      warning("La palette indiquee n'est pas valide : la palette par defaut est utilisee")
    }
  }
  # Si pal est NULL => defaut
  if(is.null(pal)){
    palette <- palette_function()
  }

  # Pour modifier la palette (desaturer, eclaircir, foncer)
  if(desaturate != 0){
    palette <- colorspace::desaturate(palette, desaturate)
  }
  if(lighten != 0){
    palette <- colorspace::lighten(palette, lighten)
  }
  if(darken != 0){
    palette <- colorspace::darken(palette, darken)
  }

  return(palette)
}
