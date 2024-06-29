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
    data_W <- data %>%
      as_survey_design()
  }
  if(any(class(data) %in% c("svyrep.design")) & all(class(data) %ni% c("tbl_svy"))){
    message("Input: objet survey")
    data_W <- data %>%
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
    data_W <- data %>%
      as_survey_design(...)
  }

  # message("Variables du design :", " cluster : ", paste(names(data_W$cluster), collapse = " "), " | strata : ",  paste(names(data_W$strata), collapse = " "), " | weights : ",  paste(names(data_W$allprob), collapse = " "))

  # On extrait les variables du design
  attr_design <- attr(data_W, "survey_vars") %>%
    as.character() %>%
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
  message("Sampling design -> ", paste(vec_design, collapse = " "))

  return(data_W)

}


#' load_and_active_fonts
#'
#' Function to load and activate fonctionr's built-in fonts.
#' Available fonts, included in the package itself, are "Roboto", "Montserrat", "Gotham Narrow", "Helvetica Neue", "League Gothic" and "Amatic". Default is "Roboto".
#'
#' @return
#' @export
#'
#' @examples
#'
load_and_active_fonts <- function(){

  # On ajoute les polices contenues dans le package et on les active
  sysfonts::font_add(family = "Montserrat", regular = paste0(system.file("font", package = "fonctionr"), "/Montserrat-Regular.otf"))
  sysfonts::font_add(family = "Roboto", regular = paste0(system.file("font", package = "fonctionr"), "/Roboto-Regular.ttf"))
  sysfonts::font_add(family = "Gotham Narrow", regular = paste0(system.file("font", package = "fonctionr"), "/GothamNarrow-Book.otf"))
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
#' @param type Type of check
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
#' @param data A dataframe in which to check
#' @param vec_list_vars A vector containing names of columns
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
#' @param vec_list_vars A named vector containing names of columns and names of input arguments
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


#' official_pal
#'
#' Internal function to produce color palettes from different institutions
#'
#' @param inst name of the institution
#' @param n number of colors
#'
#' @noRd
#'

official_pal <- function(inst,
                         n,
                         direction = 1){

  if(all(direction != -1, direction != 1)){
    stop("Direction not valid. Please use 1 for standard palette or -1 for reversed palette.")
  }

  if(inst == "OBSS"){
    pal_fct <- grDevices::colorRampPalette(c("#ff87a5", "#ffb900", "#00e1af", "#375078"))
  }
  if(inst == "IBSA"){
    pal_fct <- grDevices::colorRampPalette(c("#D95A49", "#F0D0C8", "#562821", "#9A9A9A"))
  }

  palette <- pal_fct(n)

  if(direction == -1){
    palette <- rev(palette)
  }

  return(palette)
}


#' theme_fonctionr
#'
#' @param font Font used in the graphic. See load_and_active_fonts() for available fonts.
#'
#' @return
#' @import ggplot2
#' @export
#'
#' @examples
#'
theme_fonctionr <- function(font = "Roboto") {

  load_and_active_fonts()

  theme <- theme_minimal() +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "#dddddd"),
    text = element_text(family = font),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_line(color = "black"),
    axis.text = element_text(color = "black"),
    plot.margin = margin(10, 15, 10, 10),
    plot.caption = element_text(
      color = "grey30"
    )
  )

  return(theme)
}
