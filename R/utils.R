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
  # Si data.frame (pas de replicate prévu => A FAIRE A TERME)
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
#' Fonction pour charger et activer les polices de fonctionr
#'
#' @return
#' @import showtext
#' @import sysfonts
#' @export
#'
#' @examples
#'
load_and_active_fonts <- function(){

  # On ajoute les polices contenues dans le package et on les active
  font_add(family = "Montserrat", regular = paste0(system.file("font", package = "fonctionr"), "/Montserrat-Regular.otf"))
  font_add(family = "Roboto", regular = paste0(system.file("font", package = "fonctionr"), "/Roboto-Regular.ttf"))
  font_add(family = "Gotham Narrow", regular = paste0(system.file("font", package = "fonctionr"), "/GothamNarrow-Book.otf"))
  showtext_auto()

}


#' export_excel
#'
#' Internal function to export results of fonctionr's functions into an excel file.
#'
#' @param tab_excel A dataframe with the results calculated by fonctionr's functions.
#' @param graph ggplot object showing results of fonctionr's functions.
#' @param test_stat_excel A dataframe with results of a statistical test on the data.
#' @param facet_null A logical vector. TRUE if no facet.
#' @param export_path Path to export the results in an xlsx file.
#' @param percent_fm A logical vector. TRUE if results are percentages.
#' @param fgFill Color of first row in exported excel file.
#' @param bivariate A logical vector. TRUE if results are bivariate.
#'
#' @import openxlsx
#' @import broom
#'
#' @noRd
#'
export_excel <- function(tab_excel,
                         graph,
                         test_stat_excel,
                         facet_null = NULL,
                         export_path,
                         percent_fm = NULL,
                         fgFill,
                         bivariate = NULL) {

  # Pour être intégré au fichier excel, le graphique doit être affiché => https://ycphs.github.io/openxlsx/reference/insertPlot.html
  print(graph)

  # Je formate un fichier Excel dans lequel j'exporte les résultats

  wb <- createWorkbook() # On crée l'objet dans lequel on va formater toutes les infos en vue d'un export en fichier Excel
  addWorksheet(wb, "Résultats") # On ajoute une feuille pour les résultats
  addWorksheet(wb, "Graphique") # On ajoute une feuille pour le graphique
  addWorksheet(wb, "Test statistique") # On ajoute une feuille pour le résultat du test stat

  writeData(wb, "Résultats", tab_excel, keepNA = TRUE, na.string = "NA") # On écrit les résultats en gardant les NA
  insertPlot(wb,"Graphique", dpi = 90, width = 12, height = 7)
  writeData(wb, "Test statistique", test_stat_excel) # On écrit le résultat du test stat

  setColWidths(wb, "Résultats", widths = 20, cols = 1:ncol(tab_excel)) # Largeur des colonnes
  hs <- createStyle(fontColour = "#ffffff", fgFill = fgFill,  # Style de la première ligne
                    halign = "center", textDecoration = "Bold",
                    fontName = "Arial Narrow")
  firstC <- createStyle (halign = "left", textDecoration = "Bold", # Style de la première colonne
                         fontName = "Arial Narrow")
  body <- createStyle (halign = "center", # Style des cellules du tableau
                       fontName = "Arial Narrow")
  percent <- createStyle(numFmt = "percentage")

  addStyle(wb, "Résultats", hs, cols = 1:ncol(tab_excel), rows = 1) # On applique le style à la première ligne
  addStyle(wb, "Résultats", body, cols = 2:ncol(tab_excel), rows = 2:(nrow(tab_excel)+1), gridExpand = TRUE, stack = TRUE) # On applique le style aux reste des cellules

  # 2 options selon que la fonction produit un indicateur selon 1 variable ou selon 2 (= bivarié)
  # => C'est le cas des profils-lignes : il y a alors une colonne en plus à mettre en gras dans la mise en forme ci-dessous !
  bivar_add <- 0
  if (bivariate == TRUE) {
    bivar_add <- 1
  }

  # Des if statements dans le cas où le résultat est démultiplié par modalité de facet_var => Pas les mêmes règles vu qu'il y a une colonne en plus à mettre en gras
  if (!facet_null) {
    addStyle(wb, "Résultats", firstC, cols = 1:(2+bivar_add), rows = 2:(nrow(tab_excel)+1), gridExpand = TRUE, stack = TRUE) # On applique le style à la première colonne (sans la première ligne)
    if (percent_fm == TRUE) {
      addStyle(wb, "Résultats", percent, cols = (3+bivar_add):(5+bivar_add), rows = 2:(nrow(tab_excel)+1), gridExpand = TRUE, stack = TRUE) # On applique le style de pourcentage aux proportions
    }
  }
  if (facet_null) {
    addStyle(wb, "Résultats", firstC, cols = 1:(1+bivar_add), rows = 2:(nrow(tab_excel)+1), gridExpand = TRUE, stack = TRUE) # On applique le style à la première colonne (sans la première ligne)
    if (percent_fm == TRUE) {
      addStyle(wb, "Résultats", percent, cols = (2+bivar_add):(4+bivar_add), rows = 2:(nrow(tab_excel)+1), gridExpand = TRUE, stack = TRUE) # On applique le style de pourcentage aux proportions
    }
  }

  setColWidths(wb, "Test statistique", widths = 20, cols = 1:ncol(test_stat_excel)) # Largeur des colonnes
  hs2 <- createStyle(fontColour = "#ffffff", fgFill = "grey15",  # Style de la première ligne
                     halign = "center", textDecoration = "Bold",
                     fontName = "Arial Narrow")
  body2 <- createStyle (fontName = "Arial Narrow") # Style des cellules du tableau

  addStyle(wb, "Test statistique", hs2, cols = 1:ncol(test_stat_excel), rows = 1) # On applique le style à la première ligne
  addStyle(wb, "Test statistique", firstC, cols = 1, rows = 2:(nrow(test_stat_excel)+1), gridExpand = TRUE, stack = TRUE) # On applique le style à la première colonne (sans la première ligne)
  addStyle(wb, "Test statistique", body2, cols = 2:ncol(test_stat_excel), rows = 2:(nrow(test_stat_excel)+1), gridExpand = TRUE, stack = TRUE) # On applique le style aux reste des cellules

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

# Solution trouvée ici pour les messages : https://stackoverflow.com/questions/77432872/how-paste-be-used-as-a-message-with-r-stopifnot

check_arg <- function(arg,
                      short = T,
                      type) {
  for(check_i in seq_along(arg)){
    if(!is.null(arg[[check_i]])){
      if(short == T){
        if (!length(arg[[check_i]]) == 1) stop(paste("L'argument", names(arg)[[check_i]], "n'a pas la bonne longueur (max 1)"), call. = FALSE)
      }
      if(type == "character"){
        if (!is.character(arg[[check_i]])) stop(paste("L'argument", names(arg)[[check_i]], "n'est pas au bon format (caractère)"), call. = FALSE)
      }
      if(type == "logical"){
        if (!is.logical(arg[[check_i]])) stop(paste("L'argument", names(arg)[[check_i]], "n'est pas au bon format (logique)"), call. = FALSE)
      }
      if(type == "numeric"){
        if (!is.numeric(arg[[check_i]])) stop(paste("L'argument", names(arg)[[check_i]], "n'est pas au bon format (numérique)"), call. = FALSE)
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
    if (!all(data[[var]] %in% c(0,1,NA))) stop(paste("La colonne", var, "doit être binaire (0-1 ou TRUE-FALSE)"), call. = FALSE)
  }
}


#' theme_fonctionr
#'
#' @param font Font used in the graphic. Available fonts, included in the package itself, are "Roboto", "Montserrat" and "Gotham Narrow". Default is "Roboto".
#'
#' @return
#' @import ggplot2
#' @export
#'
#' @examples
#'
theme_fonctionr <- function(font = font) {
  theme_minimal() +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "#dddddd"),
    text = element_text(family = font),
    axis.line = element_line(color = "black"),
    axis.ticks = element_blank(),
    #axis.ticks = element_line(color = "black"),
    axis.text = element_text(color = "black"),
    plot.margin = margin(10, 15, 10, 10)
  )
}
