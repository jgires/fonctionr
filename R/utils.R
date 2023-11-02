#' Fonction pour convertir en objet srvyr
#'
#' @param data un dataframe, un objet srvyr ou survey
#' @param ...
#'
#' @import srvyr
#'
#' @NoRd
#'
convert_to_srvyr <- function(data, ...) {

  # Petite fonction utile
  `%ni%` = Negate(`%in%`)

  # Si objet survey (avec replicates ou non)
  if(any(class(data) %in% c("survey.design2","survey.design")) & all(class(data) %ni% c("tbl_svy"))){
    message("Input : objet survey")
    data_W <- data %>%
      as_survey_design()
  }
  if(any(class(data) %in% c("svyrep.design")) & all(class(data) %ni% c("tbl_svy"))){
    message("Input : objet survey")
    data_W <- data %>%
      as_survey_rep()
  }
  # Si objet srvyr (avec replicates ou non)
  if(any(class(data) %in% c("tbl_svy"))){
    message("Input : objet srvyr")
    data_W <- data
  }
  # Si data.frame (pas de replicate prévu => A FAIRE A TERME)
  if(any(class(data) %ni% c("survey.design2","survey.design")) & any(class(data) %ni% c("tbl_svy")) & any(class(data) %in% c("data.frame"))){
    message("Input : data.frame")
    data_W <- data %>%
      as_survey_design(...)
  }

  # message("Variables du design :", " cluster : ", paste(names(data_W$cluster), collapse = " "), " | strata : ",  paste(names(data_W$strata), collapse = " "), " | weights : ",  paste(names(data_W$allprob), collapse = " "))
  print(attr(data_W, "survey_vars"))

  return(data_W)

}


#' Fonction pour charger et activer les polices
#'
#' @import showtext
#' @import sysfonts
#'
#' @NoRd
#'
load_and_active_fonts <- function(){

  # On ajoute les polices contenues dans le package et on les active
  font_add(family = "Montserrat", regular = paste0(system.file("font", package = "fonctionr"), "/Montserrat-Regular.otf"))
  font_add(family = "Roboto", regular = paste0(system.file("font", package = "fonctionr"), "/Roboto-Regular.ttf"))
  font_add(family = "Gotham Narrow", regular = paste0(system.file("font", package = "fonctionr"), "/GothamNarrow-Book.otf"))
  showtext_auto()

}


#' Fonction pour exporter en excel les résultats des fonctions de fonctionr
#'
#' @param tab_excel
#' @param graph
#' @param test_stat_excel
#' @param facet_null
#' @param export_path
#' @param percent_fm
#' @param fgFill
#'
#' @import openxlsx
#' @import broom
#'
#' @NoRd
#'
export_excel <- function(tab_excel = tab_excel,
                         graph = graph,
                         test_stat_excel = test_stat_excel,
                         facet_null = NULL,
                         export_path = export_path,
                         percent_fm = NULL,
                         fgFill = fgFill,
                         bivariate = NULL) {

  # Pour être intégré au fichier excel, le graphique doit être affiché => https://ycphs.github.io/openxlsx/reference/insertPlot.html
  print(graph)

  # Je formate un fichier Excel dans lequel j'exporte les résultats

  wb <- createWorkbook() # On crée l'objet dans lequel on va formater toutes les infos en vue d'un export en fichier Excel
  addWorksheet(wb, "Résultats") # On ajoute une feuille pour les résultats
  addWorksheet(wb, "Graphique") # On ajoute une feuille pour le graphique
  addWorksheet(wb, "Test statistique") # On ajoute une feuille pour le résultat du test stat

  writeData(wb, "Résultats", tab_excel, keepNA = TRUE, na.string = "NA") # On écrit les résultats en gardant les NA
  insertPlot(wb,"Graphique", dpi = 80, width = 12, height = 8)
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
