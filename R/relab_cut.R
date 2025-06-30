#' relab_cut
#'
#' Function to recode the default labels of a factor created by cut() from base R
#'
#' @param vec The vector to be recoded
#' @param suffix The suffix to be indicated after the values
#' @param right TRUE if categories have been created with parameter right = TRUE in cut().
#' @param lang The language of new labels
#'
#' @returns A vector
#' @export
#'
#' @examples
#' cut(1:1000, breaks = 5, include.lowest = TRUE, right = FALSE) |>
#' table()
#'
#' cut(1:1000, breaks = 5, include.lowest = TRUE, right = FALSE) |>
#' relab_cut(suffix = "€", right = FALSE) |>
#' table()
#'
relab_cut <- function(vec, # Vecteur factor à recoder
                      suffix = NULL, # Suffixe
                      right = TRUE, # Argument qui a été utilisé pour cut
                      lang = "fr") {
  if (lang == "fr") {
    more <- "Plus de"
    less <- "Moins de"
  }
  if (lang == "nl") {
    more <- "Meer dan"
    less <- "Minder dan"
  }
  if (lang == "en") {
    more <- "More than"
    less <- "Less than"
  }
  if (lang == "sign") {
    more <- ">"
    less <- "<"
  }

  # Check
  vec_check <- stringr::str_detect(levels(vec), "(\\[|\\()(-|)(([:digit:]+[.]|)[:digit:]+|Inf),(-|)(([:digit:]+[.]|)[:digit:]+| Inf)(\\]|\\))")
  if(sum(vec_check) != length(unique(vec))){
    stop("Le formatage de la variable à recoder n'est pas compatible avec la fonction relab_cut()")
  }

  # On enlève crochets et parenthèses
  levels(vec) <- stringr::str_replace_all(levels(vec), "\\[|\\]|\\(|\\)", "")
  # On transforme la virgule entre les valeurs en tiret
  levels(vec) <- stringr::str_replace_all(levels(vec), ",", "-")

  # Pour la 1ère cat, on écrit "Moins de" + "DEUXIEME CHIFFRE" (identifié au tiret devant, qu'on enlève après)
  levels(vec)[1] <- paste(less, stringr::str_extract(utils::head(levels(vec), 1), "-([:digit:]+[.]|)[:digit:]+$")) |>
    stringr::str_replace("-", "")
  # Pour la dernière cat, on écrit "Plus de" + PREMIER CHIFFRE
  levels(vec)[length(levels(vec))] <- paste(more, stringr::str_extract(utils::tail(levels(vec), 1), "^([:digit:]+[.]|)[:digit:]+"))

  # On détecte le nombre de décimales
  # On extrait la partie décimales de tous les nombres, et on les place à la suite dans un vecteur vec_digits
  vec_digits <- stringr::str_extract_all(levels(vec), "[.][:digit:]+") |>
  unlist()

  # Si pas de décimales détectées
  if (identical(vec_digits, character(0))) {
    dec <- 0
  }
  # Si décimales détectées
  else {
    # On détecte le max de décimales dans vec_digits => cela définit les unités à ajouter /supprimer
    dec <- max(nchar(vec_digits)) - 1
  }

  if (right == FALSE) {
    # On enlève 1 unité à la dernière cat
    levels(vec)[length(levels(vec))] <- stringr::str_replace(levels(vec)[length(levels(vec))], "([:digit:]+[.]|)[:digit:]+", \(x) as.character(as.numeric(x) - (1 / (10^dec))))
    # Pour ttes les cat sauf la 1ère (pas besoin) et la dernière (déjà fait) : +1 unité car on choppe le - dans le string_replace, et converti en numerique ça fait un chiffre négatif !
    # NOTE : ça peut tomber sur les premières et dernières cat si 2 catégories à cut() : mais ça ne modifie rien car -[:digit:]+ (avec le signe négatif) ne sélectionne rien (à cause des "Plus de" / "Moins de")
    # NOTE2 : malgré tout, je mets un if statement, car l'opération est inutile si nlevels(vec) > 2
    if(nlevels(vec) > 2){
      levels(vec)[2:(length(levels(vec)) - 1)] <- stringr::str_replace(levels(vec)[2:(length(levels(vec)) - 1)], "-([:digit:]+[.]|)[:digit:]+$", \(x) as.character(as.numeric(x) + (1 / (10^dec))))
    }
  }
  if (right == TRUE) {
    # On ajoute 1 unité à tous les premiers chiffres sauf la dernière cat (pas besoin)
    levels(vec)[1:(length(levels(vec)) - 1)] <- stringr::str_replace(levels(vec)[1:(length(levels(vec)) - 1)], "([:digit:]+[.]|)[:digit:]+", \(x) as.character(as.numeric(x) + (1 / (10^dec))))
  }

  levels(vec) <- stringr::str_replace_all(levels(vec), "-", " - ") # Plus joli ? On le fait pas au début car ça permet l'astuce du chiffre négatif
  levels(vec) <- paste0(levels(vec), suffix) # On ajoute un suffixe (symbole pour l'unité)

  return(vec)
}
