#' relab_cut
#'
#' Function to recode the default labels of a factor created by cut() from base R into more intuitive labels
#'
#' @param vec The vector to be recoded. It should be produced by cut(). Notice that the labels may not include scientific notation. Thus, in cut(), dig.lab argument should be high enough in order to produce labels without scientific notation.
#' @param suffix The suffix to be indicated after the values. Usualy, the unit of the variable will be used (e.g. euros, percents). Default is NULL, for no suffix.
#' @param right TRUE if categories have been created with parameter right = TRUE in cut(). FALSE if categories have been created with parameter right = FALSE in cut(). Default is TRUE.
#' @param lang Language of new labels. Possibilities are "fr" (french), "nl" (dutch) and "en" (english). Default is "fr".
#'
#' @returns A vector with new labels
#' @export
#'
#' @examples
#' cut(1:1000, breaks = 5, include.lowest = TRUE, right = FALSE) |>
#' table()
#'
#' cut(1:1000, breaks = 5, include.lowest = TRUE, right = FALSE, dig.lab = 4) |>
#' relab_cut(suffix = "€", right = FALSE) |>
#' table()
#'
relab_cut <- function(vec, # Vecteur factor a recoder
                      suffix = NULL, # Suffixe
                      right = TRUE, # Argument qui a ete utilise pour cut
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

  # Check le nombre de modalites du vecteur dont le format est valide
  vec_check <- stringr::str_detect(levels(vec), "(\\[|\\()(-|)(([:digit:]+[.]|)[:digit:]+|Inf),(-|)(([:digit:]+[.]|)[:digit:]+| Inf)(\\]|\\))")
  sum_values_vec <- sum(vec_check)
  # On ajoute 1 s'il y a au moins 1 NA
  if(anyNA(vec)){
    sum_values_vec <- sum_values_vec + 1
  }
  # Toutes les valeurs detectees doivent etre OK (somme des OK == toutes les valeurs)
  if(sum_values_vec != length(unique(vec))){
    stop("Le formatage de la variable a recoder n'est pas compatible avec la fonction relab_cut()")
  }

  # On enleve crochets et parentheses
  levels(vec) <- stringr::str_replace_all(levels(vec), "\\[|\\]|\\(|\\)", "")
  # On transforme la virgule entre les valeurs en tiret
  levels(vec) <- stringr::str_replace_all(levels(vec), ",", "-")

  # Pour la 1ere cat, on ecrit "Moins de" + "DEUXIEME CHIFFRE" (identifie au tiret devant, qu'on enleve apres)
  levels(vec)[1] <- paste(less, stringr::str_extract(utils::head(levels(vec), 1), "-([:digit:]+[.]|)[:digit:]+$")) |>
    stringr::str_replace("-", "")
  # Pour la derniere cat, on ecrit "Plus de" + PREMIER CHIFFRE
  levels(vec)[length(levels(vec))] <- paste(more, stringr::str_extract(utils::tail(levels(vec), 1), "^([:digit:]+[.]|)[:digit:]+"))

  # On detecte le nombre de decimales
  # On extrait la partie decimales de tous les nombres, et on les place a la suite dans un vecteur vec_digits
  vec_digits <- stringr::str_extract_all(levels(vec), "[.][:digit:]+") |>
  unlist()

  # Si pas de decimales detectees
  if (identical(vec_digits, character(0))) {
    dec <- 0
  }
  # Si decimales detectees
  else {
    # On detecte le max de decimales dans vec_digits => cela definit les unites a ajouter /supprimer
    dec <- max(nchar(vec_digits)) - 1
  }

  if (right == FALSE) {
    # On enleve 1 unite a la derniere cat
    levels(vec)[length(levels(vec))] <- stringr::str_replace(levels(vec)[length(levels(vec))], "([:digit:]+[.]|)[:digit:]+", \(x) as.character(as.numeric(x) - (1 / (10^dec))))
    # Pour ttes les cat sauf la 1ere (pas besoin) et la derniere (deja fait) : +1 unite car on choppe le - dans le string_replace, et converti en numerique ca fait un chiffre negatif !
    # NOTE : ca peut tomber sur les premieres et dernieres cat si 2 categories a cut() : mais ca ne modifie rien car -[:digit:]+ (avec le signe negatif) ne selectionne rien (a cause des "Plus de" / "Moins de")
    # NOTE2 : malgre tout, je mets un if statement, car l'operation est inutile si nlevels(vec) > 2
    if(nlevels(vec) > 2){
      levels(vec)[2:(length(levels(vec)) - 1)] <- stringr::str_replace(levels(vec)[2:(length(levels(vec)) - 1)], "-([:digit:]+[.]|)[:digit:]+$", \(x) as.character(as.numeric(x) + (1 / (10^dec))))
    }
  }
  if (right == TRUE) {
    # On ajoute 1 unite a tous les premiers chiffres sauf la derniere cat (pas besoin)
    levels(vec)[1:(length(levels(vec)) - 1)] <- stringr::str_replace(levels(vec)[1:(length(levels(vec)) - 1)], "([:digit:]+[.]|)[:digit:]+", \(x) as.character(as.numeric(x) + (1 / (10^dec))))
  }

  levels(vec) <- stringr::str_replace_all(levels(vec), "-", " - ") # Plus joli ? On le fait pas au debut car ca permet l'astuce du chiffre negatif
  levels(vec) <- paste0(levels(vec), suffix) # On ajoute un suffixe (symbole pour l'unite)

  return(vec)
}
