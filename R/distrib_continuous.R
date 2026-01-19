#' distrib_continuous
#'
#' Function to describe a continuous variable from complex survey data
#'
#' @name distrib_continuous
#'
#' @param data A dataframe or an object from the survey package or an object from the srvyr package.
#' @param quanti_exp An expression that define the variable to be described.
#' @param type "mean" to compute mean as the central value ; "median" to compute median as the central value.
#' @param facet A supprimer?
#' @param filter_exp An expression that filters the data, preserving the design.
#' @param ... All options possible in as_survey_design in srvyr package.
#' @param na.rm.facet A supprimer?
#' @param quantiles quantiles to be computed in the distribution. Default are deciles.
#' @param bw The smoothing bandwidth to be used. The kernels are scaled such that this is the standard deviation of the smoothing kernel. Default is 1.
#' @param resolution Resolution of the density curve. Default is 1024.
#' @param limits Limits of the x axe of the graphic. Does not apply to the computation. Default is NULL to show the entire distribution on the graphic.
#' @param show_mid_line TRUE if you want to show the mean or median (depending on type) as a line on the graphic. FALSE if you do not want to show it. Default is TRUE.
#' @param show_ci_lines TRUE if you want to show confidence interval of the mean or median (depending on type) as lines on the graphic. FALSE if you do not want to show it as lines. Default is TRUE.
#' @param show_ci_area TRUE if you want to show confidence interval of the mean or median (depending on type) as a coloured area on the graphic. FALSE if you do not want to show it as an area. Default is FALSE.
#' @param show_quant_lines TRUE if you want to show quantiles as lines on the graphic. FALSE if you do not want to show them as lines. Default is FALSE.
#' @param show_n TRUE if you want to show on the graphic the number of individuals in the sample in each quantile FALSE if you do not want to show the numbers. Default is FALSE.
#' @param show_value TRUE if you want to show the mean/median (depending on type) on the graphic. FALSE if you do not want to show the mean/median. Default is TRUE.
#' @param show_labs TRUE if you want to show axes, titles and caption labels. FALSE if you do not want to show any label on axes and titles. Default is TRUE.
#' @param digits Numbers of digits showed on the value labels on the graphic. Default is 0.
#' @param unit Unit showed on the graphic. Default is no unit.
#' @param dec Decimal mark shown on the graphic. Depends on lang: "," for fr and nl ; "." for en.
#' @param pal color of the density area. maybe one color or a vector with several colors.
#' @param color color of the density line.
#' @param font Font used in the graphic. See load_and_active_fonts() for available fonts.
#' @param title Title of the graphic.
#' @param subtitle Subtitle of the graphic.
#' @param xlab X label on the graphic. As coord_flip() is used in the graphic, xlab refers to the X label on the graphic, after the coord_flip(), and not to the x variable in the data. If xlab = NULL, X label on the graphic will be "Moyenne : " + quanti_exp or "Medianne : " + quanti_exp. To show no X label, use xlab = "".
#' @param ylab Y label on the graphic. As coord_flip() is used in the graphic, ylab refers to the Y label on the graphic, after the coord_flip(), and not to the y variable in the data. If ylab = NULL, Y label on the graphic will be group. To show no Y label, use ylab = "".
#' @param caption Caption of the graphic.
#' @param lang The language of the indications on the chart. Possibilities: "fr", "nl", "en". Default is "fr".
#' @param theme Theme of the graphic. IWEPS adds y axis lines and ticks.
#' @param coef_font A multiplier factor for font size. Default is 1. Usefull when exporting the plot for a publication (for instance with a Quarto document).
#' @param export_path Path to export the results in an xlsx file. The file includes three sheets : the table, the graphic and the statistical test.
#'
#' @return A list that contains a table (tab), a graphic (garph) and a density table (dens) and a quantile table (quant)
#' @import rlang
#' @import survey
#' @import srvyr
#' @import dplyr
#' @import ggplot2
#' @export
#'
#' @examples
#' # Loading of data
#' data(eusilc, package = "laeken")
#'
#' # Computation, taking sample design into account
#' eusilc_dist_c <- distrib_c(
#'   eusilc,
#'   quanti_exp = eqIncome,
#'   strata = db040,
#'   ids = db030,
#'   weight = rb050,
#'   limits = c(0, 50000),
#'   title = "Distribution of eq. income",
#'   subtitle = "Example with austrian SILC data from 'laeken' package"
#' )
#'
#' # Results in graph form
#' eusilc_dist_c$graph
#'
#' # Results in table format
#' eusilc_dist_c$tab
#'
distrib_continuous <- function(data,
                          quanti_exp,
                          type = "median",
                          facet = NULL,
                          filter_exp = NULL,
                          ...,
                          na.rm.facet = TRUE,
                          quantiles = seq(.1, .9, .1),
                          bw = 1,
                          resolution = 1024,
                          limits = NULL,
                          # show_mid_point = TRUE,
                          show_mid_line = TRUE,
                          show_ci_lines = TRUE,
                          show_ci_area = FALSE,
                          show_quant_lines = FALSE,
                          show_n = FALSE,
                          show_value = TRUE,
                          show_labs = TRUE,
                          digits = 0,
                          unit = "",
                          dec = NULL,
                          pal = c("#00708C", "mediumturquoise"),
                          color = NA,
                          font ="Roboto",
                          title = NULL,
                          subtitle = NULL,
                          xlab = NULL,
                          ylab = NULL,
                          caption = NULL,
                          lang = "fr",
                          theme = NULL,
                          coef_font = 1,
                          export_path = NULL) {

  # 1. CHECKS DES ARGUMENTS --------------------

  # Un check imperatif
  if((missing(data) | missing(quanti_exp)) == TRUE){
    stop("Les arguments data et quanti_exp doivent etre remplis")
  }

  # Check des autres arguments
  check_arg(
    arg = list(
      type = type,
      unit = unit,
      dec = dec,
      color = color,
      font = font,
      title = title,
      subtitle = subtitle,
      xlab = xlab,
      ylab = ylab,
      caption = caption,
      lang = lang,
      theme = theme
    ),
    type = "character"
  )
  # Je supprime pour pouvoir generer automatiquement des palettes dans l'argument avec des fonctions
  # check_arg(
  #   arg = list(
  #     pal = pal
  #   ),
  #   type = "character",
  #   short = F
  # )
  check_arg(
    arg = list(
      show_mid_line = show_mid_line,
      show_ci_lines = show_ci_lines,
      show_ci_area = show_ci_area,
      show_quant_lines = show_quant_lines,
      show_n = show_n,
      show_value = show_value,
      show_labs = show_labs
    ),
    type = "logical"
  )
  check_arg(
    arg = list(
      bw = bw,
      resolution = resolution,
      digits = digits,
      coef_font = coef_font
    ),
    type = "numeric"
  )
  check_arg(
    arg = list(
      quantiles = quantiles,
      limits = limits
    ),
    type = "numeric",
    short = F
  )

  # Check que les arguments avec choix precis sont les bons
  match.arg(type, choices = c("mean", "median"))
  lang <- tolower(lang)
  match.arg(lang, choices = c("fr", "nl", "en"))

  # Check que limit ne contient que 2 valeurs
  if(!is.null(limits) & length(limits) != 2){
    stop("limits doit etre un vecteur contenant 2 valeurs (min et max)")
  }

  # On cree une quosure de facet & filter_exp => pour if statements dans la fonction (voir ci-dessous)
  # Solution trouvee ici : https://rpubs.com/tjmahr/quo_is_missing
  quo_facet <- enquo(facet)
  quo_filter <- enquo(filter_exp)

  # On procede d'abord a un test : il faut que toutes les variables entrees soient presentes dans data => sinon stop et erreur
  # On cree un vecteur string qui contient toutes les variables entrees
  # Solution trouvee ici : https://stackoverflow.com/questions/63727729/r-how-to-extract-object-names-from-expression

  # On detecte d'abord les variables entrees dans l'expression pour calculer la moyenne/mediane
  vec_quanti_exp <- all.vars(substitute(quanti_exp))
  names(vec_quanti_exp) <- rep("quanti_exp", length(vec_quanti_exp)) # On cree un vecteur nomme pour la fonction check_input ci-dessous
  vars_input_char <- vec_quanti_exp
  # On ajoute facet si non-NULL
  if(!quo_is_null(quo_facet)){
    vec_facet <- c(facet = as.character(substitute(facet)))
    vars_input_char <- c(vars_input_char, vec_facet)
  }
  # On ajoute filter si non-NULL
  if(!quo_is_null(quo_filter)){
    vec_filter_exp <- all.vars(substitute(filter_exp))
    names(vec_filter_exp) <- rep("filter_exp", length(vec_filter_exp))
    vars_input_char <- c(vars_input_char, vec_filter_exp)
  }
  # Ici le check a proprement parler
  check_input(data,
              vars_input_char)

  # Dictionnaire
  if(lang == "fr"){
    if(is.null(dec)){
      dec <- ","
    }
    lang_test_uni <- NULL
    lang_dens <- paste0("Densit","\u00e9")
  }
  if(lang == "nl"){
    if(is.null(dec)){
      dec <- ","
    }
    lang_test_uni <- NULL
    lang_dens <- "Densiteit"
  }
  if(lang == "en"){
    if(is.null(dec)){
      dec <- "."
    }
    lang_test_uni <- NULL
    lang_dens <- "Density"
  }


  # 2. PROCESSING DES DONNEES --------------------

  # On convertit d'abord data en objet srvyr
  data_W <- convert_to_srvyr(data, ...)

  # # On ne garde que les colonnes entrees en input
  # data_W <- data_W |>
  #   select(all_of(unname(vars_input_char)))

  # On filtre si filter est non NULL
  if(!quo_is_null(quo_filter)){
    data_W <- data_W |>
      filter({{ filter_exp }})
  }
  # On supprimes les NA sur la variable de facet si non-NULL
  if (na.rm.facet == T) {
    if(!quo_is_null(quo_facet)){
      data_W <- data_W |>
        filter(!is.na({{ facet }}))
    }
  }

  # On supprime les NA sur la/les variable(s) quanti dans tous les cas, sinon ambigu => de cette facon les n par groupe sont toujours les effectifs pour lesquels la/les variable(s) quanti sont non missing (et pas tout le groupe : ca on s'en fout)
  # On les affiche via message (pour verification)
  message("Variable(s) detectee(s) dans quanti_exp : ", paste(vec_quanti_exp, collapse = ", "))
  # On calcule les effectifs avant filtre
  before <- data_W |>
    summarise(n=unweighted(n()))
  # On filtre via boucle => solution trouvee ici : https://dplyr.tidyverse.org/articles/programming.html#loop-over-multiple-variables
  for (var in vec_quanti_exp) {
    data_W <- data_W |>
      filter(!is.na(.data[[var]]))
  }
  # On calcule les effectifs apres filtre
  after <- data_W |>
    summarise(n=unweighted(n()))
  # On affiche le nombre de lignes supprimees (pour verification)
  message(paste0(before[[1]] - after[[1]]), " lignes supprimees avec valeur(s) manquante(s) pour le(s) variable(s) de quanti_exp")

  # On recalcule quanti_exp dans une variable unique si c'est une expression a la base => necessaire pour calculer la densite
  data_W <- data_W |>
    mutate(
      quanti_exp_flattened = {{ quanti_exp }}
    )
  # On convertit la variable de facet en facteur si facet non-NULL
  if(!quo_is_null(quo_facet)){
    data_W <- data_W |>
      mutate(
        "{{ facet }}" := droplevels(as.factor({{ facet }}))) # droplevels pour eviter qu'un level soit encode alors qu'il n'a pas d'effectifs (pb pour le test khi2)
  }


  # 3. CALCUL DE L'INDICE CENTRAL (MEDIANE/MOYENNE) --------------------

  if (!quo_is_null(quo_facet)) {
    data_W <- data_W |>
      group_by({{ facet }})
  }

  # Calcul de la moyenne ou mediane et ses IC
  tab <- data_W |>
    summarise(
      indice = if (type == "median") {
        survey_median({{ quanti_exp }}, na.rm = T, vartype = "ci")
      } else if (type == "mean") survey_mean({{ quanti_exp }}, na.rm = T, vartype = "ci"),
      n_sample = unweighted(n()), # On peut faire n(), car les NA ont ete supprimes partout dans l'expression (precedemment dans la boucle) => plus de NA
      n_weighted = survey_total(vartype = "ci")
    ) |>
    ungroup()


  # 4. CALCUL DE LA DENSITE ET DES QUANTILES --------------------

  # On identifie la variable de ponderation inclue dans dotdotdot (...) pour la passer aussi a la densite
  var_weights <- substitute(...())$weights

  # On estime la densite de la variable quanti
  estDensity <- stats::density(data_W$variables[["quanti_exp_flattened"]],
    n = resolution,
    adjust = bw,
    subdensity = T,
    weights = if (is.null(var_weights)) {
      NULL
    # On introduit la variable de ponderation identifiee dans var_weights mais transformee pour que la somme = 1
    } else if (!is.null(var_weights)) data_W$variables[[as.character(var_weights)]] / sum(data_W$variables[[as.character(var_weights)]]),
    na.rm = T
  )

  # print(sum(data_W$variables[[as.character(var_weights)]]/sum(data_W$variables[[as.character(var_weights)]])))

  # On calcule les quantiles avec survey et on les stocke dans un data.frame
  estQuant_W <- as.data.frame(svyquantile(~quanti_exp_flattened,
    design = data_W,
    quantiles = unique(quantiles),
    ci = T,
    na.rm = T
  )[[1]]) |>
    tibble::rownames_to_column(var = "probs")

  # On cree un data.frame avec les densites, et on cree les classes de quantiles (a quel quantile x appartient) en croisant x avec le vecteur de quantiles
  df_dens <- data.frame(
    x = estDensity$x,
    y = estDensity$y,
    quantFct = findInterval(estDensity$x, estQuant_W$quantile) + 1,
    segment = NA # Pour pouvoir identifier les quantiles que j'ajoute ci-dessous
  )

  # print(sum(df_dens$y))

  # Il faut qu'il y ait dans l'estimation de la densite les valeurs EXACTES des quantiles, pour que le ggplot puisse couper exactement aux quantiles !
  # => Je les ajoute avec add_case, en estimant y avec approx()
  # Je le fais dans une boucle, pour avoir tous les quantiles.
  # SOLUTION INSPIReE DE CE CODE : https://stackoverflow.com/questions/74560448/how-fill-geom-ribbon-with-different-colour-in-r
  for(i in seq_along(unique(quantiles))){
    df_dens <- df_dens |>
      tibble::add_case(
        x = estQuant_W$quantile[i],
        y = stats::approx(df_dens$x, df_dens$y, xout = estQuant_W$quantile[i], ties = "mean")$y,
        quantFct = i,
        segment = TRUE
      ) |>
      tibble::add_case(
        x = estQuant_W$quantile[i],
        y = stats::approx(df_dens$x, df_dens$y, xout = estQuant_W$quantile[i], ties = "mean")$y,
        quantFct = i+1
      )
  }

  # On isole les quantiles avec leurs coordonnees y de densite (pour les afficher avec le ggplot)
  quant_seg <- df_dens |>
    filter(segment == TRUE)

  # Je transforme l'appartenance au quantile en facteur, pour le ggplot
  df_dens <- df_dens |>
    mutate(quantFct = as.factor(quantFct))

  if (show_n == TRUE) {
    # Les effectifs par quantile
    quantile_n <- data_W |>
      mutate(quantFct = cut({{ quanti_exp }},
        include.lowest = T,
        right = F,
        breaks = c(-Inf, estQuant_W$quantile, Inf)
      )) |>
      group_by(quantFct) |>
      summarise(n = unweighted(n()))

    # On joint les coordonnees x du debut des classes de quantile aux effectifs par quantile
    vec_bord_inf_quant <- c(min(data_W$variables[["quanti_exp_flattened"]], na.rm = T), estQuant_W$quantile)
    vec_bord_sup_quant <- c(estQuant_W$quantile, max(data_W$variables[["quanti_exp_flattened"]], na.rm = T))
    quantile_n$coord_x <- vec_bord_inf_quant
    quantile_n$coord_max <- vec_bord_sup_quant

    # Si l'utilisateur indique des limites, alors on supprime les effectifs de toutes les classes de quantile pas affichees en entier
    if(!is.null(limits)){
      quantile_n <- quantile_n |>
        filter(coord_x > limits[1]) # Les premieres classes si elles sont coupees
      if(max(data_W$variables[["quanti_exp_flattened"]], na.rm = T) > limits[2])
        quantile_n <- quantile_n |>
          filter(coord_max < limits[2]) # Et les dernieres si elles sont coupees
    }
  }

  # On estime la densite de la moyenne ou mediane et ses CI
  df_dens <- df_dens |>
    mutate(central = NA) |>
    tibble::add_case(
      x = tab$indice,
      y = stats::approx(df_dens$x, df_dens$y, xout = tab$indice, ties = "mean")$y,
      central = "indice"
    ) |>
    tibble::add_case(
      x = tab$indice_low,
      y = stats::approx(df_dens$x, df_dens$y, xout = tab$indice_low, ties = "mean")$y,
      central = "indice_low"
    ) |>
    tibble::add_case(
      x = tab$indice_upp,
      y = stats::approx(df_dens$x, df_dens$y, xout = tab$indice_upp, ties = "mean")$y,
      central = "indice_upp"
    )

  # On identifie toutes les valeurs de densite comprises dans les IC => permet de creer une region sur le ggplot
  central <- df_dens |>
    filter(x >= tab$indice_low & x <= tab$indice_upp)

  # # On enleve les lignes creees pour la moyenne ou mediane et ses CI du df de densite => pas necessaire, et duplique potentiels avec certains quantiles
  # df_dens <- df_dens |>
  #   filter(is.na(central))


  # 5. CREATION DU GRAPHIQUE --------------------

  if(all(isColor(pal)) == TRUE){
    # Si condition remplie on ne fait rien => on garde la palette
  } else {
    # Sinon on met la couleur par defaut
    message("Une couleur indiquee dans pal n'existe pas : la palette de couleurs par defaut est utilisee")
    pal <- c("#00708C", "mediumturquoise")
  }

  # La palette divergente => varie selon que le nombre de quantiles soit pair ou impair
  if(length(estQuant_W$quantile) %% 2 == 0){
    palette <- grDevices::colorRampPalette(pal)((length(estQuant_W$quantile)/2)+1)
    palette <- c(palette, rev(palette)[-1])
  }
  if(length(estQuant_W$quantile) %% 2 == 1){
    palette <- grDevices::colorRampPalette(pal)(((length(estQuant_W$quantile)+1)/2)+1)
    palette <- c(palette, rev(palette)[-c(1,2)])
    # palette <- c(rev(rev(palette)[-2]), rev(palette)[-1])
  }

  # Les limites de la variable quanti si non indiquee par l'utilisateur => pour ggplot
  if(is.null(limits)){
    lim_min <- min(data_W$variables[["quanti_exp_flattened"]], na.rm = T)
    lim_max <- max(data_W$variables[["quanti_exp_flattened"]], na.rm = T)
    limits <- c(lim_min, lim_max)
  }

  # On calcule la valeur max de la densite, pour l'ecart des geom_text dans le ggplot
  max_ggplot <- max(df_dens$y)

  # Le graphique ggplot

  graph <- ggplot(
    data = df_dens[is.na(df_dens$central), ] # Sans les valeurs centrales, sinon ggplot les plot avec couleur NA
  ) +
    geom_ribbon(
      aes(
        x = x, ymin = 0, ymax = y, fill = quantFct
      ),
      alpha = 1
    ) +
    geom_line(
      aes(
        x = x,
        y = y
      ),
      color = color,
      linewidth = .7
    ) +
    scale_x_continuous(
      breaks = c(limits[1], estQuant_W$quantile, limits[2]),
      labels = scales::label_dollar(
        accuracy = 1/10^digits, # On transforme le nombre de digits en un format compatible avec accuracy
        prefix = "",
        suffix = unit,
        big.mark = "",
        decimal.mark = dec),
      limits = limits,
      expand = expansion(mult = c(.01, .05))
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0.005, 0.05))
    ) +
    scale_fill_manual(
      values = palette
    ) +
    theme_fonctionr(
      font = font,
      theme = theme,
      coef_font = coef_font
    ) +
    theme(
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "#dddddd"),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      legend.position = 'none'
    ) +
    labs(title = title,
         subtitle = subtitle
    )

  # Pour caption

  if (!is.null(caption)) { # Permet de passer a la ligne par rapport au test stat
    caption <- paste0("\n", stringr::str_wrap(caption, width = 100))
  }

  graph <- graph +
    labs(
      caption = paste0(
        lang_test_uni,
        caption
      )
    )

  # Ajouter les segments des quantiles
  if (show_quant_lines == T) {
    graph <- graph +
      geom_segment(
        data = quant_seg,
        aes(x = x,
            y = 0,
            yend = y),
        # linetype = "dotted"
        alpha = .15
      )
  }

  # Ajouter l'aire des CI
  if (show_ci_area == T) {
    graph <- graph +
      geom_ribbon(
        data = central,
        aes(
          x = x, ymin = 0, ymax = y
        ),
        fill = "darkorange",
        alpha = .3
      )
  }

  # Ajouter le nombre d'individus au besoin
  if (show_n == TRUE) {
    graph <- graph +
      geom_text(
        data = quantile_n,
        aes(
          y = 0 + (0.01 * max_ggplot),
          x = coord_x + (0.001 * limits[2]), # Pour ajouter des labels avec les effectifs
          label = paste0("n=", n),
          family = font),
        size = coef_font * fonctionr_font_size(type = "little"),
        alpha = 0.5,
        hjust = 0, # Justifie a droite
        vjust = 1,
        angle = 90 # pour incliner
      )
  }

  # Ajouter les limites des IC
  if (show_ci_lines == T) {
    # graph <- graph +
    #   geom_errorbarh(
    #     data = tab,
    #     aes(
    #       xmin = indice_low,
    #       xmax = indice_upp,
    #       y = 0
    #     ),
    #     alpha = 0.5,
    #     height = max(df_dens$y) / 25
    #   )

    graph <- graph +
      geom_segment(
        data = central[(central$central == "indice_low" | central$central == "indice_upp") & !is.na(central$central), ],
        aes(x = x,
            y = 0,
            yend = y),
        linetype = "dashed",
        linewidth = 1,
        alpha = .4
      )
  }

  # Ajouter la ligne de la tendance centrale
  if (show_mid_line == T) {
    graph <- graph +
      geom_segment(
        data = central[central$central == "indice" & !is.na(central$central), ],
        aes(
          x = x,
          y = 0,
          yend = y
        ),
        linewidth = 1,
        alpha = .6
      )
  }

  # # Ajouter le point de la tendance centrale
  # if (show_mid_point == T) {
  #   graph <- graph +
  #     geom_point(
  #       data = tab,
  #       aes(x = indice,
  #           y = 0)
  #     )
  # }

  if (show_value == T) {
    graph <- graph  +
      geom_text(
        data = central[central$central == "indice" & !is.na(central$central), ],
        aes(
          x = x,
          y = y,
          label = paste0(stringr::str_replace(round(x,
                                                    digits = digits),
                                              "[.]",
                                              dec),
                         unit),
          family = font),
        size = coef_font * fonctionr_font_size(type = "little"),
        vjust = -1,
        color = "black",
        alpha = 0.9)
  }

  # Ajouter les axes
  if(show_labs == TRUE){
    # X ---
    if(any(is.null(xlab), xlab != "")){
      graph <- graph +
        labs(x = ifelse(is.null(xlab),
                        paste0(deparse(substitute(quanti_exp))),
                        xlab))
    }
    if(all(!is.null(xlab), xlab == "")){
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
          labs(y = lang_dens)
      }
    }
    if(all(!is.null(ylab), ylab == "")){
      graph <- graph +
        labs(y = NULL)
    }
  }

  # Masquer les axes si show_labs == FALSE
  if(show_labs == FALSE){
    graph <- graph +
      labs(x = NULL,
           y = NULL)
  }


  # 6. RESULTATS --------------------

  # Dans un but de lisibilite, on renomme les indices "mean" ou "median" selon la fonction appelee
  if (type == "mean") {
    tab <- tab |>
      rename(mean = indice,
             mean_low = indice_low,
             mean_upp = indice_upp)
  }

  if (type == "median") {
    tab <- tab |>
      rename(median = indice,
             median_low = indice_low,
             median_upp = indice_upp)
  }

  # On cree l'objet final
  res <- list()
  res$dens <- df_dens[,c("x", "y", "quantFct", "central")]
  res$tab <- tab
  res$quant <- estQuant_W[, c("probs", "quantile", "ci.2.5", "ci.97.5")]
  res$graph <- graph

  if (!is.null(export_path)) {
    # L'export en excel

    # Pour many_prop, test pas encore implemente => on cree un data.frame a la main
    test_stat_excel <- data.frame(Parameter = c("test.error"),
                                  Value = "Test pas encore implemente dans distrib_continuous",
                                  row.names = NULL)

    # J'exporte les resultats en Excel
    export_excel(tab_excel = tab,
                 graph = graph,
                 test_stat_excel = test_stat_excel,
                 quantiles = res$quant,
                 density = res$dens,
                 facet_null = TRUE,
                 export_path = export_path,
                 percent_fm = FALSE,
                 fgFill = "#00708C",
                 bivariate = FALSE,
                 dens = "uni")
  }

  return(res)

}


#' @rdname distrib_continuous
#' @export
distrib_c <- function(...) {
  distrib_continuous(...)
}
