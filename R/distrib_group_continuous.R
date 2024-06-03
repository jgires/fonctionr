#' distrib_group_continuous
#'
#' Function to compare means or medians in different groups from complex survey data. It produces a table, a graphic and a statistical test.
#'
#' @name distrib_group_continuous
#'
#' @param data A dataframe or an object from the survey package or an object from the srvyr package.
#' @param group A variable defining groups to be compared.
#' @param quanti_exp An expression that define the variable the variable to be described.
#' @param type Type of central value : "mean" to compute mean as the central value by group ; "median" to compute median as the central value by group..
#' @param facet A supprimer?
#' @param filter_exp An expression that filters the data, preserving the design.
#' @param ... All options possible in as_survey_design in srvyr package.
#' @param na.rm.group TRUE if you want to remove observations with NA on the group variable. FALSE if you want to create a group with the NA value for the group variable. NA in the variables included in quanti_exp are not affected in this argument. All the observation with a NA in the variables included in quanti_exp are excluded.
#' @param na.rm.facet Argument a supprimer?
#' @param quantiles Quantiles to be computed in the distributions. Default are deciles.
#' @param moustache_probs A vector defining the proportions of the population used to draw the moustache. Default is 0.95, 0.8, 0.5 to draw a moustache with three groups containing respectively 50 percent, 80 percent and 95 percent of the population closest to the median.
#' @param bw The smoothing bandwidth to be used. The kernels are scaled such that this is the standard deviation of the smoothing kernel. Default is 1.
#' @param resolution Resolution of the density curve. Default is 512
#' @param height Height of the curves. Default is 0.8
#' @param limits Limits of the x axe of the graphic. Does not apply to the computation. Default is NULL to show the entire distribution on the graphic.
#' @param reorder TRUE if you want to reorder the groups according to the mean/median (depending on type). NA value, in case if na.rm.group = FALSE, is not included in the reorder (A VERIFIER).
#' @param show_mid_point TRUE if you want to show the mean or median (depending on type) as a point on the graphic. FALSE if you do not want to show it. Default is TRUE.
#' @param show_mid_line TRUE if you want to show the mean or median (depending on type) as a line on the graphic. FALSE if you do not want to show it. Default is FALSE
#' @param show_ci_errorbar TRUE if you want to show confidence interval of the mean or median (depending on type) as an error bar on the graphic. FALSE if you do not want to show it as lines. Default is TRUE.
#' @param show_ci_lines TRUE if you want to show confidence interval of the mean or median (depending on type) as lines on the graphic. FALSE if you do not want to show it as lines. Default is FALSE
#' @param show_ci_area TRUE if you want to show confidence interval of the mean or median (depending on type) as a coloured area on the graphic. FALSE if you do not want to show it as an area. Default is FALSE.
#' @param show_quant_lines TRUE if you want to show quantiles as lines on the graphic. FALSE if you do not want to show them as lines. Default is FALSE.
#' @param show_moustache TRUE if you want to show the moustache on the graphic. FALSE if you do not want to show it. Default is TRUE.
#' @param show_n TRUE if you want to show on the graphic the number of individuals in the sample in each group. FALSE if you do not want to show this number. Default is FALSE.
#' @param show_value TRUE if you want to show the mean/median of each group on the graphic. FALSE if you do not want to show the mean/median. Default is TRUE.
#' @param show_lab TRUE if you want to show axes, titles and caption labels. FALSE if you do not want to show any label on axes and titles. Default is TRUE.
#' @param digits Numbers of digits showed on the value labels on the graphic. Default is 0.
#' @param unit Unit showed on the graphic. Default is no unit.
#' @param dec Decimal mark shown on the graphic. Default is ",".
#' @param pal Color of the density areas. Can be one or sereval colors to create a palette.
#' @param pal_moustache Color of the moustache. Can be one or sereval colors to create a palette.
#' @param color Color of the density curve. Has to be one color.
#' @param alpha Transparence of the density curve. Default is 1.
#' @param font Font used in the graphic. Available fonts, included in the package itself, are "Roboto", "Montserrat" and "Gotham Narrow". Default is "Roboto".
#' @param wrap_width_y Number of characters before going to the line in the labels of the groups. Default is 25
#' @param title Title of the graphic.
#' @param subtitle Subtitle of the graphic.
#' @param xlab X label on the graphic. As coord_flip() is used in the graphic, xlab refers to the X label on the graphic, after the coord_flip(), and not to the x variable in the data. If xlab = NULL, X label on the graphic will be "Moyenne : " + quanti_exp or "Medianne : " + quanti_exp. To show no X label, use xlab = "".
#' @param ylab Y label on the graphic. As coord_flip() is used in the graphic, ylab refers to the Y label on the graphic, after the coord_flip(), and not to the y variable in the data. If ylab = NULL, Y label on the graphic will be group. To show no Y label, use ylab = "".
#' @param caption Caption of the graphic.
#' @param export_path Path to export the results in an xlsx file. The file includes three sheets : the table, the graphic and the statistical test.
#'
#' @return A list that contains a table, a graphic and a statistical test
#' @import rlang
#' @import survey
#' @import srvyr
#' @import dplyr
#' @import ggplot2
#' @export
#'
#' @examples
#'
distrib_group_continuous <- function(data,
                               group,
                               quanti_exp,
                               type = "median",
                               facet = NULL,
                               filter_exp = NULL,
                               ...,
                               na.rm.group = TRUE,
                               na.rm.facet = TRUE,
                               quantiles = seq(.1, .9, .1),
                               moustache_probs = c(.95, .8, .5),
                               bw = 1,
                               resolution = 512,
                               height = .8,
                               limits = NULL,
                               reorder = FALSE,
                               show_mid_point = TRUE,
                               show_mid_line = FALSE,
                               show_ci_errorbar = TRUE,
                               show_ci_lines = FALSE,
                               show_ci_area = FALSE,
                               show_quant_lines = FALSE,
                               show_moustache = TRUE,
                               show_n = FALSE,
                               show_value = TRUE,
                               show_lab = TRUE,
                               digits = 0,
                               unit = "",
                               dec = ",",
                               pal = "#e0dfe0",
                               pal_moustache = c("#EB9BA0", "#FAD7B1"),
                               color = NA,
                               alpha = 1,
                               font ="Roboto",
                               wrap_width_y = 25,
                               title = NULL,
                               subtitle = NULL,
                               xlab = NULL,
                               ylab = NULL,
                               caption = NULL,
                               export_path = NULL) {

  # 1. CHECKS DES ARGUMENTS --------------------

  # Un check imperatif
  if((missing(data) | missing(group) | missing(quanti_exp)) == TRUE){
    stop("Les arguments data, group et quanti_exp doivent etre remplis")
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
      caption = caption
    ),
    type = "character"
  )
  check_arg(
    arg = list(
      pal = pal,
      pal_moustache = pal_moustache
    ),
    type = "character",
    short = F
  )
  check_arg(
    arg = list(
      na.rm.group = na.rm.group,
      reorder = reorder,
      show_mid_point = show_mid_point,
      show_mid_line = show_mid_line,
      show_ci_errorbar = show_ci_errorbar,
      show_ci_lines = show_ci_lines,
      show_ci_area = show_ci_area,
      show_quant_lines = show_quant_lines,
      show_moustache = show_moustache,
      show_value = show_value,
      show_lab = show_lab
    ),
    type = "logical"
  )
  check_arg(
    arg = list(
      bw = bw,
      resolution = resolution,
      height = height,
      digits = digits,
      alpha = alpha,
      wrap_width_y = wrap_width_y
    ),
    type = "numeric"
  )
  check_arg(
    arg = list(
      quantiles = quantiles,
      moustache_probs = moustache_probs,
      limits = limits
    ),
    type = "numeric",
    short = F
  )

  # Check des probs
  check_prob(
    arg = list(
      quantiles = quantiles,
      moustache_probs = moustache_probs
    )
  )

  # Check que les arguments avec choix precis sont les bons
  match.arg(type, choices = c("mean", "median"))

  # Check que limit ne contient que 2 valeurs
  if(!is.null(limits) & length(limits) != 2){
    stop("limits doit etre un vecteur contenant 2 valeurs (min et max)")
  }

  # On empeche une hauteur de plus de 1 si palette multicolore (ca deconne si chevauchement)
  if(height > 1 & length(pal) > 1){
    message("Une hauteur de plus de 1 n'est pas possible lorsque la palette est multicolore. La hauteur est redefinie a .8")
    height <- .8
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
  # On ajoute groupe
  vec_group <- c(group = as.character(substitute(group)))
  vars_input_char <- c(vars_input_char, vec_group)
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


  # 2. PROCESSING DES DONNEES --------------------

  # On convertit d'abord data en objet srvyr
  data_W <- convert_to_srvyr(data, ...)

  # # On ne garde que les colonnes entrees en input
  # data_W <- data_W %>%
  #   select(all_of(unname(vars_input_char)))

  # On filtre si filter est non NULL
  if(!quo_is_null(quo_filter)){
    data_W <- data_W %>%
      filter({{ filter_exp }})
  }
  # On supprime les NA sur le groupe si na.rm.group = T
  if (na.rm.group == T) {
    data_W <- data_W %>%
      filter(!is.na({{ group }}))
  }
  # On supprimes les NA sur la variable de facet si non-NULL
  if (na.rm.facet == T) {
    if(!quo_is_null(quo_facet)){
      data_W <- data_W %>%
        filter(!is.na({{ facet }}))
    }
  }

  # On supprime les NA sur la/les variable(s) quanti dans tous les cas, sinon ambigu => de cette facon les n par groupe sont toujours les effectifs pour lesquels la/les variable(s) quanti sont non missing (et pas tout le groupe : ca on s'en fout)
  # On les affiche via message (pour verification)
  message("Variable(s) detectee(s) dans quanti_exp : ", paste(vec_quanti_exp, collapse = ", "))
  # On calcule les effectifs avant filtre
  before <- data_W %>%
    summarise(n=unweighted(n()))
  # On filtre via boucle => solution trouvee ici : https://dplyr.tidyverse.org/articles/programming.html#loop-over-multiple-variables
  for (var in vec_quanti_exp) {
    data_W <- data_W %>%
      filter(!is.na(.data[[var]]))
  }
  # On calcule les effectifs apres filtre
  after <- data_W %>%
    summarise(n=unweighted(n()))
  # On affiche le nombre de lignes supprimees (pour verification)
  message(paste0(before[[1]] - after[[1]]), " lignes supprimees avec valeur(s) manquante(s) pour le(s) variable(s) de quanti_exp")

  # On convertit la variable de groupe en facteur si pas facteur
  # + on recalcule quanti_exp dans une variable unique si c'est une expression a la base => necessaire pour calculer la densite
  data_W <- data_W %>%
    mutate(
      "{{ group }}" := droplevels(as.factor({{ group }})), # droplevels pour eviter qu'un level soit encode alors qu'il n'a pas d'effectifs (pb pour le test stat potentiel)
      quanti_exp_flattened = {{ quanti_exp }}
    )
  # On convertit la variable de facet en facteur si facet non-NULL
  if(!quo_is_null(quo_facet)){
    data_W <- data_W %>%
      mutate(
        "{{ facet }}" := droplevels(as.factor({{ facet }}))) # droplevels pour eviter qu'un level soit encode alors qu'il n'a pas d'effectifs (pb pour le test khi2)
  }


  # 3. TEST STATISTIQUE --------------------

  # Ici je remplace les NA pour les groupes / facet par une valeur "NA"
  # L'idee est de recoder les NA des 2 variables group et facet en level "NA", pour que le test stat s'applique aussi aux NA
  if(na.rm.group == F){
    data_W <- data_W %>%
      # Idee : fct_na_value_to_level() pour ajouter un level NA encapsule dans un droplevels() pour le retirer s'il n'existe pas de NA
      mutate("{{ group }}" := droplevels(forcats::fct_na_value_to_level({{ group }}, "NA"))
      )
  }
  if (na.rm.facet == F) {
    # idem sur la variable de facet si non-NULL
    if(!quo_is_null(quo_facet)){
      data_W <- data_W %>% # On enleve sequentiellement les NA de group puis facet
        mutate("{{ facet }}" := droplevels(forcats::fct_na_value_to_level({{ facet }}, "NA"))
        )
    }
  }

  # On realise les tests statistiques
  # Solutions trouvees ici :
  # https://stackoverflow.com/questions/27261232/passing-argument-to-lm-in-r-within-function
  # https://stackoverflow.com/questions/72384740/passing-data-variables-to-r-formulas
  # https://stackoverflow.com/questions/52856711/use-function-arguments-in-lm-formula-within-function-environment
  # Pour regTermTest => explique par Lumley himself : https://stackoverflow.com/questions/72843411/one-way-anova-using-the-survey-package-in-r
  quanti_exp_fmla <- "quanti_exp_flattened" # Un string car on a cree la variable "en dur" dans la fonction
  if(quo_is_null(quo_facet)){
    group_fmla <- as.character(substitute(group))
    fmla <- stats::as.formula(paste(quanti_exp_fmla, "~", group_fmla))
    fmla2 <- stats::as.formula(paste("~", group_fmla))
  }
  # Avec facet : prevoir une boucle pour chacune des modalite de facet => A FAIRE PLUS TARD
  if(!quo_is_null(quo_facet)){
    group_fmla <- as.character(substitute(facet))
    fmla <- stats::as.formula(paste(quanti_exp_fmla, "~", group_fmla))
    fmla2 <- stats::as.formula(paste("~", group_fmla))
  }

  if(type == "mean"){
    model <- svyglm(fmla, design = data_W)
    test.stat <- regTermTest(model, fmla2)
    test.stat[["call"]] <- paste(quanti_exp_fmla, " ~ ", group_fmla)
  }
  if(type == "median"){
    test.stat <- svyranktest(fmla, design = data_W, test = "KruskalWallis")
  }
  # /!\ NOTE : ca fonctionne mais j'ai peur d'utiliser eval => solution precedente choisie, qui a tout de meme le pb de ne pas garder la formule dans le call
  # if(type == "median"){
  #   if(na.rm.group == T){
  #     eval(substitute(test.stat <- svyranktest(quanti_exp ~ group, design = data_W, test = "KruskalWallis")))
  #   }
  #   if(na.rm.group == F){
  #     eval(substitute(test.stat <- svyranktest(quanti_exp ~ group, design = data_W_NA, test = "KruskalWallis")))
  #   }
  # }


  # 4. CALCUL DE L'INDICE CENTRAL (MEDIANE/MOYENNE) --------------------

  # Si non facet
  if (quo_is_null(quo_facet)) {
    data_W <- data_W %>%
      group_by({{ group }})
  }
  # Si facet
  if (!quo_is_null(quo_facet)) {
    data_W <- data_W %>%
      group_by({{ facet }}, {{ group }})
  }

  # On calcule l'indicateur par groupe (mean ou median selon la fonction appelee)
  if (quo_is_null(quo_facet)) {
    tab <- data_W %>%
      summarise(
        indice = if (type == "median") {
          survey_median({{ quanti_exp }}, na.rm = T, vartype = "ci")
        } else if (type == "mean") survey_mean({{ quanti_exp }}, na.rm = T, vartype = "ci"),
        n_sample = unweighted(n()), # On peut faire n(), car les NA ont ete supprimes partout dans l'expression (precedemment dans la boucle) => plus de NA
        n_weighted = survey_total(vartype = "ci")
      ) %>%
      ungroup()
  }

  # On calcul un nouvel ordre dans order si on veut reordonner les groupes selon la valeur calculee
  if (reorder == T) {
    tab_order <- tab %>%
      arrange(desc(indice)) %>%
      mutate(order = row_number()) %>%
      select(group = {{ group }}, order)
  }


  # 5. CALCUL DE LA DENSITE ET DES QUANTILES --------------------

  # On identifie la variable de ponderation inclue dans dotdotdot (...) pour la passer aussi a la densite
  var_weights <- substitute(...())$weights

  # On cree des objets vides pour les remplir dans la boucle ci-dessous
  estQuant_W <- tibble()
  df_dens <- tibble()
  quant_seg <- tibble()
  boxplot_df <- tibble()
  # On cree un vecteur sur lequel on va boucler => ce sont les levels de group (qui est un facteur)
  vec_group.i <- rev(levels(data_W$variables[[deparse(substitute(group))]]))

  for(group.i in seq_along(vec_group.i)){

    # On filtre data pour le group.i
    data_W_group <- data_W %>%
      filter({{ group }} == vec_group.i[group.i])

    # On estime la densite de la variable quanti pour le group.i
    estDensity_group <- stats::density(data_W_group$variables[["quanti_exp_flattened"]],
      n = resolution,
      adjust = bw,
      subdensity = T,
      weights = if (is.null(var_weights)) {
        NULL
      # On introduit la variable de ponderation identifiee dans var_weights mais transformee pour que la somme = 1
      } else if (!is.null(var_weights)) data_W_group$variables[[as.character(var_weights)]] / sum(data_W_group$variables[[as.character(var_weights)]]),
      na.rm = T
    )

    # print(sum(data_W_group$variables[[as.character(var_weights)]]/sum(data_W_group$variables[[as.character(var_weights)]])))

    # On calcule les quantiles du group.i avec survey et on les stocke dans un data.frame
    estQuant_W_group <- as.data.frame(svyquantile(~quanti_exp_flattened,
      design = data_W_group,
      quantiles = unique(quantiles),
      ci = T,
      na.rm = T
    )[[1]]) %>%
      tibble::rownames_to_column(var = "probs") %>% # On cree une colonne qui contient le quantile
      mutate(
        group = vec_group.i[group.i], # Pour savoir quel groupe
        level = group.i # Pour savoir quel level (en valeur numerique)
      )

    # On cree un data.frame avec les densites, et on cree les classes de quantiles (a quel quantile x appartient) en croisant x avec le vecteur de quantiles
    df_dens_group <- data.frame(
      group = vec_group.i[group.i],
      level = group.i,
      x = estDensity_group$x,
      y = estDensity_group$y,
      quantFct = findInterval(estDensity_group$x, estQuant_W_group$quantile) + 1,
      segment = NA # Pour pouvoir identifier les quantiles que j'ajoute ci-dessous
    )

    # sum(df_dens_group$y)

    # Il faut qu'il y ait dans l'estimation de la densite les valeurs EXACTES des quantiles, pour que le ggplot puisse couper exactement aux quantiles !
    # => Je les ajoute avec add_case, en estimant y avec approx()
    # Je le fais dans une boucle, pour avoir tous les quantiles.
    # SOLUTION INSPIReE DE CE CODE : https://stackoverflow.com/questions/74560448/how-fill-geom-ribbon-with-different-colour-in-r
    for (i in seq_along(unique(quantiles))) {
      df_dens_group <- df_dens_group |>
        tibble::add_case(
          group = vec_group.i[group.i],
          level = group.i,
          x = estQuant_W_group$quantile[i],
          y = stats::approx(df_dens_group$x, df_dens_group$y, xout = estQuant_W_group$quantile[i], ties = "mean")$y,
          quantFct = i,
          segment = TRUE
        ) %>%
        tibble::add_case(
          group = vec_group.i[group.i],
          level = group.i,
          x = estQuant_W_group$quantile[i],
          y = stats::approx(df_dens_group$x, df_dens_group$y, xout = estQuant_W_group$quantile[i], ties = "mean")$y,
          quantFct = i + 1
        )
    }

    # On rbind progressivement les df de densite des differents groupes
    df_dens <- rbind(df_dens, df_dens_group)
    # On rbind progressivement les df de quantiles des differents groupe
    estQuant_W <- rbind(estQuant_W, estQuant_W_group)

    # On fait les calculs necessaires pour creer les boites a show_moustaches
    if (show_moustache == T) {

      # On calcule les quantiles a partir des proportions indiquees dans moustache_probs
      moustache_quant <- c(0 + ((1-unique(moustache_probs))/2), 1 - ((1-unique(moustache_probs))/2))
      moustache_quant <- moustache_quant[order(moustache_quant)]

      # On calcule les quantiles du group.i avec survey et on les stocke dans un data.frame
      boxplot_group <- as.data.frame(svyquantile(~quanti_exp_flattened,
        design = data_W_group,
        quantiles = moustache_quant, # On indique les quantiles calcules
        ci = F,
        na.rm = T
      )[[1]]) %>%
        mutate(
          group = vec_group.i[group.i],
          level = group.i
        )
      # On rbind progressivement les df de quantiles des differents groupe
      boxplot_df <- rbind(boxplot_df, boxplot_group)
    }
  }

  # On restructure boxplot_df pour ggplot
  # Le but est de calculer xbegin & xend par proportion + par groupe, pour indiquer a quelles valeurs de x commencent et finissent chaque "moustache" pour geom_rect
  if (show_moustache == T) {
    boxplot_df <- boxplot_df %>%
      pivot_longer(
        cols = !c(group, level),
        names_to = "probs",
        values_to = "quantile",
        names_transform = as.numeric
      ) %>%
      mutate(
        moustache_prob = ifelse( # On fait l'inverse que precedemment : on retrouve les proportions a partir des quantiles
          probs > .5,
          1 - ((1 - probs) * 2),
          1 - (probs * 2)
          ),
        position = ifelse(
          probs > .5,
          "end",
          "begin"
        )
      )
    if (reorder == T) {
      boxplot_df <- boxplot_df %>%
        left_join(tab_order, by = "group") %>%
        mutate(level = order)
    }
    boxplot_df_begin <- boxplot_df %>% filter(position == "begin") %>% select(group, level, moustache_prob, xbegin = quantile) %>% mutate(across(everything(), as.character)) # Pour la jointure ci-dessous les variables doivent etre en caractere
    boxplot_df_end <- boxplot_df   %>% filter(position == "end")   %>% select(group, level, moustache_prob, xend = quantile)   %>% mutate(across(everything(), as.character))
    boxplot_df <- boxplot_df_begin %>%
      left_join(boxplot_df_end, by = c("group", "level", "moustache_prob")) %>%
      mutate(
        moustache_prob = as.character(moustache_prob), # On met moustache_prob en caractere pour le fill sur le ggplot
        across(
          c(xbegin, xend, level),
          as.numeric # On remet les variables numeriques en numerique
        )
      )

  }

  # Si reorder = T, on definit un nouveau level a partir de order (calcule plus haut)
  if (reorder == T) {
    df_dens <- df_dens %>%
      left_join(tab_order, by = "group") %>%
      mutate(level = order)
  }

  # On calcule y_ridges, pour ploter chaque densite de groupe a un y different sur des multiples de 1 (0, 1, 2, 3, ..., n)
  df_dens <- df_dens %>%
    # group_by(group) %>%
    mutate(y_ridges = (y / max(y)) * height) %>%
    # ungroup() %>%
    mutate(
      y_ridges = y_ridges + (level - 1),
      quantFct = as.factor(quantFct) # Je transforme l'appartenance au quantile en facteur, pour le ggplot
    )

  # On isole les quantiles avec leurs coordonnees y de densite (pour les afficher avec le ggplot)
  quant_seg <- df_dens %>%
    filter(segment == TRUE)

  # On inclut les levels a tab => necessaire pour ggplot
  # Utilise aussi pour le nom des groupes dans le ggplot
  tab_level <- df_dens %>%
    group_by(group) %>%
    summarise(level = first(level)) %>%
    rename("{{ group }}" := group) # Pour la jointure
  tab <- tab %>%
    left_join(tab_level, by = names(select(., {{group}}))) # SOLUTION TROUVEE ICI : https://stackoverflow.com/questions/48449799/join-datasets-using-a-quosure-as-the-by-argument

  central <- tibble()
  # On estime la densite de la moyenne ou mediane et ses CI
  for(group.i in seq_along(vec_group.i)){
    df_dens_central <- df_dens |>
      filter(group == vec_group.i[group.i])

    # print(vec_group.i[group.i])
    # print(tab$indice_low[tab[[1]] == vec_group.i[group.i]])
    # print(tab$indice[tab[[1]] == vec_group.i[group.i]])
    # print(tab$indice_upp[tab[[1]] == vec_group.i[group.i]])

    df_dens_central <- df_dens_central %>%
      mutate(central = NA) %>%
      tibble::add_case(
        group = vec_group.i[group.i],
        level = group.i,
        x = tab$indice[tab[[1]] == vec_group.i[group.i]],
        y = stats::approx(df_dens_central$x, df_dens_central$y, xout = tab$indice[tab[[1]] == vec_group.i[group.i]], ties = "mean")$y,
        central = "indice"
      ) %>%
      tibble::add_case(
        group = vec_group.i[group.i],
        level = group.i,
        x = tab$indice_low[tab[[1]] == vec_group.i[group.i]],
        y = stats::approx(df_dens_central$x, df_dens_central$y, xout = tab$indice_low[tab[[1]] == vec_group.i[group.i]], ties = "mean")$y,
        central = "indice_low"
      ) %>%
      tibble::add_case(
        group = vec_group.i[group.i],
        level = group.i,
        x = tab$indice_upp[tab[[1]] == vec_group.i[group.i]],
        y = stats::approx(df_dens_central$x, df_dens_central$y, xout = tab$indice_upp[tab[[1]] == vec_group.i[group.i]], ties = "mean")$y,
        central = "indice_upp"
      )

    # On rbind progressivement les df de densite des differents groupes
    central <- rbind(central, df_dens_central)
  }

  # car central = df_dens avec les indices de centralite en plus
  df_dens <- central

  # On cree un df agrege avec une ligne par groupe qui comprend des colonnes avec l'indice + bornes des CI
  central_CI <- central |>
    filter(!is.na(central)) %>%
    select(-level, -y, -quantFct, -segment, -y_ridges) %>%
    pivot_wider(
      names_from = "central",
      values_from = "x"
      )

  if (reorder == T) {
    central_CI <- select(central_CI, -order)
    central <- central |>
      select(-order) %>%
      left_join(tab_order, by = "group") %>%
      mutate(level = order)
  }

  # On identifie toutes les valeurs de densite comprises dans les IC PAR GROUPE => permet de creer une region PAR GROUPE sur le ggplot
  central <- central |>
    left_join(central_CI, by = "group") %>%
    mutate(
      y_ridges = (y / max(y)) * height
    ) %>%
    group_by(group) %>%
    filter(x >= indice_low & x <= indice_upp) %>%
    ungroup() %>%
    mutate(
      y_ridges = y_ridges + (level - 1)
    )


  # 6. CREATION DU GRAPHIQUE --------------------

  if(all(isColor(pal)) == TRUE){
    # Si condition remplie on ne fait rien => on garde la palette
  } else {
    # Sinon on met la couleur par defaut
    message("Une couleur indiquee dans pal n'existe pas : la couleur par defaut est utilisee")
    pal <- "#e0dfe0"
  }

  # La palette divergente => varie selon que le nombre de quantiles soit pair ou impair
  if(length(estQuant_W_group$quantile) %% 2 == 0){
    palette <- grDevices::colorRampPalette(pal)((length(unique(estQuant_W$probs))/2)+1)
    palette <- c(palette, rev(palette)[-1])
  }
  if(length(estQuant_W_group$quantile) %% 2 == 1){
    palette <- grDevices::colorRampPalette(pal)(((length(unique(estQuant_W$probs))+1)/2)+1)
    palette <- c(palette, rev(palette)[-c(1,2)])
    # palette <- c(rev(rev(palette)[-2]), rev(palette)[-1])
  }

  # Palette pour les moustaches, selon le nombre de proportions dans moustache_probs
  if (show_moustache == T) {

    if(all(isColor(pal_moustache)) == TRUE){
      # Si condition remplie on ne fait rien => on garde la palette
    } else {
      # Sinon on met la couleur par defaut
      message("Une couleur indiquee dans pal_moustache n'existe pas : la couleur par defaut est utilisee")
      pal_moustache <- c("#EB9BA0", "#FAD7B1")
    }

    pal_mous_calc <- grDevices::colorRampPalette(pal_moustache)(length(unique(moustache_probs)))
  }

  # Les limites de la variable quanti si non indiquee par l'utilisateur => pour ggplot
  if(is.null(limits)){
    lim_min <- min(data_W$variables[["quanti_exp_flattened"]], na.rm = T)
    lim_max <- max(data_W$variables[["quanti_exp_flattened"]], na.rm = T)
    limits <- c(lim_min, lim_max)
  }

  # Le graphique ggplot

  graph <- ggplot(
    data = df_dens,
  ) +
    geom_ribbon(
      data = df_dens[!is.na(df_dens$quantFct), ], # Car geom_ribbon n'accepte pas les NA => warning si je le fais pas !
      aes(
        x = x, ymin = level - 1, ymax = y_ridges, # ymin a level - 1 car commence a 0
        fill = quantFct,
        group = interaction(group, quantFct) # Ici le groupe doit etre l'interaction du groupe et des quantiles pour dessiner correctement les ribbon par groupe
      ),
      alpha = alpha
    ) +
    geom_line(
      data = df_dens[!is.na(df_dens$y_ridges), ],
      aes(
        x = x,
        y = y_ridges,
        group = group
      ),
      color = color,
      linewidth = .7
    ) +
    # geom_line(
    #   aes(
    #     x = x,
    #     y = level - 1,
    #     group = group
    #   ),
    #   color = color,
    #   linewidth = .7
    # ) +
    scale_x_continuous(
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
      breaks = tab_level$level - 1,
      labels = stringr::str_wrap(tab_level[[1]], width = wrap_width_y),
      expand = expansion(mult = c(0.005, 0.05))
    ) +
    scale_fill_manual(
      values = palette,
      guide = "none"
    ) +
    theme_fonctionr(font = font) +
    theme(
      legend.position = "bottom"
    ) +
    labs(title = title,
         subtitle = subtitle
    )

  # Pour caption

  if (!is.null(caption)) { # Permet de passer a la ligne par rapport au test stat
    caption <- paste0("\n", stringr::str_wrap(caption, width = 100))
  }

  if (type == "mean") {
    graph <- graph +
      labs(
        caption = paste0(
          "GLM : ", scales::pvalue(test.stat$p[1], add_p = T),
          caption
        )
      )
  }
  if (type == "median") {
    graph <- graph +
      labs(
        caption = paste0(
          "Kruskal Wallis : ", scales::pvalue(test.stat$p.value[1], add_p = T),
          caption
        )
      )
  }

  # Ajouter l'aire des CI
  if (show_ci_area == T) {
    graph <- graph +
      geom_ribbon(
        data = central,
        aes(
          x = x, ymin = level - 1, ymax = y_ridges,
          group = group
        ),
        fill = "darkorange",
        alpha = .3 * alpha
      )
  }

  # Ajouter les segments des quantiles
  if (show_quant_lines == T) {
    graph <- graph +
      geom_segment(
        data = quant_seg,
        aes(x = x,
            y = level - 1,
            yend = y_ridges),
        # linetype = "dotted",
        alpha = .15
      )
  }

  # Ajouter la ligne de la tendance centrale
  if (show_mid_line == T) {
    graph <- graph +
      geom_segment(
        data = central[(central$central == "indice") & !is.na(central$central), ],
        aes(x = x,
            y = level - 1,
            yend = y_ridges,
            group = group),
        linewidth = 1,
        alpha = .6
      )
  }

  # Ajouter les limites des IC (segment)
  if (show_ci_lines == T) {
    graph <- graph +
      geom_segment(
        data = central[(central$central == "indice_low" | central$central == "indice_upp") & !is.na(central$central), ],
        aes(x = x,
            y = level - 1,
            yend = y_ridges,
            group = group),
        linetype = "dashed",
        linewidth = .7,
        alpha = .4
      )
  }

  # Ajouter les moustaches
  if (show_moustache == T) {
    graph <- graph +
      ggnewscale::new_scale_fill() + # Ici je suis oblige de reinitialiser une nouvelle palette avec le package ggnewscale => je vois pas d'autre moyen facile
      geom_rect(
        data = boxplot_df,
        aes(
          xmin = xbegin, xmax = xend, ymin = level - 1.1, ymax = level - .9,
          fill = moustache_prob
        ),
        alpha = 1
      ) +
      scale_fill_manual(
        values = pal_mous_calc,
        name = "Proportion d'observations"
      )
  }

  # Ajouter les limites des IC (errorbar)
  if (show_ci_errorbar == T) {
    graph <- graph +
      geom_errorbarh(
        data = tab,
        aes(
          xmin = indice_low,
          xmax = indice_upp,
          y = level - 1
        ),
        alpha = 0.5,
        height = .2
      )
  }

  # Ajouter le point de la tendance centrale
  if (show_mid_point == T) {
    graph <- graph +
      geom_point(
        data = tab,
        aes(x = indice,
            y = level - 1)
      )
  }

  # Afficher la valeur de la tendance centrale
  if (show_value == T) {
    graph<-graph  +
      geom_text(
        data = tab,
        aes(
          x = indice,
          y = level - 1,
          label = paste0(stringr::str_replace(round(indice,
                                                    digits = digits),
                                              "[.]",
                                              dec),
                         unit),
          family = font),
        size = 3,
        vjust = -1,
        color = "black",
        alpha = 0.9)
  }

  # Ajouter les axes
  if(show_lab == TRUE){
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
          labs(y = paste0(deparse(substitute(group))))
      }
    }
    if(all(!is.null(ylab), ylab == "")){
      graph <- graph +
        labs(y = NULL)
    }
  }

  # Masquer les axes si show_lab == FALSE
  if(show_lab == FALSE){
    graph <- graph +
      labs(x = NULL,
           y = NULL)
  }


  # 7. RESULTATS --------------------

  # Dans un but de lisibilite, on renomme les indices "mean" ou "median" selon la fonction appelee
  if (type == "mean") {
    tab <- tab %>%
      rename(mean = indice,
             mean_low = indice_low,
             mean_upp = indice_upp)
  }

  if (type == "median") {
    tab <- tab %>%
      rename(median = indice,
             median_low = indice_low,
             median_upp = indice_upp)
  }


  # On cree l'objet final
  res <- list()
  res$dens <- df_dens[, c("group", "x", "y", "quantFct", "y_ridges", "central")]
  res$tab <- tab[, !names(tab) %in% c("level")]
  res$quant <- estQuant_W[, !names(estQuant_W) %in% c("level", "se")]
  res$graph <- graph
  if (show_moustache == T) {
    res$moustache <- boxplot_df[, !names(boxplot_df) %in% c("level")]
  }
  return(res)

}


#' @rdname distrib_group_continuous
#' @export
distrib_group_c <- function(...) {
  distrib_group_continuous(...)
}
