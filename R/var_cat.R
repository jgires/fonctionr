
#' var_cat : fonction pour réaliser facilement un grpahique et une analyse d'une distribution d'une variable qualitative d'un sondage complexe
#'
#' @param data
#' @param var_cat
#' @param reorder
#' @param na.rm
#' @param title
#' @param xlabel
#' @param ...
#'
#' @return
#' @import ggplot2
#' @import survey
#' @import srvyr
#' @import dplyr
#' @import forcats
#'
#' @export
#'
#' @examples

var_cat <- function(data,#données en format srvyr
                    var_cat,#variable catégorielle
                    facet_var = NULL,
                    filter_exp = NULL,
                    na.rm = T,
                    fill = "deepskyblue3",
                    show_n = FALSE,
                    show_prop = TRUE,
                    prop_method = "beta",#possibilité de choisir la methode d'ajustement des IC, car empiriqument, j'ai eu des problèmes avec logit
                    title = "", # le titre du graphique
                    xlabel = "", # le nom de l'axe de la variable catégorielle
                    font ="Arial", #quelle font par défaut?
                    digits = 0,
                    ...
                       ) {



  # Petite fonction utile
  `%ni%` = Negate(`%in%`)

  extrafont::loadfonts(quiet = TRUE)######charger les fonts. Peut-être ajouter extrafont dans les package nécéssaire si ce n'est déjà fait -------


  # On crée une quosure de facet_var & filter_exp => pour if statements dans la fonction (voir ci-dessous)
  #FAIRE AUSSI POUR FILTER
  quo_facet <- enquo(facet_var)
  quo_filter <- enquo(filter_exp)


  # On convertit d'abord en objet srvyr
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

  ##appliquer l'éventuel filtre
  if(!quo_is_null(quo_filter)){
    data_W <- data_W %>%
      filter({{ filter_exp }})
  }


#transformer en facteur la variable catégorielle

#
#
       if (na.rm == T){
         data_W<-data_W %>%
     filter(!is.na({{var_cat}})) %>%
     mutate(
       "{{ var_cat }}" := droplevels(as.factor({{ var_cat }}))
     )
       }
   else {
     data_W<-data_W %>%
       mutate("{{ var_cat }}" := droplevels(fct_na_value_to_level(as.factor({{ var_cat }}), "NA")))

   }


  # On convertit également la variable de facet en facteur si facet non-NULL
   if(!quo_is_null(quo_facet)){
     data_W <- data_W %>%
       mutate(
         "{{ facet_var }}" := droplevels(as.factor({{ facet_var }}))) # droplevels pour éviter qu'un level soit encodé alors qu'il n'a pas d'effectifs (pb pour le test khi2)
   }

  #faire la table------------------


  if (quo_is_null(quo_facet)) {
    table<-data_W %>%
  group_by({{var_cat}}) %>%
    srvyr::summarize(prop = survey_prop(vartype="ci", proportion = T, prop_method = {{prop_method}}),n_weighted=survey_total(vartype="ci"),n=n()) # si l'on met les poids pondéré, je trouve nécessaire et pertinent de mettre leurs IC
  }
  if(!quo_is_null(quo_facet)) {table<-data_W %>%
    group_by({{facet_var}},{{var_cat}}) %>%
    srvyr::summarize(prop = survey_prop(vartype="ci", proportion = T, prop_method = {{prop_method}}),n_weighted=survey_total(vartype="ci"),n=n()) # si l'on met les poids pondéré, je trouve nécessaire et pertinent de mettre leurs IC

    }


#faire le graphique ---------------

#créer variable max_ggplot

  max_ggplot <- max(table$prop, na.rm = TRUE)


# On crée la palette

  palette <-{{fill}}

  #le graphique proprement dit

  graph<-table %>%
    ggplot( aes(x={{var_cat}},
                y=prop,
    )) +
    geom_bar(stat="identity",
             fill = palette) +
    geom_errorbar(aes(ymin=prop_low, ymax=prop_upp),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.5))+
    theme_minimal()+
    scale_y_continuous(labels = scales::label_percent(scale = 100))   +
    scale_x_discrete()+
    theme(
       panel.grid.minor.y = element_blank(),
       panel.grid.minor.x = element_line(color = "#dddddd"),
       panel.grid.major.y = element_blank(),
       panel.grid.major.x = element_line(color = "#dddddd"),
       axis.line = element_line(color = "black"),
       axis.ticks= element_line(color = "black"),
       legend.position = "none",
       text=element_text(family={{font}}),
       axis.text=element_text(color="black"))+
    ggtitle({{title}})+
    xlab({{xlabel}})+
    ylab("")+


    coord_flip()

#ajouter les prop au besoin
if (show_prop == TRUE){
  graph<-graph  +
    geom_text(aes(y = prop_upp+(0.1*max_ggplot),###j'ai l'impression que le 1, c'est bon, mais c'est à vérifier
                  label = paste(round(prop*100,
                                      digits = {{digits}}),
                                "%"),
                  family = {{font}}),
              color = "black")
}


#ajouter les factets au besoin


if (!quo_is_null(quo_facet)) {
  graph <- graph +
    facet_wrap(vars({{ facet_var }}))
}

#ajouter le nombre d'individus au besoin

    if (show_n == TRUE) {
      graph <- graph +
        geom_text(
          aes(
            y = 0 + (0.0001 * max_ggplot), # Pour ajouter des labels avec les effectifs en dessous des barres
            label = paste0("n=", n),
            family = {{font}}
          ),
          alpha = 0.7,
          hjust = 0 # Justifié à droite
        )
    }



  ##retourner les résultat
  res <- list()
  res$tab <- table
  res$graph <- graph

  return(res)

}

