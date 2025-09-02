# PREMIÈRE CHOSE À FAIRE : ouvrir le projet RStudio, si pas encore fait, pour se
# placer dans le bon dossier.


# --- 1. INSTALLATION ET CHARGEMENT DES PACKAGES ---
# ==============================================================================

# Liste des packages nécessaires
packages <- c(
  "dplyr",       # Manipulation de données
  "FactoMineR",  # Analyse factorielle (ACM)
  "factoextra",  # Visualisations pour analyses factorielles
  "explor",      # Interface interactive pour analyses factorielles
  "ggrepel",     # Labels sans chevauchement dans ggplot2
  "ggtext",      # Formatage avancé du texte dans ggplot2
  "readr",       # Lecture de fichiers CSV
  "ggplot2"      # Visualisations
)

# Fonction pour installer et charger les packages
install_and_load <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
  library(pkg, character.only = TRUE)
}

# Appliquer la fonction à chaque package
invisible(sapply(packages, install_and_load))


# --- 2. IMPORTATION DES DONNÉES ---
# ==============================================================================

# Lecture du fichier CSV 
bdd <- read_csv("extrait_acm.csv")

# Affichage de la structure des données importées
str(bdd)


# --- 3. PRÉPARATION ET NETTOYAGE DES DONNÉES ---
# ==============================================================================

# Traitement des valeurs manquantes et recodage des variables
bdd <- bdd %>% 
  # Remplacer les NA dans la variable politique par "Not said"
  mutate(pol_lr_scale_agg = ifelse(is.na(pol_lr_scale_agg),
                                   "Not said",
                                   pol_lr_scale_agg)) %>% 
  # Recoder "Not applicable" en "No occupation"
  mutate(soc_occupation_group = recode(soc_occupation_group,
                                       "Not applicable" = "No occupation")) %>% 
  # Convertir toutes les variables en facteurs (sauf l'âge qui est numérique)
  mutate(across(-soc_age, as.factor)) %>% 
  # Renommer les variables pour plus de clarté
  rename(
    age = soc_age,
    education = soc_education_group,
    socioprofessional_group = soc_csp_insee_agg,
    occupation_group = soc_occupation_group,
    political_vote = pol_lr_scale_agg
  )

# Découpage de l'âge en catégories :
# Jeunes : < 35 ans, Adultes : 35-60 ans, Retraités : > 60 ans
bdd$age <- cut(bdd$age,
               breaks = c(-Inf, 35, 60, Inf),
               labels = c("young", "adult", "retired"),
               include.lowest = TRUE)
bdd$age <- as.factor(bdd$age)

# Vérification des transformations
str(bdd)
summary(bdd)


# --- 4. RÉALISATION DE L'ACM ---
# ==============================================================================

# Les colonnes 1 à 5 sont définies comme variables supplémentaires (quali.sup)
# Ces variables ne participent pas à la construction des axes mais sont
# projetées. Il s'agit des variables : age, education, socioprofessional_group,
# occupation_group et political_vote.
res.mca <- MCA(bdd, quali.sup = 1:5)


# --- 5. PREMIÈRE ANALYSE DES RÉSULTATS ---
# ==============================================================================

explor(res.mca) # Ouverture d'une interface interractive


# --- 6. DICTIONNAIRE DE RENOMMAGE DES MODALITÉS ---
# ==============================================================================

# Mapping pour renommer les modalités TV et Radio avec des noms plus lisibles
labels_tvradio <- c(
  
  # Chaînes de télévision
  "chainesteletf1"  = "TF1",
  "chainesteletf2"  = "France 2", 
  "chainesteletf3"  = "France 3",
  "chainesteletf4"  = "Canal+",
  "chainesteletf5"  = "France 5",
  "chainesteletf6"  = "M6",
  "chainesteletf7"  = "Arte",
  "chainesteletf8"  = "C8",
  "chainesteletf9"  = "W9",
  "chainesteletf10" = "TMC",
  "chainesteletf11" = "TFX",
  "chainesteletf12" = "NRJ 12",
  "chainesteletf13" = "LCP/Public Sénat",
  "chainesteletf14" = "France 4",
  "chainesteletf15" = "BFM TV",
  "chainesteletf16" = "CNews",
  "chainesteletf17" = "CSTAR",
  "chainesteletf18" = "Gulli",
  "chainesteletf19" = "L'Équipe",
  "chainesteletf20" = "6ter",
  "chainesteletf21" = "RMC Story",
  "chainesteletf22" = "LCI",
  "chainesteletf23" = "France Info",
  "chainesteletf24" = "Autre chaîne",
  
  # Stations de radio
  "stationsradioeurope" = "Europe 1",
  "stationsradiofrancebleu" = "ICI (ex France Bleu)",
  "stationsradiofranceinter" = "France Inter",
  "stationsradiormc" = "RMC",
  "stationsradiortl" = "RTL",
  "stationsradiofranceinfo" = "France Info",
  "stationsradioradioclassique" = "Radio Classique",
  "stationsradiofranceculture" = "France Culture",
  "stationsradionfmbusiness" = "BFM Business",
  "stationsradiosudradio" = "Sud Radio",
  "stationsradiorfi" = "RFI",
  "stationsradioradiolocale" = "Radio locale"
  
)


# --- 7. DÉFINITION DES VARIABLES SUPPLÉMENTAIRES ---
# ==============================================================================

# Liste des modalités pour chaque variable qualitative supplémentaire
# ATTENTION : il faut absolument que les modalités de la base de données
# correspondent à ce qui est affiché ici. S'il y a une modalité en plus ou des
# NA, il faut re-nettoyer la base de données dans les sections précédentes.
variables_quali_supp <- list(
  "education" = c("High school/Short post-secondary", "Less than high school",
                  "University degree"),
  "socioprofessional_group" = c("Firm owners", "Higher occupations", "Inactive",
                                "Intermediate occupations", "Lower occupations"),
  "occupation_group" = c("Blue collar workers", "Clerical workers", "Executives",
                         "Farm owner", "Intermediate", "No occupation"),
  "political_vote" = c("Center", "Left", "Not said", "Right"),
  "age" = c("young", "adult", "retired")
)


# --- 8. FONCTION DE VISUALISATION PERSONNALISÉE ---
# ==============================================================================

#' Création d'un graphique ACM personnalisé
#'
#' @param res.mca Résultat de l'ACM (objet MCA)
#' @param variables_quali_supp Liste des variables supplémentaires
#' @param labels_tvradio Dictionnaire de renommage des modalités
#' @param axes Vecteur des axes à représenter (défaut : c(1, 2))
#' @param longueur_accroches Distance des labels par rapport aux points
#' @param taille_boxes Taille du texte des labels
#' 
#' @return Objet ggplot
#' 
#' @description
#' Cette fonction crée une représentation graphique personnalisée de l'ACM avec
#' - points noirs pour les chaînes TV
#' - points bleus pour les stations radio  
#' - points rouges avec formes spécifiques pour les variables supplémentaires
#' - quadrillage pour faciliter la lecture
#' - labels repositionnés automatiquement pour éviter les chevauchements

plot_mca_custom <- function(res.mca, variables_quali_supp, labels_tvradio,
                            axes = c(1, 2), longueur_accroches = 0.5,
                            taille_boxes = 3.5) {
  
  # Extraction des coordonnées pour les axes choisis
  coord <- res.mca$var$coord[, axes]           # Variables actives
  coord_sup <- res.mca$quali.sup$coord[, axes] # Variables supplémentaires
  
  # Filtrage des modalités "YES" (variables binaires), et on elève ce "YES" dans
  # le nom des modalités
  coord_yes <- coord[grep("_YES$", rownames(coord)), , drop = FALSE]
  rownames(coord_yes) <- gsub("_YES$", "", rownames(coord_yes))
  
  # Création du dataframe pour les variables actives en ajoutant une
  # catégorisation TV/Radio
  df_yes <- data.frame(
    Dim1 = coord_yes[, 1],
    Dim2 = coord_yes[, 2],
    Label = rownames(coord_yes),
    Type = ifelse(grepl("chainesteletf", rownames(coord_yes)),
                  "TV",
                  ifelse(grepl("stationsradio", rownames(coord_yes)),
                         "Radio",
                         "Autre"))
  )
  
  # Création du dataframe pour les variables supplémentaires
  df_sup <- data.frame(
    Dim1 = coord_sup[, 1],
    Dim2 = coord_sup[, 2],
    Label = rownames(coord_sup),
    Type = "supp"
  )
  
  # Fusion des dataframes
  df_all <- rbind(df_yes, df_sup)
  
  # Application du renommage des labels
  df_all$Label <- dplyr::recode(df_all$Label, !!!labels_tvradio)
  
  # Configuration des formes pour les variables supplémentaires
  shape_map <- c(
    "education" = 15,                    # Carré
    "socioprofessional_group" = 17,      # Triangle
    "occupation_group" = 21,             # Cercle vide
    "political_vote" = 8,                # Étoile
    "age" = 19                           # Cercle plein
  )
  
  # Mapping modalité -> variable de base
  modalite_to_var <- list()
  for (var_name in names(variables_quali_supp)) {
    for (modalite in variables_quali_supp[[var_name]]) {
      modalite_to_var[[modalite]] <- var_name
    }
  }
  
  # Attribution des formes
  df_all$Shape <- 16  # Forme par défaut (cercle) pour TV et Radio
  df_all$Variable_Base <- NA
  
  for (i in 1:nrow(df_all)) {
    if (df_all$Type[i] == "supp") {
      modalite_label <- df_all$Label[i]
      if (modalite_label %in% names(modalite_to_var)) {
        var_base <- modalite_to_var[[modalite_label]]
        df_all$Variable_Base[i] <- var_base
        df_all$Shape[i] <- shape_map[var_base]
      }
    }
  }
  
  # Création des labels pour la légende
  df_all$Legend_Combined <- df_all$Type
  df_all$Legend_Combined[df_all$Type == "TV"] <- "TV"
  df_all$Legend_Combined[df_all$Type == "Radio"] <- "Radio"
  df_all$Legend_Combined[df_all$Type == "supp"] <- paste0(
    "Supp: ", df_all$Variable_Base[df_all$Type == "supp"]
  )
  
  # Configuration du quadrillage
  x_min <- floor(min(df_all$Dim1) * 2) / 2
  x_max <- ceiling(max(df_all$Dim1) * 2) / 2
  y_min <- floor(min(df_all$Dim2) * 2) / 2
  y_max <- ceiling(max(df_all$Dim2) * 2) / 2
  
  # Lignes principales
  breaks_major_x <- seq(x_min, x_max, by = 1)
  breaks_major_y <- seq(y_min, y_max, by = 1)
  # Lignes secondaires
  breaks_minor_x <- setdiff(seq(x_min, x_max, by = 0.5), breaks_major_x)
  breaks_minor_y <- setdiff(seq(y_min, y_max, by = 0.5), breaks_major_y)
  
  # Ordre des éléments de légende
  df_all$Legend_Combined <- factor(
    df_all$Legend_Combined,
    levels = c("TV", "Radio", paste0("Supp: ", names(shape_map)))
  )
  
  # Calcul des pourcentages d'inertie pour les titres des axes
  perc_x <- round(res.mca$eig[axes[1], 2], 1)
  perc_y <- round(res.mca$eig[axes[2], 2], 1)
  x_lab <- paste0("\nDim ", axes[1], " (", perc_x, "%)")
  y_lab <- paste0("Dim ", axes[2], " (", perc_y, "%)\n")
  
  # Création du graphique ggplot
  p <- ggplot(df_all, aes(x = Dim1, y = Dim2, label = Label)) +
    
    # Quadrillage
    geom_vline(xintercept = breaks_major_x, linetype = "dashed",
               color = "grey70", linewidth = 0.5) +
    geom_hline(yintercept = breaks_major_y, linetype = "dashed",
               color = "grey70", linewidth = 0.5) +
    geom_vline(xintercept = breaks_minor_x, linetype = "dotted",
               color = "grey70", linewidth = 0.45) +
    geom_hline(yintercept = breaks_minor_y, linetype = "dotted",
               color = "grey70", linewidth = 0.45) +
    
    # Axes principaux
    geom_hline(yintercept = 0, linetype = "solid", color = "black",
               linewidth = 0.8) +
    geom_vline(xintercept = 0, linetype = "solid", color = "black",
               linewidth = 0.8) +
    
    # Points
    geom_point(aes(color = Legend_Combined, shape = Legend_Combined),
               size = 3) +
    
    # Labels avec repositionnement automatique
    geom_label_repel(aes(color = Legend_Combined), 
                     box.padding = longueur_accroches, 
                     segment.color = "grey", 
                     size = taille_boxes, 
                     show.legend = FALSE) +
    
    # Configuration des couleurs
    scale_color_manual(
      values = c("TV" = "black", 
                 "Radio" = "blue", 
                 setNames(rep("red", length(shape_map)),
                          paste0("Supp: ", names(shape_map))))
    ) +
    
    # Configuration des formes
    scale_shape_manual(
      values = c("TV" = 16, 
                 "Radio" = 16, 
                 setNames(shape_map, paste0("Supp: ", names(shape_map))))
    ) +
    
    # Labels des axes
    labs(x = x_lab, y = y_lab, color = NULL, shape = NULL) +
    
    # Thème
    theme_minimal() +
    theme(
      axis.line = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "bottom",
      legend.text = element_text(size = 12),
      legend.key.size = unit(1.2, "lines"),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12)
    ) +
    
    # Configuration de la légende
    guides(color = guide_legend(override.aes = list(size = 4)),
           shape = guide_legend(override.aes = list(size = 4)))
  
  return(p)
}


# --- 9. CRÉATION ET AFFICHAGE DU GRAPHIQUE ---
# ==============================================================================

# Création du graphique ACM
p <- plot_mca_custom(
  res.mca = res.mca,
  variables_quali_supp = variables_quali_supp,
  labels_tvradio = labels_tvradio,
  axes = c(1, 2),                    # Axes à représenter
  longueur_accroches = 0.5,          # Distance des labels
  taille_boxes = 3                   # Taille du texte
)

# Affichage du graphique
print(p)

# Les warnings sont normaux, ils alertent sur les les textes qui se chevauchant
# sur le graphque qui vient d'apparaître à droite. Pour avoir un graphique
# propre, appuyer sur "Zoom" et dimensionner la fenêtre de sorte à obtenir le
# graphique désiré.
