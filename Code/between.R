# Desc : Statitique Descriptive pour Between 

# ---- Librairies ---- 
library(readxl)
library(ggplot2)
library(xtable)
library(tidyr)
library(dplyr)
library(patchwork)

# ---- Database expérience ---- 
exp <- read_excel("data/Exp_rience.xlsx")
data <- exp[1:63, 7:21]

# ---- Séquence des questions ----
q2345 <- c(16,17,18,19,20)
q678 <- c(12,14,16,18,20)
q9   <- c(14,12,18,16,20)
q10  <- c(18,16,14,12,20)

# ---- Equilibres de Nash théoriques ---- 
load("ressources/equilibres_Nash.RData") # charge la matrice appelée `resultats`

# ---- Effectifs des réponses ---- 
effectifs <- lapply(data, table)

# ---- Matrice des effectifs ----
matrice_effectifs <- do.call(rbind, lapply(effectifs, function(x) {
  as.numeric(x)
}))
rownames(matrice_effectifs) <- paste0("q",2:16)
rownames(resultats) <- rownames(matrice_effectifs)

# ---- Aparté des équilibres de Nash pur ----
## Stockage
NashPurObs <- matrice_effectifs[c(6, 7),]
NashPurTheo <- resultats[c(6, 7),]
## Retrait
effectifs_obs <- matrice_effectifs
effectifs_theo <- resultats
matrice_effectifs <- matrice_effectifs[-c(6, 7),]
resultats <- resultats[-c(6, 7),]

# ---- Tests du chi² ----
tests_chi2 <- list()
n <- nrow(matrice_effectifs)
p_values <- numeric(n)
names(p_values) <- rownames(matrice_effectifs)

for (i in 1:n) {
  question <- rownames(matrice_effectifs)[i]
  a <- matrice_effectifs[i, ]  # Observé
  b <- resultats[i, ]          # Théorique (probabilités)
  
  if (question != "q5") {
    # Combinaison des 2 premières modalités
    a_grouped <- c(a[1] + a[2], a[-c(1, 2)])
    b_grouped <- c(b[1] + b[2], b[-c(1, 2)])
  } else {
    # Pas de regroupement pour la question 5
    a_grouped <- a
    b_grouped <- b
  }
  
  # Vérification que les probabilités sont normalisées
  if (abs(sum(b_grouped) - 1) > 1e-6) {
    warning(paste("Les probabilités théoriques de", question, "ne somment pas à 1"))
  }
  
  # Test du chi²
  test <- chisq.test(x = a_grouped, p = b_grouped, simulate.p.value = FALSE)
  
  # Stockage
  tests_chi2[[question]] <- test
  p_values[i] <- test$p.value
}


# ---- Affichage brut ----
print("P-values brutes :")
print(p_values)

# ---- Ajustement des p-values (Benjamini-Hochberg) ----
p_values_adjusted_bh <- p.adjust(p_values, method = "BH")
print(p_values_adjusted_bh)

# ---- Affichage des résidus et des valeurs attendues pour chaque question ----
for (i in 1:n) {
  cat("\n", rownames(matrice_effectifs)[i], ":\n")
  print("Effectifs observés :")
  print(tests_chi2[[i]]$observed)
  print("Effectifs attendus :")
  print(round(tests_chi2[[i]]$expected, 2))
  print("Résidus :")
  print(round(tests_chi2[[i]]$residuals, 2))
}

# ---- Résumés tables lateX ---- 
# Associer les bonnes valeurs par question
valeurs_questions <- list(
  q2 = q2345,
  q3 = q2345,
  q4 = q2345,
  q5 = q2345,
  q6 = q678,
  q7 = q678,
  q8 = q678,
  q9 = q9,
  q10 = q10,
  q11 = q678,
  q12 = q678,
  q13 = q678,
  q14 = q678,
  q15 = q678,
  q16 = q678
)

# ---- Figure p-values par méthode BH ---- 
# Création du data frame avec les p-values ajustées
df_p_values_bh <- data.frame(
  Comparaison = names(p_values),   # Les comparaisons (nom des tests)
  p_value_adjusted = p_values_adjusted_bh  # P-values ajustées (Benjamini-Hochberg)
)

# Trier par p-value ajustée croissante
df_p_values_bh_sorted <- df_p_values_bh[order(df_p_values_bh$p_value_adjusted), ]
df_p_values_bh_sorted$rank <- 1:nrow(df_p_values_bh_sorted)

# Calcul du seuil BH basé sur les p-values ajustées
alpha <- 0.05
m <- nrow(df_p_values_bh_sorted)
df_p_values_bh_sorted$bh_threshold <- (df_p_values_bh_sorted$rank / m) * alpha

# Assurer l'ordre des comparaisons dans l'axe X
df_p_values_bh_sorted$Comparaison <- factor(df_p_values_bh_sorted$Comparaison, levels = df_p_values_bh_sorted$Comparaison)

# Graphique
ggplot(df_p_values_bh_sorted, aes(x = Comparaison, y = p_value_adjusted)) +
  geom_point(aes(color = p_value_adjusted < bh_threshold), size = 2) +  # Blue if p-value < threshold, red if p-value >= threshold
  geom_line(aes(x = Comparaison, y = bh_threshold, group = 1), color = "darkgreen", size = 1) +  # Seuil BH en vert
  scale_color_manual(values = c("red", "blue"), labels = c("Non significatif", "Significatif")) +  # Couleur : rouge = non significatif, bleu = significatif
  labs(title = "",
       x = "Comparaison",
       y = "p-value",
       color = "Résultat") +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



## ---- Table des effectifs comparés ----
for (question in rownames(matrice_effectifs)) {
  # Récupération des valeurs
  valeurs <- valeurs_questions[[question]]
  obs <- matrice_effectifs[question, ]
  theorique <- round(resultats[question, ] * sum(obs), 0)
  
  # Transformation en entiers
  valeurs <- as.integer(valeurs)
  theorique <- as.integer(theorique)
  obs <- as.integer(obs)
  
  # Création du tableau
  tab <- rbind(theorique, obs)
  rownames(tab) <- c("Effectifs théoriques", "Effectifs observés")
  colnames(tab) <- as.character(valeurs)  # Utilisation des valeurs comme noms de colonnes
  
  # Affichage LaTeX avec [H] pour fixer la position
  cat("\n\n% ---- Tableau pour", question, "----\n")
  print(
    xtable(tab, caption = paste("Résultats pour", question)),
    include.rownames = TRUE,
    table.placement = "H"
  )
}

## ---- Table des p-values ----
# Création du tableau pour les p-values avec formatage
format_pval <- function(p) {
  ifelse(p < 0.05, "<0.05", formatC(p, digits = 4, format = "f"))
}

tab_pval <- cbind(
  "P-value brute" = format_pval(p_values),
  "P-value ajustée (Benjamini-Hochberg)" = format_pval(p_values_adjusted_bh)
)

# ---- Affichage LaTeX avec xtable ----
cat("\n\n% ---- Tableau des p-values ----\n")
print(
  xtable(tab_pval, caption = "P-values brutes et ajustées"),
  include.rownames = TRUE,
  table.placement = "H",
  sanitize.text.function = identity  # Pour afficher les "<" correctement
)

# ---- Histogrammes comparatifs (observé vs théorique) en pourcentage ----
plots <- list()

for (question in rownames(effectifs_obs)) {
  observe <- effectifs_obs[question, ]
  theo <- round(effectifs_theo[question, ] * sum(observe), 0)
  valeurs <- valeurs_questions[[question]]
  
  # Transformation en entiers
  valeurs <- as.integer(valeurs)
  theo <- as.integer(theo)
  observe <- as.integer(observe)
  
  # Calcul des pourcentages
  total_observe <- sum(observe)
  total_theo <- sum(theo)
  
  observe_percentage <- (observe / total_observe) * 100
  theo_percentage <- (theo / total_theo) * 100
  
  df_plot <- data.frame(
    Valeur = rep(valeurs, 2),
    Effectif = c(observe_percentage, theo_percentage),
    Type = rep(c("Observé", "Théorique"), each = length(observe))
  ) %>%
    mutate(Valeur = factor(Valeur, levels = valeurs))
  
  p <- ggplot(df_plot, aes(x = Valeur, y = Effectif, fill = Type)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("Observé" = "black", "Théorique" = "grey")) +
    labs(title = "",
         y = "Fréquence (en %)",
         x = "Choix") +
    theme_minimal()
  
  plots[[question]] <- p
}

# ---- Plots ---- 
## ---- Rapport ---- 
plots[["q7"]] + plots[["q8"]]
plots[["q2"]] + plots [["q5"]]

## ---- Annexe ----
plots[["q3"]] + plots [["q4"]]
plots[["q6"]]
plots[["q9"]] + plots [["q10"]]
plots[["q11"]] + plots [["q12"]]
plots[["q13"]] + plots [["q14"]]
plots[["q15"]] + plots [["q16"]]


question <- "q2"
observe <- effectifs_obs[question, ]
valeurs <- valeurs_questions[[question]]

# Transformation en entiers
valeurs <- as.integer(valeurs)
observe <- as.integer(observe)

# Normalisation entre 0 et 1
obs_normalise <- observe / sum(observe)

# Création du data frame
df_obs <- data.frame(
  Valeur = valeurs,
  Frequence = obs_normalise
) %>%
  mutate(Valeur = factor(Valeur, levels = valeurs))

# Plot ggplot
ggplot(df_obs, aes(x = Valeur, y = Frequence)) +
  geom_bar(stat = "identity", fill = "black") +
  labs(title = "",
       y = "Fréquence observée",
       x = "Action possible") +
  theme_minimal(base_size=14)

