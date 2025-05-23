library(dplyr)
library(xtable)

# Charger les trois fichiers
qre <- readRDS("resultats_qre.RDS") %>% select(id, Log_v_QRE = VraisemblanceMax)
ni  <- readRDS("resultats_ni.RDS")  %>% select(id, Log_v_NI  = VraisemblanceMax)
ch  <- readRDS("resultats_ch.RDS")  %>% select(id, Log_v_CH  = VraisemblanceMax)
NI_resum = readRDS("resumNI.RDS")
CH_resum = readRDS("resumCH.RDS")
qre_resum = readRDS("resumQRE.RDS")

# Chargement des résultats
res_ch_p_q <- readRDS("resultats_ch_par_question.RDS") %>%
  select(Question, Log_v_CH = LogVraisemblance, Brier_CH = Brier)

res_ni_p_q <- readRDS("resultats_par_question_NI.RDS") %>%
  select(Question, Log_v_NI = LogVraisemblance, Brier_NI = Brier)

res_qre_p_q <- readRDS("resultats_par_question_QRE.RDS") %>%
  select(Question, Log_v_QRE = LogVraisemblance, Brier_QRE = Brier)

# Fusionner les tables dans l'ordre QRE, CH, NI
logv_combined <- qre %>%
  inner_join(ch, by = "id") %>%
  inner_join(ni, by = "id") %>%
  arrange(id)

# Mise en forme : mettre en gras la meilleure log-vraisemblance
logv_formatted <- logv_combined %>%
  rowwise() %>%
  mutate(
    best_val = max(c_across(Log_v_QRE:Log_v_NI), na.rm = TRUE),
    Log_v_QRE = ifelse(as.numeric(Log_v_QRE) == best_val, 
                       paste0("\\textbf{", round(Log_v_QRE, 2), "}"), 
                       as.character(round(Log_v_QRE, 2))),
    Log_v_CH  = ifelse(as.numeric(Log_v_CH) == best_val, 
                       paste0("\\textbf{", round(Log_v_CH, 2), "}"), 
                       as.character(round(Log_v_CH, 2))),
    Log_v_NI  = ifelse(as.numeric(Log_v_NI) == best_val, 
                       paste0("\\textbf{", round(Log_v_NI, 2), "}"), 
                       as.character(round(Log_v_NI, 2)))
  ) %>%
  ungroup() %>%
  select(id, Log_v_QRE, Log_v_CH, Log_v_NI)  # Réorganise pour QRE, CH, NI

# Export LaTeX avec les valeurs en gras
print(
  xtable(
    logv_formatted,
    caption = "Comparaison des log-vraisemblances par individu pour les modèles QRE, CH et NI",
    label = "tab:logv_comparaison"
  ),
  include.rownames = FALSE,
  sanitize.text.function = identity  # Permet d'utiliser le LaTeX brut dans le tableau
)


# On crée un data frame résumé avec les valeurs moyennes de log-vraisemblance et score de Brier
resum_simple <- data.frame(
  Modèle = c("QRE", "CH", "NI"),
  `Log-vraisemblance moyenne` = round(c(mean(qre_resum[1]),
                                        mean(CH_resum[1]),
                                        mean(NI_resum[1])), 2),
  `Score de Brier moyen` = round(c(mean(qre_resum[2]),
                                   mean(CH_resum[2]),
                                   mean(NI_resum[2])), 3)
)

# Affiche en LaTeX
print(
  xtable(
    resum_simple,
    caption = "Comparaison des modèles selon la log-vraisemblance et le score de Brier moyens",
    label = "tab:resum_simple"
  ),
  include.rownames = FALSE
)

# Fusionner les trois tables sur la colonne "Question"
resultats_combines <- res_qre_p_q %>%
  inner_join(res_ch_p_q, by = "Question") %>%
  inner_join(res_ni_p_q, by = "Question")

# Mise en forme : mise en gras des meilleures valeurs
resultats_formates <- resultats_combines %>%
  rowwise() %>%
  mutate(
    # Meilleure log-vraisemblance (la plus élevée)
    max_logv = max(c_across(starts_with("Log_v_")), na.rm = TRUE),
    # Meilleur score de Brier (le plus bas)
    min_brier = min(c_across(starts_with("Brier_")), na.rm = TRUE),
    
    Log_v_QRE = ifelse(Log_v_QRE == max_logv, paste0("\\textbf{", round(Log_v_QRE, 2), "}"), as.character(round(Log_v_QRE, 2))),
    Log_v_CH  = ifelse(Log_v_CH  == max_logv, paste0("\\textbf{", round(Log_v_CH, 2), "}"), as.character(round(Log_v_CH, 2))),
    Log_v_NI  = ifelse(Log_v_NI  == max_logv, paste0("\\textbf{", round(Log_v_NI, 2), "}"), as.character(round(Log_v_NI, 2))),
    
    Brier_QRE = ifelse(Brier_QRE == min_brier, paste0("\\textbf{", round(Brier_QRE, 3), "}"), as.character(round(Brier_QRE, 3))),
    Brier_CH  = ifelse(Brier_CH  == min_brier, paste0("\\textbf{", round(Brier_CH, 3), "}"), as.character(round(Brier_CH, 3))),
    Brier_NI  = ifelse(Brier_NI  == min_brier, paste0("\\textbf{", round(Brier_NI, 3), "}"), as.character(round(Brier_NI, 3)))
  ) %>%
  ungroup() %>%
  select(Question,
         Brier_QRE, Log_v_QRE,
         Brier_CH,  Log_v_CH,
         Brier_NI,  Log_v_NI)

# Affichage sous forme de tableau LaTeX
library(xtable)
print(xtable(resultats_formates), include.rownames = FALSE, sanitize.text.function = identity)
