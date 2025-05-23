# Desc : Solveur de système de la forme AX=B
# Liste des choix : 
# i = 1 -> question 2,3 et 4 de l'expérience
# i = 2 -> question 5 de l'expérience
# i = 3 -> question 6 + 11 à 16 de l'expérience 
# i = 4 -> question 7 de l'expérience
# i = 5 -> question 8 de l'expérience 
# i = 6 -> question 9 de l'expérience 
# i = 7 -> question 10 de l'expérience

# Calcul des équilibres de Nash ----

# Initialisation matrice de résultats
resultats <- matrix(NA, nrow = 7, ncol = 5)

for(i in 1:7){
  
  if(i == 1) { 
    c1 <- c(16,26,16,16,16)
    c2 <- c(17,17,27,17,17)
    c3 <- c(18,18,18,28,18)
    c4 <- c(19,19,19,19,29)
    c5 <- c(20,20,20,20,20)
  }
  if(i==2) {
    c1 <- c(16,36,16,16,16)
    c2 <- c(17,17,37,17,17)
    c3 <- c(18,18,18,38,18)
    c4 <- c(19,19,19,19,39)
    c5 <- c(20,20,20,20,20)
  }
  if(i==3){
    c1 <- c(12,32,12,12,12)
    c2 <- c(14,14,34,14,14)
    c3 <- c(16,16,16,36,16)
    c4 <- c(18,18,18,18,38)
    c5 <- c(20,20,20,20,20)
  }
  if(i==4){
    c1 <- c(22,32,12,12,12)
    c2 <- c(14,24,34,14,14)
    c3 <- c(16,16,26,36,16)
    c4 <- c(18,18,18,28,38)
    c5 <- c(20,20,20,20,30)
  }
  if(i==5){
    c1 <- c(22,52,12,12,12)
    c2 <- c(14,24,54,14,14)
    c3 <- c(16,16,26,56,16)
    c4 <- c(18,18,18,28,58)
    c5 <- c(20,20,20,20,30)
  }
  if(i==6){
    c1 <- c(14,34,14,14,14)
    c2 <- c(12,12,32,12,12)
    c3 <- c(18,18,18,38,18)
    c4 <- c(16,16,16,16,36)
    c5 <- c(20,20,20,20,20)
  }
  if(i==7){
    c1 <- c(18,38,18,18,18)
    c2 <- c(16,16,36,16,16)
    c3 <- c(14,14,14,34,14)
    c4 <- c(12,12,12,12,32)
    c5 <- c(20,20,20,20,20)
  }
  
  # Soustraction des espérances
  e1 <- c1 - c2
  e2 <- c2 - c3
  e3 <- c3 - c4
  e4 <- c4 - c5
  
  # Matrice des coefficients A
  A <- matrix(c(e1[1], e1[2], e1[3], e1[4], e1[5],
                e2[1], e2[2], e2[3], e2[4], e2[5],
                e3[1], e3[2], e3[3], e3[4], e3[5],
                e4[1], e4[2], e4[3], e4[4], e4[5],
                1,1,1,1,1),
              nrow = 5, byrow = TRUE)
  
  # Vecteur B
  B <- c(0, 0, 0, 0, 1)
  
  # Résolution
  solution <- solve(A, B)
  resultats[i, ] <- round(solution, 2)
}

# Extraire la ligne 1
ligne1 <- resultats[1, , drop = FALSE]

# Insérer deux fois la ligne 1 pour questions 2 et 3 
resultats <- rbind(
  ligne1,   # ligne 1
  ligne1,                   # nouvelle ligne 2 (copie de la 1)
  ligne1,                   # nouvelle ligne 3 (copie de la 1)
  resultats[2:7, , drop = FALSE]  # décaler les autres lignes
)

# Extraire la ligne 3
ligne3 <- resultats[3, , drop = FALSE]

# Insérer six fois la ligne 3 pour questions 11 à 16  
resultats <- rbind(
  resultats,
  ligne3,
  ligne3,
  ligne3,
  ligne3,
  ligne3,
  ligne3
)

# Associer résultats aux questions
rownames(resultats) <- paste0("Ne", 2:16)

# Correction en équilibre de Nash pure
resultats["Ne7",] <- c(1,0,0,0,0)
resultats["Ne8",] <- c(1,0,0,0,0)

# Sauvegarde de la matrice de résultats 
save(resultats, file = "ressources/equilibres_Nash.RData")
