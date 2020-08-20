# MASTER 2 EN STATISTIQUES ET ECONOMETRIE
# EtUDIANT: TRAN NAM MAI
# EXCERICSE 2: PROGRAMMATION
# (30 lignes de commande)

# Creer la fonction simul_elec()
simul_elec <- function(n,cas,B){
  # Verifier que les arguments d'entree sont adequats au probleme: n impair, cas egale a "IC" ou "IAC_star"
  stopifnot(n%%2 == 1, (cas == "IC" | cas == "IAC_star")) 
  
  # Creer un tableau indiquant le nombre de votes pour D dans chaque etat, avec la methode de probabilite inseree par l'utilisateur de cette fonction
  voix <- as.table(replicate(B, rbinom(4, c(n, 2*n + 1, 3*n, 5*n), prob = ifelse(cas == "IC", 0.5, runif(4)))))
  
  # Creer un tableau indiquant le nombre total de votes dans chaque etat, replique B fois
  population <- as.table(replicate(B, c(n, 2*n + 1, 3*n, 5*n)))
  
  # Creer un tableau indiquant si D a le vote majeur dans chaque etat
  majeur <- as.table(t(voix > population /2))
  
  # Si D a le vote majeur dans l'etat X, X representera ses grands electeurs qui voteront pour D lors du tour national. 
  # Donc, on remplace la valeur de grand electeur par 1,2,3 et 5 respectivement pour les etats 1, 2, 3 et 4 si le D dispose de plus de la moitie des voix dans chaque etat
  majeur[,1][which(majeur[,1] == TRUE)] = 1
  majeur[,2][which(majeur[,2] == TRUE)] = 2
  majeur[,3][which(majeur[,3] == TRUE)] = 3
  majeur[,4][which(majeur[,4] == TRUE)] = 5
  
  # Creer un tableau indiquant si D gagne selon le mécanisme indirect de l'election (Oui = 1, Non = 0)
  # Ce qui signifie que le nombre de grand electeur votant pour D devrait etre superieur a la moitie du total du grand electeur de ce pays (11 etant le nombre total de grand electeur de ce pays)
  gagne <- ifelse(rowSums(majeur) > 11/2, 1, 0)
  gagne <- t(t(gagne))
  gagne <- as.table(cbind(gagne, gagne, gagne, gagne))
  
  # Creer un tableau indiquant si la difference de voix entre D et l'autre candidat est egal a 1 (Oui = 1, Non = 0)
  voix <- as.table(t(voix))
  voix[,1] <- ifelse(voix[,1] == (n + 1)/2, 1, 0)
  voix[,2] <- ifelse(voix[,2] == (2*n + 2)/2, 1, 0)
  voix[,3] <- ifelse(voix[,3] == (3*n + 1)/2, 1, 0)
  voix[,4] <- ifelse(voix[,4] == (5*n + 1)/2, 1, 0)
  
  # Creer un autre tableau indiquant le nombre d'electeurs pivot selon la formule de calcul
  majeur[,1][which(majeur[,1] == 1)] = (n + 1)/2
  majeur[,2][which(majeur[,2] == 2)] = (2*n + 2)/2
  majeur[,3][which(majeur[,3] == 3)] = (3*n + 1)/2
  majeur[,4][which(majeur[,4] == 5)] = (5*n + 1)/2
  
  # Calculer la probabilite d'etre electeur pitvot 
  pivot <- (majeur*gagne*voix) / t(population)
  
  # Les conditions de algorithme: 
  # L'etat ait ete gagne par le vainqueur de l'election: verifie
  # La difference de voix entre le gagnant et le perdant dans l'etat soit egal a 1: verifie
  # Le nombre de grands electeurs dans l'etat soit suffisament grand pour qu'un changement de camp fasse basculer l'election: verifie
  
  # Donner le moyen de probabilite d'etre electeur pivot
  colnames(pivot) <- c("etat_1", "etat_2", "etat_3", "etat_4")
  return(colMeans(pivot))
} 

# Prendre les exemples suivants pour verifier que la fonction marche bien :
res_IC <- simul_elec(n = 5, cas = "IC", B = 100000)
res_IC

res_IAC_star <- simul_elec(n = 5, cas = "IAC_star", B = 100000)
res_IAC_star

system.time(res_IC)
system.time(res_IAC_star)

# Les electeurs n'ont pas le meme impact sur le resultat de l'election selon qu'ils dans l'etat 1,2,3, et 4 parce que la probabilite qu'un electeur appartenant soit pivot est tres petite et different dans un etat d'un autre.
# En essayant n plus grand, on deduit que la probabilite qu'un electeur soit pivot converge vers 0
# La vitesse de convergence vers 0 de res_IAC_star est plus rapide que celle de res_IC, ce qui signifie que: Le plus rationnel dans le vote, le moins probable d'avoir des electeurs pivots

res_IC_large <- simul_elec(n = 123456781, cas = "IC", B = 100000)
res_IC_large

res_IAC_star_large <- simul_elec(n = 123456781, cas = "IAC_star", B = 100000)
res_IAC_star_large
