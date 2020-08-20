# Programmer la fonction split_noeud()
split_noeud <- function (y, x, n_min = 25) { 
  df <- data.frame(x, y)[order(x),] # Creer une trame de donnees a partir de x et y avec x en ordre croissant
  df$p_individu <- Inf # Initier une colonne vide contenant un nombre non fini pour la boucle
  
  for (i in n_min:(nrow(df) - n_min)) { # Pour s'assurer que chaque echantillon doit avoir une taille superieure ou egale a n_min
    sub1 <- df[1:i,]
    sub2 <- df[(i+1):nrow(df),]
    
    # Calculer la proportion de chaque individus unique dans l'æ¼ã¸¹chantillon 1 et 2
    p_unique1 <- prop.table(table(sub1$y)) 
    p_unique2 <- prop.table(table(sub2$y)) 
    
    # Calculer Q1 et Q2
    Q1 <- p_unique1 [1] * (1 - p_unique1 [1]) + p_unique1 [2] * (1 - p_unique1 [2]) 
    Q2 <- p_unique2 [1] * (1 - p_unique2 [1]) + p_unique2 [2] * (1 - p_unique2 [2]) 
    
    # Remplacer la colonne p_individu par l'impurete qui est la somme de Q1 et Q2
    df[i,'p_individu'] <- sum(Q1,Q2)
  }
  
  # On interesse a l'impurete minimale (impurity) et la valeur x correspondante (x_split)
  impurity <- min(df$p_individu) 
  x_split <- df[df$p_individu == impurity, 'x']
  return(list('x_split' = x_split, 'impurity' = impurity))
}

# Exemple d'application:
x <- runif(1000, -1, 1)
y <- ifelse(x > 0.2 + rnorm(1000, 0, 0.05), "a", "b")
(res_example <- split_noeud(y, x))

# On considere le jeu de donnees simule (df_simulate) de taille n = 2000 observations et p = 1001 variables.
n_simu <- 2000
p_simu <- 1000
set.seed(1)

df_simulate <- data.frame(x_1 = rnorm(n_simu))
for (k in 2:p_simu) {
  set.seed(k)
  df_simulate[, paste0("x_", k)] <- rnorm(n_simu)
}

set.seed(123)
bruit <- rnorm(n_simu, 0, 0.1)
df_simulate[, "y"] <- "G1"
df_simulate[df_simulate$x_40 > bruit & df_simulate$x_99 > 0.8 + bruit, "y"] <- "G2"
df_simulate[df_simulate$x_40 > bruit & df_simulate$x_99 <= 0.8 + bruit & df_simulate$x_30 > 0.5 + bruit, "y"] <- "G3"
df_simulate[df_simulate$x_40 > bruit & df_simulate$x_99 <= 0.8 + bruit & df_simulate$x_30 <= 0.5 + bruit, "y"] <- "G4"
df_simulate[df_simulate$x_40 <= bruit & df_simulate$x_150 < 0.5 + bruit, "y"] <- "G5"

# Trouver la variable parmi toutes les variables explicatives de df_simulate qui permet d'obtenir l'impurite la plus petite
# Tout d'abord, utiliser la fonction split_noeud pour trouver l'impurete et x_split pour chacque variables explicatives de df_simulate
system.time(
  my_impurity <- lapply(df_simulate[,1:5], split_noeud, y = df_simulate$y)
)

# Ensuite, creer une fonction qui aide a trouver l'impurete minimale et la variable de x associee 
my_fun <- function(my_impurity, df_simulate) {
  explicative = names(df_simulate[,1:1000])
  my_var = explicative[1]
  for (i in 2:length(my_impurity)) {
    new_impurity = my_impurity[[i]][[2]]
    if (my_impurity[[1]][[2]] > new_impurity) { # comparer un par un et garder la plus petite impurete
      my_impurity[[1]][[2]] = new_impurity
      my_var = explicative[i]
    }
  }
  y = unlist(df_simulate[,1001])
  x = as.numeric(unlist(df_simulate[my_var]))
  my_xsplit = split_noeud(y, x)[[1]]
  return(list(my_var, my_xsplit))
}

# Enfin, extraire le ræ¼ã¸¹sultat
result <- my_fun(my_impurity, df_simulate)
my_var <- result[[1]]
my_xsplit <- result[[2]]

# Reproduire le meme resultat en faisant du calcul parallele
library(snow)
P <- 4
cl <- parallel::makeCluster(P)
system.time(
  my_impurity_parallel <- clusterApply(cl, df_simulate[,1:1000], fun = split_noeud, y = df_simulate$y)
)

stopCluster(cl)
# On peux voir que lors de l'utilisation de calcul parallele, le programme s'execute beaucoup plus rapidement

result_parallel <- my_fun(my_impurity_parallel, df_simulate)

# On note ech_2 et ech_3 les deux achantillons obtenus en sæ¼ã¸¹parant l'echantillon de dapart en deux a l'atape pracadente (sur le 1er noeud).
# Tout d'abord, definir deux sous-echantillons de df_simulate
ech_2 = df_simulate[df_simulate[my_var] < my_xsplit, ]
ech_3 = df_simulate[df_simulate[my_var] >= my_xsplit, ]

# Ensuite, utiliser le calcul perallele¸le (meme methode avec my_impurity_parallel) pour chacque sous-echantillon 
# Deuxieme noeud
P <- 4
cl <- parallel::makeCluster(P)
system.time(
  my_impurity_parallel2 <- clusterApply(cl, ech_2[,1:1000], fun = split_noeud, y = ech_2$y)
)

stopCluster(cl)

result_parallel2 <- my_fun(my_impurity_parallel2, ech_2)

# Troisieme noeud
P <- 4
cl <- parallel::makeCluster(P)
system.time(
  my_impurity_parallel3 <- clusterApply(cl, ech_3[,1:1000], fun = split_noeud, y = ech_3$y)
)

stopCluster(cl)

result_parallel3 <- my_fun(my_impurity_parallel3, ech_3)

# Enfin, comparer des resultats et des temps de calcul: J'ai obtenu des resultats similaires a ceux obtenus avec rpart mais rpart est plus performant, meme lorsque j' utilise la methode de calcul parallel
require(rpart)

par(mfrow = c(1, 2), xpd = NA)
system.time(
  fit <- rpart(y ~ ., data = df_simulate,
               control = rpart.control(maxdepth = 2))
)
