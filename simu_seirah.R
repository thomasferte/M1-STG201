
# ================================================================================================================================= #
# ============                                                LIBRAIRIES                                               ============ #
# ================================================================================================================================= #

library(deSolve)


# ================================================================================================================================= #
# ============                                                FONCTIONS                                                ============ #
# ================================================================================================================================= #

# _______________ Fonction du modèle SEIRAH _______________ #

seirah_model = function(current_timepoint, state_values, param){
  
  N_pop_size <- c(12278210, 2559073, 2783039, 3303500, 5962662, 5511747, 3801797, 3340379, 5999982, 5924858, 8032377, 5055651)
  N <- sum(N_pop_size)
  
  S <- state_values[1]
  E <- state_values[2]        
  I <- state_values[3]        
  R <- state_values[4]        
  A <- state_values[5]        
  H <- state_values[6]        
  
  derS <- - param$b * S * ( I + param$alpha * A ) / N
  derE <- param$b * S * (I + param$alpha * A) / N - E/param$De
  derI <- param$rE / param$De * E - (1-param$rI) / param$Dq * I - param$rI / param$Di * I
  derR <- (param$rI * I + A) / param$Di +  H / param$Dh
  derA <- (1 - param$rE) / param$De * E -  A / param$Di
  derH <- (1-param$rI) / param$Dq * I - H / param$Dh
  
  results <- c("S" = derS, "E" = derE, "I" = derI, "R" = derR, "A" = derA, "H" = derH)
  return(list(results))
  
}

# _______________ Fonction de simulation du modèle SEIRAH avec un beta qui ne change pas _______________ #

simu_seirah <- function(beta){
  
  # NOTE :
  # --- On suppose que tous les paramètres autres que beta sont fixes.
  # --- On suppose que les effectifs initiaux sont fixes.
  
  # ___ Liste des paramètres utilisés ___ #
  myp <- list()
  myp$b <- beta
  myp$rE = 0.844
  myp$rI = 0.966
  myp$alpha = 0.55
  myp$De = 5.1
  myp$Di = 5
  myp$Dq = 11 - myp$De
  myp$Dh = 18.3 
  
  N <- 64553275
  init_E <- 31176
  init_I <- myp$rE*init_E / myp$De
  init_A <- init_I*(1-myp$rE) / myp$rE
  init_H <- (1-myp$rI)*init_I / myp$Dq
  init_R <- 12
  init_S <- N - (init_E + init_I + init_R + init_A + init_H)
  initial_values <- c(init_S, init_E, init_I, init_R, init_A, init_H)
  
  # ___ Calcul avec le solver ___ #
  output = lsoda(y = initial_values, times = seq(1, 390, by = 1), func = seirah_model, parms = myp)
  
  # ___ Regroupement des outputs sous une dataframe ___ #
  donnee <- as.data.frame(output[,])
  
  # ___ Retour des données ___ #
  return(donnee)
  
}

# _______________ Fonction de simulation du modèle SEIRAH avec un beta qui change une seule fois _______________ #

deux_beta_simu_seirah <- function(beta1, beta2, timechange){
  
  # NOTES :
  # --- On suppose que tous les paramètres autres que beta sont fixes.
  # --- On suppose que les effectifs initiaux sont fixes.
  
  # ___ Liste des paramètres utilisés ___ #
  myp <- list()
  myp$rE = 0.844
  myp$rI = 0.966
  myp$alpha = 0.55
  myp$De = 5.1
  myp$Di = 5
  myp$Dq = 11 - myp$De
  myp$Dh = 18.3 
  
  N <- 64553275
  init_E <- 31176
  init_I <- myp$rE*init_E / myp$De
  init_A <- init_I*(1-myp$rE) / myp$rE
  init_H <- (1-myp$rI)*init_I / myp$Dq
  init_R <- 12
  init_S <- N - (init_E + init_I + init_R + init_A + init_H)
  initial_values <- c(init_S, init_E, init_I, init_R, init_A, init_H)
  
  # ___ Création des 2 intervalles de temps ___ #
  t_debut <- 1
  t_fin <- 390
  t_change <- timechange
  timepoints_1 <- seq(1, timechange, by = 1)
  timepoints_2 <- seq(timechange, t_fin, by = 1)
  
  # ___ Calcul sur les deux intervalles ___ #
  myp$b <- beta1
  output1 = lsoda(y = initial_values, times = timepoints_1, func = seirah_model, parms = myp)
  myp$b <- beta2
  output2 = lsoda(y = output1[nrow(output1),2:7], times = timepoints_2, func = seirah_model, parms = myp)
  
  # ___ Regroupement des outputs sous une dataframe ___ #
  donnee = as.data.frame(rbind(output1[,],output2[2:nrow(output2),]))
  
  # ___ Retour des données ___ #
  return(donnee)
  
}

# _______________ Fonction de simulation du modèle SEIRAH avec un beta qui change plusieurs fois _______________ #

multi_beta_simu_seirah <- function(betas, temps){
  
  # NOTES :
  # --- On suppose que tous les paramètres autres que beta sont fixes.
  # --- On suppose que les effectifs initiaux sont fixes.
  
  # ___ Liste des paramètres utilisés ___ #
  myp <- list()
  myp$rE = 0.844
  myp$rI = 0.966
  myp$alpha = 0.55
  myp$De = 5.1
  myp$Di = 5
  myp$Dq = 11 - myp$De
  myp$Dh = 18.3 
  
  N <- 64553275
  init_E <- 31176
  init_I <- myp$rE*init_E / myp$De
  init_A <- init_I*(1-myp$rE) / myp$rE
  init_H <- (1-myp$rI)*init_I / myp$Dq
  init_R <- 12
  init_S <- N - (init_E + init_I + init_R + init_A + init_H)
  initial_values <- c(init_S, init_E, init_I, init_R, init_A, init_H)
  
  # ___ S'il n'y a qu'un seul beta et donc aucune valeur pour temps ___ #
  if (length(betas) == 1){
    return(simu_seirah(betas))
  }
  
  # ___ S'il y a 2 betas et 1 temps ___ #
  if (length(betas) == 2 && length(temps) == 1){
    return(deux_beta_simu_seirah(beta1 = betas[1], beta2 = betas[2], timechange = temps))
  }
  
  # ___ S'il y a au moins 3 betas ___ #
  
  # __ Calcul pour le premier intervalle __ #
  myp$b <- betas[1]
  output_a = lsoda(y = initial_values, times = seq(1, temps[1], by = 1), func = seirah_model, parms = myp)
  # __ Calcul pour le deuxième intervalle jusqu'à l'avant-dernier intervalle __ #
  for (i in (2:(length(betas)-1))){
    myp$b <- betas[i]
    output_a_bis = lsoda(y = output_a[nrow(output_a),2:7], times = seq(temps[i-1], temps[i], by = 1), func = seirah_model, parms = myp)
    output_a = rbind(output_a[,], output_a_bis[2:nrow(output_a_bis),])
  }
  # __ Calcul pour le dernier intervalle __ #
  myp$b <- betas[length(betas)]
  output_b = lsoda(y = output_a[nrow(output_a),2:7], times = seq(temps[length(temps)], 390, by = 1), func = seirah_model, parms = myp)
  # __ Regroupement des outputs __ #
  output = rbind(output_a[,], output_b[2:nrow(output_b),])
  
  # ___ Transformation en dataframe et retour des données ___ #
  donnee <- as.data.frame(output[,])
  return(donnee)
  
}


# ================================================================================================================================= #
# ============                                                PARAMÈTRES                                               ============ #
# ================================================================================================================================= #

myp <- list()
myp$b <- 2.55
myp$rE = 0.844
myp$rI = 0.966
myp$alpha = 0.55
myp$De = 5.1
myp$Di = 5
myp$Dq = 11 - myp$De
myp$Dh = 18.3 
myp$Dh_region = c(18.3, 19.5, 18, 20.6, 18.6, 17.7, 16.7, 19.6, 18.1, 17.4, 17, 18.1)

    # ___ Décommenter le code que l'on veut utiliser selon le choix entre France et régions que l'on veut faire ___ #
    # (par région : ce n'est pas pris en charge pour le moment)

# ______ France ______ #
N <- 64553275
init_E <- 31176
init_I <- myparam$rE*init_E / myparam$De
init_A <- init_I*(1-myparam$rE) / myparam$rE
init_H <- (1-myparam$rI)*init_I / myparam$Dq
init_R <- 12
init_S <- N - (init_E + init_I + init_R + init_A + init_H)

# # ______ Par régions ______  #
# region <-  c("IDF","Centre","BFC","Normandie","HDF","GrandEst","PaysLoire","Bretagne","NAquitaine","Occitanie","AURA","PACA")
# N_pop_size <- c(12278210, 2559073, 2783039, 3303500, 5962662, 5511747, 3801797, 3340379, 5999982, 5924858, 8032377, 5055651)
# N <- sum(N_pop_size)
# init_E <- c(12454, 1029, 1370, 730, 2493, 5138, 829, 494, 845, 1021, 3025, 1748)
# init_I <- myparam$rE*init_E / myparam$De
# init_A <- init_I*(1-myparam$rE) / myparam$rE
# init_H <- (1-myparam$rI)*init_I / myparam$Dq
# init_R <- rep(1,12)
# init_S <- N - (init_E + init_I + init_R + init_A + init_H)
#
# # ___ Tableau regroupement le nom des régions avec les effectifs initiaux et les effecteifs totaux de chaque région ___ #
# tab_effectifs <- cbind("reg" = region, "n_reg" = N_pop_size, "init_S_reg" = init_S, "init_E_reg" = init_E, "init_I_reg" = init_I, "init_R_reg" = init_R, "init_A_reg" = init_A, "init_H_reg" = init_H)
# tab_eff <- as.data.frame(tab_effectifs)


# ================================================================================================================================= #
# ============                                                GRAPHIQUES                                               ============ #
# ================================================================================================================================= #

# ATTENTION : b_mean est créé avec le code R 'data_graphs_regression' !
# Si l'on ne run pas ce dernier alors b_mean n'existera pas et il y aura une erreur.

# ____________ Ajour de la colonne date et nom des colonnes ____________ #
donnee$date = b_mean$date # pour faire correspondre le pas de temps à la date
#colnames(donnee)<- c("time", "S", "E", "I", "R", "A", "H")
colnames(donnee)<- c("time", "S", "E", "I", "R", "A", "H", "date")

# ____________ Passage des données en pourcentage ____________ #
donnee$S = (donnee$S)/N
donnee$E = (donnee$E)/N
donnee$I = (donnee$I)/N
donnee$R = (donnee$R)/N
donnee$A = (donnee$A)/N
donnee$H = (donnee$H)/N

# ____________ Graphiques ____________ #
plot(S ~ time, data = donnee, type='l', lwd=2, col = '#E52626', xlim = c(0,400), ylim = c(0,1), main="Courbes SEIRAH", xlab="Temps depuis le début de l'épidémie (en jours)", ylab="Proportion de la population")
points(donnee$E, type='l', lwd=2, col = '#E57126')
points(donnee$I, type='l', lwd=2, col = '#E5C826')
points(donnee$R, type='l', lwd=2, col = '#5BE526')
points(donnee$A, type='l', lwd=2, col = '#26E5AA')
points(donnee$H, type='l', lwd=2, col = '#B139E9')
legend(80,0.7, legend=c("S", "E", "I", "R", "A", "H"), col=c("#E52626", "#E57126", "#E5C826", "#5BE526", "#26E5AA", "#B139E9"), lty=1, cex=0.8)


# ================================================================================================================================= #
# ============                                                  TESTS                                                  ============ #
# ================================================================================================================================= #

  # NOTES :
  # --- Pour faire la simulation : 
  #     Changer uniquement les valeurs dans la partie "paramètres nécessaires", 
  #     puis exécuter tout le code des parties "coeur de la fonction" et "test graphique".


# ================================================================= #
# ================= Tests fonction 'simu_seirah' ================== #
# ================================================================= #

# _______ Paramètres nécessaires pour le déroulement _______ #
beta <- 2.55

# _______ Coeur de la fonction _______ #
myp <- list()
myp$b <- beta
myp$rE = 0.844
myp$rI = 0.966
myp$alpha = 0.55
myp$De = 5.1
myp$Di = 5
myp$Dq = 11 - myp$De
myp$Dh = 18.3 
N <- 64553275
init_E <- 31176
init_I <- myp$rE*init_E / myp$De
init_A <- init_I*(1-myp$rE) / myp$rE
init_H <- (1-myp$rI)*init_I / myp$Dq
init_R <- 12
init_S <- N - (init_E + init_I + init_R + init_A + init_H)
initial_values <- c(init_S, init_E, init_I, init_R, init_A, init_H)
output = lsoda(y = initial_values, times = seq(1, 390, by = 1), func = seirah_model, parms = myp)
donnee <- as.data.frame(output[,])

# _______ Test graphique _______ #
colnames(donnee)<- c("time", "S", "E", "I", "R", "A", "H")
donnee$S = (donnee$S)/N
donnee$E = (donnee$E)/N
donnee$I = (donnee$I)/N
donnee$R = (donnee$R)/N
donnee$A = (donnee$A)/N
donnee$H = (donnee$H)/N
plot(S ~ time, data = donnee, type='l', lwd=2, col = '#E52626', xlim = c(0,100), ylim = c(0,1), main="Courbes SEIRAH", xlab="Temps depuis le début de l'épidémie (en jours)", ylab="Proportion de la population")
points(donnee$E, type='l', lwd=2, col = '#E57126')
points(donnee$I, type='l', lwd=2, col = '#E5C826')
points(donnee$R, type='l', lwd=2, col = '#5BE526')
points(donnee$A, type='l', lwd=2, col = '#26E5AA')
points(donnee$H, type='l', lwd=2, col = '#B139E9')
legend(80,0.7, legend=c("S", "E", "I", "R", "A", "H"), col=c("#E52626", "#E57126", "#E5C826", "#5BE526", "#26E5AA", "#B139E9"), lty=1, cex=0.8)


# ================================================================= #
# ============ Tests fonction 'deux_beta_simu_seirah' ============= #
# ================================================================= #

# _______ Paramètres nécessaires pour le déroulement _______ #
beta1 <- 2.55
beta2 <- 1.25
timechange <- 17

# _______ Coeur de la fonction _______ #
myp <- list()
myp$rE = 0.844
myp$rI = 0.966
myp$alpha = 0.55
myp$De = 5.1
myp$Di = 5
myp$Dq = 11 - myp$De
myp$Dh = 18.3 
N <- 64553275
init_E <- 31176
init_I <- myp$rE*init_E / myp$De
init_A <- init_I*(1-myp$rE) / myp$rE
init_H <- (1-myp$rI)*init_I / myp$Dq
init_R <- 12
init_S <- N - (init_E + init_I + init_R + init_A + init_H)
initial_values <- c(init_S, init_E, init_I, init_R, init_A, init_H)
t_debut <- 1
t_fin <- 390
t_change <- timechange
timepoints_1 <- seq(1, timechange, by = 1)
timepoints_2 <- seq(timechange, t_fin, by = 1)
myp$b <- beta1
output1 = lsoda(y = initial_values, times = timepoints_1, func = seirah_model, parms = myp)
myp$b <- beta2
output2 = lsoda(y = output1[nrow(output1),2:7], times = timepoints_2, func = seirah_model, parms = myp)
donnee = as.data.frame(rbind(output1[,],output2[2:nrow(output2),]))

# _______ Test graphique _______ #
colnames(donnee)<- c("time", "S", "E", "I", "R", "A", "H")
donnee$S = (donnee$S)/N
donnee$E = (donnee$E)/N
donnee$I = (donnee$I)/N
donnee$R = (donnee$R)/N
donnee$A = (donnee$A)/N
donnee$H = (donnee$H)/N
plot(S ~ time, data = donnee, type='l', lwd=2, col = '#E52626', xlim = c(0,60), ylim = c(0,1), main="Courbes SEIRAH", xlab="Temps depuis le début de l'épidémie (en jours)", ylab="Proportion de la population")
points(donnee$E, type='l', lwd=2, col = '#E57126')
points(donnee$I, type='l', lwd=2, col = '#E5C826')
points(donnee$R, type='l', lwd=2, col = '#5BE526')
points(donnee$A, type='l', lwd=2, col = '#26E5AA')
points(donnee$H, type='l', lwd=2, col = '#B139E9')
abline(v=t_change)
legend(50,0.7, legend=c("S", "E", "I", "R", "A", "H"), col=c("#E52626", "#E57126", "#E5C826", "#5BE526", "#26E5AA", "#B139E9"), lty=1, cex=0.8)


# ================================================================= #
# ============ Tests fonction 'multi_beta_simu_seirah' ============ #
# ================================================================= #

# _______ Paramètres nécessaires pour le déroulement _______ #
#betas <- c(3, 2, 1, 0.5, 0.05)
#temps <- c(13, 18, 25, 40)

betas <- b_mean[1:389,2] # Il ne faut pas prendre le 390e beta car on s'arrête après avoir calculé les effectifs au temps 390 et pas 391.
temps <- seq(2,389, by=1) # Il ne faut pas prendre le 1 ni le 390 pour ne pas avoir un "seq(t_debut, t_fin, by = 1)" qui ne vale qu'une seule valeur, rendant                             le calcul impossible.

# _______ Coeur de la fonction _______ #
myp <- list()
myp$rE = 0.844
myp$rI = 0.966
myp$alpha = 0.55
myp$De = 5.1
myp$Di = 5
myp$Dq = 11 - myp$De
myp$Dh = 18.3 
N <- 64553275
init_E <- 31176
init_I <- myp$rE*init_E / myp$De
init_A <- init_I*(1-myp$rE) / myp$rE
init_H <- (1-myp$rI)*init_I / myp$Dq
init_R <- 12
init_S <- N - (init_E + init_I + init_R + init_A + init_H)
initial_values <- c(init_S, init_E, init_I, init_R, init_A, init_H)
if (length(betas) == 1){
  return(simu_seirah(betas))
}
if (length(betas) == 2){
  return(deux_beta_simu_seirah(beta1 = betas[1], beta2 = betas[2], timechange = temps))
}
myp$b <- betas[1]
output_a = lsoda(y = initial_values, times = seq(1, temps[1], by = 1), func = seirah_model, parms = myp)
for (i in (2:(length(betas)-1))){
  myp$b <- betas[i]
  output_a_bis = lsoda(y = output_a[nrow(output_a),2:7], times = seq(temps[i-1], temps[i], by = 1), func = seirah_model, parms = myp)
  output_a = rbind(output_a[,], output_a_bis[2:nrow(output_a_bis),])
}
myp$b <- betas[length(betas)]
output_b = lsoda(y = output_a[nrow(output_a),2:7], times = seq(temps[length(temps)], 390, by = 1), func = seirah_model, parms = myp)
output = rbind(output_a[,], output_b[2:nrow(output_b),])
donnee <- as.data.frame(output[,])

# _______ Test graphique _______ #
colnames(donnee)<- c("time", "S", "E", "I", "R", "A", "H")
donnee$S = (donnee$S)/N
donnee$E = (donnee$E)/N
donnee$I = (donnee$I)/N
donnee$R = (donnee$R)/N
donnee$A = (donnee$A)/N
donnee$H = (donnee$H)/N
plot(S ~ time, data = donnee, type='l', lwd=2, col = '#E52626', xlim = c(0,400), ylim = c(0,0.01), main="Courbes SEIRAH", xlab="Temps depuis le début de l'épidémie (en jours)", ylab="Proportion de la population")
points(donnee$E, type='l', lwd=2, col = '#E57126')
points(donnee$I, type='l', lwd=2, col = '#E5C826')
points(donnee$R, type='l', lwd=2, col = '#5BE526')
points(donnee$A, type='l', lwd=2, col = '#26E5AA')
points(donnee$H, type='l', lwd=2, col = '#B139E9')
  # _____ Ne pas run cette partie s'il y a beaucoup de changement de beta sinon ça sera trop illisible.
# for (i in c(1:length(temps))){abline(v=temps[i])}
  # _____
legend(50,0.7, legend=c("S", "E", "I", "R", "A", "H"), col=c("#E52626", "#E57126", "#E5C826", "#5BE526", "#26E5AA", "#B139E9"), lty=1, cex=0.8)

