
library(tidyverse)
library(ggthemes)

parametre = list(r = c(1, 2.1), K=c(1000, 800), N0=c(300, 10))

simulate_n_metapop <- function(parametre) 
{ 
  # faudra rendre de nouveau les parametres changeable mais quand ce sera pour n pop
  
  # CONDITIONS DE SIMULATION
  temps = 30 # nb de pas de temps (en annees)
  
  nb_pop = length(parametre$r)
  
  r = parametre[[1]] 
  K = parametre[[2]]
  N0 = parametre[[3]]
  
  # Introducing some stochasticity in the dynamic
  sigma_p = 0.1 # the variability associated with the reproduction process
  
  # INITIALISATION
  N <- array(0, dim = c(temps, nb_pop))
  Nm <- array(0, dim = c(temps, nb_pop))
  
  em = array(0, dim = c(temps, nb_pop))
  Nm[1,] = N0
  N[1,] = rlnorm(length(N0), log(Nm[1,]), sigma_p)

  # boucle du temps           
  for (t in 1:(temps - 1)) {
    Nt = N[t,]
    # REPRODUCTION
    Nm[t + 1,] = reproduction(Nt, r, K)  # reproduction
    
    # STOCHASTICITY
    N_tplus1 = add_stochasticity(Nm[t + 1,], sigma_p)
    
    # EMIGRATION
    nb_emigrant = calc_nb_emigrant(N_tplus1,K) 

    # IMMIGRATION
    nb_immigrant = calc_nb_immigrant(nb_emigrant)
    
    # Total ; N - emigrant + immigre
    N[t + 1,] = N_tplus1 -  nb_emigrant + nb_immigrant
    em[t,] = nb_emigrant
  }
  
    
  df_results = cbind(time = 1:temps,
                 Effectif = N,
                 emigration = em) %>% as.data.frame()
  
  colnames(df_results) = c("time", 
                           paste("Population", 1:nb_pop),
                           paste0("emigration_pop", 1:nb_pop))
  return(df_results)
}



reproduction = function(Nt, r, K){
  # REPRODUCTION
  nm1 = Nt + r * Nt * (1 - Nt / K)  # reproduction
  #nm2 = max(nm1  , 0.001 * K) # security line
  return(nm1)
}

add_stochasticity = function(Nm_tplus1, sigma_p){
  return(rlnorm(length(Nm_tplus1), log(Nm_tplus1), sigma_p) )
}

calc_nb_emigrant = function(N_tplus1,K){
  # calcul du nombre d'emigrant
  nb_emigrant =  pmax((N_tplus1 -  K)  , 0)  
  return(nb_emigrant)
}


calc_nb_immigrant = function(nb_emigrant){
  #chaque pop se partage equitablement les emigrants
  nb_immigrant = (sum(nb_emigrant) - nb_emigrant) / (length(nb_emigrant) - 1)
  return(nb_immigrant)
}

plot_connected_pop = function(parametre){
  df_simu = simulate_n_metapop(parametre) %>% 
    select(time,starts_with("Population")) %>% 
    pivot_longer(-time, values_to = "N", names_to = "pop")
  
  K = parametre[[2]]
  nb_of_pop = length(K)

  plot = ggplot(data = df_simu, aes(x = time, y = N, color=pop)) +
    geom_line(linewidth = 0.8, alpha = 0.8) +
    labs(title = paste("Population dynamic for", nb_of_pop ,"connected populations"),
         x = "Time",
         y = "Population size",
         color = "Population") +
    theme_hc() +
    theme(axis.title = element_text()) +
    scale_color_brewer(palette = "Set1") +
    ylim(0, 1.3*max(K))
  return(plot)
}

# plot_connected_pop(parametre)
# 
# sim2 = simulate_n_metapop2(parametre)
# 
# sim2$effectif_pop1 #effectif pop 1
# 
# 
# data2 = sim2 %>% 
#   select(time,starts_with("effectif")) %>% 
#   pivot_longer(-time, values_to = "effectif", names_to = "pop") 
# 
# 
# ggplot(data2, aes(x = time, y = effectif, col = pop)) +
#   geom_line(linewidth = 0.8, alpha = 0.8)
# 
# 
# # essai pour quatre pop
# parametre = list(r = c(-0.2, 2.1, 1.2, -0.8), 
#                  K = c(1000, 800, 250, 300), 
#                  N0=c(30, 25, 15, 20))
# 
# simulate_n_metapop2(parametre = parametre)