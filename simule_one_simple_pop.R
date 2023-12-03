

library(tidyverse)
library(ggthemes)

simulate_1_pop <- function(parametre) {
  
  # CONDITIONS DE SIMULATION
  temps = 30 # nb de pas de temps (en années)

  r = parametre[1]
  K = parametre[2]
  
  tau_p = 0.1
  
  # INITIALISATION
  N <- array(0, dim = c(temps))
  Nm <- array(0, dim = c(temps))
  
  # conditions initiales 
  Nm[1] = 10 # N0
  N[1] = rlnorm(1, log(Nm[1]), tau_p)
  
  # boucle du temps
  for (t in 1:(temps - 1)) {
    Nm[t + 1] = max(N[t] + r * N[t] * (1 - N[t] / K) , 0.0001 * K)
    N[t + 1] = rlnorm(1, log(Nm[t + 1]), tau_p)
  }
  
  res = tibble(N = N,
               time = 1:temps)
  
  return(res)
}


simulate_n_pop = function(parametre = c(1.2, 100), nb_of_pop=5){
  df_simulations = data.frame()
  for (i in 1:nb_of_pop) {
    data_pop = simulate_1_pop(parametre)
    data_pop$simu_nb = as.factor(rep(i, nrow(data_pop)))
    df_simulations = rbind(df_simulations, data_pop)
  }
  return(df_simulations)
}



plot_isolated_pop = function(parametre = c(1.2, 100), nb_of_pop=5){
  df_simu = simulate_n_pop(parametre, nb_of_pop)
  
  plot = ggplot(data = df_simu, aes(x = time, y = N, color=simu_nb)) +
    geom_line(linewidth = 0.8, alpha = 0.8) +
    labs(title = "Population dynamic for 5 isolated populations",
         x = "Time",
         y = "Population size",
         color = "Population") +
    theme_hc() +
    theme(axis.title = element_text()) +
    scale_color_brewer(palette = "Set1")
  return(plot)
}

