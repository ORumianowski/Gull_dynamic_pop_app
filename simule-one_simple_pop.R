

library(tidyverse)

simulation <- function(parametre) {
  
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



plot = ggplot()
for (j in 1:5) {
  
  data_pop = simulation(parametre = c(1.2, 100))
  
  plot = plot +
    geom_line(data = data_pop, aes(x = time, y = N), linewidth = 0.5)
}

print(plot)
