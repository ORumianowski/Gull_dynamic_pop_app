

library(tidyverse)
library(ggthemes)

simulate_1_pop <- function() {
  
  # CONDITIONS DE SIMULATION
  temps = 30 # nb de pas de temps (en années)
  
  r = c(-0.2, 2.1)
  K = c(1000, 800)
  N0 = c(30, 25)
  
  nb_pop = length(r)
  
  # what is tau_p
  tau_p = 0.1
  
  # INITIALISATION
  N <- array(0, dim = c(temps, nb_pop))
  Nm <- array(0, dim = c(temps, nb_pop))
  
  em = array(0, dim = c(temps, nb_pop))
  
  for (i in 1:nb_pop){
    # conditions initiales 
    Nm[1,i] = N0[i]
    N[1,i] = rlnorm(1, log(Nm[1]), tau_p)
  }
  
  # boucle du temps           
  for (t in 1:(temps - 1)) {
    
    for (i in 1:nb_pop){
      
      Nm[t + 1,i] = max(N[t,i] + r[i] * N[t,i] * (1 - N[t,i] / K[i]) , 0.001 * K[i]) # reproduction
      em[t,i] =  max( (Nm[t + 1,i] -  K[i])  , 0)  
      Nm[t + 1,i] = Nm[t + 1,i]  -  em[t,i]   # emigration
      Nm[t + 1,i] = max(Nm[t + 1,i] , 0.001 * K[i]) 
      N[t + 1,i] = rlnorm(1, log(Nm[t + 1,i]), tau_p) # stochasticity
      
      
    }
    
    for (i in 1:nb_pop){
      immi = sum(em[t,1:nb_pop]) - em[t,i]
      N[t + 1,i] = N[t + 1,i]  + immi #immigration
    }
  }
  
  
  
  res.N = tibble(N = N,
               time = 1:temps)
  
  res.em = tibble(em = em,
                 time = 1:temps)
  
  
  res = list("Effectif" = res.N, "emigration" =res.em ) # , "immigration" = )
  
  return(res)
}

sim = simulate_1_pop()

sim$Effectif$N[,1] #effectif pop 1


data1 = tibble(time = 1:30,
               pop = sim$Effectif$N[,1])

data2 = tibble(time = 1:30,
               pop = sim$Effectif$N[,2])

data3 = tibble(time = 1:30,
               pop = sim$emigration$em[,2])

ggplot() +
    geom_line(data = data1, aes(x = time, y = pop),linewidth = 0.8, alpha = 0.8) +
    geom_line(data = data2, aes(x = time, y = pop),linewidth = 0.8, alpha = 0.8) +
    geom_line(data = data3, aes(x = time, y = pop),linewidth = 0.8, alpha = 0.8, color = "red")

