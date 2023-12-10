

library(tidyverse)
library(ggthemes)



simulate_n_metapop <- function(parametre = array(c(c(-0.2, 2.1), c(1000, 800), c(30, 25)), dim = c(2, 3))) 
  { 
  # faudra rendre de nouveau les parametres changeable mais quand ce sera pour n pop
  
  # CONDITIONS DE SIMULATION
  temps = 30 # nb de pas de temps (en annees)
  
  nb_pop = nrow(parametre)
  
  r = parametre[1:nb_pop, 1] 
  K = parametre[1:nb_pop, 2] 
  N0 = parametre[1:nb_pop, 3] 
  
  # Introducing some stochasticity in the dynamic
  sigma_p = 0.1 # the variability associated with the reproduction process
  
  # INITIALISATION
  N <- array(0, dim = c(temps, nb_pop))
  Nm <- array(0, dim = c(temps, nb_pop))
  
  em = array(0, dim = c(temps, nb_pop))
  
  for (i in 1:nb_pop){
    # conditions initiales 
    Nm[1,i] = N0[i]
    N[1,i] = rlnorm(1, log(Nm[1]), sigma_p)
  }
  
  # boucle du temps           
  for (t in 1:(temps - 1)) {
    
    for (i in 1:nb_pop){
      
      # REPRODUCTION
      Nm[t + 1,i] = N[t,i] + r[i] * N[t,i] * (1 - N[t,i] / K[i])  # reproduction
      Nm[t + 1,i] = max(Nm[t + 1,i]  , 0.001 * K[i]) # security line
      
      # STOCHASTICITY
      N[t + 1,i] = rlnorm(1, log(Nm[t + 1,i]), sigma_p) 
      
      # EMIGRATION
      em[t,i] =  max( (Nm[t + 1,i] -  K[i])  , 0)  # calcul du nombre d'emigrant
      Nm[t + 1,i] = Nm[t + 1,i]  -  em[t,i]   # emigration
    }
    
    for (i in 1:nb_pop){
      
      # IMMIGRATION
      immi = (1/(nb_pop-1))* (sum(em[t,1:nb_pop]) - em[t,i]) #chaque pop se partage equitablement les emigrants
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

sim = simulate_n_metapop()

sim$Effectif$N[,1] #effectif pop 1


data = tibble(time = 1:30,
               pop1 = sim$Effectif$N[,1],
               pop2 = sim$Effectif$N[,2],
               emigration = sim$emigration$em[,2]) %>% 
  pivot_longer(-time, values_to = "effectif", names_to = "pop")


ggplot(data, aes(x = time, y = effectif, col = pop)) +
    geom_line(linewidth = 0.8, alpha = 0.8)


# essai pour quatre pop

simulate_n_metapop(parametre = array(c( c(-0.2, 2.1, 1.2, -0.8), c(1000, 800, 250, 300), c(30, 25, 15, 20)), dim = c(4, 3)) )
