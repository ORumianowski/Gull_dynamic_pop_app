
library(tidyverse)
library(ggthemes)
library('visNetwork') 

# 
# sigma_p : the variability associated with the reproduction process to introduce
# stochasticity in the dynamic
# temps : nb de pas de temps (en annees)
simulate_n_metapop <- function(parametre, sigma_p = 0.1, temps = 30) 
{ 
  # CONDITIONS DE SIMULATION
  nb_pop = length(parametre$r)
  r = parametre[[1]] 
  K = parametre[[2]]
  N0 = parametre[[3]]
  R0 = exp(r)
  M = K / (R0 - 1)

  # INITIALISATION
  N <- array(0, dim = c(temps, nb_pop))
  em = array(0, dim = c(temps, nb_pop))
  N[1,] = N0

  # boucle du temps           
  for (t in 1:(temps - 1)) {
    t = 16
    Nt = N[t,]
    # REPRODUCTION
    N_reprod = reproduction(Nt, R0, M)  # reproduction
    
    # STOCHASTICITY
    N_tplus1 = add_stochasticity(N_reprod, sigma_p)
    
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

reproduction = function(Nt, R0, M){
  Nmt = R0 * Nt / (1+Nt/M)
  Nmt = ifelse(Nmt < 0, K, Nmt)
  # REPRODUCTION
  # nm1 = Nt + r * Nt * (1 - Nt / K)  # reproduction
  # nm2 = pmax(nm1  , 0.001 * K) # security line
  # # Introduce a constraint to prevent N from becoming >> K
  # nm3 = ifelse(Nt > K*(1 + abs(r))/abs(r), K, nm2)
  return(Nmt)
}

add_stochasticity = function(Nm_tplus1, sigma_p){
  return(rlnorm(length(Nm_tplus1), log(Nm_tplus1), sigma_p))
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

plot_connected_pop = function(parametre, show_K=FALSE){
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
  
  if (show_K){ plot = plot +
      geom_hline(yintercept = K, 
                 linetype = "dashed", color = "black")
      }
  return(plot)
}

# plot_connected_pop(parametre, show_K=TRUE)
# # # # # 
# parametre = list(r = c(2, -0.9), K=c(281, 37), N0=c(10, 10))
# # # # 
# sim2 = simulate_n_metapop(parametre)
# # # 
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



res = simulate_n_metapop(parametre = list(r = c(1.2, 1.1, 3, 1,1),
                                          K = c(100, 12, 5, 10,54),
                                          N0 = c(15, 25, 5, 54,45)))

# pbm avec res

date = 5


emigrant_flow = function(res, date) {
  
  nb_pop = (ncol(res) - 1) / 2
  
  # recuperation des tailles de populations
  pop_size = res[date, 2:(nb_pop+1)]
  
  # calculs des flux
  res = res[date, (nb_pop + 2):(2 * nb_pop + 1)]
  id = 1:nb_pop
  tab.em  = data.frame(from = NA, to = NA, width = NA)
  
  for (n in 1:nb_pop) {
    
    em_i = res[, n] / (nb_pop - 1)
    
    tab.em_i  = data.frame(from = rep(n, (nb_pop - 1)),
                       to = id[id != n],
                       width = rep(em_i, (nb_pop - 1)))
    
    tab.em = rbind(tab.em, tab.em_i)
    
  }
  
  flow = tab.em[2:nrow(tab.em), ]
  
  return(list(flow, pop_size))
}

try_em = emigrant_flow(res, date)

# le res est anormal, je mets des valeurs possibles
try_em[[1]]$width = 1:(5*4)
try_em[[2]] = c(10,5,2,7,1)



plot_network = function(tab){
  
  pop_size = tab[[2]]
  nb_pop = length(pop_size)
  
  pop_size = 0.1 * pop_size / mean(pop_size) # necessaire pour une bonne echelle de taille
  
  edges = tab[[1]]
  
  edges$width = 2 * edges$width / mean(edges$width)
  
  nodes <- data.frame(id = 1:nb_pop,
                      value = 1:nb_pop,
                      label = paste("", 1:nb_pop))
  
  visNetwork(nodes, edges) %>% 
    visEdges(arrows = 'from', scaling = list(min = 0.5, max = 1.5))%>%
    visNodes(scaling = list(min = 8, max = 20), 
             color = list(background = "blue", 
                          border = "lightblue",
                          highlight = "purple"))%>%
    visLayout(randomSeed = 12)
}


plot_network(tab = try_em)





