library(plotly)
library(ggnet)
library(network)
library(sna)
library(ggplot2)

taille_pop <- c(10, 25, 40, 15, 5, 30, 50, 22)

#catégories
intervalles <- c(0, 10, 25, 50)
labels_categories <- c("small", "medium", "great")

categories <- cut(taille_pop, breaks = intervalles, labels = labels_categories, include.lowest = TRUE)

print(categories)

# random graph
net = rgraph(length(taille_pop), mode = "graph", tprob = 0.5)
net = network(net, directed = FALSE)

net %v% "effectif" = as.vector(categories)

p <- ggnet2(net, size = "effectif", size.palette = c("small" = 5, "medium" = 15, "great" = 50))

ggplotly(p)



