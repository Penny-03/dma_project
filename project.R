#carica i dati
getwd()
setwd('C:\\Users\\eliro\\Documents\\CHIARA\\UNIVERSITA\\DATA MANAGMENT UNIT 2')
jury_result=read.csv('Eurovision\\Final Results\\Jury\\2021_jury_results.csv', encoding="latin1", check.names = FALSE)


## add missing rows
missing=c(setdiff(names(jury_result[5:ncol(jury_result)]), jury_result$Contestant))


#aggiunge le nuove righe
num_col= ncol(jury_result)-1
nuove_righe <- data.frame(colonna1 = missing, matrix(rep(0, length(missing)*num_col), ncol = num_col))
colnames(nuove_righe)=names(jury_result)

# Unisci le nuove righe al dataframe esistente
jury_result <- rbind(jury_result, nuove_righe)

#sorting
sorted_columns <- sort(names(jury_result)[5:ncol(jury_result)])
jury_result_sort <- jury_result[, c(1:4, match(sorted_columns, names(jury_result)))]
jury_result_sort <- jury_result_sort[order(apply(jury_result_sort, 1, function(row) paste(row, collapse = ""))), ]

#sostituire tutti gli NA con zero
jury_result_sort[is.na(jury_result_sort)]=0

#creiamo la adiacent matrix

rimuovi= names(jury_result_sort[2:4])
ad_matrix= jury_result_sort[, !names(jury_result_sort) %in% rimuovi]
row.names(ad_matrix)= ad_matrix$Contestant 
ad_matrix=ad_matrix[,-1]
matrix=as.matrix(ad_matrix)


######################CREAZIONE DEL NETWORK##############
#crea il network, le frecce vanno da un paese votante ad uno che ?? stato votato
library(igraph)
g= reverse_edges(graph_from_adjacency_matrix(matrix, mode="directed", weighted = TRUE))


#restyling--> un colore per ogni parte d'europa
country_data=read.csv('Eurovision\\country_data.csv', encoding="latin1", check.names = FALSE)
missing_color=c(setdiff(country_data$country, names(ad_matrix)))
country_final=country_data[!(country_data$country %in% missing_color),]
country_sort= country_final[order(country_final$country),]

color= c("Eastern Europe"="#e74c3c","Scandanavia"="#5dade2","Western Europe"="#58b68d","Out of Europe"="#af7ac5")
V(g)$color= sapply(country_sort$region, function(group) color[group])

#cambia il colore delle edge
#edge_color= rainbow(max(E(g)$weight)+1)
color_gradient= colorRampPalette(c("#aed6f1", "#1b4f72"))
edge_color=color_gradient(( max(E(g)$weight))- (min(E(g)$weight)) +1   )
#edge.label= E(g)$weight,

#edge pi?? lunghe in base al punteggio
lungh= layout_with_fr(g, weights = E(g)$weight)

# Definizione delle lunghezze degli edge in base alla distanza (normalizzata)
#E(g)$width <- (1 / data_grav$dist) * 10
plot( g,layout=lungh, edge.color=edge_color[E(g)$weight], edge.arrow.size=0.6, edge.arrow.width=0.5, edge.width=E(g)$width,
      vertex.label.color="black", vertex.label.font=2,vertex.frame.color=NA)



#heat map
#heatmap( netm[,],Rowv = NA, Colv = NA, col=color_gradient(39), scale="none", margins=C(10,10))



################################CLUSTERING#################################

#clusters_label_prop <- cluster_label_prop(g, weights=E(g)$weight)
#membership <- membership(clusters_label_prop)
g_undirect=undirected_graph_each <- as.undirected(g, mode = "each")
clustering=cluster_louvain(g_undirect, weights=E(g)$weight)
member= membership(clustering)

# Assegna colori distinti ai cluster
#cluster_colors <- rainbow(length(unique(membership)))

# Crea una lista di gruppi di nodi basati sui cluster
groups <- split(V(g), member)


# Visualizzazione del grafo con colori originali e contorni dei cluster
plot(g, layout=lungh, edge.color=edge_color[E(g)$weight], edge.arrow.size=0.6, edge.arrow.width=0.5, edge.width=2,
     mark.groups=groups, mark.border=NA,
     main="Clustering with label propagation",
     vertex.label.color="black", vertex.label.font=2,vertex.frame.color=NA)

#, mark.col=adjustcolor(cluster_colors, alpha.f = 0.3)


################# GRAVITY MODEL ##################################

library(dplyr)
library(gravity)
library(reshape2)
library(ggplot2)

codes=read.csv('country_code.csv', encoding="latin1", check.names = FALSE)

distances=read.csv('distanze.csv',sep=";", encoding="latin1", check.names = FALSE)

#pulizia del dataset
dist_filtered <- distances %>%
  filter(distances$orig %in% codes$codes)

dist_filtered <- dist_filtered %>%
  filter(dist_filtered$dest %in% codes$codes)

dist_filtered <- dist_filtered %>%
  mutate(dist = ifelse(orig == dest, 0, dist))

# Funzione per sostituire i nomi in una colonna
replace_names <- function(column, codes) {
  for (i in 1:nrow(codes)) {
    old_name <- codes$codes[i]
    new_name <- codes$country[i]
    column <- ifelse(column == old_name, new_name, column)
  }
  return(column)
}

# Sostituisci i nomi nelle colonne `origin` e `destination`
dist_filtered$orig <- replace_names(dist_filtered$ori, codes)
dist_filtered$dest <- replace_names(dist_filtered$dest, codes)

votes <- melt(matrix, varnames = c("destination", "origin"), value.name = "vote")
data_grav <- merge(votes, dist_filtered, by.x = c("origin", "destination"), by.y = c("orig", "dest"))


#data_grav$vote[data_grav$vote == 0] <- 1e-10
#data_grav$dist[data_grav$dist == 0] <- 1
#data_grav$log_vote <- log(data_grav$vote)
data_grav$dist <- as.numeric(gsub(",", ".", data_grav$dist))
#data_grav$log_distance <- log(data_grav$dist)

data_grav$origin_id<- as.numeric(factor(data_grav$origin))
data_grav$destination_id <- as.numeric(factor(data_grav$destination))

# Costruisci il modello di gravit??
#model <- lm(log_vote ~ log_distance, data = data_grav)
model <- lm(vote ~ dist + origin_id + destination_id, data = data_grav)
plot(model)

model <- lm(vote ~ dist + origin_id + destination_id, data = data_grav)
plot(model)
# Vismodel.matrix.default()# Visualizza il riassunto del modello
summary(model)

#scriver perch?? non ho usato il modello log(faceva pi?? schifooo)

ggplot(data_grav, aes(x = `vote`, y = `dist`))


# Carica il pacchetto gravity



##cosa fare?
#- network di paesi che si votano
#- correlazione tra voti giuria e televoto/ vincita
#correlazione tra poll e televoto
#genere di canzoni che vincono( bar plot)
#percentuali di apprezzamneto


#gravity model
#gephi
#circle visualizxation




