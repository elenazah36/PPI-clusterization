rm(list = ls()) #Clear environment
dev.off()  # Clear plot, only if there IS a plot
cat("\014")  # ctrl+L, clear console

install.packages("igraph") 
install.packages("igraphdata") 
install.packages("network") 
install.packages("sna")
install.packages("ggraph")
install.packages("RColorBrewer")

install.packages("clustAnalytics")

#at the end of working with this R file, detach and free up memory
detach('package:igraph')
detach('package:network')
detach('package:sna')
detach('package:ggraph')
detach('package:visNetwork')
detach('package:ndtv')


#this is for interactive plotting in web 
install.packages('ndtv', dependencies=T)
library('ndtv')

#or with javascript
install.packages('visNetwork')
library('visNetwork') 

library(igraph)
library(plotly)
library(dplyr)
library(RColorBrewer)

library(combinat) 

library(network)

library (clustAnalytics)

load("data_small.RData") #work_mat3
diag(work_mat3) <- 0


setwd("C:/Users/Elena/Desktop/R_tutorials/Gabriela_Chiosis_Lab")
getwd()


load("data_big.RData") #work_mat2
diag(work_mat2) <- 0



# selecting 2 items out of 4  
combn(4,2)


#adjacency matrix
graph2 <- graph_from_adjacency_matrix(work_mat2, mode=
                                      "undirected")

graph3 <- graph_from_adjacency_matrix(work_mat3, mode=
                                        "undirected")


# Community detection using different algorithms
community_spinglass <- spinglass.community(graph2) #notworking
community_edge_betweenness <- edge.betweenness.community(graph2) #not working



community_fastgreedy <- fastgreedy.community(graph2)

community_walktrap <- walktrap.community(graph2)

community_leading_eigenvector <- leading.eigenvector.community(graph2)

community_label_propagation <- label.propagation.community(graph2)


groups <- membership(cluster_louvain(g))
communities <- communities(cluster_louvain(g))
plot.igraph(g, mark.groups = communities)



## Intersecting the communities, finding out common nodes ###

#combinations of 4 choose 2
combinations <- combn(4, 2)

#a list to store the intersections
intersections_list <- list()

# go through each combination
for (i in 1:ncol(combinations)) {
  # row 1 and row 2
  index1 <- combinations[1, i]
  index2 <- combinations[2, i]
  
  #names of the community structures corresponding to the indices
  name1 <- switch(index1,
                  community_fastgreedy,
                  community_leading_eigenvector,
                  community_label_propagation,
                  community_walktrap)
  
  name2 <- switch(index2,
                  community_fastgreedy,
                  community_leading_eigenvector,
                  community_label_propagation,
                  community_walktrap)
  
  #no of communities for each name (algorithm)
  num_communities_name1 <- length(name1)
  num_communities_name2 <- length(name2)
  
  # loop through each community algorithm pair and each particular community
  for (no_name1 in 1:num_communities_name1) {
    for (no_name2 in 1:num_communities_name2) {
      
      #intersection variable name
      intersect_var_name <- paste("intersect", index1, no_name1, "To", index2, no_name2, sep = "")
      
      #store the intersecion result in the list
      intersections_list[[intersect_var_name]] <- intersect(
        name1[[no_name1]],
        name2[[no_name2]]
      )
    }
  }
}





### Small clusters vs Big clusters ###


# Example community structures
community_list <- list(
  community_fastgreedy,
  community_leading_eigenvector,
  community_label_propagation,
  community_walktrap
)

# Initialize counters
fastgreedy_big <- 0
fastgreedy_small <- 0
leading_eigenvector_big <- 0
leading_eigenvector_small <- 0
label_propagation_big <- 0
label_propagation_small <- 0
walktrap_big <- 0
walktrap_small <- 0

# Threshold for considering clusters as big
threshold <- 10

# Count clusters
for (i in seq_along(community_fastgreedy)) {
  if (length(community_fastgreedy[[i]]) >= threshold) {
    fastgreedy_big <- fastgreedy_big + 1
  } else {
    fastgreedy_small <- fastgreedy_small + 1
  }
}

for (i in seq_along(community_leading_eigenvector)) {
  if (length(community_leading_eigenvector[[i]]) >= threshold) {
    leading_eigenvector_big <- leading_eigenvector_big + 1
  } else {
    leading_eigenvector_small <- leading_eigenvector_small + 1
  }
}

for (i in seq_along(community_label_propagation)) {
  if (length(community_label_propagation[[i]]) >= threshold) {
    label_propagation_big <- label_propagation_big + 1
  } else {
    label_propagation_small <- label_propagation_small + 1
  }
}

for (i in seq_along(community_walktrap)) {
  if (length(community_walktrap[[i]]) >= threshold) {
    walktrap_big <- walktrap_big + 1
  } else {
    walktrap_small <- walktrap_small + 1
  }
}


# Bar plot
barplot(
  matrix(
    c(
      fastgreedy_big, fastgreedy_small,
      leading_eigenvector_big, leading_eigenvector_small,
      label_propagation_big, label_propagation_small,
      walktrap_big, walktrap_small
    ),
    nrow = 2,
    byrow = "FALSE"
  ),
  beside = TRUE,
  col = c("blue", "red"),
  main = "Cluster Size Distribution",
  names.arg = c("Fastgreedy", "Leading Eigenvector", "Label Propagation", "Walktrap"),
  ylab = "Number of Clusters",
  legend.text = c("Big Clusters", "Small Clusters"),
  args.legend = list(cex = 0.4, pt.cex = 0.7)  # You can use this line to adjust the size of the legend text
  )




### Density and centrality: closeness, degree, betweeness centrality ###

# Assuming you have a graph object and a list of clusters for each method
graph <- graph2
methods <- c("fastgreedy", "leading_eigenvector", "label_propagation", "walktrap")
community_lists <- list(community_fastgreedy, community_leading_eigenvector, community_label_propagation, community_walktrap)

# Create an empty data frame to store results
results_df <- data.frame(Method = character(), Cluster = integer(), Density = numeric(), Closeness = numeric(), FullyConnected = logical(),  SingleNode = logical(), Degree = numeric(), Betweenness = numeric())

# Iterate through each method
for (i in seq_along(methods)) {
  method <- methods[i]

  clusters <- community_lists[[i]]$membership
  
  # Iterate through each cluster
  for (j in unique(clusters)) {
    # Extract nodes in the current cluster
    cluster_nodes <- which(clusters == j)
    
    # Create a subgraph for the current cluster
    subgraph <- induced_subgraph(graph, cluster_nodes)
    
    #See if the cluster it's fully connected 
    is_fully_connected <- is.connected(subgraph)
    
    #Check if the cluster is single noded
    is_single_node <- length(cluster_nodes) == 1
    
    # Calculate metrics
    density <- graph.density(subgraph)
    closeness <- mean(closeness(subgraph, normalized = TRUE))
    degree <- mean(degree(subgraph))
    betweenness <- mean(betweenness(subgraph))
    
    # Add results to the data frame
    results_df <- bind_rows(results_df, data.frame(Method = method, Cluster = j, Density = density, Closeness = closeness, FullyConnected = is_fully_connected, SingleNode = is_single_node, Degree = degree, Betweenness = betweenness))
  }
}



#HTML-compatible table display/plot
kable(results_df[, c("Method", "Cluster", "Density", "Closeness", "FullyConnected", "SingleNode", "Degree", "Betweenness")])



## Modularity, conductance, and clustering coefficient ##

# Create an empty data frame to store results

graph <- graph2
methods <- c("fastgreedy", "leading_eigenvector", "label_propagation", "walktrap")
community_lists <- list(community_fastgreedy, community_leading_eigenvector, community_label_propagation, community_walktrap)

evaluation_results <- data.frame(Method = character(), Modularity = numeric(), Conductance = numeric(), ClusteringCoefficient = numeric())


# Iterate through each method
for (i in seq_along(methods)) {
  method <- methods[i]
  clusters <- community_lists[[i]]$membership
  
  # Iterate through each cluster
  for (j in unique(clusters)) {
    # Extract nodes in the current cluster
    cluster_nodes <- which(clusters == j)
    
    # Create a subgraph for the current cluster
    subgraph <- induced_subgraph(graph, cluster_nodes)
    
    # Calculate metrics
    modularity <- modularity(graph, clusters)
    conductance <- conductance(graph, clusters)
    clustering_coefficient <- mean(clustering_local(graph, clusters))
    
    # Add results to the data frame
    evaluation_results <- bind_rows(evaluation_results, data.frame(Method = method, Cluster = j, Modularity = modularity, Conductance = conductance, ClusteringCoefficient = clustering_coefficient))
  }
}


## or more simple ##

# fatal error in R, don run it conductness <- conductance(graph2, community_fastgreedy$membership)

modularity_fg <- modularity(graph2, community_fastgreedy$membership)
modularity_lp <- modularity(graph2, community_label_propagation$membership)
modularity_le <- modularity(graph2, community_leading_eigenvector$membership)
modularity_wt <- modularity(graph2, community_walktrap$membership)

modularity_df <- data.frame(
  Method = c("Fastgreedy", "Label Propagation", "Leading Eigenvector", "Walktrap"),
  Modularity = c(modularity_fg, modularity_lp, modularity_le, modularity_wt)
)


community_fastgreedy_g <- fastgreedy.community(graph2)
community_label_propagation_g <- label.propagation.community(graph)
community_leading_eigenvector_g <- leading.eigenvector.community(graph)
community_walktrap_g <- walktrap.community(graph)

transitivity_PPI <- transitivity(graph2, type = "average")

modularity_fg <- modularity(graph2, community_label_propagation_g,type = "average" )
modularity_lp <- modularity(graph2, community_label_propagation_g,type = "average" )
modularity_le <- modularity(graph2, community_leading_eigenvector_g, type = "average")
modularity_wt <- modularity(graph2, community_walktrap_g, type = "average")



### Identifying the hub protein in the entire graph ####


# Set a degree centrality threshold (adjust as needed)
degree_threshold <- 20

# Calculate degree centrality for each node in the graph
degree_centrality <- degree(graph2)

# Identify hub proteins based on degree centrality threshold
hub_proteins <- names(which(degree_centrality >= degree_threshold))

kable(hub_proteins)






### Jaccard Index ###

# Function to calculate Jaccard Index
jaccard_index <- function(set1, set2) {
  intersection_size <- length(intersect(set1, set2))
  union_size <- length(union(set1, set2))
  if (union_size == 0) {
    return(0)  # Return 0 if both sets are empty
  }
  return(intersection_size / union_size)
}

# List to store Jaccard Index values for each cluster pair
jaccard_values <- list()

# Iterate through each cluster pair
for (i in 1:ncol(combinations)) {
  # Indices for the two community structures
  index1 <- combinations[1, i]
  index2 <- combinations[2, i]
  
  #names of the community structures corresponding to the indices
  name1 <- switch(index1,
                  community_fastgreedy,
                  community_leading_eigenvector,
                  community_label_propagation,
                  community_walktrap)
  
  name2 <- switch(index2,
                  community_fastgreedy,
                  community_leading_eigenvector,
                  community_label_propagation,
                  community_walktrap)
  
  #no of communities for each name (algorithm)
  num_communities_name1 <- length(name1)
  num_communities_name2 <- length(name2)
  
  # Iterate through each cluster
  # loop through each community algorithm pair and each particular community
  for (no_name1 in 1:num_communities_name1) {
    for (no_name2 in 1:num_communities_name2)  {
      
    # Jaccard name
    jaccard_var_name <- paste("JaccardIndex", index1, no_name1, "To", index2, no_name2, sep = "")
    
    # Calculate Jaccard Index
    jaccard_values[[jaccard_var_name]] <- jaccard_index(
      name1[[no_name1]], 
      name2[[no_name2]]
      )
    }
  }
  
}



# Best Jaccard values 
for (jaccard_var_name in names(jaccard_values)) {
  jaccard_value <- jaccard_values[[jaccard_var_name]]
  
  # Print only if the value is above 0.5
  if (jaccard_value > 0.5) {
    cat(jaccard_var_name, ":", jaccard_value, "\n")
  }
}

  
