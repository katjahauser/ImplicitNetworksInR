# R Implementation of implicit network extraction
# (c) 2018 Katja Hauser, Andreas Spitz, Michael Gertz
# For details, see https://dbs.ifi.uni-heidelberg.de/resources/load/

# Purpose of this file:
# This file contains functionality to analyse and visualise the network, as
# well as providing the conversion into an igraph object.

library(igraph)
library(GGally)
library(sna)
library(intergraph)
library(network)
lapply(c("sna", "intergraph", "igraph", "network"), require, character.only=T)

# A very simple visualisation using igraph. Not recommended for medium or large
# networks (too many overlaps of nodes).
simple_visualisation <- function(V, E){
  g <- graph_from_data_frame(d = E, vertices = NULL, directed = FALSE)
  plot(g)
}

# A visualisation using the color scheme used in the original LOAD paper. Not
# recommended for large networks (too much overlap of nodes).
# Visualises a network with the types: document, sentence, term, location,
# actor, organisation, date - adapt the palette, if other/fewer types are used.
load_paper_style_visualisation <- function(vert, edges){

  set.seed(2)

  g <- network(edges[,1:2], vertex.attr = vert,
               vertex.attrnames = c("label", "type"))

  # Plot the data
  ggnet2(g, mode = "fruchtermanreingold", color = "type", edge.color = "grey85", label = FALSE,
         palette = c("ACT" = "purple", "DAT" = "green", "DOC" = "black",
                     "LOC" = "red", "ORG" = "blue", "SEN" = "grey50",
                     "TER" = "grey 75"),
         size = 2, edge.alpha = 0.5)
}

# Convert the nodes into an igraph object and return it
convert_to_igraph <- function(V, E){
  g <- graph_from_data_frame(d = E, vertices = V, directed = FALSE)
  entity_types <- c("DOC", "SEN", "LOC", "ACT", "ORG", "DAT", "TER")
  node_colors <- c("white", "grey97", "red", "purple", "blue", "green", "grey")
  names(node_colors) <- entity_types
  return(g)
}

# compute PageRank of the graph and return the first few nodes (using head())
# Notice, that the Pagerank implementation of R makes use of a weight attribute
# for the edges, if one exists.
best_nodes_by_pagerank <- function (V, E) {
  g <- convert_to_igraph(V,E)
  res <- page_rank(g, directed = FALSE)$vector
  head(sort(res, decreasing = TRUE))

}


# Load the files containing the information on edges and nodes, choose the
# function used to visualize or process the data.
visualise_or_analyse_network <- function(node_file = "./V.txt", edge_file = "./E.txt",
                              visualisation_or_analysis_function =
                                load_paper_style_visualisation){

  # load data
  V <- read.table(node_file, header = TRUE, sep = "\t", colClasses = "character")
  E <- read.table(edge_file, header = TRUE, sep = "\t", colClasses = "character")

  #remove duplicates
  V <- V[!duplicated(V$label),]

  # visualize network
  visualisation_or_analysis_function(V, E)

}
