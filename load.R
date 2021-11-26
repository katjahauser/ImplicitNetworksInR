# R Implementation of implicit network extraction
# (c) 2018 Katja Hauser, Andreas Spitz, Michael Gertz
# For details, see https://dbs.ifi.uni-heidelberg.de/resources/load/

# Purpose of this file:
# The functionality in this file is used to construct a LOAD network following
# to the algorithm described in the original paper "Terms over LOAD: Leveraging
# Named Entities for Cross-Document Extraction and Summarization of Events",
# Spitz and Gertz, SIGIR 2016.

library(dplyr)

# initialize global variables
df <- data.frame()
Sd <- data.frame()
Wd <- data.frame()


# Create a new edge for two nodes and a given weight (i.e., a tibble with only
# one row).
# Handling of the edges: The edges, i.e. the tibbles, are created within nested
# apply() functions. The results of each apply() are merged into a larger
# tibble after finishing. In the end, the weights of all edges between the same
# nodes are summed up.
update_E <- function(node1, node2, weight){
  # make sure, that the edges are ordered such, that the lexicallicly smaller
  # entry is the first node. This simply makes the grouping of weights easier
  # in later steps.
  if (node1 < node2){
    new_row <- tibble(n1=node1, n2=node2, weight=weight)
  }else{
    new_row <- tibble(n1=node2, n2=node1, weight=weight)
  }
  return(new_row)
}


# Create a new entry (a tibble with one row) to represent a vertex.
# Handling: The vertices are propagated through the program analogously to the
# edges (compare update_E for a description of the basic mechanism.)
update_V <- function(l, t){
    return(tibble(label = l, type = t))
}

# For each sentence in a document create the edges between sence and document
# and process the entities and coocurrences it contains. (The notation is
# taken from the algorithm described in the LOAD paper.)
# We use local_id to navigate within the data frame and global_id to identify
# the sentence globally.
for_each_s_in_d <- function(sen, doc){
  # all edges created in this function are stored in the variable 'edges'
  edges <- tibble(n1 = character(0), n2 = character(0), weight=double(0))

  # create the edges that connect document and the sentences contained within
  edges <- rbind(edges, update_E(paste0("d", as.character(doc)),
           paste0("s", as.character(Sd[Sd$local_id ==
                                         suppressWarnings(as.numeric(sen[1])),
                           "global_id"])),
           1))

  # get NEs in sentence
  Ns <- Wd[Wd$sen == suppressWarnings(as.numeric(sen[1]))
           & Wd$type %in% NE_types, ]
  # create edges linking NEs and sentences
  if(nrow(Ns) > 0){
    edges <- rbind(edges, bind_rows(apply(Ns, 1, function(token)
                       update_E(paste0("s",as.character(sen["global_id"])),
                                          as.character(token["tok"]), 1))))
  }

  # get terms in senctence
  Ts <- Wd[Wd$sen==suppressWarnings(as.numeric(sen[1]))
           & Wd$type %in% term_types,]
  # create edges linking terms and sentences
  if(nrow(Ts) > 0){
    edges <- rbind(edges, bind_rows(apply(Ts, 1, function(token)
      update_E(as.character(token["tok"]),
               paste0("s",  as.character(sen["global_id"])), 1))))
  }

  # create edges linking terms and NEs
  if(nrow(Ns) > 0 && nrow(Ts) > 0 && weighted_distance_term_NE == FALSE){
    edges <- rbind(edges, bind_rows(unlist(apply(Ts, 1, function(term)
      apply(Ns, 1, function(entity) update_E(as.character(entity["tok"]),
                                              as.character(term["tok"]), 1))),
      recursive=FALSE)))
  }
  return(edges)
}


# given two words calculate their weighted distance from their sentence-wise
# distance
decide_edge_distance <- function(word1, word2){
  weight <- exp(-abs(as.numeric(word2["sen"]) - as.numeric(word1["sen"])))
  return(update_E(as.character(word1["tok"]), as.character(word2["tok"]),
                  weight))
}


# This function handles the creation of edges between NEs and terms, i.e. it
# makes sure that no self-edges or unwanted edges between NEs and terms are
# created.
decide_edge <- function(word1, word2){
  # last word (word1) in a data frame does not provide valid word2
  if (!is.na(word2["sen"])){
    # disallow self-edges
    if(word1["tok"] != word2["tok"]){
      # edges between named entities
      if(word1["type"] %in% NE_types && word2["type"] %in% NE_types){
        if(!(calculate_edges_for_same_type == FALSE
             && word1["type"] == word2["type"])){
          return(decide_edge_distance(word1, word2))
        }
        # edges between named entities and terms
      } else if (weighted_distance_term_NE == TRUE &&
                 ((word1["type"] %in% NE_types &&
                   word2["type"] %in% term_types) ||
                  (word2["type"] %in% NE_types &&
                   word1["type"] %in% term_types))){
        return(decide_edge_distance(word1, word2))
      }
    }
  }
}


# Given an instance, find all possibly coocurring instances (given the cutoff
# parameter) and calculate their weight (i.e. hand them to the function
# decide_edge which decides whether or not to add an edge. For details see
# the description of 'decide_edges'.)
find_coocurrences <- function(word1){
  Wd <<- Wd[-c(1),]
  coocurrences <- Wd[abs(suppressWarnings(as.numeric(Wd[, "sen"]))
                     - suppressWarnings(as.numeric(word1["sen"]))) <= cutoff, ]

  return(bind_rows(apply(coocurrences, 1,
                         function(word2) decide_edge(word1, word2))))
}


# This function is applied to each document. It extracts the sentences, starts
# the calculations done per sentence (the creation of edges, essentially) and
# collects the results.
for_each_document <- function(d){
  # get all sentences in the document.
  S <- as.data.frame(unique(df[df["doc"] == d, "sen"]))
  # set local and gobal id of a sentence. global_id is created from the id of
  # the document that contains the sentence and the number of the sentence in
  # the document (e.g. 2-4 for the 4th sentence in the 2-nd document), local_id
  # is used to calculate the coocurrences and is the position of the sentence
  # within the document (e.g., 2 for the 2nd sentence).
  Sd <<- cbind.data.frame(S,
                          lapply(S, function(entry) paste(d, entry, sep="-")))
  Sd <<- setNames(Sd, c("local_id", "global_id"))
  setNames(Sd, c("local_id", "global_id"))

  # The usage of Wd (words of a document) differs from the pseudocode in the
  # LOAD paper. (We add terms as well as named entities to the list of vertices
  # and distinguish between terms and NE in for_each_s_in_d, when deciding what
  # edges to create.
  Wd <<- df[df$doc == d,]

  # ensure the dataframe is ordered by sentence id, so we can stop calculating
  # the coocurrences, as soon as one token with a distance larger than the
  # cut-off parameter turns up
  Wd <<- Wd[with(Wd, order(sen)), ]

  # create vertices for the document, the sentences, and instances within it.
  vertices <- tibble(label = character(0), type = character(0))
  vertices <- rbind(vertices, update_V(paste0("d", d), "DOC"))
  vertices <- rbind(vertices, bind_rows(apply(Sd, 1, function(sen)
    update_V(paste0("s", as.character(sen["global_id"])), "SEN"))))
  vertices <- rbind(vertices, bind_rows(apply(Wd, 1, function(entry)
    update_V(entry["tok"], entry["type"]))))

  # create edges for the document.
  edges <- tibble(n1 = character(0), n2 = character(0), weight=double(0))
  edges <- rbind(edges, bind_rows(apply(Sd, 1,
                                         function(s) for_each_s_in_d(s, d))))
  edges <- rbind(edges, bind_rows(apply(Wd, 1, function(w)
    find_coocurrences(w))))

 return(list("vertices" = vertices, "edges" = edges))
}


# This function starts all calculations to create the load network.
# In the first part, the necessary data structures are set up, in the second
# part the resulting edges are postprocessed (i.e. edges between the same nodes
# are merged). The algorithm is based on the algorithm described in Andreas
# Spitz' LOAD paper.
calculate_load_network <- function(path_to_data =
                                     "./preprocessed_documents.txt"){
  df <<- read.table(path_to_data, header = TRUE,
                    col.names = c("doc", "sen", "tok", "uid", "type"),
                    colClasses = "character")

  # use a uid instead of a name to identify an entity, if TRUE.
  if(use_uid == TRUE)
  {
   df <<- select(df, doc, sen, tok = uid, type)
  }

  # create data structures needed for the computation
  number_of_entities <- nrow(df) + length(unique(df$doc)) +
    nrow(unique(cbind(df$doc, df$sen)))
  V <- tibble(label = character(0), type = character(0))
  E <- tibble(n1 = character(0), n2 = character(0), weight=double(0))
  Sd <<- setNames(data.frame(matrix(ncol = 2, nrow = 0)),
                  c("local_id", "global_id"))
  Wd <<- setNames(data.frame(matrix(ncol = 4, nrow = 0)),
                  c("doc", "sen", "tok", "type"))

  # -----------------------------------
  # start the calculation of the network and process the results
  # -----------------------------------

  # The variable edges_and_vertices contains for each document (sic) a tibble
  # with the corresponding vertices and another tibble with the corresponding
  # edges. i.e. 2d entries for d documents. The odd entries are the tibbles
  # containing the vertices, the even ones are the tibbles containing the
  # edges.
  edges_and_vertices <- unlist(
    apply(unique(df["doc"]), 1, function(d) for_each_document(d)),
    recursive = FALSE)

  # assign the odd entries of edges_and_vertices (the vertices for each
  # document) to V, and the even entries (the edges for each document) to E.
  V <- unique(rbind(V, bind_rows(edges_and_vertices[
    unlist(lapply(1:length(edges_and_vertices), function(x) x %%2 !=0))])))
  E <- rbind(E, bind_rows(edges_and_vertices[
    unlist(lapply(1:length(edges_and_vertices), function(x) x %%2 ==0))]))

  # order the vertices by label
  V <- V[order(as.vector(V$label)), ]
  # order the edges by label
  E <- E[order(as.vector(E$n1), as.vector(E$n2)), ]
  # sum up the edge weights for edges linking the same two nodes. We apply a
  # lexical ordering on the nodes when creating them, so we don't have to deal
  # with cases like Alice--Bob and Bob--Alice as the latter is never created in
  # the algorithm.
  E <- summarize(group_by(E, n1, n2), weight = sum(weight))
  # multiple occurrences of the same entity in one sentence are not counted
  E[grep("s[0-9]*-[0-9]*", E$n1), "weight"] <- 1
  E[grep("s[0-9]*-[0-9]*", E$n2), "weight"] <- 1

  # write data to files.
  write.table(V, "V.txt", sep = "\t", row.names = FALSE, col.names = TRUE)
  write.table(E, "E.txt", sep = "\t", row.names = FALSE, col.names = TRUE)

  # create simple gazetteer
  # V <- V[V$type %in% NE_types,]
  # V <- add_column(V, uid = paste0("uid-", 1:nrow(V)))
  # write.table(V, "80_Days_Gazetteer.txt", sep = "\t", row.names = FALSE, col.names = TRUE)
}
