# R Implementation of implicit network extraction
# (c) 2018 Katja Hauser, Andreas Spitz, Michael Gertz
# For details, see https://dbs.ifi.uni-heidelberg.de/resources/load/

# Purpose of this file:
# You can configure the details of the pipeline here. The file is structured in
# the order of the execution of the pipeline.
# Note: To run the pipeline, see file: start_pipeline.R

# -----------------------------------------------------------------------------
# load_documents.R
#
# step 1
#
# read in the test files
# -----------------------------------------------------------------------------

# The Big Picture: There are two mutually exclusive ways to extract the data
# on which the LOAD-network will be constructed. The first uses existing R
# libraries to extract named entities (ner.R) from given text files, the
# second uses a gazetteer for the same task (gazetteer.R).
# To use the first variant you MUST NOT provide a gazetteer, to use the second
# you MUST.

# Important: Be aware, that the order in which the documents are displayed in
# your GUI might differ from the order in which the documents are read in R: A
# GUI might display chapter_1.txt before chapter_10.txt, but they are read
# into R (and displayed with ls on the console, by the way) the other way
# round. (It's solved easiest by adding a leading 0 to the enumeration, i.e.
# chapter_1.txt -> chapter_01.txt.) I'm emphasizing this, because this issue
# makes the results in later steps look buggy, even though they aren't.

# -------------------------------------

# This variable sets the path from which the data will be read. You can provide
# either a path to a single file or to a directory.
# IMPORTANT: To read a file, load_doc_type must be set to NULL, otherwise provide
# the type of file to be read.
# e.g. "./Testdocuments/Around_the_World_in_80_Days_Chapters/"
#load_doc_path <- "./Testdocuments/Around_the_World_in_80_Days_Chapters/"
 load_doc_path <- "./Testdocuments/Around_the_World_in_80_Days_Chapters/Around_the_World_in_80_Days_Chapter_01.txt"

# NULL (for a single document) or a valid type (to read from a directory)
# e.g: "txt" or NULL
#load_doc_type <- "txt"
 load_doc_type <- NULL

# The path to the gazetteer. The type must be listed in
# valid_gazeteer_input_formats below.
# Expected format of file: token (char) TAB type(char) [TAB uid (char)]
# e.g. NULL or "./Testdocuments/80_Days_Gazetteer_full.txt"
#load_doc_gazetteer <- NULL
 load_doc_gazetteer <- "./Testdocuments/80_Days_Gazetteer_Chapter_01_only_NE.txt"

# ONLY needed when using a gazetteer. Set to TRUE, if a uid (e.g. a
# Wikidata ID) is provided. Otherwise an ID will be created for you.
load_doc_uses_uid <- TRUE

# You can provide the header to your own import function here (set to NULL to
# use the in-built one).
# Signature: own_import_function(path, type, gazetteer, uses_uid)
# Output: a list with two entries: document and gazetteer. document is a tibble
# of format id (int), document(char). If no gazetteer is used, gazetteer is
# NULL, otherwise a tibble of the format token (char), type (char)
# [, uid (char)].
load_doc_own_import_function <- NULL

# -------------------------------------

# Here the formats for which input functions have been implemented are listed.
# DO ONLY CHANGE, if you have included the required functionality.
valid_input_formats <- c("txt")
valid_gazeteer_input_formats <- c("txt")


# -----------------------------------------------------------------------------
# ner.R
#
# step 2 - variant A
#
# perform NER with inbuild R functions
# (mutually exclusive with step 2 variant B)
# -----------------------------------------------------------------------------

# Provide the functionality used to extract the named entities.
# Default: simple_NER (based on openNLP)
# Signature: ne_recognition_function(documents) (where documents is a tibble of
# format id (int), document(char))
# Expected Output: a tibble of format document id (int), sentence id (int),
# token (char), uid (char), type (char) with names: doc_id, sen_id, tok, uid,
# type
source("ner.R")
ne_recognition_function <- simple_NER

# Include terms into the network. Default: TRUE
ner_get_terms <- TRUE

# -------------------------------------

# Characters or combinations of characters that are removed from the documents
# with gsub for some basic preprocessing of the sentences.
ner_remove <- "\\.|,|;|\"|\\(|\\)|\\[|\\]"

# Characters that are substituted by a space instead of being removed.
ner_substitute <- "\n|--"


# -----------------------------------------------------------------------------
# gazetteer.R
#
# step 2 - variant B
#
# perform NER using a gazetteer
# (mutually exclusive with step 2 variant A)
# -----------------------------------------------------------------------------

library(stopwords)

# Set the stopwords that will be removed from the corpus.
# default: stopwords::stopwords()
source("gazetteer.R")
gaz_stop_words <- stopwords::stopwords()

# Set the function used to generate morphological forms from the gazetteer
# (e.g. genitive s: Bob -> Bob's). The same uid is used for all derivations of
# a word.
# default: naive_English_morphology_NE_only. Notice, that this function only
# generates morphological forms for named entities, not for terms, e.g.:
# fish, TER, uid-3
# Bob, ACT, uid-4
# Bob's, ACT, uid-4
gaz_morphology_function <- naive_English_morphology_NE_only

# Preprocessing of the document, i.e. splitting it into sentences and tokens.
# default: naive_preprocessing_documents (splits sentences at "." and tokens at
# " ". See gazetteer.R for details.)
gaz_document_preprocessing_function <- naive_preprocessing_documents

# These symbols are removed from the gazetteer in later steps.
# You MUST NOT remove full stops, exclamation or question marks, as they are
# used to end sentences. (If you remove them, you obtain one long sentence per
# document. The same applies for spaces, but then you get one long word ;) )
gazetteer_remove <- ",|;|\"|\\(|\\)|\\[|\\]"

# You can change the list of named entity types and term types in step 3 after
# the break.


# -----------------------------------------------------------------------------
# load.R
#
# step 3
#
# calculate the LOAD network
# -----------------------------------------------------------------------------

# Determine, whether a weighted distance should be used on edges between terms
# and named entities. (This does increases the computation timeXXX)
# The weight will be set to 1, if no weighted distance is used. (default: TRUE)
weighted_distance_term_NE <- TRUE

# Determine, whether edges between the nodes of the same entity type shall be
# computed. (This was not done in the original LOAD paper, but in the Evelin
# paper.) Edges between terms are never calculated, regardless of this
# variable. (default: TRUE)
calculate_edges_for_same_type <- TRUE

# The cutoff parameter to determine the maximal sentence-wise distance of
# entities to be still considered a co-occurrence. (integer >=0, default: 5)
cutoff <-5

# If you use a uid to identify entities (e.g. a Wikidata ID), you can set this
# parameter to TRUE to use the uid instead of the entity's name to create the
# network. This is relevant for entities that are referred to by more than one
# name, e.g. ("Einstein", uid-1) and ("Albert Einstein", uid-1) or
# morphological forms of the same name (e.g. Alice and Alice's), in case they
# are not handled by the stemmer. These tokens are not merged in the default
# configuration (parameter set to FALSE).
# Notice, that in the first version of this package, it is only possible to
# provide the necessary proper uid when using a gazetteer.
use_uid <- FALSE

# ---------------------------------------

# Types of named entities. The default is location, organisation, actor, date:
# c("LOC", "ORG", "ACT", "DAT")
NE_types <- c("LOC", "ORG", "ACT", "DAT")

# Types of terms. Default: c("TER")
term_types <- c("TER")


# -----------------------------------------------------------------------------
# display_load_network.R
#
# step 4
#
# visualize the network
# -----------------------------------------------------------------------------

# Select the function that is used to visualize or analyse the network.
# Default visualisation: load_paper_style_visualization
# Obtain the network as igraph object: convert_to_igraph
# Signature: visualisation_or_analysis_function(vertices, edges) with data
# frames (vertices and edges) in formats:
# vertices: label (char), type (char)
# edges: n1 (char), n2 (char), weight (double)
source("./display_load_network.R")
#vis_or_analysis_function <- best_nodes_by_pagerank
 vis_or_analysis_function <- load_paper_style_visualisation
