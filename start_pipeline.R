# R Implementation of implicit network extraction
# (c) 2018 Katja Hauser, Andreas Spitz, Michael Gertz
# For details, see https://dbs.ifi.uni-heidelberg.de/resources/load/

# Purpose of this file:
# This script assembles all parts of the pipeline and runs them.
# Run it by calling: source("start_pipeline")

# Package dependencies:
#
# This script depends on a number of R packages which are loaded in the individual subscripts.
# Before you run the code for the first time, these have to be installed by uncommenting the
# lines below:
#
#install.packages("NLP")
#install.packages("openNLP")
#install.packages("SnowballC")
#install.packages("stopwords")
#install.packages("igraph")
#install.packages("Ggally")
#install.packages("sna")
#install.packages("intergraph")
#install.packages("readtext")

source("./config.R")
source("./load_documents.R")
source("./ner.R")
source("./gazetteer.R")
source("./load.R")
source("./display_load_network.R")

# load and annotate documents, either by automated entity recognition, or based on a gazetteer
print("Loading documents.")
documents_and_gazetteer <- load_documents(
  load_doc_path,
  load_doc_type,
  load_doc_gazetteer,
  load_doc_uses_uid,
  load_doc_own_import_function
)

if (is.null(documents_and_gazetteer[["gazetteer"]])) {
    print("Extracting named entities with inbuilt R functions.")
    named_entity_recognition(documents_and_gazetteer[["document"]],ne_recognition_function)
} else{
    print("Extracting named entities with a gazetteer.")
    label_using_gazetteer(
      documents_and_gazetteer[["document"]],
      documents_and_gazetteer[["gazetteer"]],
      gaz_stop_words,
      gaz_morphology_function,
      gaz_document_preprocessing_function
    )
}

# create the implicit network from the input data
print("Calculating network.")
calculate_load_network()

# Visualize the network data as a graph
print("Visualizing network.")
graphplot <- visualise_or_analyse_network(visualisation_or_analysis_function = vis_or_analysis_function)
show(graphplot)
