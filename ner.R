# R Implementation of implicit network extraction
# (c) 2018 Katja Hauser, Andreas Spitz, Michael Gertz
# For details, see https://dbs.ifi.uni-heidelberg.de/resources/load/

# Purpose of this file:
# In this file we implement the NER functionality for the pipeline using
# existing NER libraries for R.

library(tibble)
library(NLP)
library(openNLP)
library(dplyr)
library(stringr)
library(SnowballC)
library(readr)

# copied from https://rpubs.com/lmullen/nlp-chapter
# Returns the annotations from an AnnotatedPlainTextDocument of the given
# type. Convenience function.
entities <- function(doc, kind) {
  s <- doc$content
  a <- annotations(doc)[[1]]
  if(hasArg(kind)) {
    k <- sapply(a$features, `[[`, "kind")
    s[a[k == kind]]
  } else {
    s[a[a$type == "entity"]]
  }
}

# extract the named entities (location, organization, actor, date)
extract_named_entities <- function(document){
  # essentially following the implementation described in
  # https://rpubs.com/lmullen/nlp-chapter

  # Use openNLP to obtain lists of locations, organizations, actors and dates
  # in the document.
  # The call of NLP::annotate is necessary, as ggplot2 masks annotate.
  document <- as.String(document)
  annotations <- AnnotatedPlainTextDocument(document, NLP::annotate(document,
                                                                    list(
    Maxent_Sent_Token_Annotator(),
    Maxent_Word_Token_Annotator(),
    Maxent_Entity_Annotator(kind = "location"),
    Maxent_Entity_Annotator(kind = "organization"),
    Maxent_Entity_Annotator(kind = "person"),
    Maxent_Entity_Annotator(kind = "date"))))

  # garbage collection to ensure there is enough memory (which may occur on
  # smaller computers)
  gc(verbose = FALSE)

  # extract named entities
  named_entities <- tibble(tok = character(0), type = character(0))
  if (length(entities(annotations, "location") > 0)){
    named_entities <- bind_rows(named_entities, lapply(
      tolower(unique(entities(annotations, "location"))),
      function(word) tibble(tok = word, type = "LOC")))
  }
  if (length(entities(annotations, "organization") > 0)){
    named_entities <- bind_rows(named_entities, lapply(
      tolower(unique(entities(annotations, "organization"))),
      function(word) tibble(tok = word, type = "ORG")))
  }
  if (length(entities(annotations, "person") > 0)){
    named_entities <- bind_rows(named_entities, lapply(
      tolower(unique(entities(annotations, "person"))),
      function(word) tibble(tok = word, type = "ACT")))
  }
  if (length(entities(annotations, "date") > 0)){
    named_entities <- bind_rows(named_entities, lapply(
      tolower(unique(entities(annotations, "date"))),
      function(word) tibble(tok = word, type = "DAT")))
  }
  if(ner_get_terms == TRUE){
    NE_and_terms <- left_join(tibble(tok = tolower(words(annotations))),
                              named_entities, by = "tok")
    NE_and_terms[is.na(NE_and_terms["type"]), "type"] <- "TER"
  }else{
    NE_and_terms <- named_entities
  }
  NE_and_terms
}

# This is a simple implementation for preprocessing documents. i.e. splitting
# them into words, providing them with an id indicating the sentence they
# originated from (starting from 1 in each document), as well as the id of the
# document.
# The document is split into sentences at every occurrence of ". ", which may
# erroneously include cases like "Mr. Smith", and then into words at every
# space, which leads to ignoring named entities for which the token contains a
# space, e.g. "Saint Petersburg".
# A number of symbols are removed in the process (see ner_remove in config
# file), some characters, e.g. \n, are replaced by a space instead - see
# ner_substitute in config file.
# All words are converted to lower case and stop words are removed.
# Input: tibble (id, document)
# Output: tibble (doc_id, sen_id, word)
preprocess_documents <- function(doc){
  stop_words <- stopwords::stopwords()
  sentences <- unlist(strsplit(doc[["document"]], ". ", fixed = TRUE))
  split_documents <- tibble(sen_id = c(1:length(sentences)), sen =
                              as.vector(sentences))

  # returns a list containing a tibble per sentence of format sen_id, word
  split_documents["sen"] <- apply(split_documents["sen"], 1, function(sen)
    gsub(ner_substitute, " ", sen))
  sen_id_and_words <- bind_rows(apply(split_documents, 1, function(entry)
    tibble(sen_id = rep(entry["sen_id"], str_count(entry["sen"], " ")[1] +1),
           word = tolower(unlist(
             strsplit(gsub(ner_remove, "", entry["sen"])
                      , " "))))))

  # remove stop words
  sen_id_and_words <- anti_join(sen_id_and_words, tibble(word=stop_words),
                                by = "word")
  result <- tibble(doc_id = rep(doc["id"], nrow(sen_id_and_words)),
                   sen_id = sen_id_and_words$sen_id,
                   word = sen_id_and_words$word)
}

# NER using existing functionality in R.
# Returns tibble (doc_id, sen_id, tok, uid, type) for one document.
simple_NER <- function(documents){
  # stem documents
  if (nrow(documents) == 1){
    documents["document"] <- paste(
      wordStem(unlist(str_split(documents[1,"document"], " ")),
               language = "english"),
      collapse = " ")
  } else{
    documents["document"] <- unlist(
      lapply(
        apply(documents, 1, function(doc)
          wordStem(unlist(str_split(doc["document"], " ")),
                   language = "english")),
        function(entry) paste(entry, collapse = " ")))
  }

  # extract named entities
  named_entities <- unique(bind_rows(
    lapply(documents["document"], function(doc) extract_named_entities(doc))))
  named_entities <- add_column(named_entities,
                               uid = paste("uid", 1:nrow(named_entities),
                                           sep = "-"), .after = "tok")

  # split documents into words, each containing a document id and a sentence
  # id. Remove stop words.
  split_documents <- bind_rows(apply(documents, 1, function(entry)
    preprocess_documents(entry)))

  # join named entities with information on documents and sentences
  result <- rename(left_join(split_documents, named_entities,
                             by = c("word" = "tok")), tok = word)
  result <- result[result["tok"] != "", ]

  # removes artifacts from removing full stops/periods from text (12.04. ->
  # 1204 (has no match))
  result <- result[!is.na(result["uid"]), ]
}


# Wrapper for actual functionality.
# Input: tibble(id, document
# Output: tibble(document id, sentence id, token, uid of token, type of token)
named_entity_recognition <- function(documents, ner_function = simple_NER){
  result <- ner_function(documents)

  write_delim(result, "./preprocessed_documents.txt", delim = "\t")
}
