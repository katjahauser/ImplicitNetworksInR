# R Implementation of implicit network extraction
# (c) 2018 Katja Hauser, Andreas Spitz, Michael Gertz
# For details, see https://dbs.ifi.uni-heidelberg.de/resources/load/

# Purpose of this file:
# We implement the means to detect named entities using a gazetteer in this file.

library(dplyr)
library(tibble)
library(stringr)


# Naive morphology function for the English language, that creates forms for
# genitive singular, nominative plural and genitive plural ('s, s, and s'
# or - if ending on a sibilant "'", "es", and "es'" respectively) to
# all NE-types (i.e., not to terms!). This does not make sense in all cases
# (Googles') and could produce grammatically incorrect forms for names that
# don't follow either of the two ways to construct the three additional forms
# described above. We leave it at that, though, as it covers the most common
# cases and more sophisticated morphology functions can be implemented by the
# user.
# The output is all lower case.
# Input: tibble (token, type)
# Output: tibble (token, type)
naive_English_morphology_NE_only <- function(gazetteer_entry){
  # to handle irregular genitive and plural, as in e.g., "Niklas' house"
  irregular_suffixes <- c("s", "x", "z", "c")
  if(gazetteer_entry["type"] %in% NE_types){
    if(any(sapply(irregular_suffixes, function(suffix) endsWith(
      gazetteer_entry[["token"]], suffix)))){
      result <- tibble(token = tolower(
        c(gazetteer_entry[["token"]],
          paste0(gazetteer_entry[["token"]], "'"),
          paste0(gazetteer_entry[["token"]], "es"),
          paste0(gazetteer_entry[["token"]], "es'"))),
        uid = c(rep_len(gazetteer_entry[["uid"]], 4)),
        type = c(rep_len(gazetteer_entry[["type"]], 4)))
    } else {
      result <-tibble(token = tolower(
        c(gazetteer_entry[["token"]],
          paste0(gazetteer_entry[["token"]], "'s"),
          paste0(gazetteer_entry[["token"]], "s"),
          paste0(gazetteer_entry[["token"]], "s'"))),
        uid = c(rep_len(gazetteer_entry[["uid"]], 4)),
        type = c(rep_len(gazetteer_entry[["type"]], 4)))
    }
  } else if (gazetteer_entry["type"] %in% term_types){
    result <- tibble(token = tolower(gazetteer_entry[["token"]]),
                     uid = gazetteer_entry[["uid"]],
                     type = gazetteer_entry[["type"]])
  } else {
    warning("Type of word '", gazetteer_entry[["token"]], "' (type: '",
            gazetteer_entry[["type"]], "') is neither a type given for named
            entities nor for terms. Skipping word.")
    result <- tibble(token = character(0), uid = character(0),
                     type = character(0))
  }
  return(result)
}

# This is a naive implementation for preprocessing documents. i.e. splitting
# them into words, providing them with a id indicating the sentence they
# originated from (starting from 1 in each document), as well as the id of the
# document. (in short: tokenization + adding an id)
# The document is split into sentences at every occurrence of ". ", which may
# erroneously include cases like "Mr. Smith", and then into words at every
# space, which leads to ignoring named entities for which the token contains a
# space, e.g. "Saint Petersburg".
# All words are converted to lower case and stop words are removed.
# Input: tibble (id, document)
# Output: tibble (doc_id, sen_id, word)
naive_preprocessing_documents <- function(doc, stop_words){
  sentences <- unlist(strsplit(doc[["document"]], "\\.|\\!|\\?"))
  split_documents <- tibble(sen_id = c(1:length(sentences)), sen =
                              as.vector(sentences))

  # returns a list containing a tibble per sentence of format sen_id, word
  sen_id_and_words <- bind_rows(apply(split_documents, 1, function(entry)
    tibble(sen_id = rep(entry["sen_id"], str_count(entry["sen"], " ")[1] +1),
           word = tolower(unlist(
             strsplit(gsub(gazetteer_remove, "", entry["sen"]), " "))))))

  # remove stop words
  sen_id_and_words <- anti_join(sen_id_and_words, tibble(word=stop_words),
                                by = "word")
  result <- tibble(doc_id = rep(doc["id"], nrow(sen_id_and_words)),
                   sen_id = sen_id_and_words$sen_id,
                   word = sen_id_and_words$word)
}

# The actual application of the gazetteer. We chose an inner join to remove
# words that are in the text but not in the gazetteer.
# Input: tibble (id, document), tibble (token, type), function, vector
apply_gazetteer <- function(doc, gazetteer, split_function, stop_words){
  split_sentences <- split_function(doc, stop_words)
  result <- inner_join(split_sentences, gazetteer, by = c("word" = "token"))
}

# Quasi main function that turns a set of documents into a tibble that can be
# used to create a LOAD network.
# All words (in the documents and the gazetteer alike) are converted to lower
# case.
# By default the stopwords from snowball provided by R for the English
# language are used.
# The morphology_function is used to create relevant morphological forms from
# the words (i.e. named entities and terms) in the gazetteer. For the English
# language that would be singular and plural genitive, as well as (nominative)
# plural. (e.g. person, person's, persons, persons') The default function
# provides a naive implementation. An own function can be implemented. If no
# morphological function should be applied, the parameter can be set to NULL.
# The document_preprocessing_function splits the documents into words, removes
# stop words and adds document and sentences ids.  An own function can be
# provided.
# Input: tibble(id, document), tibble (token, type) or
# tibble(token, type, uid), vector, function, function
# Output: tibble(doc_id, sen_id, token, type, uid) (colnames = TRUE when
# written to file)
label_using_gazetteer <- function(documents, gazetteer,
                                  stop_words = stopwords::stopwords(),
                                  morphology_function =
                                    naive_English_morphology_NE_only,
                                  document_preprocessing_function =
                                    naive_preprocessing_documents){
  if(is.null(gazetteer)) {
    stop("Error: The gazetteer is NULL - most likely something went wrong",
         "reading the gazetteer.")
  }
  morphologized_gazetteer <- tibble()

  # add uid
  if(ncol(gazetteer) == 2){
    gazetteer <- add_column(gazetteer,
                            uid = paste("uid", 1:nrow(gazetteer), sep = "-"),
                            .after = "token")
  }
  # construct morphological forms
  if (!is.null(morphology_function)){
    morphologized_gazetteer <- bind_rows(
      morphologized_gazetteer, apply(gazetteer[,c("token", "uid", "type")],
                                     1, morphology_function))
  # don't construct morphological forms
  } else {
    morphologized_gazetteer <- gazetteer
  }
  result <- rename(bind_rows(apply(documents, 1, function(doc)
    apply_gazetteer(doc, morphologized_gazetteer,
                    document_preprocessing_function,
                    stop_words))), token = word)
  write_delim(result, "./preprocessed_documents.txt", delim = "\t")
}
