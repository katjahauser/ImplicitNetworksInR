# R Implementation of implicit network extraction
# (c) 2018 Katja Hauser, Andreas Spitz, Michael Gertz
# For details, see https://dbs.ifi.uni-heidelberg.de/resources/load/

# Purpose of this file:
# In this file we provide the means to load documents and gazetteers from
# different sources.

library(tibble)
library(readtext)


# read a gazetteer
read_gazetteer <- function(gazetteer, uses_uid){
  gazetteer_type <- tail(strsplit(gazetteer, ".", fixed = TRUE)[[1]], n = 1)

  # read txt gazetteer
  if (gazetteer_type == "txt"){
    tryCatch({
      if(uses_uid == TRUE){
        gaz <- as_tibble(read.table(gazetteer, sep = "\t", colClasses = "character",
                                    col.names = c("token", "type", "uid")))
      }else{
        gaz <- as_tibble(read.table(gazetteer, sep = "\t", colClasses = "character",
                                    col.names = c("token", "type")))
      }
    }, warning = function(war){
      print(war)
      if(grepl("No such file or directory", war)){
        stop("Error: File not found - check the provided path.")
      }
    })
  } else {
    stop(paste0("Error: You tried to import a gazetteer in a format for which",
                " no import function has been implemented yet. Valid formats",
                " are: ", valid_gazeteer_input_formats, ". Otherwise you can",
                " implement your own_import_function.", sep = ""))
  }
}


# read document
read_document <- function(document, type){
  # read txt document
  if (type == "txt"){
    tryCatch({
      doc <- tibble("document" = readtext(document)$text)
      doc <- add_column(doc, id = c(1:nrow(doc)), .before = "document")
    }, error = function(err){
      stop(paste(err, "Error during reading the document or directory.",
                 "Please check whether the path and type are correct.",
                 "e.g.: load_documents(\"./documents/test.txt\") or",
                 "load_documents(\"./documents/csv_documents/\"), type = ",
                 "\"csv\". This error also occurs, if a valid directory and",
                 "a valid type are given, but no such file resides within",
                 "the directory."))
    }
    )

  } else {
    stop(paste0("Error: You tried to import data in a format for which no import ",
                "function has been implemented, yet. Valid formats are: ",
                valid_input_formats, ". Otherwise you can implement your",
                " own_import_function."))
  }
}

# Load the documents to be used later. The path can either point to a single
# file or a directory. In the former case the file is imported (a type must
# not be given in this case), in the latter case all files of the given type
# in the directory are read.
# If a gazetteer is supposed to be used, a path to the gazetteer has to be
# provided, otherwise it is ignored.
# An own function to read data can be used by handing it to
# own_import_function - the header should take the first three arguments of
# load_documents: [my_own_function](path, type, gazetteer).
# If uses_uid = TRUE, a uid is used to identify the entities within the
# pipeline (only if a gazetteer is used). If it is set TRUE, the gazetteer is
# read as three columns (token, type, uid), for uses_uid = FALSE only the
# former two are considered.
# The function returns a list with two elements: a tidy data frame (tibble) of
# the documents (format: id, document) and another tibble with the gazetteer
# (format: token, type) or NULL, if no gazeteer was passed to the function.
load_documents <- function(path, type = NULL, gazetteer = NULL,
                           uses_uid = FALSE, own_import_function = NULL){
  tryCatch({
    # use own import function, if provided
    if (!is.null(own_import_function)){
      result <- own_import_function(path, type, gazetteer, uses_uid)
    } else {
      result <- list()

      # load documents
      doc <- tibble()
      if (is.null(type)){ # single document
        doc <- read_document(path, tail(strsplit(path, ".", fixed = TRUE)[[1]],
                                        n = 1))
      } else { # from directory
        doc <- read_document(paste0(path, "*", type), type)
      }
      result[["document"]] <- doc

      # load gazetteer, if one was provided
      if (!is.null(gazetteer)){
        result[["gazetteer"]] <- read_gazetteer(gazetteer, uses_uid)
      }else{
        result["gazetteer"] <- list(NULL)
      }
    }
    return(result)
  }, error = function(err){
    print(err)
  })
}
