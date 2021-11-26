# R Implementation of implicit network extraction

(c) 2018 Katja Hauser, Andreas Spitz, Michael Gertz

**For details, see https://dbs.ifi.uni-heidelberg.de/resources/load/**

This collection of scripts provides an R implemention of the load
algorithm for the extraction of implicit networks, as described in:
"Terms over LOAD: Leveraging Named Entities for Cross-Document Extraction
and Summarization of Events", Spitz and Gertz, SIGIR 2016.

In a nutshell, the scripts take a collection of documents as input, annotate
named entities, extract, terms, and create an implicit network from the results.
The output is provided as tibbles (=data frames) of (1) nodes in the network,
(2) edges in the network, and (3) an igraph graph object. Annotation can be performed
either by automated named entity recognition, or by using a custom gazetteer. All
annotation steps in the pipeline can be replaced by custom annotation functions,
e.g. for other languages.

The entire collection of scripts can be executed as-is on the included example
documents from Jules Verne's "In 80 Days Around the World" by calling:
source("start_Pipeline.R"), but it is recommended to take a look at config.R
first, which also contains a thorough documentation of the pipeline steps.
