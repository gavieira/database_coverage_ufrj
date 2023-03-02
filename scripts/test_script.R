library(tidyverse)
library(bibliometrix)

#sourcing functions script
source('scripts/00_functions.R')

#loading .Rdata objects
load('output/data/dfs.Rdata')
load('output/data/analyses.Rdata')
load('output/data/summaries.Rdata')

View(analyses)

doc_count <- list()

doc_count$dimensions <- dfs$dimensions %>% mutate(db = 'dimensions') %>% count(DT) %>% mutate(db = 'dimensions')

doc_count$wos <- dfs$wos %>% mutate(db = 'wos') %>% count(DT) %>% mutate(db = 'wos')

doc_count$lens <- dfs$lens %>% mutate(db = 'lens') %>% count(DT) %>% mutate(db = 'lens')

doc_count$scopus <- dfs$scopus %>% mutate(db = 'scopus') %>% count(DT) %>% mutate(db = 'scopus')

View(doc_count[1:4] %>% bind_rows())
