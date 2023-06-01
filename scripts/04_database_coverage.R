#Loading libraries
library(tidyverse)
library(bibliometrix)
library(treemapify)
library(patchwork)

#sourcing functions script
source('scripts/00_functions.R')

#loading .Rdata objects
load('output/data/dfs.Rdata')
load('output/data/analyses.Rdata')
load('output/data/summaries.Rdata')

aux_vars <- list()

#Checking the first rows of each database
aux_vars$first_rows <- lapply(dfs, head, n=3) %>% bind_rows(.id = 'id')

View(aux_vars$first_rows)
#write.csv(aux_vars$first_rows, "/home/gabriel/first_rows.csv", row.names = FALSE) #Saving to file to better visualize results

#After visual inspection of the first rows of each db, we have selected the fields that will be used to determine overlapping documents
#DI (DOI)
#TI (Publication Title)
#SO (Source)
#AU (Author)
#PY (Publication Year)
#VL (Volume)
#JI (ISO Source Abbreviation)
#J9 (29-Character Source Abbreviation)
#SR_FULL (Complete Source Record - Author + Year + Source Abbreviation) - Looks like a nice field, even though Dimensions and Lens have some records without the source abbreviation
#PU (Publisher) # No caso da Dimensions, essa informação está no campo "Publisher"
#ISSN # Campo presente apenas para dimensions. Na lens, essa informação está no campo "ISSNs". Na scopus e wos, no campo 'SN'.

#Gerando uma dataset apenas das publicações que ocorrem em todas as bases (apenas pelo DOI)
aux_vars$duplicated_doi_all_dbs <- dfs %>%
  bind_rows(.id = 'id') %>%
  group_by(DI) %>%
  filter(n() == 4) %>%
  ungroup() %>%
  arrange(DI)

#Obtendo os elementos únicos de um dado campo (para ver quais bases possuem apenas NAs)
unique_elements <- lapply(dfs, function(df) unique(df[['AR']]))
View(unique_elements)

#Verificação de quantas entradas não possem source abreviation no campo SR_FULL dos dataframes
lapply(unique_elements, function(db) sum(grepl("CHARACTER\\(0\\)", db)))
lapply(unique_elements, function(db) sum(grepl("CHARACTER.0.", db)))

#Selecionando colunas relacionadas ao ISNN/ISBN - referencia para nome das colunas: https://www.bibliometrix.org/documents/Field_Tags_bibliometrix.pdf
View(
  aux_vars$first_rows %>%
  select(database, ISSN, ISSNs, ISBN, IS, SN, eissn, BN) #ISBN and BN are book numbers
)

#Selecionando colunas (provavelmente) associadas ao número de páginas
View(
  aux_vars$first_rows %>%
  select(database, PP, Start.Page, End.Page, PN) #PP is probably not page number
)




#O que fazer:
#1 - Separar os documentos que apresentam 4 matches pelo DOI
#2 - Trabalhar apenas com documentos que tenham menos 3 matches - podem ter um quarto, mas o DOI não foi recuperado
#3 - Limpar os dados (remoção de acentos (US-ASCII), seria interessante tirar pontuação tbm?)
#4 - Fazer (a princípio) a análise com base na distancia de edição (levenshtein) do título, nome do primeiro author, periódico e ano de publicação.
#5 - Tentar pegar a análise e gerar as permutações. Uma forma seria


aux_vars$not_all_dbs <- dfs %>%
  bind_rows(.id = 'id') %>%
  group_by(DI) %>%
  filter(n() != 4)



#### Getting a subset dataframe for testing (only documents from 2008)

aux_vars$dfs_subset <- lapply(dfs, function(df) { filter(df,  PY == 2008) } )

lapply(aux_vars$dfs_subset, nrow)



#### Getting a dataframe with records containing duplicated dataframes (for post-cleaning)

# Apply the get_duplicate_rows_at_column function to each dataframe in the 'dfs' list,
# filtering for duplicates based on the specified column ('DI' - doi)
aux_vars$duplicated_doi_within_dbs <- lapply(dfs, get_duplicate_rows_at_column, column_name = 'DI') %>%
  bind_rows(.id = 'id') %>%
  arrange(DI)

aux_vars$ duplicated_doi_within_dbs
