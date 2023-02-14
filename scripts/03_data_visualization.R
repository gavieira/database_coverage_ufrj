library(tidyverse)
library(bibliometrix)

#Running biblioAnalysis for each dataset (takes a while...)

#biblio_dimensions <- biblioAnalysis(read_csv('data/merged_data/dimensions.csv'))
#biblio_scopus <- biblioAnalysis(read_csv('data/merged_data/scopus.csv'))
#biblio_wos <- biblioAnalysis(read_csv('data/merged_data/wos.csv'))
#biblio_lens <- biblioAnalysis(read_csv('data/merged_data/lens.csv', col_types = cols(AU_CO = 'c'))) #If not specified as a character ('c') column, the biblioanalysis will fail

#Saving bilioanalysis variables to a file so we don't need to always recreate them from scratch 

#save(biblio_dimensions, biblio_lens, biblio_scopus, biblio_wos, file = 'output/data/biblioanalyses.Rdata')

#Loading the biblioanalyses objects into the environment
load('output/data/biblioanalyses.Rdata')

