library('tidyverse')
library('bibliometrix')
library('parallel')

#sourcing functions script
source('scripts/00_functions.R')

#Loading .rds objects
dfs <- readRDS('output/data/dfs.rds')

###Step 1 - Getting dataframe with duplicates

#Getting rows with duplicated TITLE and PUBLICATION YEAR
duplicates_list <- mclapply(dfs, function(df) get_duplicates(df, fields = c('TI', 'PY') ), mc.cores = 4)


duplicates_df <- bind_rows(duplicates_list, .id = "DATABASE")

write_csv(duplicates_df, 'output/data/duplicates.csv') 
saveRDS(duplicates_list, file = 'output/data/duplicates.rds')

##Step 2: data deduplication/cleaning

#Saving total number of documents in each dataset before removing duplicates
saveRDS(lapply(dfs, dim), file = 'output/data/duplicated_dfs_total_docs.rds')


#Getting data into a single deduplicated dataframe (Title and PUBLICATION YEAR)

dfs <- lapply(dfs, function(df) get_cleaned_df(df, fields = c('TI', 'PY') ) )
saveRDS(dfs, file = 'output/data/dfs.rds')


##Step 3: Bibliometrix's biblioAnalysis function

analyses <- mclapply(dfs, function(df) biblioAnalysis(df), mc.cores = 4)

#saving the list of biblioAnalysis objects to a .Rdata file
saveRDS(analyses, file = 'output/data/analyses.rds')


##Step 4: Bibliometrix's summarise function

analyses <- readRDS('output/data/analyses.rds')

summaries <- mclapply(analyses, function(analysis) summary(analysis), mc.cores = 4)

#Adding summaries to list and saving it to a .Rdata file

saveRDS(summaries, file = 'output/data/summaries.rds')
