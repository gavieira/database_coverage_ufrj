library('tidyverse')
library('bibliometrix')
library('parallel')

#sourcing functions script
source('scripts/00_functions.R')

#Loading .rds objects
dfs <- readRDS('output/data/raw_dfs.rds')

###Step 1 - Getting dataframe with duplicates


#Changing all '' fields to NA
dfs <- lapply(dfs, function(df) mutate_all(df, ~ifelse(. == "", NA, .)) )


mclapply(dfs, function(df) nrow(get_duplicates(df, fields = everything())), mc.cores = 4) #Checking the number of identical rows in a given dataset

#Since there are no identical rows in any of the databases, we can:
#(i) get the duplicates based only on DOI - this will allow us to see if there is a single DOI assigned to multiple publications 
#(ii) get the duplicates based on other data (Title and PubYear) - useful for DOIless records
#(iii) join the two datasets  and remove identical rows

#Getting rows with duplicated DOI
duplicates_di <- mclapply(dfs, function(df) {
  get_duplicates(df, fields = c('DI')) %>%
                   filter(!is.na(DI))
  }, mc.cores = 4)

#Getting rows with duplicated TITLE and PUBLICATION YEAR
duplicates_ti <- mclapply(dfs, function(df) {
  get_duplicates(df, fields = c('TI', 'PY')) %>%
    filter(!is.na(TI) | !is.na(PY))
}, mc.cores = 4)


View(duplicates_di)
View(duplicates_ti)


#Joining the two datasets and removing identical rows
merged_duplicates <- mapply(bind_rows, duplicates_di, duplicates_ti, SIMPLIFY = FALSE)
View(merged_duplicates)

unique_merged_duplicates <- lapply(merged_duplicates, distinct)
View(unique_merged_duplicates)


#Putting duplicates in a single list, then saving it into a rds file
duplicates <- list(doi = duplicates_di,
                   title = duplicates_ti,
                   all = unique_merged_duplicates)

saveRDS(duplicates, file = 'output/data/duplicates.rds')


#Saving duplicates into a single table, then writing it to a file
duplicates_df <- bind_rows(duplicates$all, .id = "DATABASE")

write_csv(duplicates_df, 'output/data/duplicates.csv') 

##Step 2: data deduplication/cleaning

#Getting data into a single deduplicated dataframe (Title and PUBLICATION YEAR)

dfs <- lapply(dfs, function(df) get_cleaned_df(df) )
saveRDS(dfs, file = 'output/data/dfs.rds')


##Step 3: Bibliometrix's biblioAnalysis function

dfs <- readRDS('output/data/dfs.rds')

dfs$Lens$AU_CO <- as.character(dfs$Lens$AU_CO) #Changing this column to character in order to run bibliometrix functions

#View(dfs$Lens)

analyses <- mclapply(dfs, function(df) biblioAnalysis(df), mc.cores = 4)

#saving the list of biblioAnalysis objects to a .Rdata file
saveRDS(analyses, file = 'output/data/analyses.rds')


##Step 4: Bibliometrix's summarise function

analyses <- readRDS('output/data/analyses.rds')

summaries <- mclapply(analyses, function(analysis) summary(analysis), mc.cores = 4)

#Adding summaries to list and saving it to a .Rdata file

saveRDS(summaries, file = 'output/data/summaries.rds')
