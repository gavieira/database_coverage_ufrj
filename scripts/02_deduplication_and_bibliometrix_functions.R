library('tidyverse')
library('bibliometrix')

#sourcing functions script
source('scripts/00_functions.R')

#Loading .Rdata objects
load('output/data/dfs.Rdata')

###Step 1 - Getting duplicates dataframe

dbs <- get_db_names(dfs)

duplicates_df <- map2(dfs, dbs, get_cleaned_df, fields = everything()) %>%
  lapply(get_duplicates, fields = c('database','TI')) %>% #sorting and getting duplicates by database and title
  bind_rows()
  

write_csv(duplicates_df, 'output/data/duplicates.csv') 
save(duplicates_df, file = 'output/data/duplicates.Rdata') 

#After manual inspection of the duplicates, we chose to use the fields X and Y to remove duplicates from our dataset


##Step 2: data deduplication/cleaning


#Getting data into a single deduplicated dataframe
#dbs <- c('dimensions','wos','lens','scopus')
#files <- paste(dbs,'.csv', sep = '')
#dfs <- paste(dbs,'_df', sep = '')
#all_data <- map2(files, dbs, get_cleaned_df, fields = everything())


#Getting individual deduplicated dataframes
scopus_df <- get_cleaned_df(dfs$scopus, 'scopus')
dimensions_df <- get_cleaned_df(dfs$dimensions, 'dimensions')
lens_df <- get_cleaned_df(dfs$lens, 'lens')
wos_df <- get_cleaned_df(dfs$wos, 'wos')


#Updating the 'dfs' list and .Rdata file with the cleaned versions of the dataframes
dfs <- list(dimensions = dimensions_df, 
            lens = lens_df, 
            scopus = scopus_df, 
            wos = wos_df)

save(dfs, file = 'output/data/dfs.Rdata')

##Step 3: Bibliometrix's biblioAnalysis function

#Running biblioAnalysis function for each dataset (takes a while...)
dimensions_biblio <- biblioAnalysis(dfs$dimensions)
scopus_biblio <- biblioAnalysis(dfs$scopus)
wos_biblio <- biblioAnalysis(dfs$wos)
lens_biblio <- biblioAnalysis(dfs$lens)

#Adding biblioAnalysis objects to a single list
analyses <- list(dimensions = dimensions_biblio, lens = lens_biblio, scopus = scopus_biblio, wos = wos_biblio)

#saving the list of biblioAnalysis objects to a .Rdata file
save(analyses, file = 'output/data/analyses.Rdata')



##Step 4: Bibliometrix's summarise function

load('output/data/analyses.Rdata')

dimensions_summary <- summary(analyses$dimensions)
scopus_summary <- summary(analyses$scopus)
wos_summary <- summary(analyses$wos)
lens_summary <- summary(analyses$lens)


#Adding summaries to list and saving it to a .Rdata file

summaries <- list(dimensions = dimensions_summary, lens = lens_summary, scopus = scopus_summary, wos = wos_summary)

save(summaries, file = 'output/data/summaries.Rdata')
