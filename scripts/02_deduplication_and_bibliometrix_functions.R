library('tidyverse')
library('bibliometrix')

#sourcing functions script
source('scripts/00_functions.R')

#Loading .Rdata objects
load('output/data/dfs.Rdata')

###Step 1 - Getting duplicates dataframe

#Function to extract duplicates based on fields
get_duplicates <- function(df, db, fields = everything()) {
 dups <- df %>%
 arrange(across({{ fields }})) %>%
 group_by(across({{ fields }})) %>%
 filter(n()>1)
 return(dups)
}

#Function to read_in data and:
#1- add a new column to identify the source database
#2- remove duplicates (default: use all columns)
get_cleaned_df <- function(df, db, fields = everything()) {
  df <- as.data.frame(df)%>%
  distinct(across({{ fields }}), .keep_all = T) %>%
  mutate(database = db) %>%
  select(-starts_with('X.')) #Additional step to get rid of noninformative columns from WoS datasets
  return(df)
}


#Getting name of databases from list objects
get_db_names <- function(dblist){
  dbs <- str_to_title(names(dblist)) #Getting db names from list attributes
  for (i in 1:length(dbs)){ 
    if (dbs[i] == 'Wos'){
      dbs[i] <- 'WoS' #Changing 'Wos' to 'WoS'
    }
  }
  return(dbs)
}

dbs <- get_db_names(dfs)

duplicates_df <- map2(dfs, dbs, get_cleaned_df, fields = everything()) %>%
  lapply(get_duplicates, fields = c('database','TI')) %>% #sorting and getting duplicates by database and title
  bind_rows()
  

write_csv(duplicates_df, 'output/data/duplicates.csv') 
save(duplicates_df, file = 'output/data/duplicates.Rdata') 

#After manual inspection of the duplicates, we chose to use the fields X and Y to remove duplicates from our dataset


##Step 2: data deduplication


#Getting data into a single deduplicated dataframe
#dbs <- c('dimensions','wos','lens','scopus')
#files <- paste(dbs,'.csv', sep = '')
#dfs <- paste(dbs,'_df', sep = '')
#all_data <- map2(files, dbs, get_cleaned_df, fields = everything())


#Getting individual deduplicated dataframes
scopus_df <- get_cleaned_df(scopus_df, 'scopus')
dimensions_df <- get_cleaned_df(dimensions_df, 'dimensions')
lens_df <- get_cleaned_df(lens_df, 'lens')
wos_df <- get_cleaned_df(wos_df, 'wos')

##Step 3: Bibliometrix's biblioAnalysis function

#Running biblioAnalysis function for all each dataset (takes a while...)
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
