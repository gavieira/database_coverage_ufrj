library('tidyverse')

#Function to read_in data and:
#1- add a new column to identify the source database
#2- remove duplicates (default: use all columns)

get_cleaned_df <- function(file, db, fields = everything()) {
  df <- read_csv(file, col_types = cols(.default = 'c')) %>% #If the columns are not set to 'c' (character), some data imports could fail
  #df <- read_csv(file) %>% #If the columns are not set to 'c' (character), some data imports could fail
  distinct(across({{ fields }}), .keep_all = T) %>%
  mutate(database = db) %>%
  select(-starts_with('X.')) #Additional step to get rid of many unnecessary columns from WoS datasets
  return(df)
}


#Function to extract duplicates based on fields

get_duplicates <- function(df, fields = everything()) {
 dups <- df %>%
 arrange(across({{ fields }})) %>%
 group_by(across({{ fields }})) %>%
 filter(n()>1)
 return(dups)
}


#Getting data into a single dataframe
#dbs <- c('dimensions','wos','lens','scopus')
#files <- paste(dbs,'.csv', sep = '')
#dfs <- paste(dbs,'_df', sep = '')
#all_data <- map2(files, dbs, get_cleaned_df, fields = everything())


#Getting individual dataframes

scopus_df <- get_cleaned_df('data/merged_data/scopus.csv', 'scopus')
dimensions_df <- get_cleaned_df('data/merged_data/dimensions.csv', 'dimensions')
lens_df <- get_cleaned_df('data/merged_data/lens.csv', 'lens')
wos_df <- get_cleaned_df('data/merged_data/wos.csv', 'wos')


###Getting duplicates dataframes

dbs <- c('dimensions','wos','lens','scopus')
files <- paste0('data/merged_data/',dbs,'.csv', sep = '')

dupes <- map2(files, dbs, get_cleaned_df, fields = everything()) %>%
  lapply(get_duplicates, fields = c('database','TI')) %>% #sorting and getting duplicates by database and title
  bind_rows()

write_csv(dupes, 'output/data/duplicates.csv') 

########

biblio_dimensions <- biblioAnalysis(read_csv('dimensions.csv'))
biblio_scopus <- biblioAnalysis(read_csv('scopus.csv'))
biblio_wos <- biblioAnalysis(read_csv('wos.csv'))
biblio_lens <- biblioAnalysis(read_csv('lens.csv', col_types = cols(AU_CO = 'c')))

##OBS: The lens_df does not feature the 'C1' field. This field corresponds to the 'Author Address'. Author affiliation is not available in the lens default .csv export. However, it is available in the json export.