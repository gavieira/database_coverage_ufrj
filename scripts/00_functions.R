library(tidyverse)
library(bibliometrix)

#Function to write merged dfs in a custom output directory
write_df <- function(df, outdir, filename) {
  dir.create(outdir, showWarnings = F)
  write_csv(df, sprintf("%s/%s", outdir, filename))
}


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


#General function to extract dataframe attributes in the 'summaries' list while adding an extra column (database name)
get_info_from_summaries <- function(list, database, attribute_name) {
  attribute_index <- match(attribute_name, names(list)) #Getting index of desired information (attribute has to be a dataframe object)
  info <- as.data.frame(list[attribute_index]) %>% #Getting desired attribute through index
  mutate(db = database) #Creating database column
  return(info)
}
