library(tidyverse)
library(bibliometrix)
library(uuid)

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
  if ('Citing.Works.Count' %in% names(df) ) { #Dealing with lens, which has citations in the 'Citing.Works.Count' field
    df <- mutate(df, TC = as.numeric(Citing.Works.Count))
  }
  df <- as.data.frame(df)%>%
  distinct(across({{ fields }}), .keep_all = T) %>%
  mutate(database = db) %>%
  select(-starts_with('X.')) %>% #Additional step to get rid of noninformative columns from WoS datasets
  filter(PY <= 2022) #Keeping documents published up to 2022
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


#Function to calculate the h-index
hindex <- function(citations) {
  #' Receives a list (dataframe column) or vector and returns the h-index
  if ( typeof(citations) == 'list' ) {
    citations <- unlist(citations) #If func receives a list, convert it to a vector
  }
  sorted <- sort(citations, decreasing = T) # sorting vector by decreasing order
  return( sum(sorted >= seq_along(sorted)) ) # returning the number h of publications with citation number >= h
}


#Function to calculate the g-index
gindex <- function(citations) {
  #' Receives a list (dataframe column) or vector and returns the g-index
  if ( typeof(citations) == 'list' ) {
    citations <- unlist(citations) #If func receives a list, convert it to a vector
  }
  sorted <- sort(citations, decreasing = T) # sorting vector by decreasing order
  sorted_cumsum <- cumsum(sorted) # calculating the cumulative sum of citations
  return( sum(sorted_cumsum >= seq_along(sorted)^2)) # returning the number g of publications that combined have a citation number >= g**2
}


#Function to calculate the e-index
eindex <- function(citations) {
  #' Receives a list (dataframe column) or vector and returns the e-index
  if ( typeof(citations) == 'list' ) {
    citations <- unlist(citations) #If func receives a list, convert it to a vector
  }
  sorted <- sort(citations, decreasing = T) # Sorting vector by decreasing order
  h_core_citations <- sorted[sorted >= seq_along(sorted)] # Getting vector containing only h-core document citations
  eindex <- sqrt(sum(h_core_citations) - length(h_core_citations)^2) # Calculating the e-index (square root of the net excess citations of papers included in the h core)
  return( round(eindex, 2)) # Returning a rounded version of the e-index
}


#Function to calculate the hc-index (contemporary h-index)
hcindex <- function(citations, pubyear, 
                    current_year = format(Sys.Date(), "%Y") ) {
  # Receives dataframe columns (citations, pubyear) as arguments, and returns the hc-index (contemporary h-index)
  # Default current_year value is based on system time, though it can be overwriten by a custom value
  current_year = as.integer(current_year) #Converting current_year to integer
  
  if ( any(pubyear >= current_year) ) { #Raising a warning
    warning("There is at least one document with age zero or negative. Assuming document age of 1 year.")
  }
  
  hc_df <- bind_cols(citations = citations, pubyear = pubyear) %>%   #Binding columns of two vectors
    drop_na() %>% #Removing any rows with NAs (WoS has some documents with unknown publication date)
    mutate(age = current_year - pubyear,   #Calculating document age
           age = ifelse(age < 1, 1, age), #Changing documents with less than 1 year of age to 1
           score = round(citations*4/age, 2) ) %>% #Calculating score (rounded)
    arrange( desc(score) ) #Arrange by descending score
  
  return( sum(hc_df$score >= seq_along(hc_df$score)) ) # returning the number hc of publications with citation score >= hc
}


# Define a function to retrieve duplicate values in a specified column of a dataframe
get_duplicate_rows_at_column <- function(df, column_name) {
  column_name <- sym(column_name) #Converting column_name variable to symbol
  duplicates <- df %>%  # Start with the input dataframe
    filter(!is.na(!!column_name), !!column_name != "") %>%  # Remove rows where the column value is NA or an empty string
    group_by(!!column_name ) %>%     # Group the data by the specified column
    filter(n() > 1) %>%     # Keep only groups with more than one occurrence
    arrange(!!column_name)     # Arrange the data by the specified column
  return(duplicates)   # Return the resulting dataframe with duplicate values
}


#Function to generate universally unique identifiers (UUIDs) for rows of a dataframe
generate_uuid <- function(df, new_column = 'UUID') {
  new_column <- sym(new_column)
  df %>%
  mutate(!!new_column := sapply(1:nrow(df), UUIDgenerate)) %>%
  relocate(!!new_column, .before = 1)
}


#Function to check if a single column has only unique values
col_all_unique <- function(df, column) {
  df %>%
    bind_rows() %>%
    summarise(all_unique = n_distinct( !!sym(column) ) == n() )
}


#Function to test the penalty on the levenshtein distance for a given multiplier
test_lev_penalty <- function(multiplier) {
  vector <- (1:10)**2 * multiplier
  names(vector) <- 1:10
  return(vector)
}
