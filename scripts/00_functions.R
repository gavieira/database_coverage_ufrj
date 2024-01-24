library(tidyverse)
library(bibliometrix)
library(biblioverlap)

#Function to write merged dfs in a custom output directory
write_df <- function(df, outdir, filename) {
  dir.create(outdir, showWarnings = F)
  write_csv(df, sprintf("%s/%s", outdir, filename))
}


#Function to extract duplicates based on fields
get_duplicates <- function(df, fields = everything()) {
 dups <- df %>%
   mutate( across({{ fields }}, ~ifelse(. == '', NA, .)))  %>%  # Convert empty strings to NA for fields  
   arrange(across({{ fields }})) %>% 
   group_by(across({{ fields }})) %>% 
   filter(n()>1)
 return(dups)
}



#Function to read_in data and perform some cleaning procedures on it
get_cleaned_df <- function(df) {
  if ('Citing.Works.Count' %in% names(df) ) { #Dealing with some particularities of lens, which is the only database with the 'Citing.Works.Count' field
    df <- df %>% 
      mutate(TC = as.numeric(Citing.Works.Count), #Creating a TC column (which is the field name for citations in other dbs) based on the Citing.Works.Count column
             AU_CO = as.character(AU_CO)) #Converting the AU_CO field into character (it has only NAs, so R reads it automatically as a boolean col)
      
  }
  
  df <- as.data.frame(df)%>%
    mutate( across(c(DI, TI, SR_FULL), ~ifelse(. == '', NA, .)))  %>%  # Convert empty strings to NA for fields  
    filter(!(is.na(DI) & is.na(TI))) %>% #removes rows where both DI and TI are NA (can't determine uniqueness without these fields)
    select(-starts_with('X.')) %>% #Additional step to get rid of noninformative columns from WoS datasets 
    filter(PY <= 2022) #Keeping only documents published up to 2022
  
  no_dupes <- df %>%
    filter(!is.na(DI)) %>% # Removes DOIless records (quite a significant amount of records)
    distinct(DI, .keep_all = TRUE) %>% #Keeps unique DOI records
    bind_rows(., filter(df, is.na(DI))) %>% # Reinserts DOIless records at the end of the dataframe, which is useful in the next filter (gets as many docs with DOI as possible)
    distinct(TI, SR_FULL, .keep_all = TRUE) %>%
    mutate()
  
  return(no_dupes)
}


#Function for internal testing
check_na <- function(db_list, colname) {
  lapply(db_list, function(df) {nrow(df %>% 
                                       mutate( across(everything(), ~ifelse(. == '', NA, .)))  %>%
                                       filter(is.na({{colname}})))})
}

#Function for internal testing
get_dupes <- function(df, cols) {
    is_duplicate <- duplicated(df[, cols], incomparables = NA) | duplicated(df[, cols], fromLast = TRUE, incomparables = NA) 
    return(df[is_duplicate, c('DI', 'TI', 'DT', 'AU', 'SO', 'J9', 'PY', 'SR', 'SR_FULL')])
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

#Function to check if total document number of docs in dfs and aux_vars$main_info match
dfs_and_main_info_totals_match <- function(dfs, main_info, db_name) {
    database_total <- nrow(dfs[[tolower(db_name)]])
    main_info_total <- main_info %>%
      filter(db == db_name,
             description == 'Documents') %>%
      select(result)
    
   return( setNames(database_total == main_info_total, db_name ) )
}


#Function to normalize fields and extract only relevant fields for downstream analyses 
normalize_df <- function(df) { 
  decades = c("Pre-1983", "1983-1992",  
              "1993-2002", "2003-2012",  
              "2013-2022", "Undefined") #Creating a vector that will be used to group our data by decade
  normalized_df <- df %>% 
    filter(!is.na(PY)) %>% #Removing any documents with unknown publication year
    remove_rownames %>% # Removing rownames from the dataframe
    select(DB, DI, TI, DT, PY, SO, J9, TC, AU) %>%
    mutate(AU = sub(";.*$", "", AU), #Getting only the first author from the 'AU' column; patter removes text after the first ';' separator
           decade = case_when( #Adding decade of document
             PY < 1983 ~ decades[1],
             between(PY, 1983,1992) ~ decades[2],
             between(PY, 1993,2002) ~ decades[3],
             between(PY, 2003,2012) ~ decades[4],
             between(PY, 2013,2022) ~ decades[5],
             .default = decades[6]),
           decade = factor(decade, levels = decades),
           DT = case_when( #Normalizing document type
             DT %in% c('ARTICLE', 'JOURNAL ARTICLE', 'REVIEW', 'ARTICLE; EARLY ACCESS', 'REVIEW; EARLY ACCESS', 
                       'ARTICLE; DATA PAPER', 'ARTICLE; RETRACTED PUBLICATION', 
                       'ARTICLE; DATA PAPER; EARLY ACCESS', 'REPRINT') ~ 'Articles',
             DT %in% c('PROCEEDING', 'CONFERENCE PROCEEDINGS ARTICLE', 'CONFERENCE PROCEEDINGS', 
                       'CONFERENCE PAPER', 'CONFERENCE REVIEW', 'PROCEEDINGS PAPER', 'MEETING ABSTRACT',
                       'ARTICLE; PROCEEDINGS PAPER') ~ 'Proceedings itens', 
             DT %in% c('BOOK', 'BOOK CHAPTER', 'CHAPTER', 'ARTICLE; BOOK CHAPTER', 'REVIEW; BOOK CHAPTER') ~ 'Books and book chapters', #Does not include book reviews
             DT %in%  c('UNIDENTIFIED') ~ 'Unidentified', 
             DT %in%  c('PREPRINT') ~ 'Preprint',
             .default = 'Other')) #Adding a last value to aggregate all other DTs
  
  return(normalized_df)
}


get_doctype_count_df <- function(dfs) {
    doctype_count_df <- dfs %>%
      bind_rows(.id = 'db') %>%
      group_by(db, DT) %>%
      summarize(doc_count = n()) %>%
      ungroup() %>%
      complete(db, DT, fill = list(doc_count = 0))
    return(doctype_count_df)
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



#Function to save a list of plots to a specified path (png)
save_plot_list <- function(plot_list, filenames = names(plot_list),
                           plot_outpath = 'output/plots/',
                           width = 20, height = 9, unit = 'in', 
                           dpi = 300) {
  sapply(1:length(plot_list), function(index){
    ggsave(paste0(plot_outpath,filenames[[index]],".png"), plot = plots[[index]], width = width, height = height, dpi = dpi)
  })
}


custom_venn <- function(uuid_list, title) {
  return(ggVennDiagram(uuid_list) + 
           scale_fill_gradient(low = "#A7C7E7", high = "#08306B") +
           scale_x_continuous(expand = expansion(mult = .2)) + 
           guides(fill='none') +
           labs(title = title) +
           theme(plot.title = element_text(size = 20, hjust = 0.5))) #Returning ggplot venn diagram
}


venn_by_doctype <- function(doctype = "All", db_list_matched, all_matches = TRUE) {
  if (doctype != 'All') {
    db_list_subset <- lapply(db_list_matched, function(db)  db %>% filter(DT == doctype) )
    if (all_matches) {
      db_list_matched <- get_all_subset_matches(db_list_subset, db_list_matched)
    } else {
      db_list_matched <- db_list_subset
    }
  } #Filtering records for a given doctype (if necessary)
  uuid_list <- lapply(db_list_matched, function(db) db$UUID) #Extracting UUID list for each base
  if (length(uuid_list) < 2) {
    stop("At least 2 non-empty lists are required to create a Venn diagram.")
  }
  return(custom_venn(uuid_list, title = doctype))
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

#Function to check if a single column has only unique values
col_all_unique <- function(df, column) {
  df %>%
    bind_rows() %>%
    summarise(all_unique = n_distinct( !!sym(column) ) == n() )
}
