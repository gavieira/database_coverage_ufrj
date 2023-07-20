##I need to implement the document matching in two steps:
#1 - Match only DOI values and attribute unique score (score = 2, for instance)
# Do it for each pairwise combination and remove records from db2 from subsequent DOI matching
#2 - Match string values of unmatched documents and calculate score
# Again, do it for each pairwise combination and remove records from db2 from subsequent DOI matching
# Assume match for record as soon as a score above the cutoff is calculated and proceed to next record
# Put dbs in the following order: wos, scopus, dimensions, lens

#We will make an assumption here:
#That if the a document has a DOI, all databases WILL feature that DOI 
#Thus, the second step will only try to match documents without DOI

#OBS: There are duplicated DOI's in the databases. I need to remove those before calculating the final overlap


library(parallel)
library(ggVennDiagram)

############################## FIRST STEP #####################################


#Quick cleaning of duplicated DOIs
dbs_coverage_clean <- lapply(dbs_coverage, function(db) db[!duplicated(db$DI, incomparables = NA),] )


sum(is.na(dbs_coverage_clean$dimensions$DI))
sum(is.na(dbs_coverage$dimensions$DI))

#Checking number of records in cleaned/uncleaned dataframes
dim(dbs_coverage$dimensions)
dim(dbs_coverage_clean$dimensions)

#Checking if DOI column has no duplicates in cleaned/uncleaned dataframes
lapply(dbs_coverage, function(db) any(duplicated(db$DI, na.rm = TRUE)) )
lapply(dbs_coverage_clean, function(db) any(duplicated(db$DI, na.rm = TRUE)) )




#Function that receives two lists of DOIs and returns a list containing the DOI matches of list1 against list2
parallel_doi_match <- function(db1, db2, n_cores = detectCores()) {
  db1_doi <- db1$DI #Extracting DOI column from db1
  db2_doi <- db2$DI #Extracting DOI column from db2
  cl <- makeCluster(n_cores) #Starts the cluster for parallel computing (by default, uses all cores in the system)
  clusterExport(cl, varlist = c("db1_doi", "db2_doi"), envir = environment()) #Exporting the variables received by the function to the cluster
  doi_matches <- parLapply(cl, 
                       seq_along(db1_doi), 
                       function (x) match(db1_doi[x], db2_doi, incomparables = c(NULL, NA, ''))  ) #Calculating which elements in list2 correspond to the DOIs in list1
  stopCluster(cl) #Stopping the cluster
  return(doi_matches) #Returning a list where the indices correspond to position in list1, and the values correspond to the rows with matching DOI in list2
}

#Updating db2 based on DOI matching
update_db2_doi <- function(db1, db2, doi_matches) {
  #db1_doi <- db1$DI #Extracting DOI column from db1
  #db2_doi <- db2$DI #Extracting DOI column from db2
  #doi_matches <- parallel_doi_match(db1_doi, db2_doi)
  for (i in seq_along(doi_matches)) {
    db2_index <- doi_matches[[i]]
    if ( !is.na(db2_index) ) { #If doi_match_index is not NA (in other words, if a matching DOI is found)
      db2[db2_index,'score'] <- 2 #Updating the score value of the db2 record to 2 (value could be changed later)
      db2[db2_index,'UUID'] <- db1[i,'UUID'] #db2 row receives the matching uuid from db1
    }
  }
  #return(list('matches' = matches, 'db2' = db2))
  return(db2)
}


########################### SECOND STEP ######################################

#Function to test if there are any NA values in a vector
test_na <- function(vector) {
  if ( any(is.na(vector)) ) {
    return (TRUE)
  } else {
    return (FALSE)
  }
}

#Compares two rows and calculates score based on similarity between fields
#Depends on the 'test_na' function to determine if there are NAs in at least one of the records in each field assessed
#When there is at least one NA, the score for the field will be zero
#Skips rows with a match
calc_score <- function (row1, row2) {
    score <- 0  #Initializing score
    title <- ifelse( test_na(c(row1['TI'], row2['TI'])), 0, 
                     pmax(0.6 - adist(row1['TI'], row2['TI'])[1] * 0.1, 0) ) #Score for publication title - adist() calculates levenshtein distance, while pmax replaces negative values with 0
    if (title >= 0.1) { #if title is >= 0.1, there's still a chance of matching, so we'll calculate the other score components.
      year <-ifelse( test_na(c(row1['PY'], row2['PY'])) || row1['PY'] != row2['PY'], 0, 0.3 ) #Score for publication year
      source <- ifelse( test_na(c(row1['J9'], row2['J9'])), 0, 
                        pmax(0.3 - adist(row1['J9'], row2['J9'])[1]*0.1, 0) ) #Score for publication source
      author <- ifelse( test_na(c(row1['AU'], row2['AU'])), 0, 
                        pmax(0.3 - adist(row1['AU'], row2['AU'])[1]*0.1, 0)  )#Score for publication author
      score <- title + year + source + author }
      return( list(db1_id = as.numeric(row1['index']), 
                   score = score, 
                   db2_id = as.numeric(row2['index'])) ) #Returning score, index of record in db1 and db2 (all as numeric values)
}



#This function subsets the original db1 and db2 in order to get only the necessary columns for score-based document matching - It keeps only documents: (i) that doesn't have a DOI; and (ii) that haven't been matched before
subset_db_for_score_match <- function(df) {
  df %>%  
    rownames_to_column(var = "index") %>%
    filter(is.na(DI) & score < 1) %>%
    select(index,TI,PY,J9,AU)
}


#This function calculates scores for unmatched documents without a DOI between two databases
#Since the matching procedure is quite strict, we'll assume that the first hit above the cutoff (1 by default) will be the only matched document against that particular record and go to the next document in db1, saving computational time
#
parallel_score_match <- function(db1,db2, n_cores = detectCores(), cutoff = 1) {
  #Subsetting data (and subsequently reducing RAM usage)
  db1_subset <- subset_db_for_score_match(db1)
  db2_subset <- subset_db_for_score_match(db2)
  cl <- makeCluster(n_cores, outfile = '') #Starts the cluster for parallel computing (by default, uses all cores in the system)
  clusterExport(cl, varlist = c("calc_score", "test_na")) #Exporting the variables received by the function to the cluster
  clusterExport(cl, varlist = c("db1_subset", "db2_subset", "cutoff"), envir = environment()) #Exporting the variables received by the function to the cluster
  score_matches <- parLapplyLB(cl, 1:nrow(db1_subset), function(i) { #parLapplyLB implements Load Balancing, so we'll not have some cores lagging behind at the end of the analysis
    print(paste("Running analysis for row1", i))
    for (j in 1:nrow(db2_subset)) {
      calc_score_list <- calc_score(db1_subset[i,], db2_subset[j,])
      if ( calc_score_list$score >= cutoff )  { #return the first match that has score >= cutoff
        return(list('db1_id' = calc_score_list$db1_id, 
                    'score' = calc_score_list$score, 
                    'db2_id' = calc_score_list$db2_id))
      } else { next } #If match's score is < cutoff, go to next db2 row (will result in NULL if no score >= cutoff is found)
    }
  })
  stopCluster(cl) #Stopping the cluster
  score_matches <- Filter(function(x) !is.null(x), score_matches) #Removing any null elements from list
  return (score_matches) #Returning a list of lists containing the positions in list1, and the values correspond to the rows with matching DOI in list2
}


update_db2_score <- function(db1, db2, score_match_list) {
  for (match in score_match_list) {
    db1_index <- match$db1_id
    db2_index <- match$db2_id
    score <- match$score
    db2[db2_index,'score'] <- score #Updating the score value of the record 
    db2[db2_index,'UUID'] <- db1[db1_index,'UUID'] #Inheriting uuid from db1's matching record 
  }
  return(db2)
}


################################ FINAL MATCHING ################################

db_analyses <- function(db_list, db_order = names(db_list)) {
  combs <- combn(db_order, 2) #Getting ordered db pairwise combinations
  doi_matches <- list() 
  score_matches <- list()
  for (i in 1:ncol(combs)) { #For each pairwise combination of databases, we'll match the documents based on DOI and score, and then modify some fields in db2
    db1_name <- combs[1,i] #db1 name for current pairwise combination
    db2_name <- combs[2,i] #db2 name for current pairwise combination
    comb_name <- paste0(db1_name,'_',db2_name) #Name of combination (db1_db2) - Will be used to identify each list of matches
    db1 <- db_list[[db1_name]] #Getting db1 by name from the db_list
    db2 <- db_list[[db2_name]] #Getting db2 by name from the db_list
    print(paste('Matching by DOI for pair', comb_name))
    doi_matches[[comb_name]] <- parallel_doi_match(db1, db2) #Obtaining matches by DOI
    print(paste('Matching by SCORE for pair', comb_name))
    score_matches[[comb_name]] <- parallel_score_match(db1, db2) #Obtaining matches by score
    print('Updating matched documents in db2')
    db_list[[db2_name]] <- update_db2_doi(db1, db2, doi_matches[[comb_name]]) #Saving modified db2 with doi matches to db_list
    db_list[[db2_name]] <- update_db2_score(db1, db2, score_matches[[comb_name]]) #Saving modified db2 with score matches to db_list
  }
  return (list(db_list = db_list, 
              doi_matches = doi_matches,
              score_matches = score_matches)) #Returning db_list, doi and score matches
}


dbs_coverage_sample <- lapply(dbs_coverage, function(df) {
  df %>% slice_max(TC, n = 10000, with_ties = FALSE)
})


test <- db_analyses(dbs_coverage_sample)

View(test$doi_matches)
View(test$score_matches)

#final_test <- db_analyses(dbs_coverage_clean)


View(test$wos)


uuid <- lapply(test, function(db) db$UUID)

ggVennDiagram(uuid)

