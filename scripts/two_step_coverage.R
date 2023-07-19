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
library(foreach)

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


#Updating db2 based on DOI matching
update_db2_doi <- function(db1, db2, comb_name) {
  db1_doi <- db1$DI #Extracting DOI column from db1
  db2_doi <- db2$DI #Extracting DOI column from db2
  matches <- parallel_doi_match(db1_doi, db2_doi)
  for (i in seq_along(matches)) {
    db2_index <- matches[[i]]
    print(paste(round(i/length(matches) * 100, 2),"% (",i,"/",length(matches),") of rows processed for", comb_name, " - ", db2_index))
    if ( !is.na(db2_index) ) { #If doi_match_index is not NA (in other words, if a matching DOI is found)
      db2[db2_index,'score'] <- 2 #Updating the score value of the db2 record to 2 (value could be changed later)
      db2[db2_index,'UUID'] <- db1[i,'UUID'] #db2 row receives the matching uuid from db1
    }
  }
  #return(list('matches' = matches, 'db2' = db2))
  return(db2)
}


#Function that receives two lists of DOIs and returns a list containing the DOI matches of list1 against list2
parallel_doi_match <- function(db1_doi, db2_doi, n_cores = detectCores()) {
  cl <- makeCluster(n_cores) #Starts the cluster for parallel computing (by default, uses all cores in the system)
  clusterExport(cl, varlist = c("db1_doi", "db2_doi"), envir = environment()) #Exporting the variables received by the function to the cluster
  matches <- parLapply(cl, 
                       seq_along(db1_doi), 
                       function (x) match(db1_doi[x], db2_doi, incomparables = c(NULL, NA, ''))  ) #Calculating which elements in list2 correspond to the DOIs in list1
  stopCluster(cl) #Stopping the cluster
  return(matches) #Returning a list where the indices correspond to position in list1, and the values correspond to the rows with matching DOI in list2
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
                     pmax(0.6 - adist(row1['TI'], row2['TI'])[1] **2 * 0.01, 0) ) #Score for publication title - adist() calculates levenshtein distance, while pmax replaces negative values with 0
    if (title >= 0.1) { #if title is >= 0.1, there's still a chance of matching, so we'll calculate the other score components.
      year <-ifelse( test_na(c(row1['PY'], row2['PY'])) || row1['PY'] != row2['PY'], 0, 0.3 ) #Score for publication year
      source <- ifelse( test_na(c(row1['J9'], row2['J9'])), 0, 
                        pmax(0.3 - adist(row1['J9'], row2['J9'])[1]*0.1, 0) ) #Score for publication source
      author <- ifelse( test_na(c(row1['AU'], row2['AU'])), 0, 
                        pmax(0.3 - adist(row1['AU'], row2['AU'])[1]*0.1, 0)  )#Score for publication author
      score <- title + year + source + author }
      return( list(db1_id = row1['index'], score = score, db2_id = row2['index']) ) 
}

View(dbs_coverage_clean$dimensions[is.na(dbs_coverage_clean$dimensions$DI),])


lapply(dbs_coverage_clean, function(db) db %>% count(is.na(DI)))

lapply(dbs_coverage, function(db) db %>% count(is.na(DI)))


subset_db_for_calc_score <- function(df) {
  df %>%  
    rownames_to_column(var = "index") %>%
    filter(is.na(DI) & score < 1) %>%
    select(index,TI,PY,J9,AU)
}

colnames(dbs_coverage$dimensions)

View(subset_db_for_calc_score(dbs_coverage$dimensions))


parallel_score_match_outer <- function(db1,db2, n_cores = detectCores()) {
  db1_subset <- subset_db_for_calc_score(db1)
  db2_subset <- subset_db_for_calc_score(db2)
  cl <- makeCluster(n_cores, outfile = '') #Starts the cluster for parallel computing (by default, uses all cores in the system)
  clusterExport(cl, varlist = c("calc_score", "test_na")) #Exporting the variables received by the function to the cluster
  clusterExport(cl, varlist = c("db1_subset", "db2_subset"), envir = environment()) #Exporting the variables received by the function to the cluster
  score_matches <- parLapply(cl, 1:nrow(db1_subset), function(i) {
      scores <- lapply(1:nrow(db2_subset), function(j) {
        calc_score(db1_subset[i,], db2_subset[j,])
      })
      return(scores)
  })
  stopCluster(cl) #Stopping the cluster
  return(score_matches) #Returning a list where the indices correspond to position in list1, and the values correspond to the rows with matching DOI in list2
}

test <- parallel_score_match_outer(db1, db2)

parallel_compare_dbs(dbs_coverage_clean$dimensions, dbs_coverage_clean$lens) 

parallel_score_match_inner <- function(db1, db2, n_cores = detectCores()) {
  db1_subset <- subset_db_for_calc_score(db1)
  db2_subset <- subset_db_for_calc_score(db2)
  cl <- makeCluster(n_cores) #Starts the cluster for parallel computing (by default, uses all cores in the system)
  clusterExport(cl, varlist = c("calc_score", "test_na")) #Exporting the variables received by the function to the cluster
  clusterExport(cl, varlist = c("db1_subset", "db2_subset"), envir = environment()) #Exporting the variables received by the function to the cluster
  for (i in 1:nrow(db1_subset)) {
      scores <- lapply(1:nrow(db2_subset), function(j) {
        calc_score(db1_subset[i,], db2_subset[j,])
      })
      return(scores)
  }
  stopCluster(cl) #Stopping the cluster
  return(score_matches) #Returning a list where the indices correspond to position in list1, and the values correspond to the rows with matching DOI in list2
}


db1_subset <- subset_db_for_calc_score(dbs_coverage$dimensions)
db2_subset <- subset_db_for_calc_score(dbs_coverage$lens)

test <- lapply(1:nrow(db2_subset), function(i) calc_score(db1_subset[1,], db2_subset[j,]))


max_index <- which.max(sapply(test, function(lst) lst$score))
max_index


View(subset_db_for_calc_score(dbs_coverage_clean$dimensions))

test2 <- parallel_score_match(dbs_coverage_clean$dimensions, dbs_coverage_clean$lens)
test2 <- update_db2_score(dbs_coverage_clean$dimensions, dbs_coverage_clean$lens)

update_db2_score <- function(db1,db2) {
  db1_subset <- subset_db_for_calc_score(db1)
  db2_subset <- subset_db_for_calc_score(db2)
  for (i in 1:nrow(db1_subset)) {
    print(paste("Running analysis for db1 row", db1_subset[i,'index']))
    if (need_calc_score(db1[i,]) == FALSE) { next }
    for (j in 1:nrow(db2_subset)) {
      if (need_calc_score(db2_subset[j,]) == FALSE) { next }
      score <- calc_score(db1_subset[i,], db2_subset[j,])
      if ( score$score >= 1 )  {
        db2[db2_subset[j,'index'],'score'] <- score
        db2[db2_subset[j,'index'],'UUID'] <- db1[db1_subset[i,'index'],'UUID']
        break
      }
    }
  }
  return(db2)
}

update_db2_score <- function(db1,db2) {
  db1_subset <- subset_db_for_calc_score(db1)
  db2_subset <- subset_db_for_calc_score(db2)
  for (i in 1:nrow(db1_subset)) {
    print(paste("Running analysis for db1 row", db1_subset[i,'index']))
    for (j in 1:nrow(db2_subset)) {
      score <- calc_score(db1_subset[i,], db2_subset[j,])
      if ( score >= 1 )  {
        db2[db2_subset[j,'index'],'score'] <- score
        db2[db2_subset[j,'index'],'UUID'] <- db1[db1_subset[i,'index'],'UUID']
        break
      }
    }
  }
  return(db2)
}

gc()

test2 <- update_db2_score(dbs_coverage_clean$wos, dbs_coverage_clean$lens)

View(test2[test2$score >= 1,])
    
dbs_coverage_clean$dimensions %>%
  rownames_to_column(var = 'index') %>%
  filter(is.na(DI) & score < 1)


update_db2_score <- function(db1,db2) {
  for (i in 1:nrow(db1)) {
    print(paste("Running analysis for db1 row", i))
    if (need_calc_score(db1[i,]) == FALSE) { next }
    for (j in 1:nrow(db2)) {
      if (need_calc_score(db2[j,]) == FALSE) { next }
      score <- calc_score(db1[i,], db2[j,])
      #print(paste('Calculating score for row1',i, 'and row2', j, "- Score: ", score))
      if ( score >= 1 )  {
        db2[j,'score'] <- score
        db2[j,'UUID'] <- db1[i,'UUID']
        break
      }
    }
  }
 return( db2 ) 
}

test2 <- update_db2_score(dbs_coverage_clean$dimensions, dbs_coverage_clean$lens)

parallel_score_match <- function(db1,db2, n_cores = detectCores()) {
  db1_subset <- subset_db_for_calc_score(db1)
  db2_subset <- subset_db_for_calc_score(db2)
  cl <- makeCluster(n_cores, outfile = '') #Starts the cluster for parallel computing (by default, uses all cores in the system)
  clusterExport(cl, varlist = c("calc_score", "test_na")) #Exporting the variables received by the function to the cluster
  clusterExport(cl, varlist = c("db1_subset", "db2_subset"), envir = environment()) #Exporting the variables received by the function to the cluster
  score_matches <- parLapply(cl, 1:nrow(db1_subset), function(i) {
    print(paste("Running analysis for row1", i))
    for (j in 1:nrow(db2_subset)) {
      calc_score_list <- calc_score(db1_subset[i,], db2_subset[j,])
      if ( calc_score_list$score >= 1 )  {
        return(list('db1_id' = calc_score_list$db1_id, 
                    'score' = calc_score_list$score, 
                    'db2_id' = calc_score_list$db2_id))
      } else { next } 
    }
  })
  stopCluster(cl) #Stopping the cluster
  return(score_matches) #Returning a list where the indices correspond to position in list1, and the values correspond to the rows with matching DOI in list2
}
parallel_score_match <- function(db1,db2, n_cores = detectCores()) {
  db1_subset <- subset_db_for_calc_score(db1)
  db2_subset <- subset_db_for_calc_score(db2)
  cl <- makeCluster(n_cores, outfile = '') #Starts the cluster for parallel computing (by default, uses all cores in the system)
  clusterExport(cl, varlist = c("calc_score", "test_na")) #Exporting the variables received by the function to the cluster
  clusterExport(cl, varlist = c("db1_subset", "db2_subset"), envir = environment()) #Exporting the variables received by the function to the cluster
  score_matches <- parLapply(cl, 1:nrow(db1_subset), function(i) {
    print(paste("Running analysis for row1", i))
    for (j in 1:nrow(db2_subset)) {
      calc_score_list <- calc_score(db1_subset[i,], db2_subset[j,])
      if ( calc_score_list$score >= 1 )  {
        return(list('db1_id' = calc_score_list$db1_id, 
                    'score' = calc_score_list$score, 
                    'db2_id' = calc_score_list$db2_id))
      } else { next } 
    }
  })
  stopCluster(cl) #Stopping the cluster
  return(score_matches) #Returning a list where the indices correspond to position in list1, and the values correspond to the rows with matching DOI in list2
}


parallel_score_match_lb <- function(db1,db2, n_cores = detectCores()) {
  db1_subset <- subset_db_for_calc_score(db1)
  db2_subset <- subset_db_for_calc_score(db2)
  cl <- makeCluster(n_cores, outfile = '') #Starts the cluster for parallel computing (by default, uses all cores in the system)
  clusterExport(cl, varlist = c("calc_score", "test_na")) #Exporting the variables received by the function to the cluster
  clusterExport(cl, varlist = c("db1_subset", "db2_subset"), envir = environment()) #Exporting the variables received by the function to the cluster
  score_matches <- parLapplyLB(cl, 1:nrow(db1_subset), function(i) {
    print(paste("Running analysis for row1", i))
    for (j in 1:nrow(db2_subset)) {
      calc_score_list <- calc_score(db1_subset[i,], db2_subset[j,])
      if ( calc_score_list$score >= 1 )  {
        return(list('db1_id' = calc_score_list$db1_id, 
                    'score' = calc_score_list$score, 
                    'db2_id' = calc_score_list$db2_id))
      } else { next } 
    }
  })
  stopCluster(cl) #Stopping the cluster
  return(score_matches) #Returning a list where the indices correspond to position in list1, and the values correspond to the rows with matching DOI in list2
}


parallel_score_match_break <- function(db1,db2, n_cores = detectCores()) {
  db1_subset <- subset_db_for_calc_score(db1)
  db2_subset <- subset_db_for_calc_score(db2)
  cl <- makeCluster(n_cores, outfile = '') #Starts the cluster for parallel computing (by default, uses all cores in the system)
  clusterExport(cl, varlist = c("calc_score", "test_na")) #Exporting the variables received by the function to the cluster
  clusterExport(cl, varlist = c("db1_subset", "db2_subset"), envir = environment()) #Exporting the variables received by the function to the cluster
  score_matches <- parLapply(cl, 1:nrow(db1_subset), function(i) {
    print(paste("Running analysis for row1", i))
    found_match <- FALSE  # Initialize a flag variable
    for (j in 1:nrow(db2_subset)) {
      calc_score_list <- calc_score(db1_subset[i,], db2_subset[j,])
      if ( calc_score_list$score >= 1 )  {
        match <- list('db1_id' = calc_score_list$db1_id, 
                    'score' = calc_score_list$score, 
                    'db2_id' = calc_score_list$db2_id)
        found_match <- TRUE
        break #Exits the for loop
      }
    }
    if (!found_match) return(NULL)
    return(match)
  })
  stopCluster(cl) #Stopping the cluster
  return(score_matches) #Returning a list where the indices correspond to position in list1, and the values correspond to the rows with matching DOI in list2
}

parallel_score_match_break_lb <- function(db1,db2, n_cores = detectCores()) {
  db1_subset <- subset_db_for_calc_score(db1)
  db2_subset <- subset_db_for_calc_score(db2)
  cl <- makeCluster(n_cores, outfile = '') #Starts the cluster for parallel computing (by default, uses all cores in the system)
  clusterExport(cl, varlist = c("calc_score", "test_na")) #Exporting the variables received by the function to the cluster
  clusterExport(cl, varlist = c("db1_subset", "db2_subset"), envir = environment()) #Exporting the variables received by the function to the cluster
  score_matches <- parLapplyLB(cl, 1:nrow(db1_subset), function(i) {
    print(paste("Running analysis for row1", i))
    found_match <- FALSE  # Initialize a flag variable
    for (j in 1:nrow(db2_subset)) {
      calc_score_list <- calc_score(db1_subset[i,], db2_subset[j,])
      if ( calc_score_list$score >= 1 )  {
        match <- list('db1_id' = calc_score_list$db1_id, 
                    'score' = calc_score_list$score, 
                    'db2_id' = calc_score_list$db2_id)
        found_match <- TRUE
        break #Exits the for loop
      }
    }
    if (!found_match) return(NULL)
    return(match)
  })
  stopCluster(cl) #Stopping the cluster
  return(score_matches) #Returning a list where the indices correspond to position in list1, and the values correspond to the rows with matching DOI in list2
}


parallel_score_match_optimized <- function(db1,db2, n_cores = detectCores()) {
  db1_subset <- subset_db_for_calc_score(db1) %>% mutate(matched = NA)
  db2_subset <- subset_db_for_calc_score(db2) %>% mutate(matched = NA)
  cl <- makeCluster(n_cores, outfile = '') #Starts the cluster for parallel computing (by default, uses all cores in the system)
  clusterExport(cl, varlist = c("calc_score", "test_na")) #Exporting the variables received by the function to the cluster
  clusterExport(cl, varlist = c("db1_subset", "db2_subset"), envir = environment()) #Exporting the variables received by the function to the cluster
  score_matches <- parLapply(cl, 1:nrow(db1_subset), function(i) {
    print(paste("Running analysis for row1", i))
    found_match <- FALSE  # Initialize a flag variable
    for (j in 1:nrow(db2_subset)) {
      if (!is.na(db2_subset[j,'matched'])) { next }
      calc_score_list <- calc_score(db1_subset[i,], db2_subset[j,])
      if ( calc_score_list$score >= 1 )  {
        match <- list('db1_id' = calc_score_list$db1_id, 
                    'score' = calc_score_list$score, 
                    'db2_id' = calc_score_list$db2_id)
        db2_subset[j,'matched'] <- 1
        found_match <- TRUE
        break #Exits the for loop
      }
    }
    if (!found_match) return(NULL)
    return(match)
  })
  stopCluster(cl) #Stopping the cluster
  return(score_matches) #Returning a list where the indices correspond to position in list1, and the values correspond to the rows with matching DOI in list2
}

parallel_score_match_optimized_lb <- function(db1,db2, n_cores = detectCores()) {
  db1_subset <- subset_db_for_calc_score(db1) %>% mutate(matched = NA)
  db2_subset <- subset_db_for_calc_score(db2) %>% mutate(matched = NA)
  cl <- makeCluster(n_cores, outfile = '') #Starts the cluster for parallel computing (by default, uses all cores in the system)
  clusterExport(cl, varlist = c("calc_score", "test_na")) #Exporting the variables received by the function to the cluster
  clusterExport(cl, varlist = c("db1_subset", "db2_subset"), envir = environment()) #Exporting the variables received by the function to the cluster
  score_matches <- parLapplyLB(cl, 1:nrow(db1_subset), function(i) {
    print(paste("Running analysis for row1", i))
    found_match <- FALSE  # Initialize a flag variable
    for (j in 1:nrow(db2_subset)) {
      if (!is.na(db2_subset[j,'matched'])) { next }
      calc_score_list <- calc_score(db1_subset[i,], db2_subset[j,])
      if ( calc_score_list$score >= 1 )  {
        match <- list('db1_id' = calc_score_list$db1_id, 
                    'score' = calc_score_list$score, 
                    'db2_id' = calc_score_list$db2_id)
        db2_subset[j,'matched'] <- 1
        found_match <- TRUE
        break #Exits the for loop
      }
    }
    if (!found_match) return(NULL)
    return(match)
  })
  stopCluster(cl) #Stopping the cluster
  return(score_matches) #Returning a list where the indices correspond to position in list1, and the values correspond to the rows with matching DOI in list2
}


gc()


ttime1 <- system.time(test1 <- parallel_score_match(dbs_coverage_clean$dimensions, dbs_coverage_clean$lens))

ttime2 <- system.time(test2 <- parallel_score_match_break(dbs_coverage_clean$dimensions, dbs_coverage_clean$lens))

ttime3 <- system.time(test3 <- parallel_score_match_optimized(dbs_coverage_clean$dimensions, dbs_coverage_clean$lens))


ttime4 <- system.time(test4 <- parallel_score_match_lb(dbs_coverage_clean$dimensions, dbs_coverage_clean$lens))

ttime5 <- system.time(test2 <- parallel_score_match_break_lb(dbs_coverage_clean$dimensions, dbs_coverage_clean$lens))

ttime6 <- system.time(test3 <- parallel_score_match_optimized_lb(dbs_coverage_clean$dimensions, dbs_coverage_clean$lens))

ttime1
ttime2
ttime3
ttime4
ttime5
ttime6

View(test2)

View(Filter(Negate(is.null), test2))

View(test2)

stopCluster(cl)

View(test2[test2$score >= 1,])


dbs_coverage_clean$lens[dbs_coverage_clean$lens$score >= 1,]


dbs_coverage_no_doi <- lapply(dbs_coverage, function(db) db %>% filter(is.na(DI)) )


lapply(dbs_coverage_no_doi, nrow)

db_analyses <- function(db_list, db_order = names(db_list)) {
  combs <- combn(db_order, 2) #Getting ordered db pairwise combinations
  score_matrices <- list() #Initializing a list to gather the score_matrices
  for (i in 1:ncol(combs)) { #For each pairwise combination of databases, we'll calculate a score matrix and modify some fields in db2
    db1_name <- combs[1,i] #db1 name for current pairwise combination
    db2_name <- combs[2,i] #db2 name for current pairwise combination
    comb_name <- paste0(db1_name,'_',db2_name) #Name of combination (db1_db2) - Will be used to identify each matrix
    db1 <- db_list[[db1_name]] #Getting db1 by name from the db_list
    db2 <- db_list[[db2_name]] #Getting db2 by name from the db_list
    
    #score_matrix <- get_score_matrix(db1, db2, comb_name) #Calculating score_matrix
    #db_list[[db2_name]] <- update_db2_matches(db1, db2, score_matrix) #Saving modified db2 to db_list
    #score_matrices[[comb_name]] <- rename_score_matrix_dims(db1_name, db2_name, score_matrix) #Adding a renamed score matrix to the 'score_matrices' list
  }
  return (list('db_list' =  db_list, 'score_matrices' =  score_matrices)) #Returning both the modified db_list and score_matrices as elements of a list
}
