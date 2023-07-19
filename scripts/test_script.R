library(Matrix)
library(parallel)

options(mc.cores = detectCores() ) 


#Function to test if a vector has any NA values in it
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
  if ( row1['score'] < 1 & row2['score'] < 1 ) { #Only calculate score if rows analyzed did not find a match before
    doi <- ifelse( test_na(c(row1['DI'], row2['DI'])) || row1['DI'] != row2['DI'], 0, 1 ) #Score for publication DOI 
    title <- ifelse( test_na(c(row1['TI'], row2['TI'])), 0, 
                     pmax(0.6 - adist(row1['TI'], row2['TI'])[1] **2 * 0.01, 0) ) #Score for publication title - adist() calculates levenshtein distance, while pmax replaces negative values with 0
    if (doi + title < 0.1) { #if doi+title is <0.1, there's no way to reach the threshold (1), so we'll save some processing power and just go to the next document
     return( 0 ) 
    }
    year <-ifelse( test_na(c(row1['PY'], row2['PY'])) || row1['PY'] != row2['PY'], 0, 0.3 ) #Score for publication year
    source <- ifelse( test_na(c(row1['J9'], row2['J9'])), 0, 
                      pmax(0.3 - adist(row1['J9'], row2['J9'])[1]*0.1, 0) ) #Score for publication source
    author <- ifelse( test_na(c(row1['AU'], row2['AU'])), 0, 
                      pmax(0.3 - adist(row1['AU'], row2['AU'])[1]*0.1, 0)  )#Score for publication author
    final_score <- doi + title + year + source + author
    return(final_score)
  } else { 0 }
}

get_document_match <- function(row1, db2, score_cutoff = 1) {
                          scores <- apply(db2, MARGIN = 1, function(row2) { calc_score(row1, row2) }) #Caculates the score 
                          scores <- unname(scores) #Removing names from the resulting vector
                          max_score <- max(scores, na.rm = TRUE) #Getting value of (first) maximum obtained score. Will return warning if no match is found (scores vector will have only NAs)
                          if (max_score >= score_cutoff) { #If max_score is higher than cutoff (default = 1)
                            max_index <- which.max(scores) #Getting the index of the row with the highest score
                            db2[max_index, 'score'] <- max_score
                            db2[max_index, 'UUID'] <- row1['UUID']
                          }
                          return( db2 )
}


compare_dbs <- function(db1, db2) {
  for (i in 1:nrow(db1)) {
    row1 <- db1[i, ]
    db2 <- get_document_match(row1, db2) 
    }
  return (db2)
}

#system.time(test <- lapply(1:nrow(dbs_coverage$lens), function(j) calc_score(dbs_coverage$dimensions[1,], dbs_coverage$lens[j,]) ))
#
#system.time(test <- mclapply(1:nrow(dbs_coverage$lens), function(j) calc_score(dbs_coverage$dimensions[1,], dbs_coverage$lens[j,]) ))

#View(test)


#system.time(test <- lapply(1:nrow(db1), function(i) {
#  mclapply(1:nrow(db2), function(j) {
#    calc_score(db1[i,], db2[j,]) 
#    })
#}))

#Function that calculates a score matrix based on the 'calc_score' function for each combination of rows between db1 and db2
#get_score_matrix <- function(db1, db2) {
#  scores <- lapply(1:nrow(db1), function(i) { #First laplly that goes over each row of db1
#    print(paste("Starting comparison for row", i))
#    row1 <- db1[i,]
#    lapply(1:nrow(db2), function(j) { #Second lapply that goes over each row of db2 - using parallel processing
#      row2 <- db2[j,]
#      calc_score(row1, row2) #Calculates score for current rows
#      })
#  }) 
#  
#  score_matrix <- matrix(unlist(scores), nrow = length(scores), byrow = TRUE) #Creating a matrix based on the list of lists present in the 'scores' variable
#  return (score_matrix) #Returning the score matrix
#}

get_score_matrix <- function(db1, db2, comb_name) {
  db1_nrows <- nrow(db1)
  db2_nrows <- nrow(db2)
  score_matrix <- Matrix(0, nrow = db1_nrows, ncol = db2_nrows, sparse = TRUE)
  for (i in 1:db1_nrows) { #This for goes over each row of db1
    print(paste(round(i/db1_nrows * 100, 2),"% (",i,"/",db1_nrows,") of rows processed for", comb_name))
    row1 <- db1[i,]
    scores <- foreach(i=1:db2_nrows) %dopar% {
      row2 <- db2[j,]
      calc_score(row1, row2)  } #This lapply goes over each row of db2 (using parallel processing) and calculates score for current rows
    for (j in seq_along(scores)) {
      if (scores[[j]] >= 1) {
        score_matrix[i, j] <- scores[[j]]
      }
    }
  }
  return ( score_matrix)
}


get_score_matrix(dbs_coverage$dimensions, dbs_coverage$lens, 'dimensions_lens')

get_score_matrix_mc_inner <- function(db1, db2, comb_name) {
  db1_nrows <- nrow(db1)
  db2_nrows <- nrow(db2)
  score_matrix <- Matrix(0, nrow = db1_nrows, ncol = db2_nrows, sparse = TRUE)
  cl <- makeCluster (16)
  clusterExport(cl, varlist = c("calc_score", "test_na"))  # Export 'db2' to the cluster workers
  clusterExport(cl, varlist = "db2", envir = environment())
  for (i in 1:db1_nrows) { #This for goes over each row of db1
    print(paste(round(i/db1_nrows * 100, 2),"% (",i,"/",db1_nrows,") of rows processed for", comb_name))
    row1 <- db1[i,]
    scores <- parLapply(cl, 1:nrow(db2), fun = function (x, row1) {
      calc_score(row1, db2[x,]) }, row1 = row1) #This lapply goes over each row of db2 (using parallel processing) and calculates score for current rows
    for (j in seq_along(scores)) {
      if (scores[[j]] >= 1) {
        score_matrix[i, j] <- scores[[j]] 
      }
    }
    }
  stopCluster(cl)
  return (score_matrix)
}

get_score_matrix_inner <- function(db1, db2, comb_name) {
  db1_nrows <- nrow(db1)
  db2_nrows <- nrow(db2)
  cl <- makeCluster (16)
  clusterExport(cl, varlist = c("calc_score", "test_na"))  # Export 'db2' to the cluster workers
  clusterExport(cl, varlist = "db2", envir = environment())
  score_matrix <- Matrix(0, nrow = db1_nrows, ncol = db2_nrows, sparse = TRUE)
  for (i in 1:db1_nrows) { #This for goes over each row of db1
    print(paste(round(i/db1_nrows * 100, 2),"% (",i,"/",db1_nrows,") of rows processed for", comb_name))
    row1 <- db1[i,]
    scores <- parLapply(cl, 1:nrow(db2), fun = function (x, row1) {
      calc_score(row1, db2[x,]) }, row1 = row1) #This lapply goes over each row of db2 (using parallel processing) and calculates score for current rows
    for (j in seq_along(scores)) {
      if (scores[[j]] >= 1) {
        score_matrix[i, j] <- scores[[j]] 
      }
    }
    }
  stopCluster(cl)
  return ( score_matrix)
}




get_scores <- function(row1, db2) {
  scores <- lapply(1:nrow(db2), function (x) calc_score(row1, db2[x,]) ) #This lapply goes over each row of db2 (using parallel processing) and calculates score for current rows
  return(scores)
}


get_score_matrix_inner <- function(db1, db2, comb_name) {
  db1_nrows <- nrow(db1)
  db2_nrows <- nrow(db2)
  score_rows <- list()
  for (i in 1:db1_nrows) { #This for goes over each row of db1
    print(paste(round(i/db1_nrows * 100, 2),"% (",i,"/",db1_nrows,") of rows processed for", comb_name))
    row1 <- db1[i,]
    scores <- foreach(i=1:db2_nrows) %dopar% {
      row2 <- db2[j,]
      calc_score(row1, row2)  } #This lapply goes over each row of db2 (using parallel processing) and calculates score for current rows
    score_rows[[i]] <- scores
    }
  return ( score_rows)
}

test <- matrix(0, nrow = nrow(dbs_coverage$dimensions), ncol = nrow(dbs_coverage$lens))

get_score_matrix_outer <- function(db1, db2, comb_name) {
  db1_nrows <- nrow(db1)
  db2_nrows <- nrow(db2)
  #score_matrix <- Matrix(0, nrow = db1_nrows, ncol = db2_nrows, sparse = TRUE)
  #score_matrix <- matrix(0, nrow = db1_nrows, ncol = db2_nrows)
  #cl <- makeCluster(16, outfile = '')
  #clusterExport(cl, varlist = c("get_scores", "calc_score", "test_na"))  # Export 'db2' to the cluster workers
  #clusterExport(cl, varlist = c("db1","db2","comb_name", "score_matrix"), envir = environment())
  score_lists <- foreach(i=1:nrow(db1), .combine = 'rbind') %dopar% {
  #score_lists <- parLapply(cl, 1:nrow(db1), function(i) {
    row1 <- db1[i,]
    print(paste(round(i/nrow(db1) * 100, 2),"% (",i,"/",nrow(db1),") of rows processed for", comb_name))
    scores <- lapply(1:nrow(db2), function(j) calc_score(row1, db2[j,]) ) #Generates a Matrix (?)
    scores
    #scores <- get_scores(row1, db2) #This lapply goes over each row of db2 (using parallel processing) and calculates score for current rows
    #return(scores)
  #}
  }
  #stopCluster(cl)
  return(score_lists)
}

inner_time <- system.time(test1 <- get_score_matrix_inner(dbs_coverage$dimensions[1:32,], dbs_coverage$lens, 'dimensions_lens'))
outer_time <- system.time(test2 <- get_score_matrix_outer(dbs_coverage$dimensions[1:32,], dbs_coverage$lens, 'dimensions_lens'))



test <- function() {
  score_matrix <- matrix(0, nrow = 100, ncol = 100)
  cl <- makeCluster(16, outfile = '')
  result <- parLapply(cl, 1:100, function(i) {
    test <- lapply(1:100, function(j) i * j)
    for (j in seq_along(test)) { 
    return(test)
      if (test[[j]] >= 100) { score_matrix[i,j] <- test[[j]] }
    }
  })
  stopCluster(cl)
  #return(score_matrix)
}

cl <- makeCluster(16, outfile = '')
test <- clusterApply(cl, 1:10, function(i) i**2)
stopCluster(cl)
typeof(test)

mytest <- test()
typeof(mytest)
View(mytest)

system.time(test <- get_scores(db1[2,], db2))
View(test)
typeof(test)

inner <- system.time(test_inner <- get_score_matrix_inner(db1, db2, "dimensions_lens"))
outer <- system.time(test_outer <- get_score_matrix_outer(db1, db2, "dimensions_lens"))
get_score_matrix_outer(db1, db2, "dimensions_lens")



typeof(test_outer)


stdout(get_score_matrix_outer(db1, db2, "dimensions_lens"))

test_outer[2,32]
length(test_outer)
for (j in 1:nrow(test_outer) ) { print(test_outer[j]) }


rename_score_matrix_dims <- function(db1_name, db2_name, score_matrix) {
  dimnames(score_matrix) <- list(paste(db1_name, 1:nrow(score_matrix), sep = '.' ),  #Renaming score matrix's rows
                             paste(db2_name, 1:ncol(score_matrix), sep = '.'))   #Renaming score matrix's columns    
  return(score_matrix)
}

update_db2_matches <- function(db1, db2, score_matrix) {
  for (i in 1:ncol(score_matrix)) { #For each column in the score matrix (which corresponds to the records in db2)
    max_score <- max(score_matrix[,i]) #Obtaining the (first) highest score avaliable for the db2 record
    if (max_score >= 1) { #If the highest score is equal or more than the cutoff (1), update the db2
      max_index <- which.max(score_matrix[,i]) #Getting row index of  highest score in matrix (which corresponds to row of the matching document in db1)
      db2[i,]$score <- max_score #Updating the score value of the record
      db2[i,]$UUID <- db1[max_index,]$UUID #Inheriting the uuid from the matching record in db2
    }
  }
  return( db2 )
}


db_analyses <- function(db_list, db_order = names(db_list)) {
  combs <- combn(db_order, 2) #Getting ordered db pairwise combinations
  score_matrices <- list() #Initializing a list to gather the score_matrices
  for (i in 1:ncol(combs)) { #For each pairwise combination of databases, we'll calculate a score matrix and modify some fields in db2
    db1_name <- combs[1,i] #db1 name for current pairwise combination
    db2_name <- combs[2,i] #db2 name for current pairwise combination
    comb_name <- paste0(db1_name,'_',db2_name) #Name of combination (db1_db2) - Will be used to identify each matrix
    db1 <- db_list[[db1_name]] #Getting db1 by name from the db_list
    db2 <- db_list[[db2_name]] #Getting db2 by name from the db_list
    score_matrix <- get_score_matrix(db1, db2, comb_name) #Calculating score_matrix
    db_list[[db2_name]] <- update_db2_matches(db1, db2, score_matrix) #Saving modified db2 to db_list
    score_matrices[[comb_name]] <- rename_score_matrix_dims(db1_name, db2_name, score_matrix) #Adding a renamed score matrix to the 'score_matrices' list
  }
  return (list('db_list' =  db_list, 'score_matrices' =  score_matrices)) #Returning both the modified db_list and score_matrices as elements of a list
}



stopCluster(cl)

dbs_coverage_sample <- lapply(dbs_coverage, function(df) {
  df %>% slice_max(TC, n = 100, with_ties = FALSE)
})

test <- db_analyses(dbs_coverage_sample)
View(test)
merged_df <- bind_rows(test$db_list)

View(test$db_list$lens)

View(merged_df %>%
  filter(duplicated(UUID) | duplicated(UUID, fromLast = TRUE) ) %>%
  arrange(UUID))



results <- db_analyses(dbs_coverage)


db1 <- dbs_coverage$dimensions
db2 <- dbs_coverage$lens

get_score_matrix(dbs_coverage_sample[[combs[1,1]]], dbs_coverage_sample[[combs[2,1]]])

typeof(test$`dimensions-lens`)

dbs_coverage_sample[[combs[1,1]]]
dbs_coverage_sample[combs[2,1]]

get_db_combs <- function(db1, db2) {
  
}


get_score_matrix_inner <- function(db1, db2) {
  db1_nrows <- nrow(db1)
  db2_nrows <- nrow(db2)
  score_matrix <- Matrix(0, nrow = db1_nrows, ncol = db2_nrows, sparse = TRUE)
  #for (i in 1:db1_nrows) { #This for goes over each row of db1
  for (i in 1:db1_nrows) { #This for goes over each row of db1
    print(paste("Starting comparison for row", i))
    row1 <- db1[i,]
    scores <- mclapply(1:db2_nrows, function(j) {
      row2 <- db2[j,]
      calc_score(row1, row2)  }) #This lapply goes over each row of db2 (using parallel processing) and calculates score for current rows
    for (j in seq_along(scores)) {
      if (scores[[j]] >= 1) {
        score_matrix[i, j] <- scores[[j]]
      }
    }
  }
  return (score_matrix)
}


library(foreach)
library(doParallel)
library(doMC)
registerDoParallel(cores=16)
registerDoMC(cores=16)

get_score_matrix_outer <- function(db1, db2) {
  db1_nrows <- nrow(db1)
  db2_nrows <- nrow(db2)
  score_matrix <- Matrix(0, nrow = db1_nrows, ncol = db2_nrows, sparse = TRUE)
  #for (i in 1:db1_nrows) { #This for goes over each row of db1
  mclapply(1:20, function(i) { #This goes over each row of db1
    print(paste("Starting comparison for row", i))
    row1 <- db1[i,]
    scores <- lapply(1:db2_nrows, function(j) {
      row2 <- db2[j,]
      calc_score(row1, row2)  }) #This lapply goes over each row of db2 (using parallel processing) and calculates score for current rows
    for (j in seq_along(scores)) {
      if (scores[[j]] >= 1) {
        score_matrix[i, j] <- scores[[j]]
      }
    }
  })
  return (score_matrix)
}



mclapp <- system.time(test1 <- get_score_matrix_inner(dbs_coverage$dimensions, dbs_coverage$lens))

foreach2 <- system.time(test3 <- get_score_matrix_outer(dbs_coverage$dimensions, dbs_coverage$lens))

mclapp
foreach

test1 == test2

#mclapply(1:nrow(db2), function(j) {
#    calc_score(db1[i,], db2[j,]) 
#    })
#  print(paste("Finished comparison for row", i))


View(test)
View(test_p)
View(test_f)

inner_mc <- system.time( no_mclapply <- lapply(1:20, function(i) {
  mclapply(1:nrow(dbs_coverage$dimensions), function(j) {
    calc_score(dbs_coverage$dimensions[i,], dbs_coverage$lens[j,])
  })
  print(paste("Finished", i))
}))

no_mc <- system.time( no_mclapply <- lapply(1:20, function(i) {
  lapply(1:nrow(dbs_coverage$dimensions), function(j) {
    calc_score(dbs_coverage$dimensions[i,], dbs_coverage$lens[j,])
  })
  print(paste("Finished", i))
}))

identical(test, test_p)


print()

identical(inner_mc, no_mc)
View(inner_mclapply)
View(no_mclapply)


no_mc['elapsed'] / inner_mc['elapsed']
  
nested_result

View(do.call(rbind, nested_result))


data <- list(a = 1, b = 2, c = 3)

result <- lapply(data, function(x) {
  invisible(print(x))
  x * 2
})

print(result)

