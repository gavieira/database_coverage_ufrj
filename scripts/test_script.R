
options(mc.cores = detectCores() - 2 ) 


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

system.time(test <- lapply(1:nrow(dbs_coverage$lens), function(j) calc_score(dbs_coverage$dimensions[1,], dbs_coverage$lens[j,]) ))

system.time(test <- mclapply(1:nrow(dbs_coverage$lens), function(j) calc_score(dbs_coverage$dimensions[1,], dbs_coverage$lens[j,]) ))

View(test)


system.time(test <- lapply(1:nrow(db1), function(i) {
  mclapply(1:nrow(db2), function(j) {
    calc_score(db1[i,], db2[j,]) 
    })
}))

#Function that calculates a score matrix based on the 'calc_score' function for each combination of rows between db1 and db2
get_score_matrix <- function(db1,db2) {
  scores <- lapply(1:nrow(db1), function(i) { #First laplly that goes over each row of db1
    print(paste("Starting comparison for row", i))
    lapply(1:nrow(db2), function(j) { #Second lapply that goes over each row of db2 
      calc_score(db1[i,], db2[j,]) #Calculates score for current rows
      })
  }) 
  score_matrix <- do.call(rbind, scores) #Since lapply returns a list of lists, we need to unpack it with do.call, which will in turn pass it to the rbind function. In this case, rows refer to db1 records, while columns refer to db2's.
  
  #Setting row and col prefixes for score_matrix
  row_prefix <- unique(db1$DB)
  col_prefix <- unique(db2$DB)
  
  #Creating vectors with prefix+index
  row_names <- paste(row_prefix, 1:nrow(score_matrix), sep = '.' )
  col_names <- paste(col_prefix, 1:ncol(score_matrix), sep = '.')
  
  #Finally, changing matrix row and column names
  dimnames(score_matrix) <- list(row_names, col_names)
  
  return ( score_matrix)

}


db_order <- c('dimensions', 'lens', 'scopus', 'wos')
dbs_coverage_sample <- lapply(dbs_coverage, function(df) {
  df %>% slice_max(TC, n = 100, with_ties = FALSE)
})

combs <- combn(dbs, 2)
View(combs)

for (i in 1:ncol(combs)) {
  print(combs[1,i])
  print(combs[2,i])
}

combs[,1]
dbs_coverage[[combs[1,1]]]


db_analyses <- function(db_list, db_order) {
  combs <- combn(db_order, 2)
  score_matrices <- list()
  for (i in 1:ncol(combs)) {
    db1_name <- combs[1,i]
    db2_name <- combs[2,i]
    comb_name <- paste0(db1_name,'-',db2_name)
    print(comb_name)
    db1 <- db_list[[db1_name]]
    db2 <- db_list[[db2_name]]
    score_matrices[[comb_name]] <- get_score_matrix(db1, db2)
  }
  return (score_matrices)
}

test <- db_analyses(dbs_coverage_sample, db_order)
View(test)
View(test$`dimensions-lens`)

get_score_matrix(dbs_coverage_sample[[combs[1,1]]], dbs_coverage_sample[[combs[2,1]]])

dbs_coverage_sample[[combs[1,1]]]
dbs_coverage_sample[combs[2,1]]

get_db_combs <- function(db1, db2) {
  
}






mclapply(1:nrow(db2), function(j) {
    calc_score(db1[i,], db2[j,]) 
    })
  print(paste("Finished comparison for row", i))
})


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

