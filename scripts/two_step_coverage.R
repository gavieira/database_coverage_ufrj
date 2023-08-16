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

library(tidyverse)
library(parallel)
library(ggVennDiagram)
library(ComplexUpset)
library(patchwork)
library(gridExtra)


############################## FIRST STEP #####################################


#Quick cleaning of duplicated DOIs
dbs_coverage_clean <- lapply(dbs_coverage, function(db) {
  db %>% 
    filter(!duplicated(DI, incomparables = NA)) %>% #Keeping only unique DOIs
    #rownames_to_column(var = 'index') %>% #Adding a column with the index of row
  mutate( DT = case_when( 
      DT %in% c('ARTICLE', 'JOURNAL ARTICLE', 'REVIEW', 'ARTICLE; EARLY ACCESS', 'REVIEW; EARLY ACCESS', 
                     'ARTICLE; DATA PAPER', 'ARTICLE; RETRACTED PUBLICATION', 
                     'ARTICLE; DATA PAPER; EARLY ACCESS', 'REPRINT') ~ 'Articles',
      DT %in% c('PROCEEDING', 'CONFERENCE PROCEEDINGS ARTICLE', 'CONFERENCE PROCEEDINGS', 
                     'CONFERENCE PAPER', 'CONFERENCE REVIEW', 'PROCEEDINGS PAPER', 'MEETING ABSTRACT',
                     'ARTICLE; PROCEEDINGS PAPER') ~ 'Proceedings itens', 
      DT %in% c('BOOK', 'BOOK CHAPTER', 'CHAPTER', 'ARTICLE; BOOK CHAPTER', 'REVIEW; BOOK CHAPTER') ~ 'Books and book chapters', #Does not include book reviews
      DT %in%  c('', NA) ~ 'Unidentified', 
      DT %in%  c('PREPRINT') ~ 'Preprint',
      .default = 'Other')) #Adding a last value to aggregate all other doctypes
  } )
  

unique(dbs_coverage_clean$scopus$DT)

#Normalize publication type field


#Checking number of records in cleaned/uncleaned dataframes
dim(dbs_coverage$dimensions)
dim(dbs_coverage_clean$dimensions)

#Checking if DOI column has no duplicates in cleaned/uncleaned dataframes
lapply(dbs_coverage, function(db) any(duplicated(db$DI, na.rm = TRUE)) )
lapply(dbs_coverage_clean, function(db) any(duplicated(db$DI, na.rm = TRUE)) )


subset_db_for_doi_match <- function(df) {
  df %>%  
    #rownames_to_column(var = "index") %>%
    filter(!is.na(DI) & score < 1) %>%
    select(index,DI, TI)
}


test <- subset_db_for_doi_match(dbs_coverage_clean$wos)
test2 <- subset_db_for_doi_match(dbs_coverage_clean$scopus)

View(test)
View(test2)

View(test %>% filter(index == 4))
View(test2 %>% filter(index == 32020))

View(dbs_coverage_clean$scopus[32020,])

match(test[32,'DI'], test2$DI, incomparables = c(NULL, NA, ''))


test[2,]
test2[41977,]

dbs_coverage_clean$dimensions[2,]
dbs_coverage_clean$lens[52466,]

#Function that receives two lists of DOIs and returns a list containing the DOI matches of list1 against list2
#We'll also give the DOI matching a "fake score" that will be used to identify which rows have been matched in previous pairwise comparisons
parallel_doi_match <- function(db1, db2, n_cores = detectCores(), doi_score = 2) {
  subset_db1 <- subset_db_for_doi_match(db1) #Extracting DOI info from db1
  subset_db2 <- subset_db_for_doi_match(db2) #Extracting DOI info from db2
  cl <- makeCluster(n_cores) #Starts the cluster for parallel computing (by default, uses all cores in the system)
  #cl <- makeCluster(n_cores, outfile = '') #Starts the cluster for parallel computing (by default, uses all cores in the system)
  clusterExport(cl, varlist = c("subset_db1", "subset_db2", "doi_score"), envir = environment()) #Exporting the variables received by the function to the cluster
  doi_matches <- parLapplyLB(cl,  1:nrow(subset_db1),  function(i) {
    
                         match <- match(subset_db1[i, 'DI'], subset_db2$DI, incomparables = c(NULL, NA, ''))  
                         if (!is.na(match)) {
                           return( list('db1_id' = as.numeric(subset_db1[i, 'index']),
                                     'score' = doi_score,
                                     'db2_id' = as.numeric(subset_db2[match, 'index'])) ) }
                         }) #Calculating which elements in list2 correspond to the DOIs in list1
  stopCluster(cl) #Stopping the cluster
  doi_matches <- Filter(function(x) !is.null(x), doi_matches) #Removing any null elements from list
  return(doi_matches) #Returning a list where the indices correspond to position in list1, and the values correspond to the rows with matching DOI in list2
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
    #rownames_to_column(var = "index") %>%
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
  cl <- makeCluster(n_cores) #Starts the cluster for parallel computing (by default, uses all cores in the system)
  #cl <- makeCluster(n_cores, outfile = '') #Starts the cluster for parallel computing (by default, uses all cores in the system)
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


################################ FINAL MATCHING ################################


update_db2_matches <- function(db1, db2, match_list) {
  for (match in match_list) {
    db1_index <- match$db1_id
    db2_index <- match$db2_id
    score <- match$score
    db2[db2_index,'score'] <- score #Updating the score value of the record 
    db2[db2_index,'UUID'] <- db1[db1_index,'UUID'] #Inheriting uuid from db1's matching record 
  }
  return(db2)
}

#dbs_coverage_clean$lens <- NULL

View(dbs_coverage_clean)

db_analyses <- function(db_list, db_order = names(db_list)) {
  db_list <- lapply(db_list, function(db) db %>% rownames_to_column(var = 'index') ) #Creating column that will keep the index of the original db row even when splitting the data for doi and score matching
  combs <- combn(db_order, 2) #Getting ordered db pairwise combinations
  matches <- list()
  for (i in 1:ncol(combs)) { #For each pairwise combination of databases, we'll match the documents based on DOI and score, and then modify some fields in db2
    db1_name <- combs[1,i] #db1 name for current pairwise combination
    db2_name <- combs[2,i] #db2 name for current pairwise combination
    comb_name <- paste0(db1_name,'_',db2_name) #Name of combination (db1_db2) - Will be used to identify each list of matches
    db1 <- db_list[[db1_name]] #Getting db1 by name from the db_list
    db2 <- db_list[[db2_name]] #Getting db2 by name from the db_list
    print(paste('Matching by DOI for pair', comb_name))
    doi_matches <- parallel_doi_match(db1, db2) #Obtaining matches by DOI
    print(paste('Matching by SCORE for pair', comb_name))
    score_matches <- parallel_score_match(db1, db2) #Obtaining matches by score
    print('Updating matched documents in db2')
    #matches[[comb_name]] <- c(doi_matches, score_matches)
    all_matches <- c(doi_matches, score_matches)
    matches[[comb_name]] <- lapply(all_matches, function(lst) { 
      lst$db1 <- db1_name 
      lst$db2 <- db2_name
      return(lst) } )
    db_list[[db2_name]] <- update_db2_matches(db1, db2, matches[[comb_name]]) #Saving modified db2 to db_list
  }
  return (list(db_list = db_list, 
              matches = matches)) #Returning db_list and matches
}

View(test$db_list$wos %>% 
  filter(is.na(TI)) )

dbs_coverage_sample <- lapply(dbs_coverage_clean, function(df) {
  df %>% slice_max(TC, n = 10000, with_ties = FALSE)
})

test <- db_analyses(dbs_coverage_sample)

View(update_db2_matches(dbs_coverage_sample$dimensions, dbs_coverage_sample$lens, test$matches$dimensions_lens))


View(test$matches$dimensions_lens)





matches_df_subset <- bind_rows(test$db_list) %>%
  filter(duplicated(UUID) | duplicated(UUID, fromLast = TRUE)) %>%
  #filter(is.na(DI)) %>%
  arrange(UUID)

View(matches_df_subset)  

View(head(dbs_coverage_sample))

View(test$matches)

test <- db_analyses(dbs_coverage_sample)
test <- db_analyses(dbs_coverage_clean, db_order = c('wos', 'scopus', 'dimensions','lens'))
View(test)

#Establishing publication pairs across databases (takes a long time)

results_time <- system.time(results <- db_analyses(dbs_coverage_clean))


dbs_coverage_clean$scopus


View(results)

results_time

#Saving results into a .rds file
saveRDS(results, file = 'output/data/db_coverage.rds')

#Loading results from rds file
results <- readRDS('output/data/db_coverage.rds')


View(results$db_list)

dbs_overlap <- results$db_list
matches <- results$matches

View(matches)
View(test$matches)

uuid <- lapply(dbs_overlap, function(db) db$UUID)

View(uuid)

#Different palletes
ggVennDiagram(uuid, edge_size = 5) + scale_fill_gradient(low = "#0073E6", high = "#001F3F")
ggVennDiagram(uuid, edge_size = 5) + scale_fill_gradient(low = "#89CFF0", high = "#08306B")
ggVennDiagram(uuid, edge_size = 5) + scale_fill_gradient(low = "#87CEEB", high = "#08306B")
ggVennDiagram(uuid, edge_size = 5) + scale_fill_gradient(low = "#A7C7E7", high = "#08306B")
ggVennDiagram(uuid, edge_size = 5) + scale_fill_gradient(low = "#C6DBEF", high = "#001F3F") #This one, maybe?
ggVennDiagram(uuid, edge_size = 5) + scale_fill_gradient(low = "lightblue", high = "#08306B")
ggVennDiagram(uuid) + labs(title = "All")

library(ComplexHeatmap)

m = make_comb_mat(uuid)
UpSet(m,
      top_annotation = upset_top_annotation(m, add_numbers = TRUE, height = unit(15, "cm")),
      right_annotation = upset_right_annotation(m, add_numbers = TRUE),
      comb_order = rev(order(comb_size(m))) )

UpSet(m, comb_order = rev(order(comb_size(m))) )

UpSet(m, top_annotation = 
        HeatmapAnnotation(
          "Intersection size" = anno_barplot(
            comb_size(m), 
            border = FALSE, 
            gp = gpar(fill = "black"), 
            height = unit(10, "cm"),
            axis_param = list(side = "right")
          ), 
          annotation_name_side = "right", 
          annotation_name_rot = 0),  )  



#Making plots for each document type

unique(results$db_list$lens$DT)


doctypes <- c("All", "Articles", "Books and book chapters", "Proceedings itens", "Preprint", "Other")

venn_by_doctype <- function(db_list, doctype = "All" ) {
  if (doctype != 'All') {
    db_list <- lapply(db_list, function(db)  db %>% filter(DT == doctype) )
    } #Filtering records for a given doctype (if necessary)
  uuid_list <- lapply(db_list, function(db) db$UUID) #Extracting UUID list for each base
  uuid_list <- discard(uuid_list, function(x) length(x) == 0)
  #return(uuid_list)
  return(ggVennDiagram(uuid_list) + 
          scale_fill_gradient(low = "#A7C7E7", high = "#08306B") +
          scale_x_continuous(expand = expansion(mult = .2)) + 
          guides(fill='none') +
          labs(title = doctype) +
          theme(plot.title = element_text(size = 20, hjust = 0.5))) #Returning ggplot venn diagram
  }
  

venn_diagrams <- function(db_list, doctypes) {
 venn <- lapply(doctypes, venn_by_doctype, db_list = db_list)
 names(venn) <- doctypes
 return(venn)
}

test <- venn_diagrams(results$db_list, doctypes)

View(test)

test$Unidentified

test$Preprint
ggVennDiagram(uuid)

ggVennDiagram(test[['Articles']])


library(gridExtra)

grid.arrange(grobs = test, ncol = 3)


View(test$Preprint)

View(discard(test$Preprint, function(x) length(x$UUID) == 0))


generate_dt_plots


articles <- filter_by_doctype(results$db_list, doctype = 'Articles')

uuid <- lapply(articles, function(db) db$UUID)
ggVennDiagram(uuid)

matches_df_subset <- bind_rows(test$db_list) %>%
  filter(duplicated(UUID) | duplicated(UUID, fromLast = TRUE)) %>%
  filter(is.na(DI)) %>%
  arrange(UUID)
View(matches_df_subset)  

View(test$db_list$wos)


matches_df <- bind_rows(results$db_list) %>%
  filter(duplicated(UUID) | duplicated(UUID, fromLast = TRUE)) %>%
  #filter(is.na(DI)) %>%
  arrange(UUID)

View(matches_df)  

length(unique(matches_df$J9))


View(test$matches)

#dbs_coverage_sample$dimensions[1,]
#dbs_coverage_sample$lens[9793,]
#dbs_coverage_sample$scopus[489,]
#dbs_coverage_sample$wos[35669,]


View(dbs_coverage_clean$dimensions)

dbs_coverage_clean$dimensions[4,]
dbs_coverage_clean$lens[89797,]
dbs_coverage_clean$scopus[29757,]
dbs_coverage_clean$wos[41512,]

View(results)


results$db_list$dimensions[4,]
results$db_list$lens[89797,]

View(dbs_coverage_clean$dimensions %>% 
       rownames_to_column('index') %>%
       filter(DI == '10.1140/EPJC/S10052-020-7907-9'))
View(dbs_coverage_clean$wos %>% 
       rownames_to_column('index') %>%
       filter(DI == '10.1140/EPJC/S10052-020-7907-9'))

subset_db_for_score_match <- function(df) {
  df %>%  
    rownames_to_column(var = "index") %>%
    filter(is.na(DI) & score < 1) %>%
    select(index,TI,PY,J9,AU)
}

scopus_df <- dbs_coverage_clean$scopus
wos_df <- dbs_coverage_clean$wos






View(test$wos)

View(test$db_list$wos)


uuid <- lapply(results$db_list, function(db) db$UUID)
ggVennDiagram(uuid)

# Save the plot to a PDF file
pdf("output/plots/db_coverage.pdf", width = 1920, height = 1080)

ggVennDiagram(uuid)

# Print the plot to the PDF file
dev.off()


#test <- bind_rows(lapply(dfs, head, n = 5))

View(test)
View(head(dfs$scopus, n = 2000))


doi_venn <- lapply(dbs_coverage_clean, function(db) {
  filtered <- db %>%
    filter(!is.na(DI))
  return(filtered$DI)
})

ggVennDiagram(doi_venn)

doi_percentage <- lapply(dbs_coverage_clean, function(db) {
  db %>%
    mutate(DOI = case_when(is.na(DI) ~ 'no_DOI',
                           .default = 'DOI')) %>%
    select(DOI)
  
})

bind_rows(dbs_coverage_clean, .id = 'db') %>%
    mutate(DOI = case_when(is.na(DI) ~ 'absent',
                           .default = 'present')) %>%
  mutate(DOI = factor(DOI, levels = c('present','absent'))) %>%
  select(db, DOI) %>%
  group_by(db,DOI) %>%
  summarise(count = n()) %>%
  group_by(db) %>%
  mutate(perc = count / sum(count) * 100) %>%
  #ggplot(aes(x= perc, y = db, fill = DOI)) +
  ggplot(aes(y= perc, x = db, fill = DOI)) +
  geom_bar(stat = 'identity') + 
  geom_text(aes(label = round(perc, 2)), position = position_stack(vjust = 0.5))
  

  
matching_summary_df <- function(matched_db_list) {
  #Getting dataframes
  all_data <- bind_rows(matched_db_list)
  matched_data <-  all_data %>% 
    filter(duplicated(UUID)) %>% #Filtering only rows with duplicated UUID (docs with match)
    distinct(UUID, .keep_all = TRUE) #And then obtaining only one (the first) occurrence of each matched document
  #Getting values
  summary <- list()
  summary$total <- nrow(all_data)
  summary$unique <- nrow(all_data %>% distinct(UUID, .keep_all = TRUE))
  summary$duplicates <- summary$total - summary$unique
  summary$matched <- nrow(matched_data)
  summary$unmatched <- summary$unique - summary$matched
  summary$matched_doi <- nrow(matched_data %>% filter(!is.na(DI)))
  summary$matched_score <- nrow(matched_data %>% filter(is.na(DI)))
  #return(summary)
  #Getting dataframe
  categories <- c('total', 'unique/duplicates', 'unique/duplicates', 'unique', 'unique', 'matched', 'matched')
  doc_subset_levels <- c('total',  'duplicates', 'unique', 'unmatched', 'matched',  'matched_doi', 'matched_score' )
  summary_df <- data.frame(summary) %>%
           pivot_longer(cols = everything(), names_to = "doc_subset", values_to = "n_docs" ) %>%
           mutate(doc_subset = factor(doc_subset, levels = doc_subset_levels)) %>%
           mutate(category = factor(categories, levels = unique(categories)), .after = doc_subset) %>%
           group_by(category) %>%
           mutate(perc_inside_category = round(n_docs / sum(n_docs) * 100, 1))
  return(summary_df)
}


matching_summary_plot <- function(matching_summary_df) {
  plot <- matching_summary_df %>% 
    ggplot(aes(x = category, y = n_docs, fill = doc_subset)) +
    geom_bar(stat = 'identity', position = 'stack') +
    geom_text(aes(label = paste0(n_docs," (",perc_inside_category,"%)" ) ), position = position_stack(vjust = 0.5))
  
  return(plot)
}


matching_df <- matching_summary_df(results$db_list)
View(matching_df)
View(matching_summary_df(results$db_list))
matching_summary_plot(matching_df)

matching_summary_plot

nrow(all_data %>% 
    filter(duplicated(UUID) | duplicated(UUID, fromLast = TRUE)))

View(all_data %>% 
    filter(duplicated(UUID) | duplicated(UUID, fromLast = TRUE)) %>%
    distinct(UUID, .keep_all = TRUE) %>%
      arrange(UUID))

nrow(all_data %>% 
    filter(duplicated(UUID) | duplicated(UUID, fromLast = TRUE)) %>%
    distinct(UUID, .keep_all = TRUE) %>%
      arrange(UUID))

nrow(all_data %>% 
    filter(duplicated(UUID) | duplicated(UUID, fromLast = TRUE)) %>%
      arrange(UUID))

nrow(all_data %>% 
    filter(duplicated(UUID)) %>%
    arrange(UUID))

nrow(all_data %>% 
    filter(duplicated(UUID)) %>%
      distinct(UUID, .keep_all = TRUE))


nrow(all_data %>% 
    filter(!duplicated(UUID)) %>%
      arrange(UUID))

45296 + 4433 + 2598 + 14069 + 2484 + 4513 + 573 + 1796 + 693 + 5016 + 13801 +
  31483 + 16199 + 14881 + 7099


