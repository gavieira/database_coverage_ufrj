#Loading libraries
library(tidyverse)
library(bibliometrix)
library(treemapify)
library(patchwork)
library(uuid)

#sourcing functions script
source('scripts/00_functions.R')

#loading .Rdata objects
load('output/data/dfs.Rdata')
load('output/data/analyses.Rdata')
load('output/data/summaries.Rdata')

aux_vars <- list()

#Checking the first rows of each database
aux_vars$first_rows <- lapply(dfs, head, n=3) %>% bind_rows(.id = 'id')

View(aux_vars$first_rows)
#write.csv(aux_vars$first_rows, "/home/gabriel/first_rows.csv", row.names = FALSE) #Saving to file to better visualize results

#After visual inspection of the first rows of each db, we have selected the fields that will be used to determine overlapping documents
#DI (DOI)
#TI (Publication Title)
#SO (Source)
#AU (Author)
#PY (Publication Year)
#VL (Volume)
#JI (ISO Source Abbreviation)
#J9 (29-Character Source Abbreviation)
#SR_FULL (Complete Source Record - Author + Year + Source Abbreviation) - Looks like a nice field, even though Dimensions and Lens have some records without the source abbreviation
#PU (Publisher) # No caso da Dimensions, essa informação está no campo "Publisher"
#ISSN # Campo presente apenas para dimensions. Na lens, essa informação está no campo "ISSNs". Na scopus e wos, no campo 'SN'.

#Gerando uma dataset apenas das publicações que ocorrem em todas as bases (apenas pelo DOI)
aux_vars$duplicated_doi_all_dbs <- dfs %>%
  bind_rows(.id = 'id') %>%
  group_by(DI) %>%
  filter(n() == 4) %>%
  ungroup() %>%
  arrange(DI)

#Obtendo os elementos únicos de um dado campo (para ver quais bases possuem apenas NAs)
unique_elements <- lapply(dfs, function(df) unique(df[['AR']]))
View(unique_elements)

#Verificação de quantas entradas não possem source abreviation no campo SR_FULL dos dataframes
lapply(unique_elements, function(db) sum(grepl("CHARACTER\\(0\\)", db)))
lapply(unique_elements, function(db) sum(grepl("CHARACTER.0.", db)))

#Selecionando colunas relacionadas ao ISNN/ISBN - referencia para nome das colunas: https://www.bibliometrix.org/documents/Field_Tags_bibliometrix.pdf
View(
  aux_vars$first_rows %>%
  select(database, ISSN, ISSNs, ISBN, IS, SN, eissn, BN) #ISBN and BN are book numbers
)

#Selecionando colunas (provavelmente) associadas ao número de páginas
View(
  aux_vars$first_rows %>%
  select(database, PP, Start.Page, End.Page, PN) #PP is probably not page number
)


#O que fazer:
#1 - Separar os documentos que apresentam 4 matches pelo DOI
#2 - Trabalhar apenas com documentos que tenham menos 3 matches - podem ter um quarto, mas o DOI não foi recuperado
#Se partirmos do pressuposto que as bases indexarão o DOI se ele existir, seria mais fácil. Mas vamos tentar desse jeito por enquanto
#3 - Limpar os dados (remoção de acentos (US-ASCII), seria interessante tirar pontuação tbm?)
#4 - Fazer (a princípio) a análise com base na distancia de edição (levenshtein) do título, nome do primeiro author, periódico e ano de publicação.
#5 - Tentar pegar a análise e gerar as permutações.

aux_vars$not_all_dbs <- dfs %>%
  bind_rows(.id = 'id') %>%
  group_by(DI) %>%
  filter(n() != 4)

View(aux_vars$not_all_dbs)

#### Getting a subset dataframe for testing (only documents from 2008)

aux_vars$dfs_subset <- lapply(dfs, function(df) { filter(df,  PY == 2008) } )

lapply(aux_vars$dfs_subset, nrow)



#### Getting a dataframe with records containing duplicated dataframes (for post-cleaning)

# Apply the get_duplicate_rows_at_column function to each dataframe in the 'dfs' list,
# filtering for duplicates based on the specified column ('DI' - doi)
aux_vars$duplicated_doi_within_dbs <- lapply(dfs, get_duplicate_rows_at_column, column_name = 'DI') %>%
  bind_rows(.id = 'id') %>%
  arrange(DI)

aux_vars$duplicated_doi_within_dbs



######

View( dbs_coverage %>%
  map_dfr(~ slice(.x, 10))  )





#Novo Plano:
#Pegar apenas 5 colunas:
#DOI, Title, Year, Author, Source
#DI, TI, PY, AU, SO
##TC (Times Cited) : Usado para ordenar os records (trabalhar com apenas os documentos mais citados na subamostra)
#Remover acentos e afins
#Métrica: 
##DOI = 1
##Title = 0.6 - Levenshtein * 0.1 (ou 0.05)
##Year = 0.3
##Source = 0.3 - Levenshtein * 0.1
##Author = 0.3 Levenshtein * 0.1
#Fazer comparações par a par:
##Gerar ids unicos para cada documento de todas as bases
##Comparar cada documento da base 1 com os documentos das outras bases - atribuir o mesmo ID para eles se o maior  score for maior que 1 - score ficará registrado na segunda base em todas as comparações
##Para diminuir o poder computacional necessário pra essa análise, quando um documento base 1 der match com um documento de outra base, o documento da outra base será removido das próximas rodadas de comparação (documentos com score serão removidos) - Isso faz com que a ordem dos argumentos na comparação seja extremamente importante
##Comparar cada documento da base 2 (sem o valor do score) com os documentos das bases 3 e 4 - atribuir o mesmo ID para eles
##Mesma coisa para o ultimo par (bases 3 e 4)

#####################################

#Criando o novo objeto que vai conter apenas os campos de interesse para calcular a sobreposição 
#Coluna contendo o total de citações (TC) pode ser interessante para análises posteriores usando apenas o h-core (ou qqr outro core)
dbs_coverage <- lapply(dfs, function(df) {
  row.names(df) <- NULL
  #df[, c('DI', 'TI', 'PY', 'AU', 'SO', 'SR_FULL', 'JI', 'J9')] %>%
  if ('Citing.Works.Count' %in% names(df) ) { #Dealing with lens, which has citations in the 'Citing.Works.Count' field
    df <- mutate(df, TC = as.numeric(Citing.Works.Count))
  }
  df %>%
    select(DI, TI, PY, AU, J9, DT, TC) %>%
    mutate(AU = str_extract(AU, "^[^;]+") ) %>% #Getting only the first author from the 'AU' column; regex extracts the substring up to the first ";"
    mutate(across(where(is.character), toupper)) %>% #Converting all character fields to uppercase
    mutate(across(where(is.character), ~ iconv(., from = "UTF-8", to = "ASCII//TRANSLIT"))) %>% #Converting all character fields to US-ASCII (removing accentuation)
    mutate_all(trimws) %>% #Removing whitespace from all values
    mutate_all(~ifelse(. == "" | is.null(.), NA, .) ) %>% #Converting empty/null values to NA 
    mutate(J9 = ifelse(J9 == "CHARACTER(0)", NA, J9)) %>% #Adding NA to fields without journal abbreviation (which contains the "CHARACTER(0)" string)
    mutate(score = NA) #This column will be used to exclude documents that have already been matched to another database from subsequent comparisons, reducing execution time
} )

View(dbs_coverage)


#Adding a new column containing the database name as first field
dbs_coverage <- imap(dbs_coverage, ~ { 
  mutate(.x, DB = .y) %>%
  relocate(DB, .before = 1)
})


#Viewing the first records of each dataframe
View(map_df(dbs_coverage, ~ .x[1:5, ]))

#################################

#We'll use a function that relies on the 'uuid' R package to generate a new column with unique identifiers for each row
dbs_coverage <- lapply(dbs_coverage, generate_uuid, new_column = 'UUID')

#Checking if the newly created column is unique across all dbs
col_all_unique(dbs_coverage, column = 'UUID')

#Viewing the first records of each dataframe
View(map_df(dbs_coverage, ~ .x[1:5, ]))


####################################################################################################

#### Getting a subsample of the data for testing the code for scoring and database coverage


#Function to get the first rows of each dataframe in a list, as well as changing the name of list itens
subset_df_list <- function(df_list, prefix = 'df', n_recs = 1000) {
  new_list <- df_list
  names(new_list) <- paste0(prefix, seq(length(new_list)))
  new_list <- lapply(new_list, head, n = n_recs)
  return(new_list)
}


#Generating subset data (ordered by decreasing number of citations) and assigning it to a single object for each database
list2env(
  subset_df_list(
  lapply( dbs_coverage, function(df)  arrange(df, desc(TC)) ), 
  prefix = 'db',
  n_recs = 1000
), envir = .GlobalEnv)


#####################################################################################


#With the data properly formated, we need to:
#1- Create a function that implements our scoring system
#1.5- Create a function that returns all possible combination pairs from the 4 databases
#2- Apply such function from each record on db1 to each record on db2 
#3- Select the pair that has the highest value
#4- The id of db1 gets copied to db2, and the score value is added to the db2 record

#Checking column names
colnames(dbs_coverage$dimensions)


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
calc_score <- function (row1, row2) {
  doi <- ifelse( test_na(c(row1['DI'], row2['DI'])) || row1['DI'] != row2['DI'], 0, 1 ) #Score for publication DOI 
  title <- ifelse( test_na(c(row1['TI'], row2['TI'])), 0, 
                   pmax(0.6 - adist(row1['TI'], row2['TI'])[1] **2 * 0.01, 0) ) #Score for publication title - adist() calculates levenshtein distance, while pmax replaces negative values with 0
  if (doi + title < 0.1) { #if doi+title is <0.1, there's no way to reach the threshold (1), so we'll save some processing power and just go to the next document
   return(NA) 
  }
  year <-ifelse( test_na(c(row1['PY'], row2['PY'])) || row1['PY'] != row2['PY'], 0, 0.3 ) #Score for publication year
  source <- ifelse( test_na(c(row1['J9'], row2['J9'])), 0, 
                    pmax(0.3 - adist(row1['J9'], row2['J9'])[1]*0.1, 0) ) #Score for publication source
  author <- ifelse( test_na(c(row1['AU'], row2['AU'])), 0, 
                    pmax(0.3 - adist(row1['AU'], row2['AU'])[1]*0.1, 0)  )#Score for publication author
  final_score <- doi + title + year + source + author
  return(final_score)
}


result <- lapply(db1, function(row1) {
  lapply(db2, function(row2) {
    score <- calc_score(row1,row2)
    return(score)
  })
})

result

#Preciso de condição para só comparar com a db2 se o registro ainda não tiver par
#Function to apply the calc_score for a single row in db1 to all db2 rows and get the max value and index
#get_max_score <- function(row1, db2) {
#                          scores <- unname( apply(db2, MARGIN = 1, calc_score, row1 = row1) ) #Applying the calc_score function to a single row of db1 to all rows of db2 and removing names from the resulting vector
#                          return( c("max_score" = max(scores, na.rm = TRUE), 
#                                    "index" =  which.max(scores)) ) #Returning a named vector with both max_score and the index of the (first) occurrence of the max_score
#}


get_max_score <- function(row1, db2, score_cutoff = 1) {
                          scores <- apply(db2, MARGIN = 1, function(row2) { #generating a vector containing the results of the 'calc_score' function
                            if ( is.na(row2['score']) ) { calc_score(row1, row2) } #Caculates the score only for records in db2 that haven't been paired to another record (in other words, records without a score attached)
                            else { NA } #Else, returns NA to make the resulting vector present the same indices
                          }) 
                          scores <- unname(scores) #Removing names from the resulting vector
                          max_score <- max(scores, na.rm = TRUE) #Getting value of (first) maximum obtained score. Will return warning if no match is found (scores vector will have only NAs)
                          max_index <- which.max(scores) #Getting the index of the row with the highest score
                          if (max_score >= score_cutoff) { #If max_score is higher than cutoff (default = 1)
                            db2[max_index, 'score'] <- max_score #Adding the value of score to matched record in db2
                            db2[max_index, 'UUID'] <- row1['UUID'] #Copying db1 record (query) uuid to db2 matched record (subject)
                          }
                          #return (list(scores = scores, max_score = max_score, index = max_index, rows =  bind_rows(row1, db2[max_index,])))
                          return (db2)
}


compare_dbs <- function(db1, db2) {
  apply(db1, MARGIN = 1, function(row1) {
    max_index <- get_max_score(row1, db2)
    
  })
}


db2
db_teste <- get_max_score(db1[2,], db2)

db_teste[31,]


apply(db2, MARGIN = 1, calc_score, row1 = db1[2,])

scores <- get_max_score(db1[2,], db2)

View(scores)

test <- which.max(scores)

print(unique(scores))

scores['index']


View(rbind(db1[2,], db2[scores['index'],]))

my_function <- function(df) {
  # Perform operations to modify the dataframe
  df$column <- df$column + 1
  df$new_column <- "new value"
  
  # No need to return the dataframe since modifications are made in-place
}

# Create a dataframe
my_dataframe <- data.frame(column = 1:5)

# Call the function to modify the dataframe
my_function(my_dataframe)

# The changes made in the function persist in the original dataframe
print(my_dataframe)







# Initialize an empty list to store the scores
score_list <- list()

# Apply calc_score function for each row of db1 against all rows of db2


mapply(calc_score, db1, db2)


db1[1,]$DI


main_func <- function(db1, db2) {
  for ( i in 1:nrow(db1) ) {
    for ( j in 1:nrow(db2) ) {
      score <- calc_score(db1[i,], db2[j,])
    }
    return(score)
  }
}

main_func <- function(db1, db2) {
    db2$score <- c()
    for ( j in 1:nrow(db2) ) {
      print(calc_score(db1[i,], db2[j,]))
      db2$score <- calc_score(db1[i,], db2[j,])
    }
    return(db2$score)
}


main_func(db1, db2)
View(db2)


vec = c()

vec = append(vec, 12)
vec = append(vec, 14)
vec = append(vec, NULL)
vec = append(vec, 15)
vec

result <- db1 %>%
  mutate(Score = mapply(calc_score, row1 = ., row2 = db2))


test <- main_func(db1, db2)



test <- calc_score(db1[1,], db2[1,])

test
#Usar adist para calcular a distância levenshtein

#WILL NEED TO REMOVE NA's from DOIs
match_index <- match(dbs_coverage$wos$DI, dbs_coverage$scopus$DI)

match_index


#Métrica: 
##DOI = 1
##Title = 0.6 - Levenshtein * 0.1 (ou 0.05)
##Year = 0.3
##Source = 0.3 - Levenshtein * 0.1
##Author = 0.3 - Levenshtein * 0.1

#Creating dataframe objects for testing


#Function to get the first rows of each dataframe in a list, as well as changing the name of list itens
subset_df_list <- function(df_list, prefix = 'df', n_recs = 1000) {
  new_list <- df_list
  names(new_list) <- paste0(prefix, seq(length(new_list)))
  new_list <- lapply(new_list, head, n = n_recs)
  return(new_list)
}

#Generating subset data (ordered by decreasing number of citations)
dbs_coverage_subset <-  subset_df_list( lapply( dbs_coverage, function(df)  arrange(df, desc(TC)) ), 
  prefix = 'db',
  n_recs = 1000 
)

#Generating a object for each database in the subset
list2env(dbs_coverage_subset, envir = .GlobalEnv)


names(dbs_coverage)


View(db1)
View(db2)
View(db3)
View(db4)


# Example vector
vector <- c("A", "B", "C", "D")

# Nested loops for pair-to-pair comparisons
for (i in 1:length(vector)) {
  for (j in (i+1):length(vector)) {
    pair1 <- vector[i]
    pair2 <- vector[j]
    # Perform comparison or desired operation on pair1 and pair2
    # ...
    # Example: Print the pair
    cat("Pair:", pair1, "-", pair2, "\n")
  }
}

View(dbs_coverage$scopus[4,])








#testing if uuid did generate only unique identifiers across multiple tests
map(1:10, ~ {
  df <- lapply(dbs_coverage,generate_uuid)
  return(col_all_unique(df, column = 'UUID'))
})


#Function to test the penalty on the levenshtein distance for a given multiplier
test_lev_penalty <- function(multiplier) {
  vector <- (1:10)**2 * multiplier
  names(vector) <- 1:10
  return(vector)
}

test_lev_penalty(0.01)


