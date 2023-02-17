library(tidyverse)
library(bibliometrix)

#sourcing functions script
source('scripts/00_functions.R')

#loading .Rdata objects
load('output/data/dfs.Rdata')
load('output/data/analyses.Rdata')
load('output/data/summaries.Rdata')

#Time to make plots
plots <- list() #Will hold all plots
aux_vars <- list() #Will hold all auxiliary data

#Checking if all three .Rdata lists (dfs, analyses and summaries) have the databases in the same order
all(sapply(list(names(analyses), names(summaries)), FUN = identical, names(dfs))) #Comparing the order of the attributes in the dfs list to analyses and summaries

#Since this is True (meaning all lists follow the same order), we can create a vector with database names just once and from any of the 3 lists and use it in apply or map functions for any of the .Rdata lists
aux_vars$db_names <- get_db_names(dfs)


###Make plots about:
#Country collaboration
#AFF fraction #Exclude UFRJ from plot (all pubs are from UFRJ)
#Years
#Total Citations (TC) and Total Citations Per Year
#Sources
#nAUperPaper 
#AuMultiAuthoredArt
#AuSingleAuthoredArt
#Most_Cited_Papers - Would be nice to assess coverage of most cited papers and compare it with the coverage of all pubs. If it follows the same pattern as Visser et al.(2021)

#### YEAR comparisons

aux_vars$annual_prod <- map2(summaries, aux_vars$db_names, get_info_from_summaries, attribute_name = 'AnnualProduction') %>%
  bind_rows %>%
  rename('year' = 1, 'count' = 2 )

#View(aux_vars$annual_prod)

#aux_vars$annual_prod.wide <- years %>%
#  spread(db, count)


#Generating base year plot (frequency)
plots$p1 <- ggplot(aux_vars$annual_prod %>% 
               group_by(year) %>% #grouping the base df it by year
               mutate(freq = (count / sum(count)) * 100) %>% #calculating new column (frequency of documents per year for each bibliometric database)
               ungroup()) + 
  aes(x = year, y = freq, fill = db) +
  geom_bar(stat="identity")  +
  geom_hline(yintercept = seq(0, 100, by=10), color='white', alpha = .3) + # adding layer to make y axis lines visible over bar plot 
  labs(x = 'Year', y = "Frequency (%)", fill = 'Database') + #changing labels
  scale_y_continuous(breaks = seq(0, 100, by=10)) #changing number of breaks in y axis

#Year plots with vertical (p1v) and horizontal (p1h) bar lines 
#Stacked bars are percentages and absolute document counts appear as numbers (geom_text layers) inside the bars
plots$p1v <- plots$p1 + 
  geom_text(aes(label = count), size = 3, angle = 270, position = position_stack(vjust = 0.5)) + #Adding number of documents to each bar stack
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) #Changing the angle and position of x axis labels

plots$p1h <- plots$p1 +
  coord_flip() + #Swapping x and y axes
  geom_text(aes(label = count), size = 3, position = position_stack(vjust = 0.5)) 
  

# Generating year plot with lines and points (absolute document count)
plots$p2 <- ggplot(aux_vars$annual_prod) +
  aes(x = year, y = count, group = db, color = db) +
  geom_line() +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))


#Summary plots

get_main_info <- function(list, database, atribute_name) {
  
  info <- list$MainInformationDF %>% #Counting per year
  mutate(db = database) #Creating database column
  return(info)
}

#Gets information from dataframe attributes in the 'summaries' list while adding an extra column containing database name
get_info_from_summaries <- function(list, database, attribute_name) {
  attribute_index <- match(attribute_name, names(list)) #Getting index of desired information (attribute has to be a dataframe object)
  info <- as.data.frame(list[attribute_index]) %>% #Getting desired attribute through index
  mutate(db = database) #Creating database column
  return(info)
}

aux_vars$main_info <- map2(summaries, aux_vars$db_names, get_info_from_summaries, attribute_name = 'MainInformationDF') %>%
  bind_rows %>%
  rename('description' = 1, 'result' = 2) %>% #renaming columns
  mutate(across(everything(), ~na_if(., ""))) %>% #Changing fields with empty string to NA
  drop_na(result) %>%  #Removing rows where the 'result' column is NA
  mutate(data_type = case_when( 
    description %in% c("Timespan","Sources (Journals, Books, etc)","Documents","Annual Growth Rate %","Document Average Age","Average citations per doc","Average citations per year per doc","References") ~ 'main_data',
    description %in% c("Authors","Author Appearances","Authors of single-authored docs","AUTHORS COLLABORATION","Single-authored docs","Documents per Author","Co-Authors per Doc","International co-authorships %") ~ 'author_data',
    description %in%  c("Keywords Plus (ID)","Author's Keywords (DE)") ~ 'doc_contents',
    .default = 'doc_type')) ##Adding a  last column based on the descriptions to better organize the plots

View(aux_vars$main_info)


aux_vars$main_info_wide <- aux_vars$main_info %>% 
  spread('db', "result") %>% 
  arrange(data_type)

View(aux_vars$main_info_wide)


aux_vars$main_info_doctypes <- aux_vars$main_info %>%
  filter(data_type == 'doc_type') %>%
  mutate(result = as.numeric(result)) %>%
  select(-data_type) %>%
  spread(db, result) %>%
  replace(is.na(.), 0) %>%
  bind_rows(summarise_all(., ~if(is.numeric(.)) sum(.) else "Total"))
  

View(aux_vars$main_info_doctypes)


map(analyses[1:4], "Articles") %>%
  bind_rows

#summaries$dimensions$MainInformationDF

#### Country collaboration comparisons


#Most relevant sources
#Bar plot, with the 1st to the 10th most relevant sources of the database
