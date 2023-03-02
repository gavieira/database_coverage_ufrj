library(tidyverse)
library(bibliometrix)
library(treemapify)
library(patchwork)

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

#### Annual production plot

aux_vars$annual_prod <- map2(summaries, aux_vars$db_names, get_info_from_summaries, attribute_name = 'AnnualProduction') %>%
  bind_rows %>%
  rename('year' = 1, 'count' = 2 ) %>%
  mutate(year = as.character(year))

View(aux_vars$annual_prod)

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


#Plots from the 'MainInformationDF' summary attribute

### Getting the data 

aux_vars$main_info <- map2(summaries, aux_vars$db_names, get_info_from_summaries, attribute_name = 'MainInformationDF') %>%
  bind_rows %>%
  rename('description' = 1, 'result' = 2) %>% #renaming columns
  mutate(across(everything(), ~na_if(., ""))) %>% #Changing fields with empty string to NA
  drop_na(result) %>%  #Removing rows where the 'result' column is NA
  mutate(data_type = case_when( 
    description %in% c("Timespan","Sources (Journals, Books, etc)","Documents","Annual Growth Rate %","Document Average Age","Average citations per doc","Average citations per year per doc","References") ~ 'main_data',
    description %in% c("Authors","Author Appearances","Authors of single-authored docs","AUTHORS COLLABORATION","Single-authored docs","Documents per Author","Co-Authors per Doc","International co-authorships %") ~ 'author_data',
    description %in%  c("Keywords Plus (ID)","Author's Keywords (DE)") ~ 'doc_contents',
    .default = 'doctype')) ##Adding a  last column based on the descriptions to better organize the plots

View(aux_vars$main_info)


aux_vars$main_info_wide <- aux_vars$main_info %>% 
  spread('db', "result") %>% 
  arrange(data_type)

View(aux_vars$main_info_wide)


#### Doctypes

#Getiing only doctype columns
aux_vars$main_info_doctypes <- aux_vars$main_info %>%
  filter(data_type == 'doctype') %>% #Keeping only 'doctype' rows
  mutate(result = as.numeric(result)) %>% #Changing result column to numeric in order to be able to perform operations on it
  select(-data_type) #Removing data_type column (no longer needed, since our df already has only doctype rows)
  
map(analyses[1:4], "Articles") %>% 
  bind_rows #Viewing total records per database

##Obs: The total of Scopus records by document type is 94771, while the downloaded dataset features 94809 documents. Through manual inspection of our data, we have found that 38 Scopus documents have 'NA' in their DT (Document type) field and were not added to bibliometrix's summaries. We'll manually add a row containing these documents here.
  
aux_vars$main_info_doctypes <- aux_vars$main_info_doctypes %>% 
  add_row(description = NA, result = 38, db = 'Scopus') %>% #Adding Scopus records with no assigned doctype %>%
  replace_na(list(description = 'unidentified')) %>% #Changing NAs in the description column to 'unidentified'
  distinct #keep only distinct rows (in case this block of code gets run twice)

View(aux_vars$main_info_doctypes)



View(aux_vars$main_info_doctypes %>% filter(db == 'Dimensions') %>% select(c(description,result)))
View(aux_vars$main_info_doctypes %>% filter(db == 'WoS') %>% select(c(description,result)))
View(aux_vars$main_info_doctypes %>% filter(db == 'Scopus') %>% select(c(description,result)))
View(aux_vars$main_info_doctypes %>% filter(db == 'Lens') %>% select(c(description,result)))

names(aux_vars$main_info_doctypes)


###Making a faceted barplot with each db and their respective document types and counts. Order the doctypes in each facet by descending doc_count.


#It is not straightforward to order within the same facet with our data, since the 'description' column has duplicate values across different databases (such as 'article' and 'review'.
#To order within each facet, we have used acylam's suggestion (https://stackoverflow.com/questions/47580160/reorder-ggplot-barplot-x-axis-by-facet-wrap), ordering our dataframe and generating a new factor column with unique levels to be used as x-axis labels and then relabeling the x-axis with scale_x_discrete().
aux_vars$main_info_doctypes <- aux_vars$main_info_doctypes %>% 
  arrange(desc(result)) %>% #Sorting by descending results
  unite('id', description,db, sep='_', remove = FALSE) %>% #Creating new unique 'id' column by merging 'description' and 'db' cols
  mutate(id = factor(id, levels=id)) #Converting 'id' column to factor
  

#Additionally, we want to plot how many doctypes are in each database
#For that, we will need to create a new summarised df with labels to annotate each facet (reference: R Graphics Cookbook - https://r-graphics.org/recipe-annotate-facet)
aux_vars$main_info_doctypes_labels <- aux_vars$main_info_doctypes %>% 
  group_by(db) %>%
  summarise(label = paste(n(),"doctypes"))

#View(aux_vars$main_info_doctypes_labels)


plots$p4 <- aux_vars$main_info_doctypes %>%
ggplot( aes(x = id, y = result, color = db) ) +
  facet_wrap(vars(db), scales='free_x') + #Freeing x axis allows to only plot bars for doctypes that actually occur in the database
  geom_bar(stat='identity') + 
  geom_text(aes(label = result), size = 2.5, color = 'black', vjust= -0.5) + #Adding number of documents to each bar stack
  geom_text(x = Inf, y = 75000, size = 5,  color = 'black', aes(label = label), data = aux_vars$main_info_doctypes_labels, hjust=1.2) + #Annotating each facet with 'main_info_doctypes_labels'
  xlab('doctypes') +
  ylab('doc_count') +
  scale_x_discrete(breaks = aux_vars$main_info_doctypes$id,
                   labels = aux_vars$main_info_doctypes$description) + #Changing x-axis label names
  theme(axis.text.x = element_text(angle = 45,  hjust = 1, vjust = 1), legend.position = "none")

plots$p4

### Tentar fazer uma visualização em árvore (uma para cada um, talvez em um único grid do patchwork). 

### Juntar algumas categorias para fazer um plot de barras mais clássico


plots$px <- ggplot(aux_vars$main_info_doctypes, 
                   aes(area = result, subgroup = db, fill = db, label = paste(description, result, sep = '\n'))) +
  geom_treemap() +
  geom_treemap_text(fontface = "italic", colour = "white", place = "centre") +
  geom_treemap_subgroup_border(colour = "black")


plots$px


#Time to clean/merge the doctypes into fewer categories


#Articles : articles + reviews + in press
#Proceeding/meeting_items : articles, abstracts, reviews...
#Book: Books, book chapters 
#Preprint: preprint
#Unidentified
#Other: 
#editorial items: editorials, notes, letters...


##Obs:
#WoS' "article; early access" is the same as an "article in press" (https://images.webofknowledge.com/WOKRS533JR18/help/WOS/hp_results.html)
#A more detailed overview of Web of Science's Document Type field can be accessed at https://images.webofknowledge.com/images/help/WOS/hs_document_type.html
#We will consider the 'data paper' category as part of the 'Articles' group, since it is defined as "A scholarly publication describing a particular dataset or collection of datasets and usually published in the form of a peer-reviewed article in a scholarly journal".
#The Lens' glossary: https://support.lens.org/glossary/

aux_vars$main_info_doctypes_normalized <- aux_vars$main_info_doctypes %>%
  select(-id) %>% #Since the data is going to be normalized, we don't need the 'id' column for better visualization
  mutate(description = case_when( 
    description %in% c('article', 'journal article', 'review', 'article; early access', 'review; early access', 
                       'article; data paper', 'article; retracted publication', 
                       'article; data paper; early access', 'reprint') ~ 'Articles',
    description %in% c('proceeding', 'conference proceedings article', 'conference proceedings', 
                       'conference paper', 'conference review', 'proceedings paper', 'meeting abstract',
                       'article; proceedings paper') ~ 'Proceedings itens', 
    description %in% c('book', 'book chapter', 'chapter', 'article; book chapter', 'review; book chapter') ~ 'Books and book chapters', #Does not include book reviews
    description %in%  c('unidentified', 'other') ~ 'Unidentified', #The 'others' from Lens acts as an umbrella term for unidentified documents
    description %in%  c('preprint') ~ 'Preprint',
    .default = 'Other')) %>% #Adding a last value to aggregate all other doctypes
  group_by(db,description) %>%
  summarise(result =  sum(result), .groups='drop') %>% #Summing all columns with the same db and description
  spread(db, result) %>% #Spreading db field into several columns (long to wide)
  replace(is.na(.), 0) %>% #Filling NA observations with zeroes
  gather("db","result", c(Dimensions, Lens, Scopus, WoS)) #Returning df to long format
  
View(aux_vars$main_info_doctypes_normalized)
  
plots$p4.1 <- aux_vars$main_info_doctypes_normalized %>%
ggplot( aes(x = description, y = result, color = db) ) +
  facet_grid(vars(db)) + #Freeing x axis allows to only plot bars for doctypes that actually occur in the database
  geom_bar(stat='identity') + 
  geom_text(aes(label = result), size = 4, color = 'black', vjust= -0.5) + #Adding number of documents to each bar stack
  xlab('doctypes') +
  ylab('doc_count') +
  ylim(0,95000) +
  scale_x_discrete(labels = c('Articles', 'Books and book chapters', 'Proceedings itens',
                              'Preprint', 'Other', 'Unidentified')) + #Changing x-axis label names
  theme(axis.text.x = element_text(angle = 45,  hjust = 1, vjust = 1), legend.position = "none")


plots$p4.1


plots$px.1 <- ggplot(aux_vars$main_info_doctypes_normalized, 
                   aes(area = result, subgroup = db, fill = db, label = paste(description, result, sep = '\n'))) +
  geom_treemap() +
  geom_treemap_text(fontface = "italic", colour = "white", place = "centre") +
  geom_treemap_subgroup_border(colour = "black")


### Tentar fazer uma visualização em árvore (uma para cada um, talvez em um único grid do patchwork). 

### Juntar algumas categorias para fazer um plot de barras mais clássico


plots$px <- ggplot(aux_vars$main_info_doctypes, 
                   aes(area = result, subgroup = db, fill = db, label = paste(description, result, sep = '\n'))) +
  geom_treemap() +
  geom_treemap_text(fontface = "italic", colour = "white", place = "centre") +
  geom_treemap_subgroup_border(colour = "black")


plots$px

#summaries$dimensions$MainInformationDF

#### Country collaboration comparisons



#Most relevant sources
#Bar plot, with the 1st to the 10th most relevant sources of the database
