#Loading libraries
library(tidyverse)
library(bibliometrix)

#sourcing functions script
source('scripts/00_functions.R')

#loading .rds objects
  
dfs <- readRDS('output/data/dfs.rds')
analyses <- readRDS('output/data/analyses.rds')
summaries <- readRDS('output/data/summaries.rds')


View(dfs)


#Time to make plots
plots <- list() #Will hold all plots

#Checking if all three .rds lists (dfs, analyses and summaries) have the databases in the same order
all(sapply(list(names(analyses), names(summaries)), FUN = identical, names(dfs))) #Comparing the order of the attributes in the dfs list to analyses and summaries

#Since this is True (meaning all lists follow the same order), we can create a vector with database names just once and from any of the 3 lists and use it in apply or map functions for any of the .rds lists
db_names <- get_db_names(dfs)



#### Generating a basic plot with the total recovered documents from each database
total_docs <- data.frame(db = names(dfs),
                         doc_count = sapply(dfs, nrow) ) %>% 
  mutate(percentage = round( (doc_count / max(doc_count)) * 100, digits = 0))
  
View(total_docs)

plots$total_docs <- total_docs %>%
  ggplot(aes(x = fct_reorder(db, -doc_count), 
             y = doc_count, 
             fill = db, 
             label = paste0(doc_count, ' (', percentage, '%)') )) +
  geom_bar(stat = 'identity', position = position_dodge(), show.legend = FALSE) +
  geom_text(position = position_dodge(width = .9), vjust = -0.3) +
  labs(x = "Database", 
       y = "Document count")

plots$total_docs

#### Annual production plot

annual_prod <- map2(summaries, db_names, get_info_from_summaries, attribute_name = 'AnnualProduction') %>%
  bind_rows %>%
  rename('year' = 1, 'count' = 2 ) %>%
  mutate(year = as.character(year))

View(annual_prod)


##Year plots with bar lines 
##Stacked bars are percentages and absolute document counts appear as numbers (geom_text layers) inside the bars
plots$annual_prod_bar <- annual_prod %>% 
         mutate(db = )
         group_by(year) %>% #grouping the base df it by year
         mutate(freq = (count / sum(count)) * 100) %>% #calculating new column (frequency of documents per year for each bibliometric database)
         ungroup()) + 
  ggplot(aes(x = year, y = freq, fill = db)) +
  geom_bar(stat="identity")  +
  geom_hline(yintercept = seq(0, 100, by=25), color='white', alpha = .3) + # adding layer to make y axis lines visible over bar plot 
  labs(x = 'Year', y = "Frequency (%)", fill = 'Database') + #changing labels
  scale_y_continuous(breaks = seq(0, 100, by=25)) + #changing number of breaks in y axis
  geom_text(aes(label = count), size = 3, angle = 270, position = position_stack(vjust = 0.5)) + #Adding number of documents to each bar stack
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) #Changing the angle and position of x axis labels
  
plots$annual_prod_bar

# Generating year plot with lines and points (absolute document count)
plots$annual_prod_line <- ggplot(annual_prod) +
  aes(x = year, y = count, group = db, color = db) +
  geom_line() +
  geom_point() +
  labs(x = 'Year',
       y = 'Document count',
       color = 'Database') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

plots$annual_prod_line

#Plots from the 'MainInformationDF' summary attribute

### Getting the data 

main_info <- map2(summaries, db_names, get_info_from_summaries, attribute_name = 'MainInformationDF') %>%
  bind_rows %>%
  rename('description' = 1, 'result' = 2) %>% #renaming columns
  mutate(across(everything(), ~na_if(., ""))) %>% #Changing fields with empty string to NA
  drop_na(result) %>% #Removing rows where the 'result' column is NA
  mutate(data_type = case_when( 
    description %in% c("Timespan","Sources (Journals, Books, etc)","Documents","Annual Growth Rate %","Document Average Age","Average citations per doc","Average citations per year per doc","References") ~ 'main_data',
    description %in% c("Authors","Author Appearances","Authors of single-authored docs","AUTHORS COLLABORATION","Single-authored docs","Documents per Author","Co-Authors per Doc","International co-authorships %") ~ 'author_data',
    description %in%  c("Keywords Plus (ID)","Author's Keywords (DE)") ~ 'doc_contents',
    .default = 'doctype')) ##Adding a  last column based on the descriptions to better organize the plots

#View(main_info)

#Checking if dfs and main_info total number of docs match. If all TRUE, we can move forward
lapply( unique(main_info$db), function(db_name) dfs_and_main_info_totals_match(dfs, main_info, db_name) ) 


#Converting main info to wide table format (better for some comparisons)
main_info_wide <- main_info %>% 
  spread('db', "result") %>% 
  arrange(data_type)

#View(main_info_wide)


#### Doctypes

#Getiing only doctype columns
doctypes <- main_info %>%
  filter(data_type == 'doctype') %>% #Keeping only 'doctype' rows
  mutate(result = as.numeric(result), #Changing result column to numeric in order to be able to perform operations on it
         description = toupper(description), #Converting doctypes to uppercase
         description = ifelse(description == 'CONFERENCE PROCEEDINGS ARTICLE', 'PROCEEDINGS ARTICLE', description)) %>% #Substituting a reasonably long doctype
  select(-data_type) #Removing data_type column (no longer needed, since our df already has only doctype rows)
  

View(doctypes)


###Making a faceted barplot with each db and their respective document types and counts. Order the doctypes in each facet by descending doc_count.


#It is not straightforward to order within each facets with our data, since the 'description' column has duplicate values across different databases (such as 'article' and 'review'.
#To order within each facet, we have used acylam's suggestion (https://stackoverflow.com/questions/47580160/reorder-ggplot-barplot-x-axis-by-facet-wrap), ordering our dataframe and generating a new factor column with unique levels to be used as x-axis labels and then relabeling the x-axis with scale_x_discrete().
doctypes <- doctypes %>% 
  arrange(desc(result)) %>% #Sorting by descending results
  mutate(id = paste0(description, '_', db),
         id = factor(id, levels=id)) 
  

#Additionally, we want to plot how many doctypes are in each database
#For that, we will need to create a new summarised df with labels to annotate each facet (reference: R Graphics Cookbook - https://r-graphics.org/recipe-annotate-facet)
doctypes_labels <- doctypes %>% 
  group_by(db) %>%
  summarise(label = paste(n(),"doctypes"))

#View(doctypes_labels)


plots$doctypes_bar <- doctypes %>%
ggplot( aes(x = id, y = result, color = db) ) +
  facet_wrap(vars(db), scales='free_x') + #Freeing x axis allows to only plot bars for doctypes that actually occur in the database
  geom_bar(stat='identity') + 
  geom_text(aes(label = result), size = 3.5, color = 'black', vjust= -.5) + #Adding number of documents to each bar stack
  geom_text(x = Inf, y = 75000, size = 5,  color = 'black', aes(label = label), data = doctypes_labels, hjust=1.2) + #Annotating each facet with 'doctypes_labels'
  scale_x_discrete(breaks = doctypes$id,
                   labels = doctypes$description) + #Changing x-axis label names
  scale_y_continuous(expand = expansion(mult = .1)) + #Expanding y scale to fit number of documents over each bar stack
  labs(x = 'Document type',
       y = 'Document count') +
  theme(axis.text.x = element_text(angle = 45,  hjust = 1, vjust = 1),  legend.position = "none")


plots$doctypes_bar


### Each database uses their own set of categories for document classification, which makes it very difficult to compare them. To address this issue, we'll apply a normalized classification to our data.


#Articles : articles + reviews + in press...
#Proceeding/meeting_items : articles, abstracts, reviews...
#Book: Books, book chapters, review books  (not book reviews)
#Preprint: preprint
#Unidentified
#Other


##Observations:
#WoS document types: https://webofscience.help.clarivate.com/en-us/Content/document-types.html
#WoS' "article; early access" is the same as an "article in press" (https://images.webofknowledge.com/WOKRS533JR18/help/WOS/hp_results.html)
#A more detailed overview of Web of Science's Document Type field can be accessed at https://images.webofknowledge.com/images/help/WOS/hs_document_type.html
#We will consider the 'data paper' category as part of the 'Articles' group, since it is defined as "A scholarly publication describing a particular dataset or collection of datasets and usually published in the form of a peer-reviewed article in a scholarly journal".
#The Lens' glossary: https://support.lens.org/glossary/

doctypes_normalized <- doctypes %>%
  select(-id) %>% #Since the data is going to be normalized, we don't need the 'id' column for better visualization
  mutate(description = case_when( 
    description %in% c('article', 'journal article', 'review', 'article in press',
                       'article; early access', 'review; early access', 'data paper',
                       'article; data paper', 'article; retracted publication', 
                       'article; data paper; early access', 'reprint') ~ 'Articles',
    description %in% c('proceeding', 'conference proceedings article', 'conference proceedings', 
                       'conference paper', 'conference review', 'proceedings paper', 'meeting abstract',
                       'article; proceedings paper') ~ 'Proceedings itens', 
    description %in% c('book', 'book chapter', 'chapter', 'article; book chapter', 'review; book chapter') ~ 'Books and book chapters', #Does not include book reviews
    description %in%  c('unidentified') ~ 'Unidentified', 
    description %in%  c('preprint') ~ 'Preprint',
    .default = 'Other')) %>% #Adding a last value to aggregate all other doctypes
  group_by(db,description) %>%
  summarise(result =  sum(result), .groups='drop') %>% #Summing all columns with the same db and description
  spread(db, result) %>% #Spreading db field into several columns (long to wide)
  replace(is.na(.), 0) %>% #Filling NA observations with zeroes
  gather("db","result", c(Dimensions, Lens, Scopus, WoS)) #Returning df to long format

#View(doctypes_normalized)
#View(doctypes)

#Showing all document types in the 'other' category
cat(paste("'", unique(doctypes$description[!doctypes$description %in% c('article',
                  'journal article', 'review', 'article in press', 'article; early access', 'review; early access',
                  'article; data paper', 'data paper', 'article; retracted publication', 
                  'article; data paper; early access', 'reprint',
    'proceeding', 'conference proceedings article', 'conference proceedings', 
                       'conference paper', 'conference review', 'proceedings paper', 'meeting abstract',
                       'article; proceedings paper',
                  'book', 'book chapter', 'chapter', 'article; book chapter', 
    'review; book chapter', 
    NA, '', 'preprint') ]), "'", collapse = ", ", sep = "" ))

#View(doctypes_normalized)
  

##### We can make a new bar plot to compare the databases after normalization

norm_doctypes <- lapply(dfs, function(df) normalize_df(df)) %>%
  get_doctype_count_df() %>%
  mutate(DT = factor(DT, levels = c('Articles', 'Proceedings itens', 'Books and book chapters', 'Preprint', 'Unidentified', 'Other'))) %>% #Ordering factors
  group_by(db) %>%
  mutate(percentage = round( (doc_count / sum(doc_count)) * 100, digits = 1  )) #Calculating percentage column
  
  
  

View(norm_doctypes)


plots$doctypes_bar_normalized <- norm_doctypes %>%
  filter(doc_count != 0) %>%  # Filter out rows where doc_count is 0 (to have no thin color bar for when this happens)
  ggplot(aes(x = DT, y = doc_count, color = db )) +
  facet_grid(vars(db)) +
  #facet_grid(vars(factor(db, levels = c('Lens', 'Scopus', 'Dimensions', 'WoS')))) +
  geom_bar(stat='identity', position = position_dodge(), show.legend = FALSE) +
  geom_text(data = norm_doctypes, aes(label = paste0(doc_count, ' (', percentage, '%)')), position = position_dodge(width = .9), color = 'black', vjust = -0.3) + #reusing original data to add 0 using this geom
  ylim(0, 95000) +
  labs(x = "Document types",
       y = "Document count",
       fill = 'Dataset')

plots$doctypes_bar_normalized 


#lapply(norm_raw_dfs, function(df) unique(df$DT))
#lapply(norm_dfs, function(df) unique(df$DT))
#
#plots$doctypes_bar_normalized <- doctypes_normalized %>%
#  group_by(db) %>%
#  mutate(percentage = round( result/sum(result) * 100, 1 ), #Adding a percentage column
#         description = factor(description, #Converting the description column to a factor to order the x-axis of the following plot in a convenient order
#                              levels = c('Articles', 'Proceedings itens', 'Books and book chapters', 'Preprint', 'Unidentified', 'Other') ) ) %>%
#ggplot( aes(x = description, y = result, color = db) ) +
#  facet_grid(vars(db)) + #Freeing x axis allows to only plot bars for doctypes that actually occur in the database
#  geom_bar(stat='identity') + 
#  geom_text(aes(label = paste(result, ' (', percentage, '%)', sep = '')), size = 4, color = 'black', vjust= -0.5) + #Adding number of documents to each bar stack
#  xlab('doctypes') +
#  ylab('doc_count') +
#  ylim(0,95000) +
#    scale_y_log10() +
#  labs(x = "Document types",
#       y = "Document count") +
#  theme(legend.position = "none")
#
#plots$doctypes_bar_normalized


#Saving all plots
save_plot_list(plots)
