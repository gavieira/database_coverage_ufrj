#Loading libraries
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


#### Generating a basic plot with the total recovered documents from each database
plots$total_docs <- data.frame( lapply( analyses, function(x) x[['Articles']] ) ) %>% #Getting dataframe with db as colname and total documents
  pivot_longer(cols = everything(), names_to = 'db',
               values_to = 'docs') %>% # Creating a column with db names
  mutate(perc = round(docs/max(docs) * 100, 1)) %>%
  ggplot(aes(x = fct_reorder(db, -docs), y = docs, fill = db)) +
  geom_bar(stat = 'identity', show.legend = FALSE) +
  geom_text(aes(label = paste(docs," (",perc,"%)", sep = '')), vjust = -0.5) +
  labs(x = "Database", y = "Documents")


#### Annual production plot

aux_vars$annual_prod <- map2(summaries, aux_vars$db_names, get_info_from_summaries, attribute_name = 'AnnualProduction') %>%
  bind_rows %>%
  rename('year' = 1, 'count' = 2 ) %>%
  mutate(year = as.character(year))

#View(aux_vars$annual_prod)

#aux_vars$annual_prod.wide <- years %>%
#  spread(db, count)


#Generating base year plot (frequency)
plots$annual_prod_bar_base <- ggplot(aux_vars$annual_prod %>% 
               group_by(year) %>% #grouping the base df it by year
               mutate(freq = (count / sum(count)) * 100) %>% #calculating new column (frequency of documents per year for each bibliometric database)
               ungroup()) + 
  aes(x = year, y = freq, fill = db) +
  geom_bar(stat="identity")  +
  geom_hline(yintercept = seq(0, 100, by=25), color='white', alpha = .3) + # adding layer to make y axis lines visible over bar plot 
  labs(x = 'Year', y = "Frequency (%)", fill = 'Database') + #changing labels
  scale_y_continuous(breaks = seq(0, 100, by=25)) #changing number of breaks in y axis

aux_vars$annual_prod %>%
  group_by(db) %>%
  filter(n_distinct(year) > 1, n_distinct(db) > 1) %>%
ggplot(aes(x = year, y = count, color = db)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) #Changing the angle and position of x axis labels
  

#Year plots with vertical (p1v) and horizontal (p1h) bar lines 
#Stacked bars are percentages and absolute document counts appear as numbers (geom_text layers) inside the bars
plots$annual_prod_bar_vertical <- plots$annual_prod_bar_base + 
  geom_text(aes(label = count), size = 3, angle = 270, position = position_stack(vjust = 0.5)) + #Adding number of documents to each bar stack
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) #Changing the angle and position of x axis labels

plots$annual_prod_bar_horizontal <- plots$annual_prod_bar_base +
  coord_flip() + #Swapping x and y axes
  geom_text(aes(label = count), size = 3, position = position_stack(vjust = 0.5)) 
  

# Generating year plot with lines and points (absolute document count)
plots$annual_prod_line <- ggplot(aux_vars$annual_prod) +
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
  drop_na(result) %>% #Removing rows where the 'result' column is NA
  mutate(data_type = case_when( 
    description %in% c("Timespan","Sources (Journals, Books, etc)","Documents","Annual Growth Rate %","Document Average Age","Average citations per doc","Average citations per year per doc","References") ~ 'main_data',
    description %in% c("Authors","Author Appearances","Authors of single-authored docs","AUTHORS COLLABORATION","Single-authored docs","Documents per Author","Co-Authors per Doc","International co-authorships %") ~ 'author_data',
    description %in%  c("Keywords Plus (ID)","Author's Keywords (DE)") ~ 'doc_contents',
    .default = 'doctype')) ##Adding a  last column based on the descriptions to better organize the plots

View(aux_vars$main_info)


aux_vars$main_info_wide <- aux_vars$main_info %>% 
  spread('db', "result") %>% 
  arrange(data_type)

#View(aux_vars$main_info_wide)


#### Doctypes

#Getiing only doctype columns
aux_vars$doctypes <- aux_vars$main_info %>%
  filter(data_type == 'doctype') %>% #Keeping only 'doctype' rows
  mutate(result = as.numeric(result)) %>% #Changing result column to numeric in order to be able to perform operations on it
  select(-data_type) #Removing data_type column (no longer needed, since our df already has only doctype rows)
  
map(analyses[1:4], "Articles") %>% 
  bind_rows #Viewing total records per database

##Obs: The total of Scopus records by document type is 94771, while the downloaded dataset features 94809 documents. Through manual inspection of our data, we have found that 38 Scopus documents have 'NA' in their DT (Document type) field and were not added to bibliometrix's summaries. We'll manually add a row containing these documents here.
  


aux_vars$doctypes <- aux_vars$doctypes %>% 
  add_row(description = NA, result = 38, db = 'Scopus') %>% #Adding Scopus records with no assigned doctype %>%
  replace_na(list(description = 'unidentified')) %>% #Changing NAs in the description column to 'unidentified'
  distinct #keep only distinct rows (in case this block of code gets run twice)

#View(aux_vars$doctypes)


###Making a faceted barplot with each db and their respective document types and counts. Order the doctypes in each facet by descending doc_count.


#It is not straightforward to order within each facets with our data, since the 'description' column has duplicate values across different databases (such as 'article' and 'review'.
#To order within each facet, we have used acylam's suggestion (https://stackoverflow.com/questions/47580160/reorder-ggplot-barplot-x-axis-by-facet-wrap), ordering our dataframe and generating a new factor column with unique levels to be used as x-axis labels and then relabeling the x-axis with scale_x_discrete().
aux_vars$doctypes <- aux_vars$doctypes %>% 
  arrange(desc(result)) %>% #Sorting by descending results
  unite('id', description,db, sep='_', remove = FALSE) %>% #Creating new unique 'id' column by merging 'description' and 'db' cols
  mutate(id = factor(id, levels=id)) #Converting 'id' column to factor
  

#Additionally, we want to plot how many doctypes are in each database
#For that, we will need to create a new summarised df with labels to annotate each facet (reference: R Graphics Cookbook - https://r-graphics.org/recipe-annotate-facet)
aux_vars$doctypes_labels <- aux_vars$doctypes %>% 
  group_by(db) %>%
  summarise(label = paste(n(),"doctypes"))

#View(aux_vars$doctypes_labels)


plots$doctypes_bar <- aux_vars$doctypes %>%
ggplot( aes(x = id, y = result, color = db) ) +
  facet_wrap(vars(db), scales='free_x') + #Freeing x axis allows to only plot bars for doctypes that actually occur in the database
  geom_bar(stat='identity') + 
  geom_text(aes(label = result), size = 2.5, color = 'black', vjust= -0.5) + #Adding number of documents to each bar stack
  geom_text(x = Inf, y = 75000, size = 5,  color = 'black', aes(label = label), data = aux_vars$doctypes_labels, hjust=1.2) + #Annotating each facet with 'doctypes_labels'
  xlab('doctypes') +
  ylab('doc_count') +
  scale_x_discrete(breaks = aux_vars$doctypes$id,
                   labels = aux_vars$doctypes$description) + #Changing x-axis label names
  theme(axis.text.x = element_text(angle = 45,  hjust = 1, vjust = 1), legend.position = "none")

#plots$doctypes_bar


### Making a treemap plot

plots$doctypes_treemap <- ggplot(aux_vars$doctypes, 
                   aes(area = result, subgroup = db, fill = db, label = paste(description, result, sep = '\n'))) +
  geom_treemap() +
  geom_treemap_text(fontface = "italic", colour = "white", place = "centre") +
  geom_treemap_subgroup_border(colour = "black")


#plots$doctypes_treemap

### Each database uses their own set of categories for document classification, which makes it very difficult to compare them. To address this issue, we'll apply a normalized classification to our data.


#Articles : articles + reviews + in press...
#Proceeding/meeting_items : articles, abstracts, reviews...
#Book: Books, book chapters, review books  (not book reviews)
#Preprint: preprint
#Unidentified
#Other


##Obs:
#WoS document types: https://webofscience.help.clarivate.com/en-us/Content/document-types.html
#WoS' "article; early access" is the same as an "article in press" (https://images.webofknowledge.com/WOKRS533JR18/help/WOS/hp_results.html)
#A more detailed overview of Web of Science's Document Type field can be accessed at https://images.webofknowledge.com/images/help/WOS/hs_document_type.html
#We will consider the 'data paper' category as part of the 'Articles' group, since it is defined as "A scholarly publication describing a particular dataset or collection of datasets and usually published in the form of a peer-reviewed article in a scholarly journal".
#The Lens' glossary: https://support.lens.org/glossary/

aux_vars$doctypes_normalized <- aux_vars$doctypes %>%
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
  
#listing all document types in the 'other' category
cat(paste("'", unique(aux_vars$doctypes$description[!aux_vars$doctypes$description %in% c('article',
                  'journal article', 'review', 'article in press', 'article; early access', 'review; early access',
                  'article; data paper', 'data paper', 'article; retracted publication', 
                  'article; data paper; early access', 'reprint',
    'proceeding', 'conference proceedings article', 'conference proceedings', 
                       'conference paper', 'conference review', 'proceedings paper', 'meeting abstract',
                       'article; proceedings paper',
                  'book', 'book chapter', 'chapter', 'article; book chapter', 
    'review; book chapter', 
    'unidentified', 'preprint') ]), "'", collapse = ", ", sep = "" ))

#View(aux_vars$doctypes_normalized)
  

### Now that the data is normalized, we can make plots that will be more useful for comparisons between the databases

plots$doctypes_bar_normalized <- aux_vars$doctypes_normalized %>%
  group_by(db) %>%
  mutate(percentage = round( result/sum(result) * 100, 1 ), #Adding a percentage column
         description = factor(description, #Converting the description column to a factor to order the x-axis of the following plot in a convenient order
                              levels = c('Articles', 'Proceedings itens', 'Books and book chapters', 'Preprint', 'Unidentified', 'Other') ) ) %>%
ggplot( aes(x = description, y = result, color = db) ) +
  facet_grid(vars(db)) + #Freeing x axis allows to only plot bars for doctypes that actually occur in the database
  geom_bar(stat='identity') + 
  geom_text(aes(label = paste(result, ' (', percentage, '%)', sep = '')), size = 3.5, color = 'black', vjust= -0.5) + #Adding number of documents to each bar stack
  xlab('doctypes') +
  ylab('doc_count') +
  ylim(0,95000) +
  theme(axis.text.x = element_text(angle = 45,  hjust = 1, vjust = 1), legend.position = "none")

plots$doctypes_bar_normalized

View(aux_vars$doctypes_normalized %>%
  group_by(db) %>%
  mutate(perc = round( result / sum(result) * 100, 1 )))

#plots$doctypes_bar_normalized


plots$doctypes_treemap_normalized <- ggplot(aux_vars$doctypes_normalized, 
                   aes(area = result, subgroup = db, fill = db, label = paste(description, result, sep = '\n'))) +
  geom_treemap() +
  geom_treemap_text(fontface = "italic", colour = "white", place = "centre") +
  geom_treemap_subgroup_border(colour = "black")

#plots$doctypes_treemap_normalized



## In order to calculate citation metrics like the h-index, we'll need to extract citation data from the original bibliometrix dataframes
#We'll retrieve some other info as well, such as 'Publication Year' and 'database' fields

#View(lapply(dfs, head, n=5) %>% bind_rows(.id = 'id')) #Taking a look at the first 5 rows of each dataframe to select other potentially relevant fields that could be plotted against the citation data


#Getting dataframe with citations info about each document (needed to calculate the h-index and other metrics)

aux_vars$decades = c("Pre-1983", "1983-1992", 
                     "1993-2002", "2003-2012", 
                     "2013-2022", "Undefined") #Creating a vector that will be used to group our data by decade

aux_vars$citation_data <- dfs %>% 
  bind_rows(.id = 'db') %>% #Taking dataframes in list and binding them by row, while appending a 'db' column with database name 
  filter(!is.na(PY)) %>% #Removing any documents with unknown publication year
  mutate(Citing.Works.Count = as.numeric(Citing.Works.Count), 
         TC = case_when( db == 'lens'  ~ Citing.Works.Count,
    .default = TC)) %>% # Since Lens citations appear on the 'Citing.Works.Count' column, we have modified the 'TC' field to inherit citation count from this column if the record comes from lens
  mutate( UT = case_when( db == 'scopus'  ~  str_match(url, regex('^.*/?eid=(?<id>.*?)&.*$'))[,'id'], #Non-greedy regex matching to extract scopus' 'EID' from the 'url' column
    .default = UT)) %>% # Since bibliometrix does not recover scopus document identifier (EID), we have recovered it from the 'url' field
  remove_rownames %>% # Removing rownames from the dataframe
  select(db, TI, UT, DT,PY,TC) %>% # Selecting only relevant columns
  rename('pubid' = UT,
         'doctype' = DT, 
         'year' = PY, 
         'citations' = TC,
         'title' = TI) %>% # Renaming columns
  mutate( decade = case_when( #
    year < 1983 ~ aux_vars$decades[1],
    between(year, 1983,1992) ~ aux_vars$decades[2],
    between(year, 1993,2002) ~ aux_vars$decades[3],
    between(year, 2003,2012) ~ aux_vars$decades[4],
    between(year, 2013,2022) ~ aux_vars$decades[5],
    .default = aux_vars$decades[6]),
    decade = factor(decade, levels = aux_vars$decades),
    doctype = case_when( 
      doctype %in% c('ARTICLE', 'JOURNAL ARTICLE', 'REVIEW', 'ARTICLE; EARLY ACCESS', 'REVIEW; EARLY ACCESS', 
                     'ARTICLE; DATA PAPER', 'ARTICLE; RETRACTED PUBLICATION', 
                     'ARTICLE; DATA PAPER; EARLY ACCESS', 'REPRINT') ~ 'Articles',
      doctype %in% c('PROCEEDING', 'CONFERENCE PROCEEDINGS ARTICLE', 'CONFERENCE PROCEEDINGS', 
                     'CONFERENCE PAPER', 'CONFERENCE REVIEW', 'PROCEEDINGS PAPER', 'MEETING ABSTRACT',
                     'ARTICLE; PROCEEDINGS PAPER') ~ 'Proceedings itens', 
      doctype %in% c('BOOK', 'BOOK CHAPTER', 'CHAPTER', 'ARTICLE; BOOK CHAPTER', 'REVIEW; BOOK CHAPTER') ~ 'Books and book chapters', #Does not include book reviews
      doctype %in%  c('', NA) ~ 'Unidentified', 
      doctype %in%  c('PREPRINT') ~ 'Preprint',
      .default = 'Other')) #Adding a last value to aggregate all other doctypes

View(aux_vars$citation_data)



#Getting dataframe with citations info about each ARTICLE

aux_vars$citation_data_articles <- aux_vars$citation_data %>% 
  filter(doctype == 'Articles')


unique(aux_vars$citation_data$doctype)
unique(aux_vars$citation_data_articles$doctype)

##Plotting decade production (in terms of n_docs)

#plots$decade_prod_bar <-  aux_vars$citation_data %>% 
#  ggplot(aes(x = decade, fill = db)) +
#  geom_bar(position = 'dodge')
#
#plots$decade_prod_bar

##Ploting citation histogram


#aux_vars$citation_data_articles <- aux_vars$citation_data %>%
#  mutate(doctype = case_when( 
#    doctype %in% c('ARTICLE', 'JOURNAL ARTICLE', 'REVIEW', 'ARTICLE; EARLY ACCESS', 'REVIEW; EARLY ACCESS', 
#                       'ARTICLE; DATA PAPER', 'ARTICLE; RETRACTED PUBLICATION', 
#                       'ARTICLE; DATA PAPER; EARLY ACCESS', 'REPRINT') ~ 'Articles',
#    doctype %in% c('PROCEEDING', 'CONFERENCE PROCEEDINGS ARTICLE', 'CONFERENCE PROCEEDINGS', 
#                       'CONFERENCE PAPER', 'CONFERENCE REVIEW', 'PROCEEDINGS PAPER', 'MEETING ABSTRACT',
#                       'ARTICLE; PROCEEDINGS PAPER') ~ 'Proceedings itens', 
#    doctype %in% c('BOOK', 'BOOK CHAPTER', 'CHAPTER', 'ARTICLE; BOOK CHAPTER', 'REVIEW; BOOK CHAPTER') ~ 'Books and book chapters', #Does not include book reviews
#    doctype %in%  c('', NA) ~ 'Unidentified', 
#    doctype %in%  c('PREPRINT') ~ 'Preprint',
#    .default = 'Other')) #Adding a last value to aggregate all other doctypes


aux_vars$citation_data$doctype
View(aux_vars$citation_data)
View(aux_vars$citation_data %>%
       filter(db == "lens"))

table(aux_vars$citation_data_articles$doctype)

#View(aux_vars$citation_data %>% filter(citations <= 100))

plots$citation_histogram <- aux_vars$citation_data_articles %>%
  filter(citations <= 100) %>%
  ggplot(aes(x = citations)) +
  #ggplot(aes(x = citations, fill = db)) +
  geom_histogram(aes(fill = decade), breaks = c(0, 1, seq(5,100, by=5)), color = 'black') +
  geom_text(aes(label = after_stat(count)), 
            stat = 'bin', breaks = c(0, 1, seq(5,100, by=5)), 
            size = 2.5, 
            nudge_y = 3000) + #Adding number of documents to each bar stack
  #geom_text(aes(label = after_stat(count)), stat = 'bin', breaks = c(0, 1, seq(5,100, by=5)), size = 1.5, position = position_stack(vjust = 1.1)) + #Adding number of documents to each bar stack
  #geom_bar(show.legend = F) +
  facet_wrap(~ db, scale = 'free_x', ncol = 1) +
  scale_x_continuous(breaks = c(1,seq(5,100, by=5))) +
  theme(panel.grid.minor.x = element_blank())

plots$citation_histogram


plots$citation_smooth <- aux_vars$citation_data_articles %>%
  filter(citations <= 100) %>%
  ggplot(aes(x = citations, color = db)) +
  geom_smooth(aes(y = after_stat(count)), stat = 'bin', breaks = c(0, 1, seq(5,100, by=5)))

plots$citation_smooth

#Calculating h-index and other metrics (Garner et al., 2017)
#OBS: calculation of the m-quotient doesn't seem to make sense, since the sole purpose of this index is account for researchers of varying age, and we are not comparing different institutions


aux_vars$citation_metrics <- aux_vars$citation_data %>% 
  group_by(db) %>% 
  summarise(hindex = hindex(citations),
            eindex = eindex(citations),
            gindex = gindex(citations),
            hcindex = hcindex(citations = citations, pubyear = year),
            i10index = sum(citations > 10) )

aux_vars$citation_metrics_articles <- aux_vars$citation_data_articles %>% 
  group_by(db) %>% 
  summarise(n_articles = n(),
            n_citations = sum(citations),
            hindex = hindex(citations),
            eindex = eindex(citations),
            gindex = gindex(citations),
            hcindex = hcindex(citations = citations, pubyear = year),
            i10index = sum(citations > 10) )

View(aux_vars$citation_metrics_articles)

aux_vars$citation_metrics_articles %>%
  ggplot(aes(x = hindex, y = eindex)) +
  geom_line() +
  geom_point(aes(color = db))

aux_vars$citation_metrics_articles %>%
  ggplot(aes(x = hindex, y = gindex)) +
  geom_line() +
  geom_point(aes(color = db))


aux_vars$citation_metrics_articles %>%
  mutate(gindex = gindex - 163) %>%
  select(db, hindex, eindex, gindex) %>%
  pivot_longer(cols = c(eindex, gindex), names_to = 'index', values_to = 'value') %>%
  ggplot(aes(x = hindex)) +
  geom_line(aes(y = value, color = index)) +
  geom_point(aes(y = value, color = db)) +
  scale_y_continuous(
    name = 'eindex',
    sec.axis = sec_axis(trans = ~ .+163, name = 'gindex')
  ) +
  labs(color = "Legend Title", linetype = "Line Legend Label", shape = "Point Legend Label")

aux_vars$citation_metrics_articles %>%
  mutate(group = 'db') %>%
  ggplot(aes(x = group, y = gindex, fill = db)) +
  geom_bar(stat = 'identity', position = 'fill')

#Getting dataframe with citations info summarised per decade and database 

aux_vars$citation_decade_summary_articles <- aux_vars$citation_data_articles %>% 
  group_by(db, decade) %>% #Grouping data by database and decade to summarise it
  summarise(n_docs = n(), #Total number of docs per decade per db
            mean_citations = round(mean(citations), 1), #Mean of citations per decade per db
            total_citations = sum(citations),
            hindex = hindex(citations),
            eindex = eindex(citations),
            gindex = gindex(citations),
            hcindex = hcindex(citations = citations, pubyear = year),
            i10index = sum(citations > 10) 
            ) %>%
  ungroup() %>%
  pivot_longer(c(n_docs, mean_citations, total_citations, 
                 hindex, eindex, gindex, hcindex, i10index), 
               names_to = 'index_type', values_to = 'index_value') %>%
  group_by(db, index_type) %>%
  mutate(index_normalized = index_value/sum(index_value),
         index_type = factor(index_type, levels = c('n_docs', 'mean_citations', 'total_citations', 'hindex', 'eindex', 'gindex', 'hcindex', 'i10index')))

View(aux_vars$citation_decade_summary)


# Make annual production plot like the one from Author's production over time, where:
# - Size reflects number of articles; color reflects number of citations
# - Maybe restructure x-axis by merging years 'up to 19XX' into a single value
# - line (if needed) would show when each database started having records
# topAU <- authorProdOverTime(dfs$wos, k = 10, graph = TRUE)

aux_vars$citation_decade_summary %>% 
  pivot_wider(names_from = index_type, values_from = index_value)


#View(aux_vars$citation_decade_summary)

#View(aux_vars$citation_decade_summary %>% 
#  select(-index_normalized) %>%
#  pivot_wider(names_from = index_type, values_from = index_value) )

plots$decade_prod_mean_citations <- aux_vars$citation_decade_summary_articles %>% 
  select(-index_normalized) %>%
  pivot_wider(names_from = index_type, values_from = index_value) %>%
  ggplot(aes(x = decade, y = db, size = n_docs, alpha = mean_citations,
             color = db)) +
  geom_point() +
  geom_text(aes(label = n_docs), size = 3, color = 'black', alpha = 1, vjust = -3.5) + #Adding number of documents to each bar stack
  geom_text(aes(label = mean_citations), size = 3, color = 'red', alpha = 1, vjust = -5) + #Adding number of documents to each bar stack
  scale_size_continuous(range = c(1,20), breaks = c(1000,10000,30000,60000)) +
  theme(axis.text.x = element_text(angle = 45,  hjust = 1, vjust = 1)) +
  labs(alpha = 'mean_citations')

plots$decade_prod_mean_citations


plots$decade_prod_facets <- aux_vars$citation_decade_summary_articles %>% 
  #filter(!index_type %in% c('total_citations', 'gindex')) %>%
  ggplot(aes(x = decade, y = index_value, color = db)) +
  geom_point() +
  #geom_smooth(aes(group = db), se = F, alpha = .8) +
  geom_line(aes(group = db), linetype = 2, alpha = .8) +
  facet_wrap(~ index_type, scales = 'free', ncol = 2, 
             labeller = labeller(index_type = function(label) ifelse(label == "n_docs", "n_articles", label))) #Changing title of 'n_docs' 
  #theme(axis.text.x = element_text(angle = 45,  hjust = 1, vjust = 1))

plots$decade_prod_facets




##Internationalization
 

View(analyses$dimensions$Countries)

aux_vars$country_collab <- analyses %>%
  bind_rows(.id = 'db') #Taking dataframes in list and binding them by row, while appending a 'db' column with database name 


####Ideas:

# Make plots about:
# Country collaboration comparisons
# Most relevant sources
# Bar plot, with the 1st to the 10th most relevant sources of the database
# Make annual production plot like the one from Author's production over time, where:
# - Size reflects number of articles; color reflects number of citations
# - Maybe restructure x-axis by merging years 'up to 19XX' into a single value
# - line (if needed) would show when each database started having records
# topAU <- authorProdOverTime(dfs$wos, k = 10, graph = TRUE)
#Calculate H-index (and others): per year and database; per database -> use the dataframe with all the years and citations
#Plot of document language (available for all dataframes? R: NO!)


#All databases have fields regarding open access. Should I make some plots?
#Language (field only available for WoS and Scopus)

# Make plots with other fields from the main_info, like: 
# total number of sources
# Document average age
# Average Citations per doc
# N_Authors
# Author Appearances
# N_single_authored_docs
# Mean Documents per Author
# % of International co-authorships
# Talvez algo sobre as palavras-chave e/ou internacionalização? (Chord diagram ou Sankey?)

lapply(dfs, function(x) x[x[['PY']] < 1950])

#Vendo apenas os registros mais antigos (< 1950)
View(bind_rows(lapply(dfs, function(x) x %>% 
                        filter(PY < 1950) ), .id = 'db') %>% arrange(PY))

View(bind_rows(lapply(dfs, function(x) x %>% 
                        filter(PY == 1966) ), .id = 'db') %>% arrange(PY))
dfs %>%
map_df(~ {
  if (.x$PY < 1950) {
    return (.x)
  } else {
    return(NULL)
  }
})


#Plot to see if the percentage of articles with more than 10 citacions rises with document age
#Unfortunately, it did not happen
aux_vars$citation_data_articles %>%
  mutate(i10 = case_when(
    citations >= 10 ~ ">= 10 citations",
    TRUE ~ "< 10 citations")) %>%
  group_by(db, decade, i10) %>%
  summarise(cites = n()) %>%
  ungroup(i10) %>%
  mutate(perc = cites/sum(cites)) %>%
  ggplot(aes(x = decade, y = perc, fill = i10)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~ db)

View(aux_vars$citation_data_articles %>%
  filter(citations >= 10) %>%
  group_by(db, decade) %>%
  summarise(n()))

