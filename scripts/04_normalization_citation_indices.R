library(tidyverse)

#sourcing functions script
source('scripts/00_functions.R')

plots <- list() #Will hold all plots


##### We are going to (i) normalize doctype info in the original dataset and (ii) add a column to identify publications by decade, both of which will be useful for downstream analyses
dfs <- readRDS('output/data/dfs.rds')

normalized_dfs <- lapply(dfs, function(df) normalize_df(df))


View(normalized_dfs)

saveRDS(normalized_dfs, file = 'output/data/normalized_dfs.rds')


table(normalized_dfs$lens$DT)

## In order to calculate citation metrics like the h-index, we'll need to extract a subset of the normalized data 

normalized_dfs <- readRDS('output/data/normalized_dfs.rds')

sum(citation_data$Dimensions$TC == 0)
sum(normalized_dfs$Lens$TC == 0)
sum(normalized_dfs$Scopus$TC == 0)
sum(normalized_dfs$WoS$TC == 0)

citation_data <- normalized_dfs %>%
  bind_rows(.id = 'db') %>% #Taking dataframes in list and binding them by row, while appending a 'db' column with database name
  select(db, TI, DT,PY, decade, TC) %>% # Selecting only relevant columns
  rename('doctype' = DT, 
         'year' = PY, 
         'citations' = TC,
         'title' = TI) %>% # Renaming columns
  filter(doctype == 'Articles') #We'll use only articles to calculate citation indices


citation_data %>%
  #filter(between(citations, 0, 1)) %>%
  filter(citations == 0) %>%
  group_by(db) %>%
  summarise(count = n())

##Plotting decade production (in terms of articles)

#plots$decade_prod_bar <-  citation_data %>% 
#  ggplot(aes(x = decade, fill = db)) +
#  geom_bar(position = 'dodge', color = 'black')
#
#plots$decade_prod_bar
#
#plots$decade_prod_line <- citation_data %>% 
#  ggplot(aes(x = decade, y = ..count.., color = db, group = db)) +
#  geom_line(stat = 'count') +
#  geom_point(stat = 'count')
#
#plots$decade_prod_line

  

##Ploting citation histogram

plots$citation_histogram <- citation_data %>%
  group_by(db) %>%
  mutate(citations = ifelse(citations > 100, 101, citations)) %>% #this will be necessary to plot the long tail
  ggplot(aes(x = citations)) +
  geom_histogram(aes(fill = decade), breaks = c(0, 1, seq(5,100, by=5), 105), closed = 'left', color = 'black') +
  geom_density(aes(x = citations), 
               stat = 'count', 
               color = 'white', 
               linetype = 'dashed') + # Add density plot
  geom_text(aes(label = after_stat(count)), 
            stat = 'bin', breaks = c(0, 1, seq(5,100, by=5), 105), closed = 'left',
            size = 3, 
            nudge_y = 1500,
            hjust = rep(c(.9, rep(0.5, 21)), 4), # Nudging the first label of each facet a little bit to the left
            ) + #Adding number of documents to each bar stack
  facet_wrap(~ db, scale = 'free_x',  ncol = 1) +
  scale_x_continuous(breaks = c(1,seq(5,100, by=5), 105),
                     labels = c(1,seq(5,100, by=5), 'Inf')) +
  labs(x = 'Number of citations', 
       y = 'Document count',
       fill = 'Decade') +
  theme(panel.grid.minor.x = element_blank())

plots$citation_histogram


#View all bars to make sure that the most common documents are the ones without citation
citation_data %>%
  group_by(db) %>%
  mutate(citations = ifelse(citations > 100, 101, citations)) %>% #this will be necessary to plot the long tail
  ggplot(aes(x = citations)) +
  geom_histogram(aes(fill = decade), breaks = c(0, seq(1,100), 105), color = 'black') +
  #geom_text(aes(label = after_stat(count)), 
  #          stat = 'bin', breaks = c(0, seq(1,100, by=1), 105), closed = 'left',
  #          size = 3, 
  #          angle = 90,
  #          nudge_y = 3000
  #          ) + #Adding number of documents to each bar stack
  facet_wrap(~ db, scale = 'free_x', ncol = 1) +
  scale_x_continuous(breaks = c(1,seq(5,100, by=5), 105),
                     labels = c(1,seq(5,100, by=5), 'Inf')) +
  labs(x = 'Number of citations', 
       y = 'Document count',
       fill = 'Decade') +
  theme(panel.grid.minor.x = element_blank())
  
#Calculating h-index and other metrics (Garner et al., 2017)
#OBS: calculation of the m-quotient doesn't seem to make sense, since the sole purpose of this index is account for researchers of varying age, and we are not comparing different institutions

citation_metrics <- citation_data %>% 
  group_by(db) %>% 
  summarise(n_articles = n(),      
            total_citations = sum(citations), 
            hindex = hindex(citations),
            eindex = eindex(citations),
            gindex = gindex(citations),
            hcindex = hcindex(citations = citations, pubyear = year),
            i10index = sum(citations > 10) )


#saveRDS(citation_metrics, 'output/data/citation_metrics.rds') #Saving citation metrics to a .rds file

View(citation_metrics)

#Getting dataframe with citations info summarised per decade and database 

citation_decade_summary <- citation_data %>% 
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

View(citation_decade_summary)


# Make annual production plot like the one from bibliometrix's Author's production over time, where:
# - Size reflects number of articles; color reflects number of citations
# - Maybe restructure x-axis by merging years 'up to 19XX' into a single value
# - line (if needed) would show when each database started having records
# topAU <- authorProdOverTime(dfs$wos, k = 10, graph = TRUE)

citation_decade_summary %>% 
  pivot_wider(names_from = index_type, values_from = index_value)


#View(citation_decade_summary)

#View(citation_decade_summary %>% 
#  select(-index_normalized) %>%
#  pivot_wider(names_from = index_type, values_from = index_value) )

#plots$decade_prod_mean_citations <- citation_decade_summary %>% 
#  select(-index_normalized) %>%
#  pivot_wider(names_from = index_type, values_from = index_value) %>%
#  ggplot(aes(x = decade, y = db, size = n_docs, alpha = mean_citations,
#             color = db)) +
#  geom_point() +
#  geom_text(aes(label = n_docs), size = 3, color = 'black', alpha = 1, vjust = -3.5) + #Adding number of documents to each bar stack
#  geom_text(aes(label = mean_citations), size = 3, color = 'red', alpha = 1, vjust = -5) + #Adding number of documents to each bar stack
#  scale_size_continuous(range = c(1,20), breaks = c(1000,10000,30000,60000)) +
#  theme(axis.text.x = element_text(angle = 45,  hjust = 1, vjust = 1)) +
#  labs(alpha = 'mean_citations')
#
#plots$decade_prod_mean_citations


plots$decade_prod_facets <- citation_decade_summary %>% 
  #filter(!index_type %in% c('total_citations', 'gindex')) %>%
  ggplot(aes(x = decade, y = index_value, color = db)) +
  geom_point() +
  #geom_smooth(aes(group = db), se = F, alpha = .8) +
  geom_line(aes(group = db), linetype = 2, alpha = .8) +
  labs(x = 'Decade', y = 'Index value', color = 'Database') +
  facet_wrap(~ index_type, scales = 'free', ncol = 2, 
             labeller = labeller(index_type = function(label) ifelse(label == "n_docs", "n_articles", label))) #Changing title of 'n_docs' 
#theme(axis.text.x = element_text(angle = 45,  hjust = 1, vjust = 1))

plots$decade_prod_facets



##Saving all plots

#View(plots)

save_plot_list(plots)
