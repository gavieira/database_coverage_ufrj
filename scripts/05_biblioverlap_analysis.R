#Loading libraries
library(tidyverse)
library(ggVennDiagram)
library(biblioverlap)
library(patchwork)

#sourcing functions script
source('scripts/00_functions.R')

plots <- list() #variable to hold all plots

db_list <- readRDS('output/data/normalized_dfs.rds') #importing datasets


######Making a plot to show the percentage of DOIless records in each database
#Necessary to justify why there is a need to match using an approach other than the DOI


doi_dt <- db_list %>% #Generating table with count for each doctype
  bind_rows(.id = 'db') %>%
  group_by(db, DT) %>%
  summarise(present = sum(!is.na(DI)),
            absent = sum(is.na(DI))) %>%
  ungroup() %>%
  pivot_longer(cols = c(present, absent), names_to = 'doi', values_to = 'count') %>%
  complete(db, DT, doi, fill = list(count = 0)) %>%
  mutate(doi = factor(doi, levels = c('present', 'absent')))
         

doi_all <- doi_dt %>% #Generating table with count for all data
  group_by(db, doi) %>%
  summarise(count = sum(count)) %>%
  mutate(DT = 'All')
  

doi_count <- bind_rows(doi_dt, doi_all) %>% #Merging the two tables, and adding percentage column
       group_by(db, DT) %>% 
       mutate(perc = round( count / sum(count) * 100, digits = 1) )

#View(doi_count)


plots$doi_count <- doi_count %>% #Making a faceted barplot for DOi presence/absence in each database
  ggplot(aes(x = DT, y = ifelse(between(count, 1, 400), 400, count),  count, 
             fill = doi, 
             label = ifelse(is.na(perc) | perc == 0, count, paste0(count, ' (', perc,'%)') ) ) ) +
  facet_wrap(~db, nrow = 4, strip.position = 'right') +
  geom_bar(stat = 'identity', position = position_dodge()) +
  geom_text(position = position_dodge(width = 0.9), vjust = -0.5) +
  ylim(0, 96000) +
  labs(x = "Document type",
       y = "Document count") +
  scale_fill_manual(values = c('present' = '#00BFC4', 'absent' = '#F8766D'), #Change legend colors
                    name = "DOI", #Change legend title
                    labels = c('present' = 'Present', 'absent' = 'Absent')) + # Change legend label
  theme(text = element_text(size = 17)) #Changing text size


plots$doi_count

####

plots$doi_perc <-  db_list %>%
  bind_rows(.id = 'db') %>%
  group_by(db) %>%
  summarise(present = round( ( sum(!is.na(DI)) / length(DI) * 100), digits = 1 ),
            absent = round( ( sum(is.na(DI)) / length(DI) * 100), digits = 1 ) ) %>%
  pivot_longer(cols = c(present, absent), names_to = 'doi', values_to = 'perc') %>%
  mutate(doi = factor(doi, levels = c('present', 'absent'))) %>%
  ggplot(aes(x = db, y = perc, fill = doi, label = perc)) +
  geom_bar(stat = 'identity') +
  geom_text(position = position_stack(vjust = 0.5), size = 7) +
  labs(x = "Database (db)",
       y = "Percentage") +
  scale_fill_manual(values = c('present' = '#00BFC4', 'absent' = '#F8766D'), #Change legend colors
                    name = "DOI", #Change legend title
                    labels = c('present' = 'Present', 'absent' = 'Absent')) + # Change legend label
  theme(text = element_text(size = 17)) #Changing text size




#Creating the list with the names of the columns used for the matching
matching_cols <- list(DI = 'DI',
                      TI = 'TI',
                      PY = 'PY',
                      AU = 'AU',
                      SO = 'J9')

##Running biblioverlap function and saving the results to a rds file
#biblioverlap_results <- biblioverlap(db_list, matching_fields = matching_cols, n_threads = 16)
#saveRDS(biblioverlap_results, 'output/data/biblioverlap_results.rds')



#####Making biblioverlap plots

biblioverlap_results  <- readRDS('output/data/biblioverlap_results.rds') #Loading the biblioverlap results

plots$matching_summary_plot <- matching_summary_plot(biblioverlap_results$summary, add_logo = FALSE) #Generating matching_summary

##Generating venn diagram plot

db_list_matched <- biblioverlap_results$db_list #saving db_list from biblioverlap in another object

##Venn diagram for several doctypes
doctypes <- c("All", "Articles", "Proceedings itens", "Books and book chapters", "Other")

venn_subset_matches <- lapply(doctypes, function(doctype) venn_by_doctype(doctype, db_list_matched, all_matches = FALSE)) #Creating venn diagram for each doctype (only shows matches within given subset)
names(venn_subset_matches) <- doctypes #Adding name to each plot


venn_all_matches <- lapply(doctypes, function(doctype) venn_by_doctype(doctype, db_list_matched, all_matches = TRUE)) #Creating venn diagram for each doctype (shows all matches)
names(venn_all_matches) <- doctypes #Adding name to each plot


##Venn diagram for all ARTICLES in the h-core of each database
db_list_articles <- lapply(db_list_matched, function(db) filter(db, DT == 'Articles') )

hindex <- lapply(db_list_articles, function(db) hindex(db$TC) )

hcore <- setNames( 
  lapply(names(hindex), function(db_name) {
  db_list_articles[[db_name]] %>%
    filter(TC >= hindex[[db_name]])
}), names(hindex) )


hcore_subset_matches <- hcore
hcore_all_matches <- get_all_subset_matches(hcore, db_list_matched)

View(hcore_subset_matches)
View(hcore_all_matches)

venn_subset_matches$hcore <- custom_venn(lapply(hcore_subset_matches, function(df) df$UUID), title = "H-Core")
venn_all_matches$hcore <- custom_venn(lapply(hcore_all_matches, function(df) df$UUID), title = "H-Core")

venn_subset_matches$hcore
venn_all_matches$hcore



venn_order <- c("All", "Articles", "hcore", "Proceedings itens", "Books and book chapters", "Other")

### Plotting all Venn Diagrams in a single grid - saving them to the plots list
plots$venn_subset_matches <- wrap_plots(venn_subset_matches[venn_order], ncol = 3, ) + plot_annotation(tag_levels = 'A', tag_suffix = ')') &
  theme(plot.tag = element_text(size = 20) )

plots$venn_all_matches <- wrap_plots(venn_all_matches[venn_order], ncol = 3, ) + plot_annotation(tag_levels = 'A', tag_suffix = ')') &
  theme(plot.tag = element_text(size = 20) )

plots$venn_all_matches

####Saving all plots to files
save_plot_list(plots)



##Obtaining documents that have been matched
matched_recs <- db_list_matched %>%
  bind_rows(.id = 'db') %>%
  select(-DB) %>%
  arrange(UUID) %>%
  group_by(UUID) %>%
  filter(n() >= 2)

View(matched_recs)

View(matched_recs[691:693,]) #Cross-matching example

#Checking total and percentage of cross-doctype matches
doctype_matches <- matched_recs %>%
  group_by(UUID) %>%
  summarise(single_doctype = n_distinct(DT) == 1,
            multi_doctype = n_distinct(DT) > 1 ) %>%
  ungroup() %>%
  select(-UUID) %>%
  summarise(total_matches = length(single_doctype),
            single_doctype = sum(single_doctype),
            multi_doctype = sum(multi_doctype)) %>%
  pivot_longer(cols = 1:3, names_to = 'match_type',
               values_to = 'count') %>%
  mutate(perc = round( (count / max(count)) * 100, digits = 2) ) 

View(doctype_matches)


#Checking the distribution of doctypes for the subset of records classified as 'other' plus all its matches
other_subset <- lapply(db_list_matched, function(df) filter(df, DT == 'Other'))

other_doctype_matches <- get_all_subset_matches(other_subset, db_list_matched) %>%
  bind_rows(.id = 'db') %>%
  group_by(db, DT) %>%
  summarize(count = n()) %>%
  ungroup(DT) %>%
  mutate(perc = round( (count / sum(count)) * 100, digits = 2 ) )

View(other_doctype_matches)



#Obtaining venn diagram for articles with:
#0 cites in Lens/Dimensions
#1+ cites in Wos/Scopus

articles <- setNames(lapply(1:length(db_list_matched), function(index) {
  new_df <- db_list_matched[[index]] %>%
    filter(DT == 'Articles')
  new_df['db'] <- names(db_list_matched)[[index]] #Adding column with database name
  return(new_df)
}), names(db_list_matched) )

articles_citation_filtered <- lapply(articles, function(df) {
  if (unique(df[['db']]) %in% c('Lens', 'Dimensions')) {
    return(filter(df, TC == 0))
  } else {
    return(filter(df, TC == 0))
  }
})

View(articles_citation_filtered)

ggVennDiagram(lapply(articles_citation_filtered, function(df) df$UUID))


#Biblioverlap plot with documents classified as 'review' in WoS and Scopus

