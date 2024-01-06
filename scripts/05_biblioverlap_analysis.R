#Loading libraries
library(tidyverse)
library(ggVennDiagram)
library(biblioverlap)
library(gridExtra)

#sourcing functions script
source('scripts/00_functions.R')

plots <- list() #variable to hold all plots

db_list <- readRDS('output/data/normalized_dfs.rds') #importing datasets


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


venn_plot()

ggVennDiagram(lapply(db_list_matched[1:2], function(db) db$UUID ), force_upset = TRUE)

##Generating venn diagram plot

db_list_matched <- biblioverlap_results$db_list #saving db_list from biblioverlap in another object

##Venn diagram for several doctypes
doctypes <- c("All", "Articles", "Books and book chapters", "Proceedings itens")

venn <- lapply(doctypes, function(doctype) venn_by_doctype(doctype, db_list_matched )) #Creating venn diagram for each doctype (in a list)
names(venn) <- doctypes #Adding name to each plot


##Venn diagram for docs classified as 'unidentified' or 'other'
uuid_unidentified_other <- lapply(db_list_matched, function(db) {
  db <- db %>%
    filter(DT %in% c("Unidentified", "Other"))
  return(db$UUID)
})

venn$`Unidentified/Other` <- custom_venn(uuid_unidentified_other , title = "Unidentified/Other")


##Venn diagram for docs in the h-core of each database
hindex <- lapply(db_list_matched, function(db) hindex(db$TC) )

hcore <- setNames( 
  lapply(names(hindex), function(db_name) {
  db_list_matched[[db_name]] %>%
    filter(TC >= hindex[[db_name]])
}), names(hindex) )


uuid_hcore <- lapply(hcore, function(df) df$UUID)

venn$hcore <- custom_venn(uuid_hcore , title = "H-Core")


### Plotting all Venn Diagrams in a single grid - saving them to the plots list
plots$venn <- grid.arrange(grobs = venn, ncol = 3)



####Saving all plots to files
save_plot_list(plots)
