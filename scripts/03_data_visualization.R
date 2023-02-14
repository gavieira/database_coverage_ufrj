library(tidyverse)
library(bibliometrix)


#load('output/data/dataframes.Rdata') #loading dataframes

#Running biblioAnalysis for each dataset (takes a while...)

#biblio_dimensions <- biblioAnalysis(dimensions_df)
#biblio_scopus <- biblioAnalysis(scopus_df)
#biblio_wos <- biblioAnalysis(wos_df)
#biblio_lens <- biblioAnalysis(lens_df)


#biblio_dimensions <- biblioAnalysis(read_csv('data/merged_data/dimensions.csv'))
#biblio_scopus <- biblioAnalysis(read_csv('data/merged_data/scopus.csv'))
#biblio_wos <- biblioAnalysis(read_csv('data/merged_data/wos.csv'))
#biblio_lens <- biblioAnalysis(read_csv('data/merged_data/lens.csv', col_types = cols(AU_CO = 'c'))) #If not specified as a character ('c') column, the biblioanalysis will fail


#Saving bilioanalysis variables to a file so we don't need to always recreate them from scratch 

#save(biblio_dimensions, biblio_lens, biblio_scopus, biblio_wos, file = 'output/data/biblioanalyses.Rdata')


#Loading the biblioanalyses objects into the environment
load('output/data/biblioanalyses.Rdata')

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

# Generating a list containing all analyses and a vector with all database names
analyses <- list(dimensions = biblio_dimensions, lens = biblio_lens,
                 scopus = biblio_scopus, wos = biblio_wos)

dbs <- names(analyses)

# Getting years by database
get_years <- function(b_analysis, database) {
  year_df <- as.data.frame(table(b_analysis$Years), stringsAsFactors = F) %>% #Counting per year
  rename('year' = 1,'freq' = 2) %>% #Changing column names
  mutate(db = database) #Creating database column
  return(year_df)
}

years <- list()
years$long <- map2(analyses, dbs, get_years) %>%
  bind_rows

years$wide <- years$long %>%
  spread(db, freq)

View(years$long) 
View(years$wide) 


ggplot(years) +
  aes(x = year, y = freq, fill = db) +
  geom_bar(position="fill", stat="identity") +
theme(axis.text.x = element_text(angle = 45,
                                 hjust = 1, 
                                 vjust = 1))


ggplot(years) +
  aes(x = year, y = freq, group = db, color = db) +
  geom_line() +
  geom_point() +
theme(axis.text.x = element_text(angle = 45,
                                 hjust = 1, 
                                 vjust = 1))

