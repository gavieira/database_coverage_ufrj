## Merging database data into single files

library('bibliometrix')
library('tidyverse')

#sourcing functions script
source('scripts/00_functions.R')

## Using bibliometrix to merge files (needs a lot of RAM and takes a while to run...)

data_dir <- '~/ufrj_data_backup/data/' #Setting base data directory
outdir <- paste0(data_dir,'/merged_data') #Setting output directory of merged data

## Merging Dimensions data

filelist <-  list.files(path=paste0(data_dir,'/dimensions'), 
                        pattern='^.*.zip$', 
                        full.names = T)

dimensions_df <- convert2df(filelist, 
                        dbsource = 'dimensions', 
                        format = 'csv')

write_df(dimensions_df, outdir, 'dimensions.csv')

## Merging Scopus data (bibtex)

filelist <-  list.files(path=paste0(data_dir,'/scopus'), 
                        pattern='^.*.bib$', 
                        full.names = T)

scopus_df <- convert2df(filelist, 
                        dbsource = 'scopus', 
                        format = 'bibtex')

write_df(scopus_df, outdir, 'scopus.csv')

## Merging Scopus data (csv) - if i recall correctly, the csv has some fields that the bibtex file doesn't, but bibliometrix documentation https://cran.r-project.org/web/packages/bibliometrix/bibliometrix.pdf) states that the convert2df function only reads bibtex files from scopus (though it appears to read csv files as well)

#filelist <-  list.files(path=paste0(data_dir,'/scopus'), 
#                        pattern='^.*.csv$', 
#                        full.names = T)
#
#scopus_df <- convert2df(filelist, 
#                        dbsource = 'scopus', 
#                        format = 'csv')
#
#write_df(scopus_df, outdir, 'scopus_csv.csv')


## Merging Lens data

filelist <-  list.files(path=paste0(data_dir,'/lens'), 
                        pattern='^.*.csv$', 
                        full.names = T)

lens_df <- convert2df(filelist, 
                        dbsource = 'lens', 
                        format = 'csv')

write_df(lens_df, outdir, 'lens.csv')

## Merging Web of Science (WoS) data

filelist <-  list.files(path=paste0(data_dir,'/wos'), 
                        pattern='^.*.bib$', 
                        full.names = T)

wos_df <- convert2df(filelist, 
                        dbsource = 'wos', 
                        format = 'bibtex')

write_df(wos_df, outdir, 'wos.csv')


### Reading the dfs from the files (faster than using convert2df each time)

dimensions_df <- read.csv( paste0(outdir,'/dimensions.csv') )
scopus_df <- read.csv( paste0(outdir,'/scopus.csv') )
lens_df <- read.csv( paste0(outdir,'/lens.csv') )
wos_df <- read.csv( paste0(outdir,'/wos.csv') )

##We'll also be saving these dataframe objects into a .Rdata file 

dfs <- list(dimensions = dimensions_df, lens = lens_df, scopus = scopus_df, wos = wos_df) #Merging dataframes into a named list

dfs <- lapply(dfs, function(db) {select(db, -AB)}) #Removing abstract column 

saveRDS(dfs, file = 'output/data/dfs.rds')
