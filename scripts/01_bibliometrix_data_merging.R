## Merging database data into single files

library('bibliometrix')
library('tidyverse')

#sourcing functions script
source('scripts/00_functions.R')

## Using bibliometrix to merge files (needs a lot of RAM and takes a while to run...)

#Writing function to write merged dfs in a custom output directory
write_df <- function(df, outdir, filename) {
  dir.create(outdir, showWarnings = F)
  write_csv(df, sprintf("%s/%s", outdir, filename))
}

data_dir <- '/home/gabriel/ufrj_data_backup/data/' #Setting base data directory
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

## Merging Scopus data (csv)

filelist <-  list.files(path=paste0(data_dir,'/scopus'), 
                        pattern='^.*.csv$', 
                        full.names = T)

scopus_df <- convert2df(filelist, 
                        dbsource = 'scopus', 
                        format = 'csv')

write_df(scopus_df, outdir, 'scopus_csv.csv')


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


### Reading the dfs from the files


#dimensions_df <- read.csv( paste0(outdir,'dimensions.csv') )
scopus_df <- read.csv( paste0(outdir,'/scopus.csv') )
scopus_csv_df <- read.csv( paste0(outdir,'/scopus_csv.csv') )
#dimensions_df <- read.csv( paste0(outdir,'wos.csv') )

test <- bind_rows()



##We'll also be saving these dataframe objects into a .Rdata file 

dfs <- list(dimensions = dimensions_df, lens = lens_df, scopus = scopus_df, wos = wos_df)

save(dfs, file = 'output/data/dfs.Rdata')
