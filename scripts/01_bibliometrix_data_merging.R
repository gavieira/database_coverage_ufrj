## Merging database data into single files

library('bibliometrix')
library('tidyverse')

## Using bibliometrix to merge files (needs a lot of RAM and takes a while to run...)

#Writing function to write merged dfs in a custom output directory
write_df <- function(df, outdir, filename) {
  dir.create(outdir, showWarnings = F)
  write_csv(df, sprintf("%s/%s", outdir, filename))
}

data_dir <- 'data' #Setting base data directory
outdir <- paste0(data_dir,'/merged_data') #Setting output directory of merged data

## Merging Dimensions data

filelist <-  list.files(path=paste0(data_dir,'/dimensions'), 
                        pattern='^.*.zip$', 
                        full.names = T)

dimensions_df <- convert2df(filelist, 
                        dbsource = 'dimensions', 
                        format = 'csv')

write_df(dimensions_df, outdir, 'dimensions.csv')

## Merging Scopus data

filelist <-  list.files(path=paste0(data_dir,'/scopus'), 
                        pattern='^.*.bib$', 
                        full.names = T)

scopus_df <- convert2df(filelist, 
                        dbsource = 'scopus', 
                        format = 'bibtex')

write_df(scopus_df, outdir, 'scopus.csv')


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
