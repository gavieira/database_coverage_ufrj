library(tidyverse)
library(ggVennDiagram)
library(patchwork)
library(ComplexHeatmap)

options(warn=-1)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rstudioapi::writeRStudioPreference("data_viewer_max_columns", 1000L) #View more than 50 columns


#Getting all files into objexts to use with bibliometrix
#wos_files <- c(Sys.glob("data/wos/[0-9]*txt"))
wos_files <- c(Sys.glob("data/wos/[0-9]*bib"))
#scopus_files <- c(Sys.glob("data/scopus/[0-9]*csv"))
scopus_files <- c(Sys.glob("data/scopus/[0-9]*bib"))
dimensions_files <- c(Sys.glob("data/dimensions/*csv"))
lens_files <- dimensions_files <- c(Sys.glob("data/lens/*csv"))

#Getting dataframes (bibliometrix)
#wos <- convert2df(wos_files, dbsource = "wos", format='bibtex')
#scopus <- convert2df(scopus_files, dbsource = "scopus", format = 'bibtex')
#dimensions <- convert2df(dimensions_files, dbsource = "dimensions", format = 'csv')


##### Reading only doi column for each dataset (way faster and less RAM usage)

#### WoS

#Let's get the first 5 rows to look at the data
view(read_delim("data/wos/01.tsv", delim = '\t', n_max=5))
#In WoS files, the doi identifier is present in the "DI" column
#We'll need the document type(DT) and the doi (DI) columns

#Now, let's get the full data for these columns:
wos <- read_delim("data/wos/01.tsv",
                        delim='\t',
                        col_select = c("DT", "DI"))

#### Scopus

view(read_delim("data/scopus/01_2016.csv", delim = ',', n_max=5))
#We'll need the "Document Type" and "DOI" columns

scopus <- read_delim("data/scopus/01_2016.csv",
                        delim=',',
                        col_select = c("Document Type", "DOI"))


#### Dimensions

View(read_delim("data/dimensions/Dimensions-Publication-2022-06-17_18-19-07.csv", delim = ',', n_max=5, skip=1)) #Skip is necessary since first line is not tabular data

#we'll need the "Publication Type" and "DOI" columns

dimensions <- read_delim("data/dimensions/Dimensions-Publication-2022-06-17_18-19-07.csv",
                        delim = ',',
                        col_select = c("Publication Type", "DOI"),
                     skip = 1)


#### Lens

View(read_delim("data/lens/ufrj_2017-2021.csv", delim = ',', n_max=5)) #Skip is necessary since first line is not tabular data

#we'll need the "Publication Type" and "DOI" columns



### concat_df():
#Gets all files (matching a given pattern)
#Reads specified columns into temporary dataframes 
#Joins them into a single, final dataframe
#Also, adds a column ("DB") to identify from which database is the data
#Has option to skip lines

concat_df <- function(directory, pattern, delimiter,
                      columns, database, skip=0) {
    files <- list.files(directory,
                        pattern = pattern,
                        full.names = TRUE)
    temp_dfs <- lapply(X = files,
                  FUN = read_delim,
                  delim = delimiter,
                  col_select = columns,
                  col_types = 'cc',
                  skip = skip)
    final_df <- do.call(rbind, temp_dfs) #Could also use the 'bind_rows()' function from dplyr
    final_df$DB <- database #Creating 3rd column with database name
    return(final_df)
}

scopus <- concat_df('data/scopus',
          pattern = '[0-9].*csv',
          delimiter = ',',
          columns = c("Document Type", "DOI"),
          database = 'scopus')

wos <- concat_df('data/wos',
                 pattern = '[0-9].*txt',
                 delimiter = '\t',
                 columns = c("DT", "DI"),
                 database = 'wos')

dimensions <- concat_df('data/dimensions',
                        pattern = '*zip',
                        delimiter = ',',
                        columns =c("Publication Type", "DOI"),
                        database = 'dimensions',
                        skip=1)


lens <- concat_df('data/lens',
                        pattern = '*csv',
                        delimiter = ',',
                        columns =c("Publication Type", "DOI"),
                        database = 'lens')

#Getting sets of DOIs (as vectors)
#These sets are the unique values from the "DOI" vectors of the original dataframes. NA values where also removed

set_wos <- unique(na.omit(wos$DI))
set_scopus <- unique(na.omit(scopus$DOI))
set_dimensions <- unique(na.omit(dimensions$DOI))
set_lens <- unique(na.omit(dimensions$DOI))

x <- list(WoS = set_wos, Scopus = set_scopus, Dimensions = set_dimensions)


#Generating Venn Diagrams
p1 <- ggVennDiagram(x[c("WoS","Scopus")], label = 'count') + theme(legend.position = "none")
p2 <- ggVennDiagram(x[c("Dimensions","Scopus")], label = 'count') + theme(legend.position = "none")
p3 <- ggVennDiagram(x[c("Dimensions","WoS")], label = 'count') + theme(legend.position = "none")
p4 <- ggVennDiagram(x) + theme(legend.position = "none")


#Plotting in a grid (with patchwork)

layout <- "
AB
CC
"

p1 + p2 + p3 +
  plot_layout(design = layout) +
  plot_annotation(tag_levels = 'A')

p4

#### Upset Plot
# Dá pra colocar em ordem decrescente (https://jokergoo.github.io/ComplexHeatmap-reference/book/upset-plot.html#upset-making-the-plot)

m1 = make_comb_mat(x)
UpSet(m1, left_annotation = upset_left_annotation(m1))


library(UpSetR)
#More on this package here: https://cran.r-project.org/web/packages/UpSetR/vignettes/basic.usage.html

upset(fromList(x), order.by = "freq",
      text.scale = c(1.3, 1.3, 1, 1, 2, 2),
      number.angles = 0,
      point.size = 3.5, 
      line.size = 2)


#For the future - filtering rows with NA values:
teste[is.na(teste$DOI),]