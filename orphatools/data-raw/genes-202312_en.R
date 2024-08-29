library(dplyr)
library(stringr)
library(xml2)
source('R/data-genes.R')

# Read file
filepath = file.path('data-raw', 'genes-202312_en.xml')
genes_data_raw =
  read_xml(filepath, encoding = 'ISO-8859-1') %>%
  as_list()

# Check version
date = attr(genes_data_raw$JDBOR, 'ExtractionDate') %>% as.Date() %>% as.character()
if(!length(date))
  date = attr(genes_data_raw$JDBOR, 'date') %>% as.Date() %>% as.character()
lang = attr(genes_data_raw$JDBOR$DisorderList$Disorder$ExpertLink, 'lang')
version = sprintf('%s_%s', date, lang)

# Process
genes_data = process_genes(genes_data_raw)

# Save
df_versions = data.frame(version=version, location='internal', default=TRUE)
versions_filepath = file.path('inst', 'extdata', 'genes_versions.csv')
write.csv2(df_versions, versions_filepath, row.names=F)
# usethis::use_data(genes_data, internal=TRUE)

