library(dplyr)
library(stringr)
library(xml2)
source('R/data-pack.R')

# Unzip
zip_filepath = file.path('data-raw', 'pack-2023_en.zip')
zip_filename = basename(zip_filepath)
unzipdir = file.path(tempdir(), zip_filename)
utils::unzip(zip_filepath, junkpaths = TRUE, exdir=unzipdir)

# Detect files
unzipped_files = list.files(unzipdir)
nomenclature_file = unzipped_files %>%
  Filter(\(x) str_detect(x, 'ORPHAnomenclature') & str_ends(x, '.xml'),.)
classif_folder = unzipped_files %>%
  Filter(\(x) str_detect(x, 'ORPHAclassification'), .)

# Read nomenclature file
nomenclature_data_raw =
  read_xml(file.path(unzipdir, nomenclature_file),
           encoding = 'ISO-8859-1') %>%
  as_list()

# Check version
date = attr(nomenclature_data_raw$JDBOR, 'ExtractionDate') %>% as.Date() %>% as.character()
if(!length(date))
  date = attr(nomenclature_data_raw$JDBOR, 'date') %>% as.Date() %>% as.character()
lang = attr(nomenclature_data_raw$JDBOR$DisorderList$Disorder$ExpertLink, 'lang')
version = sprintf('%s_%s', date, lang)

# Process
nomenclature_data = process_nomenclature(nomenclature_data_raw)
classif_data = process_classif(file.path(unzipdir, classif_folder))
pack_data = c(nomenclature_data, classif_data)

# Save
df_versions = data.frame(version=version, location='internal', default=TRUE)
versions_filepath = file.path('inst', 'extdata', 'pack_versions.csv')
write.csv2(df_versions, versions_filepath, row.names=F)
# usethis::use_data(pack_data, internal=TRUE)

# Clean tempdir
file.remove(list.files(unzipdir, full.names = TRUE))
