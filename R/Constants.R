.ALL_COLUMNS <- c("BiocVersion", "DataProvider", "Title", "SourceFile",
    "Species", "SourceUrl", "SourceVersion", "TaxonomyId", "Genome",
    "Description", "Tags", "RDataClass", "RDataPath", ## new stuff follows
    "Coordinate_1_based", "Maintainer", "RDataVersion", "RDataDateAdded",
    "Recipe")
.DEFAULT_COLUMNS <- c("Title", "Species", "TaxonomyId", "Genome",
    "Description", "Tags", "RDataClass", "RDataPath")
## FIXME: why are these different from '.DEFAULT_COLUMNS'?
.AHINFO_COLUMNS <- c('BiocVersion', 'DataProvider', 'Description',
     'Genome', 'Tags', 'SourceUrl', 'SourceVersion', 'Species', 'RDataPath')
