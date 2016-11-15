setClass("GenomicScoresResource", contains="AnnotationHubResource")

setMethod(".get1", "GenomicScoresResource",
   function(x, ...)
{
    .require("GenomicScores")
    path2rds <- cache(getHub(x))
    object <- readRDS(path2rds)

    mdobj <- metadata(object)
    gscores <- GScores(provider=mdobj$provider,
                       provider_version=mdobj$provider_version,
                       download_url=mdobj$download_url,
                       download_date=mdobj$download_date,
                       reference_genome=mdobj$reference_genome,
                       data_pkgname=mdobj$data_pkgname,
                       data_dirpath=dirname(path2rds)) ## is 'dirname()'

    rm(object)
    gscores
})
