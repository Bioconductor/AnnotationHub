## all code for Epigenome RoadMap files

setClass("EpiMetadataResource", contains="AnnotationHubResource")

setMethod(".get1", "EpiMetadataResource",
   function(x, ...)
{
    read.delim(cache(.hub(x)))
})

setClass("EpigenomeRoadmapFileResource", contains="AnnotationHubResource")

setMethod(".get1", "EpigenomeRoadmapFileResource",
    function(x, ...)
{
    .require("rtracklayer")
    yy <- .hub(x)
    gr <- rtracklayer::import(cache(yy), format="bed", genome=yy$genome,
        extraCols=c(signalValue="numeric", pValue="numeric", qValue="numeric",
        peak="numeric"))
    .tidyGRanges(x, gr)
})

setClass("EpichmmModelsResource", contains="AnnotationHubResource")

setMethod(".get1", "EpichmmModelsResource",
    function(x, ...)
{
    .require("rtracklayer")
    yy <- .hub(x)
    gr <- rtracklayer::import(cache(yy), format="bed", genome="hg19")
    gr <- .mapAbbr2FullName(gr)
    .tidyGRanges(x, gr)
    
})


## this data is got from :
## chromhmmSegmentations/ChmmModels/coreMarks/jointModel/final/labelmap_15_coreMarks.tab
## chromhmmSegmentations/ChmmModels/coreMarks/jointModel/final/colormap_15_coreMarks.tab
## the bed file has abbr - and these 3 columns are helpful for collaborators.
.mapAbbr2FullName <- function(gr) {
    map <- as.data.frame(matrix(c(
        "1_TssA", "Active TSS", "Red", rgb(255,0,0, maxColorValue=255),
        "2_TssAFlnk", "Flanking Active TSS", "Orange Red", rgb(255,69,0, 
             maxColorValue=255),
        "3_TxFlnk", "Transcr. at gene 5' and 3'", "LimeGreen", rgb(50,205,50, 
             maxColorValue=255),
        "4_Tx", "Strong transcription", "Green", rgb(0,128,0, 
             maxColorValue=255),
        "5_TxWk", "Weak transcription", "DarkGreen", rgb(0,100,0, 
             maxColorValue=255),
        "6_EnhG", "Genic enhancers", "GreenYellow", rgb(194,225,5, 
             maxColorValue=255),
        "7_Enh", "Enhancers", "Yellow", rgb(255,255,0, 
             maxColorValue=255),
        "8_ZNF/Rpts", "ZNF genes & repeats", "Medium Aquamarine", 
             rgb(102,205,170, maxColorValue=255),
        "9_Het", "Heterochromatin", "PaleTurquoise", rgb(138,145,208, 
             maxColorValue=255),
        "10_TssBiv", "Bivalent/Poised TSS", "IndianRed", 
             rgb(205,92,92, maxColorValue=255),
        "11_BivFlnk", "Flanking Bivalent TSS/Enh", "DarkSalmon", 
             rgb(233,150,122, maxColorValue=255),
        "12_EnhBiv", "Bivalent Enhancer", "DarkKhaki", 
             rgb(189,183,107, maxColorValue=255),
        "13_ReprPC", "Repressed PolyComb", "Silver", 
             rgb(128,128,128, maxColorValue=255),
        "14_ReprPCWk", "Weak Repressed PolyComb", "Gainsboro", 
             rgb(192,192,192, maxColorValue=255),
        "15_Quies", "Quiescent/Low", "White", rgb(255,255,255, maxColorValue=255)), 
             byrow=TRUE, nrow=15), stringsAsFactors=FALSE)
    colnames(map) <- c("abbr", "name", "color_name", "color_code")
    
    ##perform the mapping
    toMatch <- mcols(gr)$name
    newdf <- map[match(toMatch, map$abbr),]
    mcols(gr) <- newdf
    gr
}
