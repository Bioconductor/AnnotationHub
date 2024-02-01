##
##  Start by investigating what we list as "Public" available but no
##  rdatadateremoved 
##

load("ehresults.RData")
dim(eh_bad)
## 20 5
eh_bad
## 1-6 https://recount.bio/data/ SSL certificate error (asked recountmethylation
## package maintainer to reach out to webmaster of site)
##
## 7-8 case sensitivity issue -- renames on Azure
##
## 9-16 https://auckland.figshare.com these seem to be available when manually
## navigated too manually. Further investigation needed? homosapienDEE2CellScore
## false positive? maybe HEAD not activated on this server?
##
## 17-20 -- leukemiaAtlas package upload was a mess, seems wrong and may have to
## delete and reupload/add to database
## 




load("ahresults.RData")
## 12487     5
ah_bad

prefix <- unique(ah_bad$location_prefix)
prefix
## [1] "https://bioconductorhubs.blob.core.windows.net/annotationhub/"
## [2] "http://amp.pharm.mssm.edu/"                                   
## [3] "http://www.pazar.info/"                                       
## [4] "http://haemcode.stemcells.cam.ac.uk/"                         
## [5] "http://egg2.wustl.edu/roadmap/data/byFileType/"               
## [6] "ftp://ftp.pride.ebi.ac.uk/"                                   
## [7] "ftp://ftp.ebi.ac.uk/pub/databases/gencode/"                   
## [8] "ftp://ftp.ensembl.org/pub/"                                   
## [9] "ftp://ftp.ncbi.nlm.nih.gov/"

for(i in prefix){
    message(i, ": ", length(which(ah_bad$location_prefix == i)))
}
## https://bioconductorhubs.blob.core.windows.net/annotationhub/: 61
## http://amp.pharm.mssm.edu/: 1
## http://www.pazar.info/: 91
## http://haemcode.stemcells.cam.ac.uk/: 16
## http://egg2.wustl.edu/roadmap/data/byFileType/: 39
## ftp://ftp.pride.ebi.ac.uk/: 1
## ftp://ftp.ebi.ac.uk/pub/databases/gencode/: 31
## ftp://ftp.ensembl.org/pub/: 12231
## ftp://ftp.ncbi.nlm.nih.gov/: 16


"http://amp.pharm.mssm.edu/"
## 1 file
## unreachable 404 :  recommend removing AH22237 : listed maintainer is
## Bioconductor Maintainer

"ftp://ftp.pride.ebi.ac.uk/"
## 1 file 
## Other files at this endpoint are okay. AH49009 : Laurent Gatto maintainer and
## mentioned in ProteomicsAnnotationHubData that was deprecated in
## 3.16. contacted

"http://haemcode.stemcells.cam.ac.uk/"
## 16 files
## HAEMCODE has moved!! Redirected "https://codex.stemcells.cam.ac.uk/"
## HaemCodeImportPreparer, Bioconductor Maintainer
## But there are 945 haemcode references in the hub, only these 16 are not
## accessible? "AH27635"-"AH28579". All 945 use the same location_prefix listed
## above.
## Bad Ids:
##    AH28151, AH28152, AH28153, AH28154, AH28155,
##    AH28257,
##    AH28262, AH28263,
##    AH28466, AH28467, AH28468, AH28469, AH28470,
##    AH28572,
##    AH28577, AH28578
##    

"ftp://ftp.ncbi.nlm.nih.gov/"
## 16 files
## dbSNPVCFPreparer, Bioconductor Maintainer
## Corresponds to 8 records because they are vcf.gz and vcf.gz.tbi
## AH57956-AH57963  
## is it an issue with ftp://ftp as when manually redirect to https site
## https://ftp.ncbi.nlm.nih.gov/pub/clinvar/vcf_GRCh38/archive_1.0/2016/
## exists. Would switching to this base url interfer with load see
## AnnotationHubData:::ncbi_dbSNPVCFFile


"ftp://ftp.ebi.ac.uk/pub/databases/gencode/"
## 31 files
## GencodeGffImportPreparer, Bioconductor Maintainer
## AH49545-AH49553  recipe is blank
## AH49554-AH49562  recipe is blank
## AH75118-AH75126  recipe is AnnotationHubData:::gencodeGFFToGRanges
## AH75127-AH75130  recipe is AnnotationHubData:::gencodeGFFToGRanges
## Newer package GenomicScores added some repeat/newer data. Maybe these are not
## needed anymore?
## will changing from ftp://ftp to the available https break load recipe?
## https://ftp.ebi.ac.uk/pub/databases/gencode/Gencode_mouse/release_M6/
## https://ftp.ebi.ac.uk/pub/databases/gencode/Gencode_human/release_23/
## https://ftp.ebi.ac.uk/pub/databases/gencode/Gencode_human/release_31/
## https://ftp.ebi.ac.uk/pub/databases/gencode/Gencode_human/release_31/GRCh37_mapping/

"http://egg2.wustl.edu/roadmap/data/byFileType/"
## 39 files
## Many more at this endpoint available
## All these files are still available but have changed to a v2 only
## example: peaks/consolidated/narrowPeak/E003-DNase.hotspot.all.peaks.v2.bed.gz
## solution: update rdatapath manually in database for these records
ah_bad[which(ah_bad$location_prefix == "http://egg2.wustl.edu/roadmap/data/byFileType/"), "rdatapath"]
##  [1] "peaks/consolidated/narrowPeak/E003-DNase.hotspot.all.peaks.bed.gz"
##  [2] "peaks/consolidated/narrowPeak/E004-DNase.hotspot.all.peaks.bed.gz"
##  [3] "peaks/consolidated/narrowPeak/E005-DNase.hotspot.all.peaks.bed.gz"
##  [4] "peaks/consolidated/narrowPeak/E006-DNase.hotspot.all.peaks.bed.gz"
##  [5] "peaks/consolidated/narrowPeak/E007-DNase.hotspot.all.peaks.bed.gz"
##  [6] "peaks/consolidated/narrowPeak/E008-DNase.hotspot.all.peaks.bed.gz"
##  [7] "peaks/consolidated/narrowPeak/E017-DNase.hotspot.all.peaks.bed.gz"
##  [8] "peaks/consolidated/narrowPeak/E021-DNase.hotspot.all.peaks.bed.gz"
##  [9] "peaks/consolidated/narrowPeak/E022-DNase.hotspot.all.peaks.bed.gz"
## [10] "peaks/consolidated/narrowPeak/E028-DNase.hotspot.all.peaks.bed.gz"
## [11] "peaks/consolidated/narrowPeak/E032-DNase.hotspot.all.peaks.bed.gz"
## [12] "peaks/consolidated/narrowPeak/E033-DNase.hotspot.all.peaks.bed.gz"
## [13] "peaks/consolidated/narrowPeak/E050-DNase.hotspot.all.peaks.bed.gz"
## [14] "peaks/consolidated/narrowPeak/E051-DNase.hotspot.all.peaks.bed.gz"
## [15] "peaks/consolidated/narrowPeak/E055-DNase.hotspot.all.peaks.bed.gz"
## [16] "peaks/consolidated/narrowPeak/E056-DNase.hotspot.all.peaks.bed.gz"
## [17] "peaks/consolidated/narrowPeak/E057-DNase.hotspot.all.peaks.bed.gz"
## [18] "peaks/consolidated/narrowPeak/E059-DNase.hotspot.all.peaks.bed.gz"
## [19] "peaks/consolidated/narrowPeak/E080-DNase.hotspot.all.peaks.bed.gz"
## [20] "peaks/consolidated/narrowPeak/E081-DNase.hotspot.all.peaks.bed.gz"
## [21] "peaks/consolidated/narrowPeak/E082-DNase.hotspot.all.peaks.bed.gz"
## [22] "peaks/consolidated/narrowPeak/E083-DNase.hotspot.all.peaks.bed.gz"
## [23] "peaks/consolidated/narrowPeak/E084-DNase.hotspot.all.peaks.bed.gz"
## [24] "peaks/consolidated/narrowPeak/E085-DNase.hotspot.all.peaks.bed.gz"
## [25] "peaks/consolidated/narrowPeak/E086-DNase.hotspot.all.peaks.bed.gz"
## [26] "peaks/consolidated/narrowPeak/E088-DNase.hotspot.all.peaks.bed.gz"
## [27] "peaks/consolidated/narrowPeak/E089-DNase.hotspot.all.peaks.bed.gz"
## [28] "peaks/consolidated/narrowPeak/E090-DNase.hotspot.all.peaks.bed.gz"
## [29] "peaks/consolidated/narrowPeak/E091-DNase.hotspot.all.peaks.bed.gz"
## [30] "peaks/consolidated/narrowPeak/E092-DNase.hotspot.all.peaks.bed.gz"
## [31] "peaks/consolidated/narrowPeak/E093-DNase.hotspot.all.peaks.bed.gz"
## [32] "peaks/consolidated/narrowPeak/E094-DNase.hotspot.all.peaks.bed.gz"
## [33] "peaks/consolidated/narrowPeak/E097-DNase.hotspot.all.peaks.bed.gz"
## [34] "peaks/consolidated/narrowPeak/E098-DNase.hotspot.all.peaks.bed.gz"
## [35] "peaks/consolidated/narrowPeak/E100-DNase.hotspot.all.peaks.bed.gz"
## [36] "peaks/consolidated/narrowPeak/E109-DNase.hotspot.all.peaks.bed.gz"
## [37] "peaks/consolidated/narrowPeak/E029-DNase.hotspot.all.peaks.bed.gz"
## [38] "peaks/consolidated/narrowPeak/E034-DNase.hotspot.all.peaks.bed.gz"
## [39] "peaks/consolidated/narrowPeak/E046-DNase.hotspot.all.peaks.bed.gz"


"https://bioconductorhubs.blob.core.windows.net/annotationhub/"
## 61 files
## ncbi and ensebml are Bioconductor maintained and added either at release or
## when new ensembl version is available
##
## EuPathDB and ontoProcData are contributed
##
## Could be an issue with special characters that needs manual adjustmenet with
## url coding 
## Or could have had corrupt upload/not existed and remove from database

ah_bad[which(ah_bad$location_prefix == "https://bioconductorhubs.blob.core.windows.net/annotationhub/"), "rdatapath"]
##  [1] "ncbi/org.Coprinopsis_cinerea_okayama7#130.eg.sqlite"                                                 
##  [2] "ncbi/org.Eremothecium_cymbalariae_DBVPG#7215.eg.sqlite"                                              
##  [3] "ncbi/uniprot/org.Coprinopsis_cinerea_okayama7#130.eg.sqlite"                                         
##  [4] "ncbi/uniprot/org.Eremothecium_cymbalariae_DBVPG#7215.eg.sqlite"                                      
##  [5] "ncbi/uniprot/3.5/org.Coprinopsis_cinerea_okayama7#130.eg.sqlite"                                     
##  [6] "ncbi/uniprot/3.5/org.Eremothecium_cymbalariae_DBVPG#7215.eg.sqlite"                                  
##  [7] "ncbi/uniprot/3.6/org.Coprinopsis_cinerea_okayama7#130.eg.sqlite"                                     
##  [8] "ncbi/uniprot/3.6/org.Eremothecium_cymbalariae_DBVPG#7215.eg.sqlite"                                  
##  [9] "ensembl/release-92/fasta/cebus_capucinus/cdna/Cebus_capucinus.Cebus_imitator-1.0.cdna.all.2bit"      
## [10] "ensembl/release-92/fasta/cebus_capucinus/dna/Cebus_capucinus.Cebus_imitator-1.0.dna_rm.toplevel.2bit"
## [11] "ensembl/release-92/fasta/cebus_capucinus/dna/Cebus_capucinus.Cebus_imitator-1.0.dna_sm.toplevel.2bit"
## [12] "ensembl/release-92/fasta/cebus_capucinus/ncrna/Cebus_capucinus.Cebus_imitator-1.0.ncrna.2bit"        
## [13] "ncbi/uniprot/3.7/org.Coprinopsis_cinerea_okayama7#130.eg.sqlite"                                     
## [14] "ncbi/uniprot/3.7/org.Coprinus_cinereus_okayama7#130.eg.sqlite"                                       
## [15] "ncbi/uniprot/3.7/org.Eremothecium_cymbalariae_DBVPG#7215.eg.sqlite"                                  
## [16] "EuPathDB/OrgDb/3.8/org.Coprinopsis_cinerea_okayama7#130.fungi.db.sqlite"                             
## [17] "EuPathDB/OrgDb/3.8/org.Albugo_laibachii_Nc14.fungi.db.sqlite"                                        
## [18] "EuPathDB/GRanges/3.8/GRanges.Coprinopsis_cinerea_okayama7#130.fungidb39.rda"                         
## [19] "ncbi/uniprot/3.8/org.Coprinopsis_cinerea_okayama7#130.eg.sqlite"                                     
## [20] "ncbi/uniprot/3.8/org.Coprinus_cinereus_okayama7#130.eg.sqlite"                                       
## [21] "ncbi/uniprot/3.9/org.Coprinopsis_cinerea_okayama7#130.eg.sqlite"                                     
## [22] "ncbi/uniprot/3.9/org.Coprinus_cinereus_okayama7#130.eg.sqlite"                                       
## [23] "ncbi/uniprot/3.10/org.Coprinopsis_cinerea_okayama7#130.eg.sqlite"                                    
## [24] "ncbi/uniprot/3.10/org.Coprinus_cinereus_okayama7#130.eg.sqlite"                                      
## [25] "ncbi/uniprot/3.11/org.Coprinopsis_cinerea_okayama7#130.eg.sqlite"                                    
## [26] "ncbi/uniprot/3.11/org.Coprinus_cinereus_okayama7#130.eg.sqlite"                                      
## [27] "ensembl/release-100/fasta/cercocebus_atys/cdna/Cercocebus_atys.Caty_1.0.cdna.all.2bit"               
## [28] "ensembl/release-100/fasta/equus_caballus/cdna/Equus_caballus.EquCab3.0.cdna.all.2bit"                
## [29] "ensembl/release-100/fasta/ficedula_albicollis/ncrna/Ficedula_albicollis.FicAlb_1.4.ncrna.2bit"       
## [30] "ensembl/release-100/fasta/fukomys_damarensis/ncrna/Fukomys_damarensis.DMR_v1.0.ncrna.2bit"           
## [31] "ensembl/release-100/fasta/hippocampus_comes/cdna/Hippocampus_comes.H_comes_QL1_v1.cdna.all.2bit"     
## [32] "ensembl/release-100/fasta/nothoprocta_perdicaria/cdna/Nothoprocta_perdicaria.notPer1.cdna.all.2bit"  
## [33] "ensembl/release-100/fasta/taeniopygia_guttata/cdna/Taeniopygia_guttata.bTaeGut1_v1.p.cdna.all.2bit"  
## [34] "ncbi/uniprot/3.12/org.Coprinopsis_cinerea_okayama7#130.eg.sqlite"                                    
## [35] "ncbi/uniprot/3.12/org.Coprinus_cinereus_okayama7#130.eg.sqlite"                                      
## [36] "ncbi/uniprot/3.13/org.Coprinopsis_cinerea_okayama7#130.eg.sqlite"                                    
## [37] "ncbi/uniprot/3.13/org.Coprinus_cinereus_okayama7#130.eg.sqlite"                                      
## [38] "ncbi/uniprot/3.14/org.Coprinopsis_cinerea_okayama7#130.eg.sqlite"                                    
## [39] "ncbi/uniprot/3.14/org.Coprinus_cinereus_okayama7#130.eg.sqlite"                                      
## [40] "ncbi/uniprot/3.15/org.Coprinopsis_cinerea_okayama7#130.eg.sqlite"                                    
## [41] "ncbi/uniprot/3.15/org.Coprinus_cinereus_okayama7#130.eg.sqlite"                                      
## [42] "ncbi/uniprot/3.16/org.Coprinopsis_cinerea_okayama7#130.eg.sqlite"                                    
## [43] "ncbi/uniprot/3.16/org.Coprinus_cinereus_okayama7#130.eg.sqlite"                                      
## [44] "ontoProcData/mondo_2022.12.01.rda"                                                                   
## [45] "ncbi/uniprot/3.17/org.Esox_lucius.eg.sqlite"                                                         
## [46] "ncbi/uniprot/3.17/org.Rhinolophus_sinicus.eg.sqlite"                                                 
## [47] "ncbi/uniprot/3.17/org.Hippoglossus_hippoglossus.eg.sqlite"                                           
## [48] "ncbi/uniprot/3.17/org.Agelaius_phoeniceus.eg.sqlite"                                                 
## [49] "ncbi/uniprot/3.17/org.Leptinotarsa_decemlineata.eg.sqlite"                                           
## [50] "ncbi/uniprot/3.17/org.Leptinotarsa_decimlineata.eg.sqlite"                                           
## [51] "ncbi/uniprot/3.17/org.Struthio_australis.eg.sqlite"                                                  
## [52] "ncbi/uniprot/3.17/org.Struthio_camelus_australis.eg.sqlite"                                          
## [53] "ncbi/uniprot/3.17/org.Drosophila_mauritiana.eg.sqlite"                                               
## [54] "ncbi/uniprot/3.17/org.Geospiza_fortis.eg.sqlite"                                                     
## [55] "ncbi/uniprot/3.17/org.Stilbospora_angustata.eg.sqlite"                                               
## [56] "ncbi/uniprot/3.17/org.Lophyrus_lecontei.eg.sqlite"                                                   
## [57] "ncbi/uniprot/3.17/org.Neodiprion_lecontei.eg.sqlite"                                                 
## [58] "ncbi/uniprot/3.17/org.Papilio_brassicae.eg.sqlite"                                                   
## [59] "ncbi/uniprot/3.17/org.Coprinopsis_cinerea_okayama7#130.eg.sqlite"                                    
## [60] "ncbi/uniprot/3.17/org.Coprinus_cinereus_okayama7#130.eg.sqlite"                                      



"http://www.pazar.info/"
## 91 files
## PazarImportPreparer, Bioconductor maintained
## url hangs
## recipe is blank
## remove?



"ftp://ftp.ensembl.org/pub/"
## 12231 files
## seems release-84 to release-109
## these are gtf? are these granges on the fly?
## false positive?
