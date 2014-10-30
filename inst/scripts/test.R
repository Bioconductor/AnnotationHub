## Things to do with the new hub

## 1) fix the downloads (currently broken)
## 2) get working set of examples scripted.
## 3) make working functions for missing stuff.

## load
library(AnnotationHub)
ah <- AnnotationHub()

## show
ah

## get metadata for first value
ah[1]

## extract 1st value
foo = ah[[1]]

## extract 2nd value (problem with back end?)
bar = ah[[2]]


## subset ah by genome
ahs <- ah[grep('ailMel1', ah$genome)]


## subset it by tags
ahs <- ah[grep('broadPeak', ah$tags)]                

## or subset by names (if known)
ah['AH3']
ah[c('AH3','AH4')]


## cache up values from the web. 
cache(ah[1:3])  

cache(ah[[3]]) ## TROUBLE???: but this doesn't really make sense does it?
## Shouldn't the thing passed to cache always be a hub object???

## So shouldn't it really be more like this?
cache(ah[3])


## vs this which seems to work without a hitch:
baz <- ah[[5]]

## use query to subset 
ahs <- query(ah, 'inparanoid8')     ## This is fine! (arguments were flipped)
# trace("query", browser, signature ="AnnotationHub")
foo <- ahs[[1]] 

## use subset in the traditional way
ahsub <- subset(ah, ah$genome=='ailMel1')


## ADD:
## display
## possibleDates -- compute on client SELECT DISTINCT rdatadateadded FROM...

## snapshotDate (setter and getter)

## snapshotVersion -- i.e. BiocInstaller::biocVersion()

## and hubUrl (https://annotationhub.bioconductor.org/)



########################
## MORE things to test:
## 
## Tests for different file types to each come down and be A-OK
## resource ID for:
## VCF: 7220                
foo = ah[['7220']]

## GRanges: 522             
foo = ah[['522']]

## data.frame: 7831         
foo = ah[['7831']]

## SQLite: 10366            
foo = ah[['10366']]

## fasta file: 3            ## This example is HUGE (need smaller one)
foo = ah[['3']]


#################################################
## Test for verifying that my change works.
## 
## Testing 1st requires that I can do this (because the recipe is old)
## select id,rdatapath,resource_id from rdatapaths where resource_id ='3';
## (and get two records)
##
## To use gamay do this:
## options(ANNOTATION_HUB_URL="http://gamay:9393")
library(AnnotationHub); 
ah = AnnotationHub(); 
#trace(".get1", browser, signature = "FaFileResource")
#debug(AnnotationHub:::.AnnotationHub_get1)
##trace("show", browser, signature = "AnnotationHub")
fa = ah[['3']]

## test the 'repaired' ensembl 75 fasta files.
fa = ahs[['10725']]

## Now I just need to change it so that it uses the ah_id from the resources 
## table (instead of the actual 'id')

##################################################
## code to hack in a value for the fasta file for ah[['3']]
## BASICALLY you have to add a row to rdatapaths such that 
## something like this:
## SELECT * FROM rdatapaths WHERE resource_id  = '3' limit 4;
## will return more than one row
## This appears to be on default prod now for at least one resource. 
## BUT: what is the file that is called 15107 in my cache?  How are these named?
## Is it the id from rdatapaths?  Or the resource_id?  
## And shouldn't it be the AHID from resources?  
## OR perhaps should the AHID be associated with the value from rdatapaths?
##################################################
## The answer is that it's the 'id' from rdatapaths that you want to used for 
## cached files (and that the methods should access).  And for the UI,
## the user should type in the 'AHID' names (and names should be modified to 
## use this)

##################################################
## The SQL to fix existing records needs to just add one row to rdatapaths for 
## each .fa file in the resources table
## Subquery:
## SELECT rdatapath || '.fai', rdataclass, resource_id FROM rdatapaths 
## WHERE rdataclass='FaFile';
## 
## 1st drop the 'extra' one
## DELETE FROM rdatapaths where id = 15107;
##
## Full query:
## INSERT INTO rdatapaths (rdatapath, rdataclass, resource_id)  
## SELECT rdatapath || '.fai', rdataclass, resource_id FROM rdatapaths 
## WHERE rdataclass='FaFile';

## But for MySQL the subquery will probably look more like this:
## SELECT concat(rdatapath, '.fai'), rdataclass, resource_id FROM rdatapaths 
## WHERE rdataclass='FaFile';

## Full MySQL query:
## INSERT INTO rdatapaths (rdatapath, rdataclass, resource_id)  
## SELECT concat(rdatapath, '.fai'), rdataclass, resource_id FROM rdatapaths 
## WHERE rdataclass='FaFile';




##################################################
## SQL Code to correct the missing version bump
## SELECT DISTINCT '3.1', resource_id FROM biocversions limit 4;
#########
## INSERT INTO biocversions (biocversion, resource_id) SELECT 
## DISTINCT '3.1', resource_id FROM biocversions;



#############################################################
## Instructions for connecting to the back end DB directly:
## on gamay just connect to the DB like this:
## mysql -p -u ahuser annotationhub
## 
##
## And for production just log in FIRST
## ssh ubuntu@annotationhub.bioconductor.org # it has your key
## 
## Then back up the DB like so:
## mysqldump -p -u ahuser annotationhub | gzip > dbdump_201410211449.sql.gz
## 
## And proceed as above.
## 





#############################################################
## For browsing around the data resources:





#################################################################
## Switching from numbers to AH IDs.
## show method fails because subsetting is now wonky
## debug(AnnotationHub:::.show)
## subsetting probably expects that the numbers in names() can be 
## used as indices somehow...

## trace("[", browser, signature = c("AnnotationHub", "numeric", "missing"))
## ah[2]

library(AnnotationHub)
ah <- AnnotationHub()
#trace("[", browser, signature = c("AnnotationHub", "character", "missing"))
ah['AH3']
#trace("[", browser, signature = c("AnnotationHub", "numeric", "missing"))
trace("[<-", browser, signature = c("AnnotationHub", "numeric", "missing", "AnnotationHub"))
ah[2]

## Problem with show method for single bracket subsets...
## Happens with single items and in the case below where (one item is dropped)
## debug(AnnotationHub:::.show)
ash = ah[1:3]
ash


## This works now
trace("[[", browser, signature = c("AnnotationHub", "character", "missing"))
debug(AnnotationHub:::.AnnotationHub_get1)
ah[['AH3']]

## TODO: this doesn't work (yet) - low priority though
ah[[c('AH3','AH4')]]

## is the same as:
ah[[2]]


