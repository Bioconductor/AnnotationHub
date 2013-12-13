library(RUnit)
library(AnnotationHub)

test_display <- function() {
    cat("Most excellent")
    #making a data.frame
    measures <- data.frame(gender = c("M", "M",
    "F"), ht = c(172, 186.5, 165), Tags = c(91,
    99, 74))

    #check if apply works
    uniqMeasures <- apply(measures,2,unique)
    checkEquals(uniqMeasures$gender, c("M","F"))

    #check if dropping a column works
    noTags <- uniqMeasures[-which(names(uniqMeasures) %in% c("Tags"))]
    checkEquals(names(noTags), c("gender","ht"))
    #checkEquals(all(names(foo2) %in% c("gender","ht")))
}
