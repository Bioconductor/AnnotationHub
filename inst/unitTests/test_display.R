library(RUnit)
library(AnnotationHub)

test_display <- function() {
    message("Most excellent")
    #making a data.frame
    measrs <- data.frame(gender = c("M", "M",
    "F"), ht = c(172, 186.5, 165), Tags = c(91,
    99, 74))


    #check if apply works
    foo <- apply(measrs,2,unique)
    checkEquals(foo$gender, c("M","F"))

    #check if dropping a column works
    foo2 <- foo[-which(names(foo) %in% c("Tags"))]
    checkEquals(names(foo2), c("gender","ht"))
    #checkEquals(all(names(foo2) %in% c("gender","ht")))
}
