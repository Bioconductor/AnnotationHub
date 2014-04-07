test_AnnotationHub_constructors <- function()
{
    checkTrue(validObject(new("AnnotationHub")))
    checkTrue(validObject(AnnotationHub:::.AnnotationHub()))
}

test_subset <- function() {
    x <- AnnotationHub()
    checkIdentical(x, x[])
    checkIdentical(names(x)[1:5], names(x[1:5]), )
    checkIdentical(names(x)[1:5], names(x[names(x)[1:5]]))
    checkIdentical(names(x)[-(1:5)], names(x[-(1:5)]))
    checkIdentical(names(x)[-(1:5)], names(x[names(x)[-(1:5)]]))

    exp <- "unknown index'__unknown__'"
    obs <- tryCatch(x["__unknown__"], error=conditionMessage)
    checkIdentical(exp, obs)
    idx <- c(head(names(x)), "__unknown__")
    obs <- tryCatch(x[idx], error=conditionMessage)
    checkIdentical(exp, obs)
}

test_metadata <- function() {
    x <- AnnotationHub()
    columns <- colnames(metadata(x))
    ncol <- length(columns)
    checkIdentical(c(length(x), ncol), dim(metadata(x)))
    checkIdentical(c(5L, ncol), dim(metadata(x[1:5])))
    checkIdentical(c(1L, ncol), dim(metadata(x[1]))) # don't drop
    cols <- sample(columns, 3)
    checkIdentical(cols, colnames(metadata(x, cols))) # respect order

    exp <- "'columns' argument values not in columns(): '__unknown__'"
    obs <- tryCatch(metadata(x, "__unknown__"), error=conditionMessage)
    checkIdentical(exp, obs)
}
