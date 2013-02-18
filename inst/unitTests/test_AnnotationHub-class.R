test_AnnotationHub_constructors <- function()
{
    checkTrue(validObject(new("AnnotationHub")))
    checkTrue(validObject(AnnotationHub:::.AnnotationHub()))
}
