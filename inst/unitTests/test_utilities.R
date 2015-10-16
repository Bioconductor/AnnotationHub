test_require <- function() {
    checkException(AnnotationHub:::.require("xxx_foo"))
}
