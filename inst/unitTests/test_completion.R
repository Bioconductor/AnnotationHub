test_completion <- function()
{
    fun <- AnnotationHub:::.DollarNames.AnnotationHub
    x <- setNames(1:3, c("abc", "acd", "ade"))
    checkIdentical(names(x), fun(x, "^"))
    checkIdentical(names(x), fun(x, "^a"))
    checkIdentical(names(x)[2], fun(x, "^ac"))

    x <- setNames(1:3, c("aac", "aad", "ade"))
    exp <- c("aa ... [2]", "ade")
    checkIdentical(exp, fun(x, "^"))
    checkIdentical(exp, fun(x, "^a"))
    checkIdentical("ade", fun(x, "^ad"))

    x <- c("aaa", "aaa")
    fun1 <- AnnotationHub:::.longestCommonPrefix
    checkIdentical(setNames(length(x), x[[1]]), fun1(x))
}
