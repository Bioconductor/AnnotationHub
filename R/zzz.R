.onLoad <- function(libname, pkgname)
{
  ns <- asNamespace(pkgname)
  AnnotationHub <- new("AnnotationHub",
                       curPath="resource",
                       paths=c("foo","fu"),
                       pattern="")
  assign("AnnotationHub", AnnotationHub, envir=ns)
  namespaceExport(ns, "AnnotationHub")
}



