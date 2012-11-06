.onLoad <- function(libname, pkgname)
{
  ns <- asNamespace(pkgname)
  AnnotationHub <- new("AnnotationHub")
  assign("AnnotationHub", AnnotationHub, envir=ns)
  namespaceExport(ns, "AnnotationHub")
}
