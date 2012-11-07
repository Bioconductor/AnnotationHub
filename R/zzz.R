.onLoad <- function(libname, pkgname)
{
  ns <- asNamespace(pkgname)
  AnnotationHub <- new("AnnotationHub",
                       curPath="resource",
                       paths=c("foo","fu"),
                       pattern="",
                       curPathExtendedYet=FALSE)
  assign("AnnotationHub", AnnotationHub, envir=ns)
  namespaceExport(ns, "AnnotationHub")
}



