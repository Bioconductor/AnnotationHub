.onLoad <- function(libname, pkgname)
{
  ns <- asNamespace(pkgname)
  curPath <- "http://wilson2.fhcrc.org/cgi-bin/R/AnnotationHub"
  paths <- .retrievePathVals(curPath)
  AnnotationHub <- new("AnnotationHub",
                       curPath=curPath,
                       paths=paths,
                       pattern="")
  assign("AnnotationHub", AnnotationHub, envir=ns)
  namespaceExport(ns, "AnnotationHub")
}



