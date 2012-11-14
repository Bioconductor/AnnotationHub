.onLoad <- function(libname, pkgname)
{
  ns <- asNamespace(pkgname)
  curPath <- "http://wilson2.fhcrc.org/cgi-bin/R/AnnotationHub"
  paths <- .retrieveNextPathVals(curPath)
  AnnotationHub <- new("AnnotationHub",
                       curPath=curPath,
                       paths=paths,
                       pattern="")
##                        curPathExtendedYet=FALSE)
  assign("AnnotationHub", AnnotationHub, envir=ns)
  namespaceExport(ns, "AnnotationHub")
}



