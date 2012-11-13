.onLoad <- function(libname, pkgname)
{
  ns <- asNamespace(pkgname)
  curPath <- "http://ec2-174-129-103-37.compute-1.amazonaws.com/cgi-bin/R/AnnotationHub/"
  paths <- .retrieveNextPathVals(curPath)
  AnnotationHub <- new("AnnotationHub",
                       curPath=curPath,
                       paths=paths,
                       pattern="",
                       curPathExtendedYet=FALSE)
  assign("AnnotationHub", AnnotationHub, envir=ns)
  namespaceExport(ns, "AnnotationHub")
}



