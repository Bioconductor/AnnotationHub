.onLoad <- function(libname, pkgname)
{
  ns <- asNamespace(pkgname)
  AnnotationHub <- new("AnnotationHub",
                        paths=c("foo.bar.","foo.baz.","foo.bar.sna."))
  assign("AnnotationHub", AnnotationHub, envir=ns)
  namespaceExport(ns, "AnnotationHub")
}



